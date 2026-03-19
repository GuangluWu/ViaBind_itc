# ITCSuite / ViaBind 代码审查报告

> 审查范围：`desktop/src/main/` (Electron 桌面壳)、`ITCSuiteWeb/app.R` + `R/` helpers、`launch_shiny.R`
> 审查时间：2026-03-19

---

## 一、Bug 与硬伤（应修复）

### 1. 🔴 `launch_shiny.R` 端口先发 READY 后绑定 — 竞态窗口

[launch_shiny.R#L165-L180](file:///Users/guanglu/scripts/ITCSuite/ITCSuiteWeb/scripts/launch_shiny.R#L165-L180)

```r
cat(sprintf("ITCSUITE_READY %s\n", jsonlite::toJSON(payload, auto_unbox = TRUE)))
flush.console()
# ... 然后才执行
shiny::runApp(appDir = app_path, host = ..., port = ...)
```

**问题**：`READY` 信号在 `runApp()` **之前**就发出了。Electron 侧的 `waitForBackendReady()` 会立即尝试 HTTP probe，但此时 Shiny HTTP server 尚未启动监听，导致：
- Electron 进入 120 次 × 500ms 的轮询循环，白白等待最多 60 秒
- 在慢机器上可能在 probe 完成前 Shiny 启动超时

**建议**：将 `ITCSUITE_READY` 输出移到 `shiny::runApp()` **之后**，或者使用 `shiny::onStart()`/`options(shiny.launch.browser = ...)` 等回调在 Shiny 实际绑定端口后再发 READY。

---

### 2. 🔴 `resolveRscript()` 非 macOS-packaged 路径下取第一个候选跳过验证

[index.js#L201-L205](file:///Users/guanglu/scripts/ITCSuite/desktop/src/main/index.js#L201-L205)

```js
for (const candidate of resolvedCandidates) {
  return candidate; // 立即返回第一个存在的候选
}
```

**问题**：仅在 `darwin + app.isPackaged` 分支才会做 `probeRscriptCandidate` 探针验证。在 **Windows packaged** 或 **Linux** 场景，取到的第一个 Rscript 可能指向损坏/不兼容的运行时，但代码直接返回，不做任何验证即使用。

**建议**：对 `win32` packaged 场景也执行 probe 验证，或至少做一个基础可执行性检查（`spawnSync` 跑 `--version`）。

---

### 3. 🟡 `errorHtml()` 存在 XSS 注入风险

[index.js#L802-L827](file:///Users/guanglu/scripts/ITCSuite/desktop/src/main/index.js#L802-L827)

```js
function errorHtml(message, logPath) {
  return `...
    <pre>${message}</pre>
    <p>Log file: <code>${logPath}</code></p>
  ...`;
}
```

**问题**：`message` 和 `logPath` 未做 HTML 转义就直接注入 HTML。虽然在 Electron context isolation 下风险有限，但恶意 R 侧输出或含特殊字符的路径名（如 `<script>`）仍可能破坏页面渲染或造成内容注入。

**建议**：对这两个变量做 HTML 实体转义（`<` → `&lt;`，`>` → `&gt;`，`&` → `&amp;`）。

---

### 4. 🟡 `bridge_contract.R` 将 reject logger 存入 `.GlobalEnv` — 多 session 冲突

[bridge_contract.R#L28-L32](file:///Users/guanglu/scripts/ITCSuite/ITCSuiteWeb/R/bridge_contract.R#L28-L32)

```r
bridge_set_reject_logger <- function(fn = NULL) {
  assign("ITCSUITE_BRIDGE_REJECT_LOGGER", fn, envir = .GlobalEnv)
}
```

**问题**：Shiny 是单进程多 session 模型。当多个用户同时连接时，后连接的 session 会覆盖全局 logger，导致前一个 session 的 bridge 拒绝事件日志丢失或路由到错误的 session。虽然当前桌面版只有单用户，但如果将来多人使用 Web 版本，这是一个数据泄露/混串风险。

**建议**：将 reject logger 存入 `session$userData` 而非 `.GlobalEnv`。

---

### 5. 🟡 `uniqueDownloadPath()` 存在 TOCTOU 竞态

[index.js#L1184-L1195](file:///Users/guanglu/scripts/ITCSuite/desktop/src/main/index.js#L1184-L1195)

```js
while (fs.existsSync(candidate)) {
  candidate = path.join(downloadsPath, `${name} (${index})${ext}`);
  index += 1;
}
return candidate;
```

**问题**：`existsSync` 检查与实际文件写入之间存在 TOCTOU（Time-of-check Time-of-use）竞态条件。如果用户同时从另一个来源下载了同名文件，可能覆盖。不过由于后续使用 `dialog.showSaveDialogSync` 且有 `showOverwriteConfirmation`，实际风险很低。

---

### 6. 🟡 `loadShinyPage()` 重试 120 次 × 500ms 无日志

[index.js#L1512-L1527](file:///Users/guanglu/scripts/ITCSuite/desktop/src/main/index.js#L1512-L1527)

```js
async function loadShinyPage(url) {
  const attempts = 120;
  for (let i = 1; i <= attempts; i += 1) {
    try {
      await mainWindow.loadURL(url);
      return;
    } catch (error) {
      if (i === attempts) throw error;
      await delay(500);
    }
  }
}
```

**问题**：静默重试 120 次（最多 60 秒），期间不输出任何诊断日志。如果出问题，完全无法从日志中看到发生了什么。而且每次调用 `mainWindow.loadURL()` 失败可能意味着 renderer 被频繁 abort/reload，对 renderer 进程有副作用。

**建议**：
1. 每 N 次重试记录一次日志
2. 减少重试次数，因为 `waitForBackendReady()` 已经做了 90s 等待

---

## 二、潜在风险

### 7. ⚠️ 同步文件 I/O 在主进程中频繁调用

多处使用 `fs.appendFileSync`（日志）、`fs.existsSync`、`fs.readFileSync`（图标 base64 编码）、`fs.mkdirSync` 等同步 API。在 Electron **主进程**中，同步 I/O 操作会阻塞事件循环：

- [index.js#L85](file:///Users/guanglu/scripts/ITCSuite/desktop/src/main/index.js#L85) — `appendFileSync` 每次写日志
- [index.js#L761](file:///Users/guanglu/scripts/ITCSuite/desktop/src/main/index.js#L761) — `readFileSync` 读取 1024×1024 PNG 并 base64 编码
- [diagnostics.js#L77](file:///Users/guanglu/scripts/ITCSuite/desktop/src/main/diagnostics.js#L77) — `readFileSync` 读取诊断日志内容

**风险**：通常在桌面场景影响不大，但如果日志文件或图标较大，或者在磁盘 I/O 繁忙时（如 NAS / 慢 USB 存储），可能导致短暂 UI 卡顿。

**建议**：
- 日志写入改为 `fs.appendFile`（异步）+ 写入队列/buffer
- 图标 base64 只在启动时做一次并缓存结果

---

### 8. ⚠️ `home_sleep_restore_store.R` 使用 RDS 格式 — 反序列化风险

[home_sleep_restore_store.R#L423-L439](file:///Users/guanglu/scripts/ITCSuite/ITCSuiteWeb/R/home_sleep_restore_store.R#L423-L439)

```r
loaded <- tryCatch(
  readRDS(path),
  ...
)
```

**问题**：`readRDS()` 可以反序列化任意 R 对象。如果 state 文件被篡改（例如用户操作或恶意程序替换），可能导致意外行为。虽然当前场景风险较低（本地桌面应用），但对于安全敏感环境不太理想。

**建议**：可考虑改用 JSON 格式存储状态，或在反序列化后做更严格的类型/结构验证（目前的 `normalize_state` 已经做了不少验证，这一点做得不错）。

---

### 9. ⚠️ `choose_port()` 使用固定范围随机端口无碰撞检测

[launch_shiny.R#L75-L83](file:///Users/guanglu/scripts/ITCSuite/ITCSuiteWeb/scripts/launch_shiny.R#L75-L83)

```r
as.integer(sample.int(20000L, size = 1L) + 30000L)
```

**问题**：端口范围 30001-50000 随机选择，但不检查端口是否可用。如果碰撞，`runApp` 会失败，进入重启循环。注释说 "let runApp fail fast on collision"，但实际上 Electron 侧会认为后端启动失败。

**建议**：用 `httpuv::startServer(port=0)` 让系统分配端口，或在 R 侧循环尝试绑定。

---

### 10. ⚠️ Electron `before-quit` 生命周期中 `event.preventDefault()` + 异步 `backend.stop()` 可能导致退出延迟

[index.js#L1648-L1662](file:///Users/guanglu/scripts/ITCSuite/desktop/src/main/index.js#L1648-L1662)

```js
app.on("before-quit", (event) => {
  if (isQuitting) return;
  isQuitting = true;
  event.preventDefault();
  Promise.resolve()
    .then(() => (backend ? backend.stop() : null))
    .finally(() => { app.quit(); });
});
```

**问题**：
- 如果 `backend.stop()` 的 SIGTERM 没有让 R 进程退出，5 秒后才 SIGKILL，用户需要等待 5 秒才能关闭应用
- 在 Windows 上，SIGTERM 不被 R 子进程响应（Windows 没有 SIGTERM 语义），可能造成 `backend.stop()` 总是等到 5 秒超时

**建议**：
- 在 Windows 上不发 SIGTERM，直接 `child.kill()`（即 SIGKILL/TerminateProcess）
- 或缩短超时时间

---

### 11. ⚠️ `load_legacy_app()` 从 `.GlobalEnv` 复制所有符号到模块环境

[app.R#L140-L144](file:///Users/guanglu/scripts/ITCSuite/ITCSuiteWeb/app.R#L140-L144)

```r
for (nm in ls(envir = .GlobalEnv, all.names = TRUE)) {
  if (!exists(nm, envir = env, inherits = FALSE)) {
    try(assign(nm, get(nm, envir = .GlobalEnv, inherits = FALSE), envir = env), silent = TRUE)
  }
}
```

**问题**：这个 "shotgun copy" 方式把 `.GlobalEnv` 中所有符号（包括不相关的）复制到每个 legacy app 的 env 中。虽然注释解释了这是为了兼容 legacy 代码的 `source(..., local = FALSE)` 模式，但：
1. 三个 legacy app 加载完成后，内存中存在三份全局环境的拷贝
2. 加载顺序会影响哪些符号被各个 env 看到

**建议**：这是一个已知的 legacy 兼容问题，短期内无法消除。但可以考虑用白名单机制只复制已知需要的符号。

---

### 12. ⚠️ 定时 autosave 每 15 秒一次，对大数据集可能造成 JSON/RDS 写入压力

[app.R#L2025-L2033](file:///Users/guanglu/scripts/ITCSuite/ITCSuiteWeb/app.R#L2025-L2033)

```r
observe({
  invalidateLater(15000, session)
  # ...
  shiny::isolate(request_sleep_restore_snapshot(reason = "periodic_autosave", source_event = "autosave"))
})
```

**问题**：每 15 秒执行一次全状态快照（含 Step1/Step2/Step3 所有数据），若用户加载了大的 ITC 数据集（数千行），每次快照序列化+写磁盘的开销可能显著。

**建议**：
1. 用 dirty flag 机制，仅在数据变化时才写快照
2. 增大间隔（如 30-60 秒）
3. 异步写入不阻塞 Shiny 反应式

---

## 三、性能优化建议

### 13. 💡 `resolveLoadingIconDataUrl()` 每次启动重新编码大图片

[index.js#L748-L766](file:///Users/guanglu/scripts/ITCSuite/desktop/src/main/index.js#L748-L766)

每次调用 `loadingHtml()` 时，都会同步读取 1024×1024 PNG 并 base64 编码。这个 data URL 会返回多次（初始加载页面、重启后端时、recovery 时）。

**建议**：在 `app.whenReady()` 时做一次编码并缓存为模块级变量。

---

### 14. 💡 `home_panel_ui` 完全使用 `renderUI` 重建整个 Home 面板

[app.R#L2104-L2308](file:///Users/guanglu/scripts/ITCSuite/ITCSuiteWeb/app.R#L2104-L2308)

每次语言切换或 import 记录变化时，整个 Home 面板的 HTML（包括联系信息、捐赠区域、引用信息等）都会被完全重新渲染和传输。

**建议**：
1. 将不变的静态部分（联系信息、引用等）拆分为独立的 `renderUI` 输出
2. 或使用 `uiOutput` + `conditionalPanel` 组合，仅重绘动态变化的 recent imports 表格
3. 利用 `session$sendCustomMessage` 对 tab labels 做部分更新（这已经做了，思路正确）

---

### 15. 💡 `with_recent_path_status()` 重复执行文件系统检查

[app.R#L1614-L1621](file:///Users/guanglu/scripts/ITCSuite/ITCSuiteWeb/app.R#L1614-L1621)

```r
with_recent_path_status <- function(records) {
  lapply(records, function(rec) {
    rec$path_exists <- recent_path_exists(rec$source_path)
    rec
  })
}
```

每次 `home_panel_ui` 渲染时调用，会对每条 import 记录做 `file.exists()` 检查。如果记录较多或路径在网络存储上，可能导致渲染延迟。

**建议**：缓存 path_exists 结果，仅在 record 首次或定时刷新时检查。

---

### 16. 💡 `probeRscriptCandidate()` 使用 `spawnSync` 同步阻塞 Electron 主进程

[index.js#L272-L315](file:///Users/guanglu/scripts/ITCSuite/desktop/src/main/index.js#L272-L315)

`spawnSync` 最多阻塞 15 秒。如果有多个候选 Rscript 都不可用，可能阻塞 Electron 主进程数十秒。

**建议**：改用异步 `spawn` + `Promise` 方式，或至少在启动时做一次缓存。

---

### 17. 💡 缺少 Content Security Policy (CSP) 配置

当前 Electron 窗口没有设置 CSP。虽然有 `contextIsolation: true` 和 `sandbox: true`，但缺少 CSP 意味着嵌入的 Shiny 页面可以加载任意外部脚本。

**建议**：通过 `session.defaultSession.webRequest.onHeadersReceived` 注入 CSP header，限制 script-src 和 connect-src。

---

## 四、代码质量 & 维护性

### 18. ✅ 亮点（做得好的部分）

| 方面 | 评价 |
|------|------|
| **输入校验** | 所有 bridge payload、IPC 消息、文件路径都有严格的 sanitize/normalize 处理，防御性编程做得很好 |
| **错误处理** | `tryCatch`/`try-catch` 使用广泛，很少有未捕获异常的路径 |
| **日志体系** | 有完整的结构化日志（main.log + app-events.log + backend.log），含 redaction 和 rotation |
| **诊断导出** | `diagnostics.js` 的隐私保护做得很好（路径脱敏、数据行过滤） |
| **Sleep/Wake 恢复** | 设计完整：suspend 快照 → pending 标记 → resume 消费 → autosave 兜底 → startup 恢复 |
| **双语 i18n** | 内联字典方案简洁实用，切换语言可持久化到 localStorage |
| **安全** | contextIsolation + sandbox + preload 通道白名单 + 导航限制，Electron 安全做得规范 |

### 19. 📌 维护性建议

- **`index.js` 1720 行过长**：建议拆分为多个模块（如 `backend-controller.js`、`power-events.js`、`menu.js`、`download.js`）
- **`ITCSuiteWeb/app.R` 2400 行过长**：server 函数内的 sleep-restore 逻辑约占 700 行，建议抽为独立模块
- **`trimScalar` 函数在 3 个文件中重复定义**：`index.js`, `preload.js`, `diagnostics.js` 各有一份，建议抽为共享 utility
- **`%||%` 在多个 R 文件中重复定义**：`app.R`, `bridge_contract.R`, `home_sleep_restore_store.R` 等都自定义了 `%||%`，建议集中定义一次

---

## 五、总结与优先级

| 优先级 | 编号 | 问题 | 类型 |
|--------|------|------|------|
| **P0** | #1 | `READY` 信号早于端口绑定 | Bug |
| **P1** | #2 | Windows/Linux 不做 Rscript 探针验证 | Bug |
| **P1** | #3 | `errorHtml` XSS 风险 | 安全 |
| **P1** | #4 | Bridge reject logger 全局变量冲突 | Bug |
| **P1** | #10 | Windows SIGTERM 无效导致退出延迟 | 兼容性 |
| **P2** | #7 | 同步 I/O 阻塞主进程 | 性能 |
| **P2** | #9 | 随机端口无碰撞检测 | 可靠性 |
| **P2** | #12 | 15 秒 autosave 对大数据集的写入压力 | 性能 |
| **P2** | #13 | 图标 base64 未缓存 | 性能 |
| **P2** | #16 | `spawnSync` 阻塞主进程 | 性能 |
| **P3** | #6 | `loadShinyPage` 重试无日志 | 可观测性 |
| **P3** | #14 | Home 面板完全重渲染 | 性能 |
| **P3** | #15 | `file.exists` 检查未缓存 | 性能 |
| **P3** | #17 | 缺少 CSP 配置 | 安全 |
