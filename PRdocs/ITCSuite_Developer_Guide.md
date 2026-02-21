# ITCSuite 开发者文档（Developer Guide）

> 品牌说明（Brand Note）  
> 本文档中的 `ITCSuite` 指 `ViaBind` 软件系列中的 ITC 套件（`ViaBind_ITCSuite`）。  
> 为保持与代码命名一致，正文继续使用 `ITCSuite`。

版本：v1.0  
日期：2026-02-21  
定位：面向开发者的上手、改造、测试、排障指南

---

## 1. 适用对象与目标

适用对象：

- 新加入项目的开发者
- 负责 Step1/Step2/Step3 功能迭代的工程师
- 需要定位跨模块问题的维护者

阅读目标：

- 当天跑通本地环境
- 能定位核心改动入口
- 能执行完整测试门禁
- 能处理常见运行故障

---

## 2. 仓库结构与模块职责

仓库根目录：仓库根目录（repository root，GitHub 首页）

核心目录：

- `ITCSuiteWeb`：宿主 Shiny 应用（统一入口、桥接、首页恢复）
- `ITCprocessor`：Step1（解析/基线/积分）
- `ITCsimfit`：Step2（模拟/拟合/诊断/导出）
- `ITCgraph`：Step3（绘图/导出）
- `desktop`：Electron 桌面壳
- `tests`：顶层测试门禁（unit/smoke/golden）
- `PRdocs`：项目文档

---

## 3. 开发环境与启动

### 3.1 基础依赖

- R 4.3+
- Node.js 20/22/24（推荐 24 LTS；不支持 Node 25）

可选环境自检（Step2 依赖）：

```bash
cd ITCsimfit
Rscript scripts/check_env.R
```

### 3.2 启动宿主应用（推荐）

在仓库根目录运行：

```bash
Rscript -e 'shiny::runApp("ITCSuiteWeb")'
```

### 3.3 启动单模块（按需）

```bash
Rscript -e 'shiny::runApp("ITCprocessor")'
Rscript -e 'shiny::runApp("ITCsimfit")'
Rscript -e 'shiny::runApp("ITCgraph")'
```

### 3.4 启动桌面壳

```bash
cd desktop
npm install
npm run dev
```

可用环境变量（desktop dev）：

- `ITCSUITE_RSCRIPT`：指定 `Rscript` 路径
- `ITCSUITE_REPO_ROOT`：覆盖 repo root
- `ITCSUITE_USE_BUNDLED_R=1`：开发态强制使用 bundled runtime

---

## 4. 关键架构约束

Step2 分层依赖方向（必须遵守）：

`Entrypoint -> Server Runtime -> Domain -> Infrastructure`

参考：`ITCsimfit/docs/phase2/module-boundaries.md`

禁止项：

- `Infrastructure -> Domain`
- `Domain -> Server Runtime`
- 无包装的跨模块可变全局状态

---

## 5. 核心开发任务与改动入口

### 5.1 新增或调整反应路径组合（Step2）

典型入口文件：

- 路径 UI 与选项值：`ITCsimfit/R/server/body/ui_i18n/02_ui_outputs_report.R`
- 条件参数控件显示：`ITCsimfit/ui.R`
- 求解与模拟方程：`ITCsimfit/R/core_logic.R`
- 参数映射与模拟包装：`ITCsimfit/R/fitting.R`
- 快照/导出中的 ActivePaths：`ITCsimfit/R/export_bundle_helpers.R`
- 从导入参数恢复路径：`ITCsimfit/R/bridge_step1_import.R`

建议流程：

1. 先改 UI 选项与显示逻辑
2. 再改核心方程与数值路径
3. 再补导入/导出/快照链路
4. 最后补测试（见第 8 节）

### 5.2 调整拟合策略（局部 optim / 全局 DEoptim）

关键文件：

- 拟合主逻辑：`ITCsimfit/R/server/body/runtime_core/02_simulation_fitting.R`
- 拟合按钮 UI：`ITCsimfit/R/server/body/ui_i18n/02_ui_outputs_report.R`

当前关键入口：

- `fit_full`：局部拟合路径（`perform_fitting(100)`）
- `fit_global`：全局 DE 路径（`perform_fitting(50, use_DE = TRUE)`）

变更建议：

- 优先保持两条路径并存（便于比较效率与全局性）
- 调参时同步更新进度文案、错误提示和报告输出
- 避免只改按钮不改后端参数边界

### 5.3 调整最近导入（Recent Imports）行为

你需要改的主要是 `ITCSuiteWeb`：

- 类型识别：`ITCSuiteWeb/R/home_recent_helpers.R`
- 持久化读写：`ITCSuiteWeb/R/home_recent_store.R`
- 记录写入/恢复主流程：`ITCSuiteWeb/app.R`

当前功能要点：

- 首页记录最近导入，覆盖 `itc` / `processed_xlsx` / `fitted_xlsx`
- 支持 Restore & Open 回到对应步骤复用文件
- 桌面态会写入 `state/home_recent_imports_v1.rds`，增强跨会话连续性

常用函数入口（`ITCSuiteWeb/app.R`）：

- `add_recent_import()`
- `get_recent_imports()`
- `restore_recent_record()`
- `register_restore_handler()`

### 5.4 调整导出工作簿结构（Step2 -> Step3）

关键文件：

- `ITCsimfit/R/export_bundle_helpers.R`
- `ITCgraph/R/bridge_plot_helpers.R`

注意事项：

- 修改 sheet 名称/顺序时，必须同步 Step3 读取逻辑
- 不要破坏 `fit_params` 中 `ActivePaths` 的恢复语义

---

## 6. 桥接与契约（Bridge Contracts）

关键 schema：

- Step1 -> Step2：`itcsuite.step1.v1`（含 `itcsuite.bundle.v1`）
- Step2 -> Step3：`itcsuite.step2_plot.v1`
- Recent Store：`itcsuite.home_recent.v1`

关键校验文件：

- `ITCSuiteWeb/R/bridge_contract.R`
- `ITCgraph/R/bridge_plot_helpers.R`

开发原则：

- 任何字段新增/改名，都要同步更新校验器和消费方
- payload 变更后必须补 smoke + golden 验证

---

## 7. 本地检查与测试门禁

### 7.1 一键门禁（推荐）

```bash
Rscript tests/run_all.R --strict
```

### 7.2 分套件运行

```bash
Rscript tests/run_unit.R --strict
Rscript tests/run_smoke.R --strict
Rscript tests/run_golden.R --strict
```

说明：

- `smoke` 依赖 `shinytest2`
- `golden` 对 `Injection / Ratio_App / heat_cal_mol` 做回归比较

### 7.3 Desktop 冒烟（可选）

```bash
cd desktop
npm run smoke:backend
npm run smoke
```

---

## 8. 测试补充建议（按改动类型）

- 改路径组合或方程：补 `ITCsimfit/tests/test_core_logic.R` 与 `ITCsimfit/tests/test_fitting.R`
- 改导入恢复或 recent imports：补 `ITCSuiteWeb/tests/testthat/test-home-recent-helpers.R`、`test-home-recent-store.R`、`test-home-desktop-helpers.R`
- 改桥接 schema：补 `ITCSuiteWeb/tests/testthat/test-bridge-contract.R`
- 改导出契约：补 `tests/golden` 用例并更新期望文件（如确属预期变化）

---

## 9. 常见问题排查（Troubleshooting）

### 9.1 Desktop 启动后看不到页面

先检查：

- `ITCSUITE_READY` / `ITCSUITE_ERROR` 是否正常输出
- 文件：`ITCSuiteWeb/scripts/launch_shiny.R`
- 解析端：`desktop/src/main/index.js`

### 9.2 Step2 全局拟合按钮报错

先检查：

- 是否安装 `DEoptim` 包
- 后端逻辑分支：`02_simulation_fitting.R` 中 `use_DE` 分支

### 9.3 最近导入恢复失败

先检查：

- 源路径是否仍存在（临时目录文件是否已被系统清理）
- `home_recent_store_load/save` 是否正常
- 对应步骤是否已注册 `register_restore_handler()`

### 9.4 Bridge payload 被拒绝

先检查：

- `schema_version` 是否匹配
- 必填字段是否缺失（如 `created_at`, `token`, `source`）
- 校验器：`ITCSuiteWeb/R/bridge_contract.R`

---

## 10. 开发提交检查清单（PR Checklist）

1. 代码改动是否遵守模块边界方向
2. 相关 schema 和消费端是否同步更新
3. `tests/run_all.R --strict` 是否通过
4. 文档是否更新（至少更新 `PRdocs` 中对应章节）
5. UI 文案改动是否处理中英一致性

---

## 11. 推荐阅读顺序

1. `PRdocs/ITCSuite_Core_Modules_Architecture.md`
2. `PRdocs/ITCSuite_External_Whitepaper.md`
3. `ITCsimfit/docs/phase2/module-boundaries.md`
4. `tests/README.md`
