[English](README.md)

[![Release](https://img.shields.io/github/v/release/GuangluWu/ViaBind_itc?display_name=tag)](https://github.com/GuangluWu/ViaBind_itc/releases/latest)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
![Platform: macOS | Windows](https://img.shields.io/badge/Platform-macOS%20%7C%20Windows-blue)

# ViaBind

ViaBind 是一个面向等温滴定量热（ITC）分析的开源桌面工作流。本仓库包含 Electron 桌面应用，以及用于数据处理、拟合和绘图的底层模块。

## 为什么是 ViaBind

- **端到端 ITC 工作流**：在同一个桌面应用中完成原始数据处理、模型拟合和论文级图形输出。
- **路径组合建模**：通过反应路径组合来构建和比较模型假设，而不是被固定单模型限制。
- **双优化策略**：并行提供局部优化（`optim`）和全局搜索（`DEoptim`），兼顾效率与稳健性。
- **本地优先运行**：Electron 桌面 + 本地 Shiny/R 运行时，支持在本地或离线实验环境下完成分析。

## 三步工作流

1. **Step 1 (ITCprocessor)**：原始 `.itc` 数据解析、基线校正和积分。
2. **Step 2 (ITCsimfit)**：模型模拟与参数约束拟合，并提供诊断信息。
3. **Step 3 (ITCgraph)**：生成发表风格图形并导出结构化结果。

更完整的技术背景请参考白皮书：[`PRdocs/ITCSuite_External_Whitepaper.md`](PRdocs/ITCSuite_External_Whitepaper.md)。

## 下载

- 最新版本下载: [github.com/GuangluWu/ViaBind_itc/releases/latest](https://github.com/GuangluWu/ViaBind_itc/releases/latest)
- 支持平台: macOS、Windows

发布文件统一放在 GitHub Releases 页面：

- macOS: 下载 `.dmg` 安装包
- Windows: 下载 `.exe` 安装程序

## 首次启动说明

- 临时公告（macOS）：由于 Apple notarization 服务器繁忙，当前 macOS 版本暂未 notarized，首次启动需要手动放行。
- Windows 对未签名安装程序可能会显示 SmartScreen 警告。如果您确认下载来源可信，可点击 `更多信息` 后继续安装。

### macOS 手动放行指南（临时）

1. 打开 `.dmg`，将 `ViaBind.app` 拖动到 `Applications`。
2. 在 `Applications` 中右键 `ViaBind.app`，选择 `打开`。
3. 如果仍被拦截，前往 `系统设置 > 隐私与安全性`，点击 `仍要打开`，然后再次启动应用。
4. 若仍提示“文件已损坏”，请在终端执行：

```bash
xattr -dr com.apple.quarantine "/Applications/ViaBind.app"
open "/Applications/ViaBind.app"
```

可选（高级用户）：如需校验哈希，可对照 `SHA256SUMS.txt` 执行：

```bash
shasum -a 256 ~/Downloads/ViaBind-0.4.5-arm64.dmg
```

## 反馈与支持

- 安装问题、程序错误与功能异常: [GitHub Issues](https://github.com/GuangluWu/ViaBind_itc/issues)
- 提交问题时，建议附上操作系统版本、ViaBind 版本，以及相关截图或日志。

## 仓库结构

- [`desktop/`](desktop/) - Electron 桌面应用与打包脚本
- [`ITCSuiteWeb/`](ITCSuiteWeb/) - 宿主 Shiny 应用
- [`ITCprocessor/`](ITCprocessor/) - ITC 数据预处理模块
- [`ITCsimfit/`](ITCsimfit/) - 拟合与模拟模块
- [`ITCgraph/`](ITCgraph/) - 绘图与导出模块
- [`Examples/`](Examples/) - 随发布版本附带的示例文件

## 从源码构建

桌面端开发与打包说明见 [`desktop/README.md`](desktop/README.md)。

典型本地开发流程：

```bash
cd desktop
npm install
npm run dev
```

典型发布构建命令：

```bash
cd desktop
npm run dist
npm run dist:win
```

## 许可证

本项目采用 [MIT License](https://github.com/GuangluWu/ViaBind_itc/blob/main/LICENSE) 开源发布。
