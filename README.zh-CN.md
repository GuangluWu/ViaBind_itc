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

1. **Step 1 (ITCprocessor)**：原始 `.itc/.txt/.nitc/.csc/.xml` 数据解析、基线校正和积分。
2. **Step 2 (ITCsimfit)**：模型模拟与参数约束拟合，并提供诊断信息。
3. **Step 3 (ITCgraph)**：生成发表风格图形并导出结构化结果。

Step 1 当前支持以下输入文件类型：

- 标准 ITC 格式：`.itc`、`.txt`
- 新增 TA/历史格式：`.nitc`、`.csc`，以及由老 TA 软件导出的 `.xml`

说明：上述新增历史格式（`.nitc`、`.csc`、老 TA 导出 `.xml`）导入可能会更慢，因为 ViaBind 在分析前需要先进行解码和转换。

更完整的技术背景请参考白皮书：[`PRdocs/ITCSuite_External_Whitepaper.md`](PRdocs/ITCSuite_External_Whitepaper.md)。

## 下载

- 最新版本下载: [github.com/GuangluWu/ViaBind_itc/releases/latest](https://github.com/GuangluWu/ViaBind_itc/releases/latest)
- 支持平台: macOS、Windows

发布文件统一放在 GitHub Releases 页面：

- macOS: 下载 `.dmg` 安装包
- Windows: 下载 `.exe` 安装程序

## 系统要求

- macOS: 仅支持 Apple Silicon（M 系列 / ARM64）
- macOS: 不支持 Intel 架构 Mac
- macOS: 需要 macOS 13 及以上
- Windows: 提供 x64 安装程序
- Windows: 建议使用性能较好的电脑；在低配置机器上处理较大数据集或进行更复杂的拟合时，可能会出现卡顿

## 首次启动说明

- macOS 版本现已完成 Apple notarization，可在合规环境中正常使用。
- Windows 对未签名安装程序可能会显示 SmartScreen 警告。如果您确认下载来源可信，可点击 `更多信息` 后继续安装。

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
