[English](README.md)

[![Release](https://img.shields.io/github/v/release/GuangluWu/ViaBind_itc?display_name=tag)](https://github.com/GuangluWu/ViaBind_itc/releases/latest)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
![Platform: macOS | Windows](https://img.shields.io/badge/Platform-macOS%20%7C%20Windows-blue)

# ViaBind

ViaBind 是一个面向等温滴定量热（ITC）分析的开源桌面工作流。本仓库包含 Electron 桌面应用，以及用于数据处理、拟合和绘图的底层模块。

## 下载

- 最新版本下载: [github.com/GuangluWu/ViaBind_itc/releases/latest](https://github.com/GuangluWu/ViaBind_itc/releases/latest)
- 支持平台: macOS、Windows

发布文件统一放在 GitHub Releases 页面：

- macOS: 下载 `.dmg` 安装包
- Windows: 下载 `.exe` 安装程序

## 首次启动说明

- 由于当前公开版本暂未完成 notarization，macOS 在首次打开时可能会拦截应用。如果出现这种情况，请前往 `系统设置 > 隐私与安全性`，手动允许应用运行。
- Windows 对未签名安装程序可能会显示 SmartScreen 警告。如果你确认下载来源可信，可点击 `更多信息` 后继续安装。

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
