[中文说明](README.zh-CN.md)

[![Release](https://img.shields.io/github/v/release/GuangluWu/ViaBind_itc?display_name=tag)](https://github.com/GuangluWu/ViaBind_itc/releases/latest)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
![Platform: macOS | Windows](https://img.shields.io/badge/Platform-macOS%20%7C%20Windows-blue)

# ViaBind

ViaBind is an open-source desktop workflow for isothermal titration calorimetry (ITC) analysis. The repository includes the Electron desktop shell and the underlying ITC processing, fitting, and plotting modules used by the app.

## Download

- Latest release: [github.com/GuangluWu/ViaBind_itc/releases/latest](https://github.com/GuangluWu/ViaBind_itc/releases/latest)
- Platforms: macOS and Windows

Release assets are published on the GitHub Releases page:

- macOS: download the `.dmg` installer
- Windows: download the `.exe` installer

## First Launch Notes

- macOS may block the app on first launch because early public releases are not notarized yet. If that happens, open `System Settings > Privacy & Security` and allow the app to run.
- Windows may show a SmartScreen warning for unsigned installers. Use `More info` and then continue if you trust the release source.

## Feedback And Support

- Bug reports and installation issues: [GitHub Issues](https://github.com/GuangluWu/ViaBind_itc/issues)
- When reporting a problem, include your OS version, ViaBind version, and screenshots or logs if available.

## Repository Layout

- [`desktop/`](desktop/) - Electron desktop app and packaging scripts
- [`ITCSuiteWeb/`](ITCSuiteWeb/) - hosted Shiny application
- [`ITCprocessor/`](ITCprocessor/) - ITC preprocessing module
- [`ITCsimfit/`](ITCsimfit/) - fitting and simulation module
- [`ITCgraph/`](ITCgraph/) - plotting and export module
- [`Examples/`](Examples/) - example/sample files bundled for releases

## Build From Source

Desktop development and packaging instructions live in [`desktop/README.md`](desktop/README.md).

Typical local development flow:

```bash
cd desktop
npm install
npm run dev
```

Typical release build commands:

```bash
cd desktop
npm run dist
npm run dist:win
```

## License

This project is released under the [MIT License](https://github.com/GuangluWu/ViaBind_itc/blob/main/LICENSE).
