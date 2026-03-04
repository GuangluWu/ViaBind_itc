[中文说明](README.zh-CN.md)

[![Release](https://img.shields.io/github/v/release/GuangluWu/ViaBind_itc?display_name=tag)](https://github.com/GuangluWu/ViaBind_itc/releases/latest)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
![Platform: macOS | Windows](https://img.shields.io/badge/Platform-macOS%20%7C%20Windows-blue)

# ViaBind

ViaBind is an open-source desktop workflow for isothermal titration calorimetry (ITC) analysis. The repository includes the Electron desktop shell and the underlying ITC processing, fitting, and plotting modules used by the app.

## Why ViaBind

- **End-to-end ITC workflow**: parse raw data, fit mechanistic models, and generate publication-ready figures in one desktop app.
- **Path-combination modeling**: build and compare model hypotheses by reaction-path combinations instead of being limited to a single fixed model.
- **Dual optimization strategies**: use local optimization (`optim`) and global search (`DEoptim`) as complementary routes for speed and robustness.
- **Local-first operation**: Electron desktop + local Shiny/R runtime keeps analysis on local machines, including offline lab environments.

## 3-Step Workflow

1. **Step 1 (ITCprocessor)**: raw `.itc/.txt/.nitc/.csc/.xml` parsing, baseline correction, and integration.
2. **Step 2 (ITCsimfit)**: simulation and constrained parameter fitting with diagnostics.
3. **Step 3 (ITCgraph)**: publication-style plotting and structured export.

Step 1 currently supports these input file types:

- Standard ITC formats: `.itc`, `.txt`
- Newly added TA/raw formats: `.nitc`, `.csc`, `.xml`

Note: the three newly added formats (`.nitc`, `.csc`, `.xml`) may import more slowly, because ViaBind needs to decode and convert them before analysis.

For deeper technical background, see the external whitepaper: [`PRdocs/ITCSuite_External_Whitepaper.md`](PRdocs/ITCSuite_External_Whitepaper.md).

## Download

- Latest release: [github.com/GuangluWu/ViaBind_itc/releases/latest](https://github.com/GuangluWu/ViaBind_itc/releases/latest)
- Platforms: macOS and Windows

Release assets are published on the GitHub Releases page:

- macOS: download the `.dmg` installer
- Windows: download the `.exe` installer

## First Launch Notes

- Temporary notice (macOS): Due to heavy Apple notarization service load, the current macOS release is not notarized yet and requires manual allow on first launch.
- Windows may show a SmartScreen warning for unsigned installers. Use `More info` and then continue if you trust the release source.

### macOS manual allow guide (temporary)

1. Open the `.dmg` and drag `ViaBind.app` to `Applications`.
2. In `Applications`, right-click `ViaBind.app` and choose `Open`.
3. If blocked, go to `System Settings > Privacy & Security`, click `Open Anyway`, then open the app again.
4. If macOS still reports the app is damaged, run:

```bash
xattr -dr com.apple.quarantine "/Applications/ViaBind.app"
open "/Applications/ViaBind.app"
```

Optional (advanced users): verify checksum with `SHA256SUMS.txt`:

```bash
shasum -a 256 ~/Downloads/ViaBind-0.4.5-arm64.dmg
```

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
