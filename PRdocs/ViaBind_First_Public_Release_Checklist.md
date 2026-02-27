# ViaBind First Public Release Checklist

Use this checklist when preparing a new public GitHub release.

## 1. Pre-release checks

- Confirm the repository only contains content you want to publish.
- Check the desktop version in `desktop/package.json`.
- Run the strict test suite from the repository root:

```bash
Rscript tests/run_all.R --strict
```

- Run the desktop smoke test:

```bash
cd desktop
npm install
npm run smoke
```

## 2. Build installers

From `desktop/`:

```bash
npm run dist
npm run dist:win
```

Expected release assets:

- macOS: `.dmg`
- Windows: `.exe`

Optional checksum file:

```bash
cd desktop/dist
shasum -a 256 *.dmg *.zip > SHA256SUMS.txt
```

## 3. Manual install verification

- Install the macOS `.dmg` on a Mac and confirm the app opens.
- Install the Windows `.exe` on a Windows machine and confirm the app opens.
- Run one basic workflow in the app after install.

## 4. Draft the GitHub release

1. Open the repository on GitHub.
2. Go to `Releases`.
3. Click `Draft a new release`.
4. Create or select a tag in the form `vX.Y.Z`.
5. Set the release title to `ViaBind vX.Y.Z`.
6. Upload the macOS and Windows installers.
7. Publish the release.

Suggested release notes:

```md
## Downloads
- macOS: download the `.dmg` installer
- Windows: download the `.exe` installer

## Notes
- Public open-source release of ViaBind.
- Source code is licensed under the MIT License.
- macOS may show a security prompt for unsigned apps.
- Windows may show SmartScreen warnings for unsigned apps.

## Feedback
Please report issues in GitHub Issues.
```

For reusable bilingual release note templates, see `PRdocs/ViaBind_Release_Notes_Template.md`.

## 5. Post-release checks

- Open `https://github.com/GuangluWu/ViaBind_itc/releases/latest`
- Confirm the page shows the new version and both installers.
- Confirm `README.md` still points to the latest release link.
- Share only the `releases/latest` URL externally.
