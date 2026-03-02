# ViaBind Release Notes Template

Use the English section, the Chinese section, or both together in a GitHub Release description.

## English

```md
## Downloads
- macOS: download the `.dmg` installer
- Windows: download the `.exe` installer

## Highlights
- Public open-source release of ViaBind.
- Desktop workflow for ITC data processing, fitting, and plotting.
- Source code is licensed under the MIT License.

## Notes
- Due to heavy Apple notarization service load, the current macOS release is temporarily not notarized.
- macOS users may need to manually allow ViaBind on first launch:
  1. Open the `.dmg` and drag `ViaBind.app` to `Applications`.
  2. Right-click `ViaBind.app` in `Applications` and choose `Open`.
  3. If blocked, open `System Settings > Privacy & Security` and click `Open Anyway`.
  4. If needed, run:
     `xattr -dr com.apple.quarantine "/Applications/ViaBind.app"`
- Optional (advanced users): verify installer checksum with `SHA256SUMS.txt`.
- Windows may show SmartScreen warnings for unsigned installers.

## Feedback
Please report issues in GitHub Issues.
```

## 中文

```md
## 下载
- macOS: 下载 `.dmg` 安装包
- Windows: 下载 `.exe` 安装程序

## 版本说明
- ViaBind 的公开开源发布版本。
- 提供 ITC 数据处理、拟合与绘图的桌面工作流。
- 源代码采用 MIT License 开源。

## 注意事项
- 由于 Apple notarization 服务器繁忙，当前 macOS 版本暂未 notarized。
- macOS 用户首次启动可能需要手动放行：
  1. 打开 `.dmg`，将 `ViaBind.app` 拖到 `Applications`。
  2. 在 `Applications` 右键 `ViaBind.app`，选择“打开”。
  3. 如果仍被拦截，前往 `系统设置 > 隐私与安全性`，点击“仍要打开”。
  4. 如有需要，执行：
     `xattr -dr com.apple.quarantine "/Applications/ViaBind.app"`
- 可选（高级用户）：如需校验安装包哈希，可对照 `SHA256SUMS.txt`。
- Windows 对未签名安装程序可能会显示 SmartScreen 警告。

## 反馈
如遇到问题，请通过 GitHub Issues 提交反馈。
```

## Combined Bilingual Example

```md
## Downloads / 下载
- macOS: download the `.dmg` installer / 下载 `.dmg` 安装包
- Windows: download the `.exe` installer / 下载 `.exe` 安装程序

## Highlights / 版本说明
- Public open-source release of ViaBind. / ViaBind 的公开开源发布版本。
- Desktop workflow for ITC data processing, fitting, and plotting. / 提供 ITC 数据处理、拟合与绘图的桌面工作流。
- Source code is licensed under the MIT License. / 源代码采用 MIT License 开源。

## Notes / 注意事项
- Due to heavy Apple notarization service load, the current macOS release is temporarily not notarized. / 由于 Apple notarization 服务器繁忙，当前 macOS 版本暂未 notarized。
- macOS first-launch manual allow:
  1. Open `.dmg` and drag app to `Applications`.
  2. Right-click app -> `Open`.
  3. Use `System Settings > Privacy & Security > Open Anyway` if blocked.
  4. Optional terminal command: `xattr -dr com.apple.quarantine "/Applications/ViaBind.app"`
- Optional (advanced users): verify checksum using `SHA256SUMS.txt`.
- macOS 首次启动手动放行：
  1. 打开 `.dmg` 并拖动到 `Applications`。
  2. 右键应用并选择“打开”。
  3. 被拦截时前往 `系统设置 > 隐私与安全性 > 仍要打开`。
  4. 可选终端命令：`xattr -dr com.apple.quarantine "/Applications/ViaBind.app"`
- 可选（高级用户）：可使用 `SHA256SUMS.txt` 进行哈希校验。
- Windows may show SmartScreen warnings for unsigned installers. / Windows 对未签名安装程序可能会显示 SmartScreen 警告。

## Feedback / 反馈
Please report issues in GitHub Issues. / 如遇到问题，请通过 GitHub Issues 提交反馈。
```
