# ITCsimfit Phase 4: 用户引导注释配置 Schema

## 1. 目标

本 schema 用于“按控件 id 绑定文案”的引导注释预埋，采用独立 CSV 管理，与现有 `i18n_translation_table.csv` 解耦。
当前阶段仅提供数据与 API，不启用 UI 引导渲染。

## 2. 版本规则

1. 当前版本：`itcsuite.guide_annotation.v1`
2. 固定写入列 `schema_version`
3. 后续升级使用 `v2/v3`，不做破坏性隐式变更

## 3. CSV 字段定义

文件：`config/guide_annotations.v1.csv`

必需列（列名固定）：

1. `schema_version`：schema 版本，必须等于 `itcsuite.guide_annotation.v1`
2. `app`：应用标识，当前建议 `ITCsimfit`（或 `all`）
3. `guide_id`：引导项唯一标识（全表唯一）
4. `control_id`：Shiny 控件 id（如 `fit_global`、`exp_file`）
5. `control_type`：控件类型枚举
6. `lang_zh`：中文文案
7. `lang_en`：英文文案
8. `severity`：提示等级枚举
9. `status`：状态枚举
10. `since_version`：起始生效版本（可空）
11. `until_version`：结束生效版本（可空）
12. `order`：显示顺序（整数，越小越靠前）
13. `notes`：备注（可空）

## 4. 枚举与约束

### 4.1 `control_type`

允许值：

1. `actionbutton`
2. `numericinput`
3. `sliderinput`
4. `fileinput`
5. `checkboxinput`
6. `checkboxgroupinput`
7. `downloadbutton`
8. `textinput`
9. `plotoutput`
10. `dtoutput`
11. `uioutput`
12. `other`

校验时统一转小写后判断。

### 4.2 `severity`

允许值：

1. `info`
2. `warning`
3. `critical`

### 4.3 `status`

允许值：

1. `active`
2. `inactive`
3. `deprecated`

## 5. 业务校验规则

1. 必需列齐全
2. `guide_id` 全表唯一
3. `control_id` 非空
4. `status=active` 的行必须同时提供 `lang_zh` 与 `lang_en`
5. `schema_version` 必须匹配目标版本
6. `since_version` / `until_version` 如非空，格式必须为 `x` 或 `x.y` 或 `x.y.z`
7. 同一行若同时有 `since_version` 和 `until_version`，则 `since <= until`

## 6. 查询与回退规则

`resolve_guide_annotation(control_id, lang, app_version)` 的默认行为：

1. 仅返回 `status=active` 的记录
2. `app` 优先匹配 `ITCsimfit`，兼容 `all`
3. 若传入 `app_version`，需命中版本区间
4. 文案回退顺序：请求语言 -> 英文 -> `guide_id`

## 7. 兼容性说明

1. 本 schema 不影响现有 bridge payload（如 `itcsuite.step2_plot.v1`）
2. 本 schema 不修改 UI 结构，不新增 UI 开关
3. 升级 schema 时需新增版本文件与校验逻辑，不覆盖旧版本语义
