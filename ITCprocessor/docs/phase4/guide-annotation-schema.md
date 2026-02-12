# ITCprocessor Phase 4: 用户引导注释配置 Schema

## 1. 目标

为 `ITCprocessor` 预埋“按控件 id 绑定文案”的配置能力，当前阶段仅提供配置/API，不启用引导 UI。

## 2. 版本

1. 当前 schema：`itcsuite.guide_annotation.v1`
2. 版本字段：`schema_version`
3. 升级通过 `v2/v3`，不做隐式破坏变更

## 3. CSV 字段（固定）

1. `schema_version`
2. `app`
3. `guide_id`
4. `control_id`
5. `control_type`
6. `lang_zh`
7. `lang_en`
8. `severity`
9. `status`
10. `since_version`
11. `until_version`
12. `order`
13. `notes`

## 4. 校验规则

1. 必需列完整
2. `guide_id` 全表唯一
3. `control_id` 非空
4. `status=active` 时 `lang_zh/lang_en` 必填
5. `schema_version` 必须匹配 `itcsuite.guide_annotation.v1`
6. `since_version/until_version` 格式为 `x` 或 `x.y` 或 `x.y.z`
7. 若 `since` 与 `until` 同时存在，要求 `since <= until`

## 5. 解析规则

`resolve_guide_annotation(control_id, lang, app_version)`：

1. 仅返回 `status=active`
2. `app` 匹配 `ITCprocessor` 或 `all`
3. 若给定 `app_version`，按区间过滤
4. 文案回退：请求语言 -> 英文 -> `guide_id`
