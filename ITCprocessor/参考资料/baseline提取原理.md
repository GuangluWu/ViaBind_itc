下面按“小白能听懂”的方式，把现在这套 baseline（基线）处理的原理拆开讲清楚。你这份逻辑全部在 [baseline.R](file:///Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor/R/baseline.R#L8-L131) 里的 `SegmentedBaseline()`。

---

**整体思路（你可以先记住这一句）**
- 它不是用一条直线去拟合全程基线，而是：在每一针注射前找一小段“稳定区域”的平均值当作锚点（anchor），最后用样条曲线把这些锚点连起来，得到一条随时间缓慢漂移的基线。

这样做的原因是：ITC 的基线通常会有缓慢漂移，用“分段取稳定均值 + 平滑曲线连接”比“一条直线”更贴近现实。

---

**函数输入分别代表什么**
见 [baseline.R:8-12](file:///Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor/R/baseline.R#L8-L12)

- `time`: 时间序列（每个采样点的时间）
- `power`: 功率信号序列（原始曲线）
- `injection_indices`: 每一针“开始”的索引位置（非常关键：代码明确说“严格使用传入的注射信息”，不再猜）  
  见 [baseline.R:4-7](file:///Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor/R/baseline.R#L4-L7)
- `injection_times`（可选）: 更精确的注射开始时间（如果你解析文件能直接拿到时间，用它更稳）
- `baseline_duration`（默认 20）: 取锚点时，往回取多长时间窗口来求均值（单位和 `time` 一致）
- `baseline_offset`（默认 0）: 在注射开始前再“提前”一点点，避免窗口贴得太近误吃到注射前沿或扰动

---

**第一步：没有注射信息就降级**
见 [baseline.R:15-20](file:///Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor/R/baseline.R#L15-L20)

- 如果 `injection_indices` 为空：没法做“每针前基线”，就用最简单的全局线性拟合 `lm(power ~ time)` 作为基线。

---

**第二步：确定每一针的注射开始时间**
见 [baseline.R:22-30](file:///Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor/R/baseline.R#L22-L30)

- 如果你提供了 `injection_times` 且长度和 `injection_indices` 一致：就直接用它。
- 否则用旧逻辑：`time[injection_indices]`（用索引去 time 里查对应时间）。

这一段的意义：索引有时会因为数据裁剪/重采样不完全可靠，用“解析到的注射时间”能减少偏差。

---

**第三步：怎么选一个锚点（anchor）**
核心在辅助函数 `get_anchor(end_idx)`，见 [baseline.R:32-56](file:///Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor/R/baseline.R#L32-L56)

它做的事可以理解为：

1. 给定一个“窗口结束点” `end_idx`（这个点必须在注射前）
2. 用 `baseline_duration` 计算窗口起始时间 `t_start = time[end_idx] - baseline_duration`
3. 从 `end_idx` 往前找，找到第一个时间不大于 `t_start` 的位置，窗口就是 `[start_idx, end_idx]`
4. 在这个窗口里：
   - `val_x` = 窗口内 time 的平均值（锚点的 x 坐标）
   - `val_y` = 窗口内 power 的平均值（锚点的 y 坐标，也就是“该段基线水平”）

为什么用“平均值”：
- 因为基线段应该是相对平稳的，取均值能抵抗噪声。

它还做了基本有效性检查（窗口至少要有 2 个点），否则返回 `NULL`。

---

**第四步：对每一针，在注射前找一个锚点**
见 [baseline.R:65-92](file:///Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor/R/baseline.R#L65-L92)

对第 `i` 针：

1. 先拿到注射开始时间 `t_inj_start`
2. 定义“目标窗口结束时间”  
   `t_target_end = t_inj_start - baseline_offset`
3. 从 `idx - 1` 开始往前走，找到第一个 `time[real_end_idx] <= t_target_end` 的点，作为窗口的结束索引 `real_end_idx`  
   见 [baseline.R:74-81](file:///Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor/R/baseline.R#L74-L81)
4. 把这个 `real_end_idx` 交给 `get_anchor()` 算锚点，能算出来就存进 `anchors_x/anchors_y`

直觉理解：
- `baseline_offset` = “离注射开始留多远”
- `baseline_duration` = “往回取多长时间来平均”

---

**第五步：额外加一个“末尾锚点”**
见 [baseline.R:94-106](file:///Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor/R/baseline.R#L94-L106)

- 最后一针后面通常有一段很长的稳定基线，这对“末尾漂移”很关键。
- 所以它会在数据结尾 `last_idx <- length(time)` 再取一次 `get_anchor(last_idx)`，相当于强行给曲线一个“结尾约束”。

---

**第六步：用样条把锚点连成一条连续基线**
见 [baseline.R:107-131](file:///Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor/R/baseline.R#L107-L131)

如果锚点数量 > 1：

1. 按 `anchors_x` 排序
2. 如果有重复的 x（可能发生在窗口均值时间相同/非常接近），就把同 x 的 y 取平均合并掉  
   见 [baseline.R:114-120](file:///Users/guanglu/Documents/myScript/ITCSuite/ITCprocessor/R/baseline.R#L114-L120)
3. 用 `smooth.spline(anchors_x, anchors_y, spar = 0.1)` 拟合一条平滑曲线
4. 再在所有 `time` 点上预测，得到整条 baseline：`predict(fit_spline, time)$y`

如果锚点数量 ≤ 1：
- 还是降级为全局线性拟合 `lm(power ~ time)`。

`spar = 0.1` 的含义（直观版）：
- 越小越“硬”，越倾向贴着锚点走；越大越“软”，更平滑但可能偏离锚点。这里选 0.1 是偏贴锚点的。

---

**你在调参时该怎么想（非常实用）**
- 发现基线被注射峰“污染”（baseline 被抬高/压低）：把 `baseline_offset` 调大一些（比如 1~5 秒，取决于你的 time 单位），让锚点窗口离注射更远。
- 发现基线太“抖”（锚点跟着噪声跑）：把 `baseline_duration` 调大（窗口更长，均值更稳）。
- 发现基线跟漂移跟不上/过于平直：适当减小 `baseline_duration`（更敏感）或调整 `spar`（但目前函数里 `spar` 写死为 0.1）。

---

**一句话总结这份实现的“体系特征”**
- 体系的关键假设是：每针注射开始前都有一段可用的稳定基线；用那段的均值代表该时刻的基线水平，再用样条把各时刻基线水平串起来，得到随时间缓慢变化的 baseline。

如果你愿意，我也可以结合你某个实际数据（给出 `baseline_duration/baseline_offset/injection_indices` 的典型范围）解释为什么某些针前锚点可能取不到、以及怎样判断锚点是否落在“恢复区”而不是“峰尾”。