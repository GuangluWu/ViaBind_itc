# R/integration.R

# 简单的梯形积分
# 计算每一针的积分热量
# 增加 integration_window 参数来限定积分范围 (单位: 数据点数)
# 增加 start_offset 参数来调整积分起点 (单位: 数据点数, 默认为 0)
integrate_peaks <- function(time, corrected_power, injection_indices, integration_window = NULL, start_offset = 0) {
  heats <- c()
  start_times <- c()
  end_times <- c()
  
  # 如果 injection_indices 只有一个或没有，无法积分
  if (length(injection_indices) < 2) return(data.frame(Injection=integer(), Heat_ucal=numeric(), StartTime_s=numeric(), EndTime_s=numeric()))
  
  # 确保 injection_indices 是排好序的
  injection_indices <- sort(unique(injection_indices))
  
  n_points <- length(time)

  for (i in 1:(length(injection_indices)-1)) {
    orig_start_idx <- injection_indices[i]
    
    # 应用 start_offset (基于点数)
    start_idx <- orig_start_idx + start_offset
    
    # 边界检查
    if (start_idx < 1) start_idx <- 1
    if (start_idx > n_points) start_idx <- n_points
    
    # 确定下一针的起点，作为当前针积分的自然边界
    # 下一针也应用同样的 offset 逻辑作为参考边界
    # 但积分区间不能跨越下一针的起点
    next_orig_idx <- injection_indices[i+1]
    next_start_idx <- next_orig_idx + start_offset
    
    # 边界检查
    if (next_start_idx < 1) next_start_idx <- 1
    if (next_start_idx > n_points) next_start_idx <- n_points
    
    # 默认积分结束点为下一针起点的前一个点
    end_idx_limit <- next_start_idx - 1
    
    # 如果 limit 比 start 还小（重叠或错乱），则无效
    if (end_idx_limit < start_idx) {
       heats <- c(heats, 0)
       # 记录时间以便调试
       t_record <- if (start_idx <= n_points) time[start_idx] else NA
       start_times <- c(start_times, t_record)
       end_times <- c(end_times, t_record)
       next
    }
    
    # 如果指定了积分窗口 (点数)
    # 改为：integration_window 表示相对于原始这一针起点 (orig_start_idx) 的结束位置偏移
    # 这样 End Point 就固定了，不会随 Start Offset 变化而漂移
    if (!is.null(integration_window) && integration_window > 0) {
      # 目标结束点 = 原始起点 + 窗口值
      target_end_idx <- orig_start_idx + integration_window
      
      # 用户手动指定了窗口，不再强制截断到下一针，允许用户自由控制
      # 这样可以解决滑块调整无效的问题
      end_idx <- target_end_idx
    } else {
      end_idx <- end_idx_limit
    }
    
    # 再次边界检查
    if (end_idx > n_points) end_idx <- n_points
    
    if (start_idx >= end_idx) {
      heats <- c(heats, 0)
      t_record <- if (start_idx <= n_points) time[start_idx] else NA
      start_times <- c(start_times, t_record)
      end_times <- c(end_times, t_record)
      next
    }
    
    # 提取这一段的数据
    t_seg <- time[start_idx:end_idx]
    p_seg <- corrected_power[start_idx:end_idx]
    
    # 梯形积分: Area = sum( (t[i+1]-t[i]) * (p[i+1]+p[i])/2 )
    # 注意: ITC功率单位通常是 ucal/s (或 uJ/s = uW)
    # 积分结果就是 ucal (或 uJ)
    
    # 使用 trapz 原理
    dt <- diff(t_seg)
    p_avg <- (head(p_seg, -1) + tail(p_seg, -1)) / 2
    area <- sum(dt * p_avg)
    
    heats <- c(heats, area)
    start_times <- c(start_times, time[start_idx])
    end_times <- c(end_times, time[end_idx])
  }
  
  # 最后一针通常积分到文件结束
  last_inj <- tail(injection_indices, 1)
  
  # Apply offset for last injection
  last_start_idx <- last_inj + start_offset
  if (last_start_idx < 1) last_start_idx <- 1
  
  if (last_start_idx < n_points) {
      # 默认到最后
      end_idx <- n_points
      
      if (!is.null(integration_window) && integration_window > 0) {
          # 同样改为基于原始起点
          target_end_idx <- last_inj + integration_window
          # 这里仍然要限制 n_points，因为没有更多数据了
          end_idx <- min(target_end_idx, n_points)
      }
      
      if (last_start_idx < end_idx) {
        t_seg <- time[last_start_idx:end_idx]
        p_seg <- corrected_power[last_start_idx:end_idx]
        dt <- diff(t_seg)
        p_avg <- (head(p_seg, -1) + tail(p_seg, -1)) / 2
        area <- sum(dt * p_avg)
        heats <- c(heats, area)
        start_times <- c(start_times, time[last_start_idx])
        end_times <- c(end_times, time[end_idx])
      } else {
        heats <- c(heats, 0)
        t_record <- if (last_start_idx <= n_points) time[last_start_idx] else NA
        start_times <- c(start_times, t_record)
        end_times <- c(end_times, t_record)
      }
  }
  
  return(data.frame(
    Injection = 0:(length(heats)-1),
    Heat_ucal = heats,
    StartTime_s = start_times,
    EndTime_s = end_times
  ))
}
