# R/data_parser.R

# 解析 ITC 文件头中的实验参数（参考 数据解释.md）
# $ 块: 总滴定数、温度、每针体积(uL)（n_injections/V_pre_ul/V_inj_ul 会在 read_itc() 中被 @ 行解析结果覆盖）
# # 块: syringe浓度(mM)、cell浓度(mM)、cell体积(mL)、温度(℃)
parse_itc_metadata <- function(lines) {
  params <- list(
    n_injections = NA_integer_,
    temperature_C = NA_real_,
    syringe_conc_mM = NA_real_,
    cell_conc_mM = NA_real_,
    cell_volume_mL = NA_real_,
    V_pre_ul = NA_real_,
    V_inj_ul = NA_real_,
    titration_interval_s = NA_real_,
    injection_volumes_ul = NULL
  )
  
  # ----- 解析 $ 块 -----
  dollar_lines <- which(grepl("^\\$", lines))
  for (i in seq_along(dollar_lines)) {
    idx <- dollar_lines[i]
    line <- trimws(lines[idx])
    # $ 19 -> 总滴定数
    if (grepl("^\\$\\s*[0-9]+\\s*$", line)) {
      n <- as.integer(gsub("^\\$\\s*([0-9]+).*", "\\1", line))
      if (i == 2 || is.na(params$n_injections)) params$n_injections <- n  # 第二个 $ 行是滴定数（第一个是 $ITC）
    }
    # $NOT 下一行是温度
    if (line == "$NOT" && idx < length(lines)) {
      next_line <- trimws(lines[idx + 1])
      if (grepl("^\\$\\s*[0-9.-]+", next_line)) {
        params$temperature_C <- as.numeric(gsub("^\\$\\s*([0-9.-]+).*", "\\1", next_line))
      }
    }
    if (grepl("^\\$\\s*[0-9.]+\\s*,", line)) {
      content <- sub("^\\$\\s*", "", line)
      parts <- strsplit(content, ",")[[1]]
      parts <- trimws(parts)
      nums <- suppressWarnings(as.numeric(parts))

      if (length(nums) >= 1 && !is.na(nums[1])) {
        if (is.null(params$injection_volumes_ul)) {
          params$injection_volumes_ul <- nums[1]
        } else {
          params$injection_volumes_ul <- c(params$injection_volumes_ul, nums[1])
        }
      }

      if (is.na(params$titration_interval_s) && length(nums) >= 3 && !is.na(nums[3])) {
        params$titration_interval_s <- nums[3]
      }
    }
  }
  if (!is.null(params$injection_volumes_ul) && length(params$injection_volumes_ul) >= 1) {
    params$V_pre_ul <- params$injection_volumes_ul[1]
    params$V_inj_ul <- if (length(params$injection_volumes_ul) >= 2) params$injection_volumes_ul[2] else params$injection_volumes_ul[1]
  }
  
  # ----- 解析 # 块（# 0, # 0.3, # 0.03, # 0.2033, # 25 ...）-----
  hash_numeric <- grep("^#\\s*[0-9.-]+", lines, value = FALSE)
  hash_vals <- numeric(0)
  for (j in hash_numeric) {
    line <- trimws(lines[j])
    val <- as.numeric(gsub("^#\\s*([0-9.-]+).*", "\\1", line))
    if (!is.na(val)) hash_vals <- c(hash_vals, val)
  }
  # 顺序: 第1个常为0, 第2=syringe_mM, 第3=cell_mM, 第4=cell_vol_mL, 第5=温度
  if (length(hash_vals) >= 2) params$syringe_conc_mM <- hash_vals[2]
  if (length(hash_vals) >= 3) params$cell_conc_mM <- hash_vals[3]
  if (length(hash_vals) >= 4) params$cell_volume_mL <- hash_vals[4]
  if (length(hash_vals) >= 5) params$temperature_C <- hash_vals[5]
  
  params
}

# 读取 ITC 数据文件
# 改进版：更精确地解析注射标记，区分数据点和元数据
# 兼容新旧格式：支持从 @ 行读取精确的注射时间
# 同时解析文件头中的实验参数，供拟合使用
# 注意：n_injections、V_pre_ul、V_inj_ul 以 @ 行解析结果为准，会在 read_itc() 内覆盖文件头 ($ 19 与 $ 0.4, 0.8... 列表) 中的对应值
read_itc <- function(file_path) {
  lines <- readLines(file_path)
  params <- parse_itc_metadata(lines)
  
  time <- c()
  power <- c()
  injection_indices <- c() # 记录注射发生的数据点索引
  parsed_injection_times <- c() # [NEW] 暂存解析出的注射时间
  parsed_injection_volumes_ul <- c() # 每一滴对应的体积 (µL)，从 @ 行解析
  parsed_injection_index <- c() # @ 行中的针序号 (如 @0,@1,...,@27)，用于用最后一针序号作为 n_inj
  
  # 用于暂存当前读取状态
  current_idx <- 0
  
  # 遍历每一行
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # MicroCal 格式特征：
    # 1. 元数据以 $ 或 # 或 % 开头
    # 2. 注射标记以 @ 开头，格式如 @3,2.0000,4.0 (第3针, 2uL, 4秒)
    # 3. 数据行是逗号分隔的数字
    
    if (grepl("^@", line)) {
      # 这是一个注射标记！
      # 它表示接下来的数据属于新的一针。
      # 在 MicroCal 文件中，@ 行之后的第一行数据就是该次注射的开始。
      # 我们记录当前已经读取的数据点数量 + 1 作为注射开始的索引
      injection_indices <- c(injection_indices, current_idx + 1)
      
      # [NEW] 尝试解析注射时间和体积
      # 新格式: @index,vol,duration,time (例如 @2,2.0001,4.0, 210.106)
      # 旧格式: @index,vol,duration (例如 @3,2.0000,4.0)
      content <- sub("^@", "", line)
      parts <- strsplit(content, ",")[[1]]
      parts <- trimws(parts)
      # 解析 @ 行中的针序号 (如 @0 或 @1,0.4,...)，用于用最后一针序号作为真实滴定数 n_inj
      inj_idx <- NA_integer_
      if (length(parts) >= 1L) {
        inj_idx <- suppressWarnings(as.integer(parts[1L]))
        if (is.na(inj_idx)) inj_idx <- NA_integer_
      }
      parsed_injection_index <- c(parsed_injection_index, inj_idx)
      
      inj_time <- NA
      if (length(parts) >= 4) {
        inj_time <- as.numeric(parts[4])
      }
      parsed_injection_times <- c(parsed_injection_times, inj_time)
      
      # 解析该针体积 (µL)，parts[2] = vol
      inj_vol_ul <- NA_real_
      if (length(parts) >= 2) {
        v <- as.numeric(parts[2])
        if (!is.na(v)) inj_vol_ul <- v
      }
      parsed_injection_volumes_ul <- c(parsed_injection_volumes_ul, inj_vol_ul)
      
      next
    }
    
    # 跳过其他非数据行
    if (grepl("^[$#%?]", line) || nchar(trimws(line)) == 0) next
    
    # 尝试解析数据行
    # 通常格式: Time, Power, ... (可能有更多列)
    if (grepl("^[0-9.-]", line)) {
      parts <- strsplit(line, ",")[[1]]
      if (length(parts) >= 2) {
        t <- as.numeric(parts[1])
        p <- as.numeric(parts[2])
        
        if (!is.na(t) && !is.na(p)) {
          time <- c(time, t)
          power <- c(power, p)
          current_idx <- current_idx + 1
        }
      }
    }
  }
  
  # 简单的完整性检查
  # 如果第一个数据点之前没有 @ 标记（通常第一针之前可能没有 @，或者 @ 在文件头）
  # 我们可以假设第1个点是第1针的开始（如果列表为空）
  if (length(injection_indices) == 0 && current_idx > 0) {
    injection_indices <- c(1)
    parsed_injection_times <- c(NA)
    parsed_injection_volumes_ul <- c(NA_real_)
    parsed_injection_index <- c(NA_integer_)
  } else if (length(injection_indices) > 0 && injection_indices[1] > 1) {
    # 如果检测到的第一个注射不是从1开始，说明前面有一段预平衡
    # 有时候用户希望把第一段也算作"第0针"或预平衡，这取决于具体需求
    # 这里我们保持原样，只信任文件里的 @ 标记
  }
  
  # [NEW] 整合最终的注射时间
  # 如果 parsed_injection_times 是 NA，说明是旧格式，使用数据点时间作为回退
  final_injection_times <- numeric(length(injection_indices))
  
  for (k in seq_along(injection_indices)) {
    idx <- injection_indices[k]
    raw_time <- NA
    if (k <= length(parsed_injection_times)) {
      raw_time <- parsed_injection_times[k]
    }
    
    if (!is.na(raw_time)) {
      final_injection_times[k] <- raw_time
    } else {
      # Fallback: 使用该针第一个数据点的时间
      if (idx <= length(time)) {
        final_injection_times[k] <- time[idx]
      }
    }
  }

  # 每针体积 (µL)：与 injections 一一对应，来自 @ 行；缺省时用 params 的 V_pre_ul / V_inj_ul 补全
  n_inj <- length(injection_indices)
  injection_volumes_ul <- if (length(parsed_injection_volumes_ul) >= n_inj) {
    parsed_injection_volumes_ul[seq_len(n_inj)]
  } else {
    rep(NA_real_, n_inj)
  }
  for (k in seq_len(n_inj)) {
    if (is.na(injection_volumes_ul[k])) {
      injection_volumes_ul[k] <- if (k == 1) params$V_pre_ul else params$V_inj_ul
    }
  }

  # 以 @ 行解析结果为准覆盖 meta：总针数用最后一针的序号（@ 行第一个数），这样 @0 不计入滴定数
  last_index <- if (length(parsed_injection_index) > 0L) parsed_injection_index[length(parsed_injection_index)] else NA_integer_
  params$n_injections <- if (!is.na(last_index)) as.integer(last_index) else length(injection_indices)
  # 首针体积：若第一个 @ 是 @0（无体积），则取第二行 @1 的体积作为 V_pre
  first_vol_idx <- 1L
  if (length(parsed_injection_index) >= 1L && parsed_injection_index[1L] == 0L && length(injection_volumes_ul) >= 2L) {
    first_vol_idx <- 2L
  }
  if (length(injection_volumes_ul) >= first_vol_idx && !is.na(injection_volumes_ul[first_vol_idx])) {
    params$V_pre_ul <- injection_volumes_ul[first_vol_idx]
  }
  # 典型每针体积：取第一针之后的第一非 NA 体积，或第一针体积
  second_vol_idx <- first_vol_idx + 1L
  if (length(injection_volumes_ul) >= second_vol_idx && !is.na(injection_volumes_ul[second_vol_idx])) {
    params$V_inj_ul <- injection_volumes_ul[second_vol_idx]
  } else if (length(injection_volumes_ul) >= first_vol_idx && !is.na(injection_volumes_ul[first_vol_idx])) {
    params$V_inj_ul <- injection_volumes_ul[first_vol_idx]
  }

  return(list(
    data = data.frame(Time = time, Power = power),
    injections = injection_indices,
    injection_times = final_injection_times,
    injection_volumes_ul = injection_volumes_ul,
    params = params
  ))
}
