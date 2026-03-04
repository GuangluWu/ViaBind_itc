# R/data_parser.R
# [COMMENT_STD][MODULE_HEADER]
# 模块职责：解析 ITC 原始文本中的元数据、注射事件与时序功率数据。
# 依赖：base R 字符串与向量处理函数。
# 对外接口：parse_itc_metadata()、read_itc()、detect_step1_source_type()、
# read_step1_input()、convert_ta_to_xlsx()、read_ta_extract_xlsx()。
# 副作用：读取文件内容；返回结构化 list/data.frame，不写外部状态。
# 变更历史：2026-02-12 - 增加 Phase 4 注释规范样板。

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
  # [COMMENT_STD][IO_CONTRACT]
  # 输入来源：用户在 Step1 UI 中选择的 .itc/.txt 文件路径。
  # 字段/类型：file_path 为单个字符串路径；输出包含 time/power/injection_indices/params 等结构。
  # 单位：时间秒，功率常见为 ucal/s，体积 uL，浓度 mM，温度摄氏度（按源文件语义）。
  # 空值策略：无法解析的字段保留 NA；注射序列不足时下游逻辑按空结果处理。
  # 输出保证：返回可供 baseline/integration 继续处理的结构化对象（字段名稳定）。
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

`%||%` <- function(x, y) if (is.null(x)) y else x

to_num1 <- function(x) {
  out <- suppressWarnings(as.numeric(x)[1])
  if (is.finite(out)) out else NA_real_
}

normalize_scalar_chr <- function(x, default = "") {
  out <- as.character(x %||% "")[1]
  out <- trimws(out)
  if (nzchar(out)) out else default
}

extract_ext <- function(path_like) {
  x <- tolower(normalize_scalar_chr(path_like, default = ""))
  if (!nzchar(x)) return("")
  ext <- tolower(tools::file_ext(x))
  if (!nzchar(ext)) return("")
  paste0(".", ext)
}

detect_step1_source_type <- function(file_path, display_name = NULL) {
  ext_display <- extract_ext(display_name)
  ext_path <- extract_ext(file_path)
  ext <- if (nzchar(ext_display)) ext_display else ext_path

  if (ext %in% c(".itc", ".txt")) return("itc")
  if (ext == ".nitc") return("ta_nitc")
  if (ext == ".csc") return("ta_csc")
  if (ext == ".xml") return("ta_xml")
  if (ext == ".xlsx") return("xlsx")
  "unknown"
}

resolve_ta_script <- function(source_type, app_dir) {
  script_name <- switch(
    source_type,
    ta_nitc = "nitc_to_xlsx.R",
    ta_csc = "csc_to_xlsx.R",
    ta_xml = "xml_to_xlsx.R",
    NULL
  )
  if (is.null(script_name)) {
    stop(sprintf("Unsupported TA source type: %s", source_type), call. = FALSE)
  }
  script_path <- file.path(app_dir, "R", script_name)
  if (!file.exists(script_path)) {
    stop(sprintf("TA converter script not found: %s", script_path), call. = FALSE)
  }
  script_path
}

build_ta_xlsx_path <- function(source_path, source_type) {
  suffix <- switch(
    source_type,
    ta_nitc = "_nitc_extract.xlsx",
    ta_csc = "_csc_extract.xlsx",
    ta_xml = "_xml_extract.xlsx",
    stop(sprintf("Unsupported TA source type: %s", source_type), call. = FALSE)
  )
  file.path(
    dirname(source_path),
    paste0(tools::file_path_sans_ext(basename(source_path)), suffix)
  )
}

convert_ta_to_xlsx <- function(source_path, source_type, app_dir, overwrite = TRUE) {
  src <- normalizePath(path.expand(source_path), winslash = "/", mustWork = FALSE)
  if (!file.exists(src)) {
    stop(sprintf("Source file not found: %s", src), call. = FALSE)
  }

  out_xlsx <- build_ta_xlsx_path(src, source_type)
  if (!isTRUE(overwrite) && file.exists(out_xlsx)) {
    return(normalizePath(out_xlsx, winslash = "/", mustWork = TRUE))
  }

  script_path <- resolve_ta_script(source_type, app_dir = app_dir)
  rscript_bin <- Sys.which("Rscript")
  if (!nzchar(rscript_bin)) {
    stop("Rscript executable not found in PATH.", call. = FALSE)
  }

  dir.create(dirname(out_xlsx), recursive = TRUE, showWarnings = FALSE)
  timeout_sec <- 90
  run_out <- tryCatch(
    system2(
      command = rscript_bin,
      # system2() does not safely preserve spaces in args on all hosts; quote file paths explicitly.
      args = c(shQuote(script_path), shQuote(src), "-o", shQuote(out_xlsx)),
      stdout = TRUE,
      stderr = TRUE,
      timeout = timeout_sec
    ),
    error = function(e) e
  )
  if (inherits(run_out, "error")) {
    stop(
      sprintf("TA convert failed for %s: %s", basename(src), conditionMessage(run_out)),
      call. = FALSE
    )
  }
  status <- attr(run_out, "status")
  if (!is.null(status) && is.finite(status) && status != 0) {
    if (status == 124) {
      stop(
        sprintf(
          "TA convert timed out after %ds for %s. Please retry or convert via command line.",
          timeout_sec,
          basename(src)
        ),
        call. = FALSE
      )
    }
    msg <- paste(run_out, collapse = "\n")
    if (!nzchar(trimws(msg))) msg <- sprintf("Rscript exited with status %s", status)
    stop(
      sprintf("TA convert failed for %s:\n%s", basename(src), msg),
      call. = FALSE
    )
  }

  if (!file.exists(out_xlsx)) {
    stop(
      sprintf("TA convert did not produce output xlsx: %s", out_xlsx),
      call. = FALSE
    )
  }
  normalizePath(out_xlsx, winslash = "/", mustWork = TRUE)
}

find_col <- function(df, candidates = character(), contains = character()) {
  nms <- names(df)
  if (length(nms) == 0) return(NA_character_)
  nms_low <- tolower(nms)
  for (cand in candidates) {
    idx <- which(nms_low == tolower(cand))
    if (length(idx) > 0) return(nms[[idx[1]]])
  }
  if (length(contains) > 0) {
    hit <- vapply(nms_low, function(nm) all(vapply(contains, grepl, logical(1), x = nm, fixed = TRUE)), logical(1))
    idx <- which(hit)
    if (length(idx) > 0) return(nms[[idx[1]]])
  }
  NA_character_
}

read_sheet_df <- function(xlsx_path, sheet_name) {
  as.data.frame(
    readxl::read_excel(xlsx_path, sheet = sheet_name),
    stringsAsFactors = FALSE
  )
}

extract_exp_kv <- function(exp_df) {
  key_col <- find_col(exp_df, candidates = c("key"))
  val_col <- find_col(exp_df, candidates = c("value"))
  if (is.na(key_col) || is.na(val_col)) {
    return(data.frame(key = character(), value = character(), stringsAsFactors = FALSE))
  }
  key <- trimws(as.character(exp_df[[key_col]]))
  value <- as.character(exp_df[[val_col]])
  keep <- nzchar(key)
  data.frame(
    key = key[keep],
    value = value[keep],
    stringsAsFactors = FALSE
  )
}

pick_exp_numeric <- function(exp_kv, keys) {
  if (nrow(exp_kv) == 0 || length(keys) == 0) return(NA_real_)
  key_low <- tolower(exp_kv$key)
  for (k in keys) {
    idx <- which(key_low == tolower(k))
    if (length(idx) == 0) next
    for (i in idx) {
      val <- to_num1(exp_kv$value[[i]])
      if (is.finite(val)) return(val)
    }
  }
  NA_real_
}

resolve_heatflow_cols <- function(heat_df) {
  time_col <- find_col(
    heat_df,
    candidates = c("Time (seconds)", "Time_s", "time", "time_seconds")
  )
  ucal_col <- find_col(
    heat_df,
    candidates = c("Raw Heat Rate (µcal / s)", "Raw Heat Rate (μcal / s)", "Raw Heat Rate (ucal / s)")
  )
  uj_col <- find_col(
    heat_df,
    candidates = c("Raw Heat Rate (µJ / s)", "Raw Heat Rate (μJ / s)", "Raw Heat Rate (uJ / s)")
  )
  list(time = time_col, ucal = ucal_col, uj = uj_col)
}

map_times_to_indices <- function(time_vec, starts) {
  vapply(starts, function(x) {
    which.min(abs(time_vec - x))
  }, integer(1))
}

read_ta_extract_xlsx <- function(xlsx_path, source_label = "") {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Missing package 'readxl'. Install with: install.packages('readxl')", call. = FALSE)
  }
  path <- normalizePath(path.expand(xlsx_path), winslash = "/", mustWork = TRUE)
  sheets <- readxl::excel_sheets(path)
  needed <- c("experiment_parameters", "titration_points", "heatflow")
  miss <- setdiff(needed, sheets)
  if (length(miss) > 0) {
    stop(sprintf("TA extract xlsx missing sheets: %s", paste(miss, collapse = ", ")), call. = FALSE)
  }

  exp_df <- read_sheet_df(path, "experiment_parameters")
  tp_df <- read_sheet_df(path, "titration_points")
  heat_df <- read_sheet_df(path, "heatflow")

  cols <- resolve_heatflow_cols(heat_df)
  if (is.na(cols$time)) {
    stop("TA extract xlsx 'heatflow' missing time column.", call. = FALSE)
  }
  if (is.na(cols$ucal) && is.na(cols$uj)) {
    stop("TA extract xlsx 'heatflow' missing power columns (µcal/s or µJ/s).", call. = FALSE)
  }

  time <- suppressWarnings(as.numeric(heat_df[[cols$time]]))
  if (!is.na(cols$ucal)) {
    power <- suppressWarnings(as.numeric(heat_df[[cols$ucal]]))
  } else {
    power_uj <- suppressWarnings(as.numeric(heat_df[[cols$uj]]))
    power <- power_uj / 4.184
  }
  keep <- is.finite(time) & is.finite(power)
  time <- time[keep]
  power <- power[keep]
  # TA exported heatflow sign convention is opposite to Step1 expectation.
  power <- -1 * power
  if (length(time) < 2) {
    stop("TA extract xlsx heatflow has insufficient valid points.", call. = FALSE)
  }
  if (any(diff(time) < 0, na.rm = TRUE)) {
    stop("TA extract xlsx heatflow time column is not non-decreasing.", call. = FALSE)
  }

  start_col <- find_col(tp_df, candidates = c("start_s"))
  width_col <- find_col(tp_df, candidates = c("original_width_s"))
  vol_col <- find_col(tp_df, candidates = c("inj_volume_uL"))
  inj_num_col <- find_col(tp_df, candidates = c("inj_num"))
  if (is.na(start_col)) {
    stop("TA extract xlsx 'titration_points' missing start_s column.", call. = FALSE)
  }

  starts <- suppressWarnings(as.numeric(tp_df[[start_col]]))
  inj_num <- if (!is.na(inj_num_col)) suppressWarnings(as.numeric(tp_df[[inj_num_col]])) else seq_along(starts)
  width <- if (!is.na(width_col)) suppressWarnings(as.numeric(tp_df[[width_col]])) else rep(NA_real_, length(starts))
  vols <- if (!is.na(vol_col)) suppressWarnings(as.numeric(tp_df[[vol_col]])) else rep(NA_real_, length(starts))

  valid_tp <- is.finite(starts)
  starts <- starts[valid_tp]
  inj_num <- inj_num[valid_tp]
  width <- width[valid_tp]
  vols <- vols[valid_tp]
  if (length(starts) < 1) {
    stop("TA extract xlsx has no valid injection starts.", call. = FALSE)
  }

  exp_kv <- extract_exp_kv(exp_df)
  default_vol <- pick_exp_numeric(exp_kv, c("DefaultInjVolume"))
  vols_filled <- vols
  if (is.finite(default_vol)) {
    vols_filled[!is.finite(vols_filled)] <- default_vol
  }

  inj_idx <- map_times_to_indices(time, starts)
  injections <- as.integer(c(1L, inj_idx))
  injection_times <- c(time[[1]], starts)
  injection_volumes_ul <- c(NA_real_, vols_filled)

  n_inj <- suppressWarnings(max(inj_num[is.finite(inj_num)], na.rm = TRUE))
  if (!is.finite(n_inj)) n_inj <- length(starts)
  n_inj <- as.integer(round(n_inj))

  temp_c <- pick_exp_numeric(exp_kv, c("Temperature", "raw::tempsetpoint"))
  syringe_mM <- pick_exp_numeric(exp_kv, c("Titrant", "raw::syringeconcentration"))
  cell_mM <- pick_exp_numeric(exp_kv, c("Titrate", "raw::cellconcentration"))
  cell_vol <- pick_exp_numeric(exp_kv, c("raw::cellvolume", "InitalTitrateVolume"))
  if (is.finite(cell_vol) && cell_vol > 10) {
    cell_vol <- cell_vol / 1000
  }

  v_pre <- if (length(vols_filled) >= 1 && is.finite(vols_filled[[1]])) vols_filled[[1]] else default_vol
  if (length(vols_filled) >= 2) {
    tail_vol <- vols_filled[2:length(vols_filled)]
    tail_vol <- tail_vol[is.finite(tail_vol)]
  } else {
    tail_vol <- numeric(0)
  }
  v_inj <- if (length(tail_vol) > 0) stats::median(tail_vol) else v_pre

  interval_s <- if (any(is.finite(width))) stats::median(width[is.finite(width)]) else NA_real_
  if (!is.finite(interval_s) && length(starts) >= 2) {
    ds <- diff(starts)
    ds <- ds[is.finite(ds)]
    if (length(ds) > 0) interval_s <- stats::median(ds)
  }

  out_label <- normalize_scalar_chr(source_label, default = basename(path))
  list(
    data = data.frame(Time = as.numeric(time), Power = as.numeric(power)),
    injections = injections,
    injection_times = as.numeric(injection_times),
    injection_volumes_ul = as.numeric(injection_volumes_ul),
    params = list(
      n_injections = n_inj,
      temperature_C = temp_c,
      syringe_conc_mM = syringe_mM,
      cell_conc_mM = cell_mM,
      cell_volume_mL = cell_vol,
      V_pre_ul = v_pre,
      V_inj_ul = v_inj,
      titration_interval_s = interval_s,
      injection_volumes_ul = vols_filled
    ),
    source = list(
      type = "ta_xlsx",
      xlsx_path = path,
      label = out_label
    )
  )
}

read_step1_input <- function(file_path, display_name = NULL, app_dir = getwd(), overwrite_ta_xlsx = TRUE) {
  fp <- normalizePath(path.expand(file_path), winslash = "/", mustWork = FALSE)
  if (!file.exists(fp)) {
    stop(sprintf("Input file not found: %s", fp), call. = FALSE)
  }

  source_type <- detect_step1_source_type(fp, display_name = display_name)
  if (source_type == "itc") {
    return(read_itc(fp))
  }
  if (source_type %in% c("ta_nitc", "ta_csc", "ta_xml")) {
    xlsx_path <- convert_ta_to_xlsx(
      source_path = fp,
      source_type = source_type,
      app_dir = app_dir,
      overwrite = overwrite_ta_xlsx
    )
    label <- normalize_scalar_chr(display_name, default = basename(fp))
    return(read_ta_extract_xlsx(xlsx_path, source_label = label))
  }
  if (source_type == "xlsx") {
    stop("Direct .xlsx import is not supported in Step1. Please import .itc/.txt/.nitc/.csc/.xml.", call. = FALSE)
  }

  stop(
    sprintf(
      "Unsupported file type for Step1 import: %s (supported: .itc/.txt/.nitc/.csc/.xml)",
      basename(fp)
    ),
    call. = FALSE
  )
}
