# ==============================================================================
# constants.R - 全局常量定义
# ==============================================================================
# 本文件定义了项目中使用的所有常量，避免魔法数字，提高代码可维护性

# ==============================================================================
# 参数边界常量
# ==============================================================================

# 参数边界定义
PARAM_BOUNDS <- list(
  # 结合常数的对数值范围 (logK)
  # logK 在 0-9 之间（对应 K = 10^0 到 10^9）
  logK = c(lower = 0, upper = 9),
  
  # 焓变范围 (ΔH, cal/mol)
  # 通常在 -15000 到 5000 cal/mol 之间
  H = c(lower = -15000, upper = 5000),
  
  # 因子修正范围 (fH, fG)
  # 用于多路径模型的修正因子，通常在 0.5 到 1.5 之间
  fH_fG = c(lower = 0.5, upper = 1.5),
  
  # 热偏移范围 (Offset, cal)
  # 基线偏移量，通常在 -1500 到 1500 cal 之间
  Offset = c(lower = -1500, upper = 1500),
  
  # 初始体积范围 (V_init, mL)
  # 会根据实际注射体积动态调整，这里定义相对范围
  V_init_multiplier = c(lower = 0, upper = 1)
)

# 第一针体积默认值（uL），无实验数据时使用；仅在此处定义，避免硬编码
V_PRE_DEFAULT_uL <- 0.3

# 默认参数值（用于初始化或重置）
DEFAULT_PARAMS <- list(
  logK = 7,        # 默认 logK = 7 (K = 10^7)
  H = -7000,       # 默认焓变 -7000 cal/mol
  fH = 1.0,          # 默认因子 = 1（无修正）
  fG = 1.0,          # 默认因子 = 1（无修正）
  Offset = 0,      # 默认无偏移
  V_init = V_PRE_DEFAULT_uL    # 默认第一针体积（无实验数据时）
)

# Huber 鲁棒回归参数
HUBER_PARAMS <- list(
  delta_default = 1.345,  # Huber 函数的默认 delta 值（标准统计学推荐值）
  delta_min = 0.1,        # delta 最小值
  delta_max = 10          # delta 最大值
)

# ==============================================================================
# 数值计算常量
# ==============================================================================

# 数值稳定性常量
EPSILON <- 1e-20         # 通用极小值阈值（用于防止除零）
EPSILON_LOG <- 1e-15     # 对数计算的极小值阈值
EPSILON_TOLERANCE <- 1e-7 # 数值收敛容差

# 求解器相关常量
SOLVER <- list(
  max_iter = 100,        # 最大迭代次数
  atol = 1e-8,           # 绝对容差
  rtol = 1e-8,           # 相对容差
  positive = TRUE        # 强制解为正值
)

# ==============================================================================
# 优化算法常量
# ==============================================================================

# DEoptim（差分进化）优化参数
DE_OPTIM <- list(
  # 种群大小计算：种群大小 = max(multiplier * n_params, min_size)
  pop_size_multiplier = 10,   # 种群大小倍数（相对于参数数量）
  min_pop_size = 50,          # 最小种群大小
  max_pop_size = 200,         # 最大种群大小（防止过大）
  
  # 迭代控制
  max_iter = 100,             # 最大迭代次数
  stagnation_iter = 20,       # 停滞迭代次数（连续多少代无改进则停止）
  
  # DE算法参数
  F = 0.8,                    # 变异因子（mutation factor）
  CR = 0.9,                   # 交叉概率（crossover probability）
  
  # 其他控制
  trace = FALSE,              # 是否显示迭代信息
  strategy = 2                # DE策略编号
)

# 局部优化（L-BFGS-B）参数
LBFGS_OPTIM <- list(
  max_iter = 500,             # 最大迭代次数
  factr = 1e7,                # 收敛因子
  pgtol = 1e-5                # 梯度容差
)

# ==============================================================================
# 数据验证常量
# ==============================================================================

# 数据质量检查阈值
DATA_VALIDATION <- list(
  min_data_points = 5,        # 最小数据点数
  max_missing_ratio = 0.2,    # 最大缺失值比例
  outlier_threshold = 3,      # 离群值检测阈值（标准差倍数）
  min_signal_noise_ratio = 2  # 最小信噪比
)

# ==============================================================================
# UI相关常量
# ==============================================================================

# UI默认值
UI_DEFAULTS <- list(
  # 滑块控件
  n_inj_min = 2,              # 最小注射次数（避免单次注射的向量化边界问题）
  n_inj_default = 26,         # 默认注射次数
  n_inj_max = 50,             # 最大注射次数
  
  # 浓度范围（mM）
  conc_cell_min = 0.01,
  conc_cell_max = 10,
  conc_cell_default = 0.03,
  
  conc_syringe_min = 0.01,
  conc_syringe_max = 50,
  conc_syringe_default = 0.6,
  
  # 体积范围（mL）
  v_cell_min = 0.05,      # 修正：最小值改为 0.05 mL (50 uL)，以支持小体积样品池
  v_cell_max = 2,
  v_cell_default = 0.2033,
  
  v_inj_min = 0.0005,      # 0.001 mL = 1 uL
  v_inj_max = 0.1,        # 0.1 mL = 100 uL
  v_inj_default = 0.0015, # 0.0015 mL = 1.5 uL
  
  v_pre_default = V_PRE_DEFAULT_uL,    # 实验第一针体积默认（无实验数据时）
  
  # 温度范围（K）
  temp_min = 273,
  temp_max = 350,
  temp_default = 298.15
)

# 通知持续时间（秒）
NOTIFICATION_DURATION <- list(
  success = 3,                # 成功消息
  info = 5,                   # 信息消息
  warning = 8,                # 警告消息
  error = 10                  # 错误消息
)

# ==============================================================================
# 文件和路径常量
# ==============================================================================

# 文件路径
FILE_PATHS <- list(
  i18n_table = "i18n_translation_table.csv",
  guide_annotations = file.path("config", "guide_annotations.v1.csv"),
  error_log = "error.log",
  session_log = "session.log"
)

# ==============================================================================
# 引导注释配置常量（Phase 4）
# ==============================================================================

# 引导注释 schema 版本（仅用于配置校验，不影响现有业务 schema）
GUIDE_SCHEMA_VERSION <- "itcsuite.guide_annotation.v1"

# 引导注释默认配置路径
GUIDE_ANNOTATIONS_PATH <- file.path("config", "guide_annotations.v1.csv")

# 当前应用标识（用于 resolver 默认 app 过滤）
GUIDE_APP_ID <- "ITCsimfit"

# 枚举定义（校验器统一转小写后比较）
GUIDE_ALLOWED_CONTROL_TYPES <- c(
  "actionbutton", "numericinput", "sliderinput", "fileinput",
  "checkboxinput", "checkboxgroupinput", "downloadbutton",
  "textinput", "plotoutput", "dtoutput", "uioutput", "other"
)
GUIDE_ALLOWED_SEVERITY <- c("info", "warning", "critical")
GUIDE_ALLOWED_STATUS <- c("active", "inactive", "deprecated")

# 文件格式
FILE_FORMATS <- list(
  csv_sep = ",",              # CSV分隔符
  csv_encoding = "UTF-8",     # 文件编码
  decimal = ".",              # 小数点
  date_format = "%Y-%m-%d %H:%M:%S"  # 日期时间格式
)

# ==============================================================================
# 物理化学常量
# ==============================================================================

# 气体常数
R_GAS_CONST <- 1.987  # cal/(mol·K)

# 单位转换
UNIT_CONVERSION <- list(
  cal_to_kcal = 1/1000,
  kcal_to_cal = 1000,
  mM_to_M = 1/1000,
  M_to_mM = 1000,
  uL_to_mL = 1/1000,
  mL_to_uL = 1000
)

# ==============================================================================
# xlsx 文件参数名映射（内部名 → 文件名，带单位后缀）
# ==============================================================================
# 用于 ITCprocessor / ITCsimfit 之间的数据交换统一命名

# 实验条件参数：内部变量名 → xlsx 文件中的列名/参数名
FILE_PARAM_MAP <- list(
  H_cell_0  = "H_cell_0_mM",
  G_syringe = "G_syringe_mM",
  V_cell    = "V_cell_mL",
  V_inj     = "V_inj_uL",
  V_pre     = "V_pre_uL",
  n_inj     = "n_inj",
  Temp      = "Temp_K"
)

# 反向映射：文件列名 → 内部变量名
FILE_PARAM_MAP_REV <- setNames(names(FILE_PARAM_MAP), unlist(FILE_PARAM_MAP))

# ==============================================================================
# 错误代码和消息
# ==============================================================================

# 错误类型代码
ERROR_CODES <- list(
  # 数据错误
  DATA_INVALID = "E001",
  DATA_MISSING = "E002",
  DATA_FORMAT = "E003",
  
  # 参数错误
  PARAM_INVALID = "E101",
  PARAM_OUT_OF_BOUNDS = "E102",
  
  # 计算错误
  SOLVER_FAILED = "E201",
  OPTIMIZATION_FAILED = "E202",
  CONVERGENCE_FAILED = "E203",
  
  # 文件错误
  FILE_NOT_FOUND = "E301",
  FILE_READ_ERROR = "E302",
  FILE_WRITE_ERROR = "E303",
  
  # 系统错误
  UNKNOWN_ERROR = "E999"
)

# ==============================================================================
# 辅助函数：获取参数边界
# ==============================================================================

#' 获取参数边界
#' 
#' @param param_name 参数名称（如 "logK1", "H2", "fH", "V_init", "Offset"）
#' @param v_inj 注射体积（仅当 param_name 为 "V_init" 时需要）
#' @param override_bounds 可选的运行时边界覆盖列表（按参数名映射为 c(lower, upper)）
#' @return 包含 lower 和 upper 的命名向量
#' 
#' @examples
#' get_param_bound("logK1")  # 返回 c(lower=1, upper=9)
#' get_param_bound("V_init", v_inj=0.01)  # 返回 c(lower=0, upper=0.01)
get_param_bound <- function(param_name, v_inj = NULL, override_bounds = NULL) {
  get_default_param_bound <- function(name, v_inj_local = NULL) {
    if(grepl("logK", name)) {
      return(PARAM_BOUNDS$logK)
    } else if(grepl("^H[0-9]+$", name)) {  # 支持 H1, H2, ... H10, H11 等
      return(PARAM_BOUNDS$H)
    } else if(grepl("fH|fG", name)) {
      return(PARAM_BOUNDS$fH_fG)
    } else if(grepl("Offset", name)) {
      return(PARAM_BOUNDS$Offset)
    } else if(name == "V_init") {
      if(is.null(v_inj_local) || is.na(v_inj_local) || v_inj_local <= 0) {
        warning("V_init bound requires valid v_inj value, using default 1.5")
        v_inj_local <- 1.5
      }
      lower_mult <- PARAM_BOUNDS$V_init_multiplier["lower"]
      upper_mult <- PARAM_BOUNDS$V_init_multiplier["upper"]
      return(c(
        lower = as.numeric(lower_mult) * v_inj_local,
        upper = as.numeric(upper_mult) * v_inj_local
      ))
    } else {
      warning(sprintf("Unknown parameter name: %s, using default bounds [0, 10]", name))
      return(c(lower = 0, upper = 10))
    }
  }

  base_bounds <- get_default_param_bound(param_name, v_inj_local = v_inj)

  # 运行时覆盖（仅在提供了合法覆盖时生效）
  if (is.list(override_bounds) && !is.null(override_bounds[[param_name]])) {
    candidate <- override_bounds[[param_name]]
    lower_c <- suppressWarnings(as.numeric(candidate["lower"])[1])
    upper_c <- suppressWarnings(as.numeric(candidate["upper"])[1])
    if (is.finite(lower_c) && is.finite(upper_c)) {
      if (lower_c > upper_c) {
        tmp <- lower_c
        lower_c <- upper_c
        upper_c <- tmp
      }
      if (lower_c == upper_c) {
        upper_c <- upper_c + .Machine$double.eps^0.5
      }
      return(c(lower = lower_c, upper = upper_c))
    }
  }
  
  base_bounds
}

#' 获取多个参数的边界
#' 
#' @param param_names 参数名称向量
#' @param v_inj 注射体积（用于 V_init）
#' @param override_bounds 可选的运行时边界覆盖列表（按参数名映射为 c(lower, upper)）
#' @return 包含 lower 和 upper 的列表，每个都是命名向量
#' 
#' @examples
#' get_parameter_bounds(c("logK1", "H1", "V_init"), v_inj=0.01)
get_parameter_bounds <- function(param_names, v_inj = NULL, override_bounds = NULL) {
  n <- length(param_names)
  lower <- numeric(n)
  upper <- numeric(n)
  names(lower) <- names(upper) <- param_names
  
  for(i in seq_along(param_names)) {
    nm <- param_names[i]
    bounds <- get_param_bound(nm, v_inj = v_inj, override_bounds = override_bounds)
    lower[i] <- bounds["lower"]
    upper[i] <- bounds["upper"]
  }
  
  return(list(lower = lower, upper = upper))
}

# ==============================================================================
# 验证函数
# ==============================================================================

#' 验证参数值是否在有效范围内
#' 
#' @param param_name 参数名称
#' @param value 参数值
#' @param v_inj 注射体积（仅当 param_name 为 "V_init" 时需要）
#' @return 逻辑值，TRUE 表示有效，FALSE 表示无效
validate_param_value <- function(param_name, value, v_inj = NULL) {
  if(is.null(value) || is.na(value)) return(FALSE)
  
  bounds <- get_param_bound(param_name, v_inj)
  return(value >= bounds["lower"] && value <= bounds["upper"])
}
