# ==============================================================================
# 3. Shiny UI
# ==============================================================================

ui <- fluidPage(
  theme = NULL,
  tags$head(
    tags$style(HTML("
      /* 状态灯样式 */
      .simfit-root .status-light { width: 12px; height: 12px; border-radius: 50%; display: inline-block; margin-right: 5px; }
      .simfit-root .light-green { background-color: #2ecc71; box-shadow: 0 0 4px #2ecc71; }
      .simfit-root .light-red { background-color: #e74c3c; box-shadow: 0 0 4px #e74c3c; animation: blink 1s infinite; }
      @keyframes blink { 0% { opacity: 1; } 50% { opacity: 0.5; } 100% { opacity: 1; } }
      
      /* 参数组左边框装饰 */
      .simfit-root .param-group { border-left: 4px solid #eee; padding-left: 10px; margin-bottom: 10px; }
      
      /* 中栏：表格容器样式（保持较矮，方便看下面的图） */
      .simfit-root .table-scroll-container {
        height: 165px; 
        overflow: hidden; /* 改为 hidden，因为 DT 表格自己处理滚动 */
        border: 1px solid #ddd;
        padding: 5px;
        background-color: #fff;
        margin-bottom: 15px;
      }
      
      /* 【新增】左栏：滑条容器样式（缩短高度，为路径构建模块腾出空间） */
      .simfit-root .slider-scroll-container {
        height: 260px;  /* Manual Tuning 滑条区高度（新增 rxn_U 后缩短以节省纵向空间） */
        overflow-y: auto; 
        padding-right: 5px; /* 防止滚动条遮挡内容 */
      }
      
      /* 紧凑化 */
      .simfit-root .well { padding: 15px; margin-bottom: 15px; }
      .simfit-root h4 { margin-top: 5px; font-weight: bold; font-size: 16px; }
      
      /* --- 按钮样式统一化 --- */
      /* 定义按钮基础变量 */
      .simfit-root {
        --btn-radius: 4px;
        --btn-shadow: 0 1px 3px rgba(0,0,0,0.1);
        --btn-font-size: 13px;
        --btn-padding: 6px 12px;
        
        /* 颜色变量 (淡色系) */
        --c-danger-bg: #f8d7da; --c-danger-text: #721c24; --c-danger-border: #f5c6cb;
        --c-success-bg: #d4edda; --c-success-text: #155724; --c-success-border: #c3e6cb;
        --c-warning-bg: #fff3cd; --c-warning-text: #856404; --c-warning-border: #ffeeba;
        --c-default-bg: #f8f9fa; --c-default-text: #343a40; --c-default-border: #dae0e5;
        --c-info-bg:    #d1ecf1; --c-info-text: #0c5460; --c-info-border: #bee5eb;
        /* 淡紫色 (全局搜索/primary)，与其它淡色按钮统一 */
        --c-primary-bg: #e8dcf5; --c-primary-text: #4a2c7a; --c-primary-border: #d4c4eb;
      }
      
      /* 通用按钮类 (覆盖 Bootstrap) */
      .simfit-root .btn {
        border-radius: var(--btn-radius) !important;
        box-shadow: var(--btn-shadow);
        font-size: var(--btn-font-size) !important;
        padding: var(--btn-padding) !important;
        border: 1px solid transparent !important;
        transition: all 0.2s;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        height: 32px; /* 统一高度 */
        background-image: none !important; /* 【关键修复】移除 Bootstrap 默认渐变 */
        text-shadow: none !important;      /* 移除文字阴影 */
      }
      
      .simfit-root .btn:hover { box-shadow: 0 2px 5px rgba(0,0,0,0.15); transform: translateY(-1px); }
      .simfit-root .btn:active { box-shadow: inset 0 1px 2px rgba(0,0,0,0.1); transform: translateY(0); }
      
      /* 颜色定义 (解决优先级问题：将 default 放在最前，且增加特异性) */
      
      /* 1. 灰色 (导出/默认) - 优先级最低 */
      .simfit-root .btn-default { 
        background-color: var(--c-default-bg) !important; 
        color: var(--c-default-text) !important; 
        border-color: var(--c-default-border) !important; 
      }
      .simfit-root .btn-default:hover, .simfit-root .btn-default:focus { background-color: #e2e6ea !important; }
      
      /* 2. 红色 (清除/自动拟合) */
      .simfit-root .btn.btn-danger { 
        background-color: var(--c-danger-bg) !important; 
        color: var(--c-danger-text) !important; 
        border-color: var(--c-danger-border) !important; 
      }
      .simfit-root .btn.btn-danger:hover, .simfit-root .btn.btn-danger:focus { background-color: #f1b0b7 !important; }
      
      /* 3. 绿色 (保存) */
      .simfit-root .btn.btn-success { 
        background-color: var(--c-success-bg) !important; 
        color: var(--c-success-text) !important; 
        border-color: var(--c-success-border) !important; 
      }
      .simfit-root .btn.btn-success:hover, .simfit-root .btn.btn-success:focus { background-color: #c3e6cb !important; }

      /* 4. 黄色 (迭代) */
      .simfit-root .btn.btn-warning { 
        background-color: var(--c-warning-bg) !important; 
        color: var(--c-warning-text) !important; 
        border-color: var(--c-warning-border) !important; 
      }
      .simfit-root .btn.btn-warning:hover, .simfit-root .btn.btn-warning:focus { background-color: #ffeeba !important; }
      
      /* 5. 蓝色 (导入) */
      .simfit-root .btn.btn-info, .simfit-root .btn-info { 
        background-color: var(--c-info-bg) !important; 
        color: var(--c-info-text) !important; 
        border-color: var(--c-info-border) !important; 
      }
      .simfit-root .btn.btn-info:hover, .simfit-root .btn.btn-info:focus { background-color: #bee5eb !important; }
      
      /* 6. 淡紫色 (全局搜索/DE) */
      .simfit-root .btn.btn-primary {
        background-color: var(--c-primary-bg) !important;
        color: var(--c-primary-text) !important;
        border-color: var(--c-primary-border) !important;
      }
      .simfit-root .btn.btn-primary:hover, .simfit-root .btn.btn-primary:focus { background-color: #ddd0f0 !important; }
      
      /* 文件上传按钮特殊处理 (映射到 淡蓝色) */
      /* 隐藏原本的进度条和文件名输入框，只留按钮本身 (或者美化整体) */
      /* 方案：Shiny fileInput 生成 .form-group -> .input-group -> label + input */
      /* 我们将 input-group 视为按钮容器 */
      
      .simfit-root .shiny-input-container { margin-bottom: 0 !important; }
      
      /* 强制文件上传控件看起来像一个按钮 */
      .simfit-root .file-input-btn-style .input-group {
         display: flex;
         width: 100%;
      }
      
      /* 隐藏文本框 (如果需要纯按钮感)，或者美化它 */
      /* 这里我们保留文本框但美化，使其与 Browse 按钮融为一体，或者让 Browse 按钮占据主导 */
      .simfit-root .file-input-btn-style .input-group-btn {
        width: 100%;
      }
      
      .simfit-root .file-input-btn-style .btn-file {
        width: 100%;
        border-radius: var(--btn-radius) !important;
        
        /* [修改] 导入按钮使用淡蓝色 */
        background-color: var(--c-info-bg) !important; 
        color: var(--c-info-text) !important;
        border: 1px solid var(--c-info-border) !important;
        background-image: none !important; /* 确保无渐变 */
        
        font-weight: normal;
        text-align: center;
      }
      
      .simfit-root .file-input-btn-style .form-control {
        display: none; /* 隐藏文件名显示框，实现纯按钮外观 */
      }
      
      /* 进度条隐藏 */
      .simfit-root .shiny-file-input-progress { display: none !important; }
      
      /* Flex 布局工具类 */
      .simfit-root .flex-btn-row {
        display: flex;
        gap: 10px;
        align-items: center;
        width: 100%;
      }
      
      .simfit-root .flex-btn-item {
        flex: 1; /* 均分宽度 */
      }
      
      /* 标题栏操作按钮布局 (响应式 Flex) */
      .simfit-root .header-action-row {
        display: flex;
        justify-content: space-between;
        align-items: center;
        flex-wrap: wrap; /* 允许换行，适应小屏幕 */
        gap: 10px;
        margin-bottom: 10px;
      }
      
      .simfit-root .header-action-row h4 {
        margin: 0; /* 移除默认边距以对齐 */
        white-space: nowrap; /* 标题不换行 */
        line-height: 1.2;
      }
      
      /* 让文件上传按钮在 Flex 容器中自适应 */
      .simfit-root .header-action-row .file-input-btn-style {
        flex: 1; /* 占据剩余空间 */
        min-width: 150px; /* 最小宽度限制，空间不足时触发换行 */
        margin-bottom: 0 !important;
      }

      /* 定制 verbatimTextOutput 生成的 pre 标签样式 */
      /* 直接通过 ID 选择器应用样式，因为 verbatimTextOutput 不支持 class 参数 */
      .simfit-root #fit_status {
        flex: 1; /* 自适应宽度 */
        height: 34px !important; /* 降低高度，与按钮匹配 */
        padding: 4px 10px !important; /* 调整内边距实现垂直居中 */
        font-size: 14px !important;
        line-height: 24px !important; /* 行高 = height - 2*padding */
        margin: 0 !important; /* 移除默认外边距 */
        background-color: #fff !important;
        border: 1px solid #ccc !important;
        border-radius: 4px !important;
        box-shadow: inset 0 1px 2px rgba(0,0,0,0.05);
        color: #333;
        transition: all 0.2s;
        overflow: hidden; /* 防止溢出 */
        white-space: nowrap; /* 单行显示 */
        text-overflow: ellipsis;
      }
      
      .simfit-root #fit_status:hover {
        border-color: #aaa !important;
        background-color: #fcfcfc !important;
      }
      
      /* --- RSS 显示框美化 --- */
      .simfit-root .rss-container {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-top: 10px;
      }

      .simfit-root .fit-rss-row {
        margin-bottom: 10px;
      }

      .simfit-root .fit-action-row {
        margin-top: 2px;
        margin-bottom: 10px;
      }

      .simfit-root .fit-error-wrap {
        margin-top: 2px;
      }

      .simfit-root .fit-error-head {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 10px;
        margin-bottom: 8px;
      }

      .simfit-root .fit-error-head h5 {
        margin: 0;
        white-space: nowrap;
      }

      .simfit-root .fit-error-head .shiny-input-container {
        margin-bottom: 0 !important;
      }

      .simfit-root .fit-error-head .checkbox {
        margin: 0 !important;
        min-height: 20px;
      }
      
      .simfit-root .rss-container h5 {
        margin: 0;
        white-space: nowrap;
        font-weight: bold;
        color: #555;
      }
      
      /* 定制 verbatimTextOutput 生成的 pre 标签样式 */
      .simfit-root .rss-display-box {
        flex: 1; /* 自适应宽度 */
        height: 34px !important; /* 降低高度，与按钮匹配 */
        padding: 4px 10px !important; /* 调整内边距实现垂直居中 */
        font-size: 14px !important;
        line-height: 24px !important; /* 行高 = height - 2*padding */
        margin: 0 !important; /* 移除默认外边距 */
        background-color: #fff !important;
        border: 1px solid #ccc !important;
        border-radius: 4px !important;
        box-shadow: inset 0 1px 2px rgba(0,0,0,0.05);
        color: #333;
        transition: all 0.2s;
        overflow: hidden; /* 防止溢出 */
        white-space: nowrap; /* 单行显示 */
        text-overflow: ellipsis;
      }
      
      .simfit-root .rss-display-box:hover {
        border-color: #aaa !important;
        background-color: #fcfcfc !important;
      }
      
      /* [新增] 模型选择表格化布局样式 */
      .simfit-root .model-row { 
        display: flex; 
        align-items: center; 
        width: 100%; 
        padding: 0; /* 移除内部填充，完全由 margin 控制高度 */
      }
      .simfit-root .col-mod { width: 75px; font-weight: bold; color: #333; }
      .simfit-root .col-par { width: 55px; font-family: monospace; color: #d35400; font-weight: bold; font-size: 0.95em; }
      .simfit-root .col-sto { width: 55px; color: #2980b9; font-style: italic; }
      .simfit-root .col-dsc { flex: 1; color: #7f8c8d; font-size: 0.85em; }
      
      .simfit-root .model-header {
        display: flex;
        font-weight: bold;
        color: #555;
        border-bottom: 1px solid #ddd;
        margin-bottom: 5px;
        padding-bottom: 4px;
        padding-left: 20px; /* Checkbox offset */
        font-size: 0.85em;
      }
      
      /* 统一复选框行高，更紧凑 */
      .simfit-root .checkbox { 
        margin-top: 2px !important; 
        margin-bottom: 2px !important; 
        min-height: 20px; /* 防止过矮 */
      }
      
      /* [新增] 标签页按钮分成两行显示 */
      .simfit-root .two-row-tabs .nav-tabs {
        flex-wrap: wrap !important;
        display: flex !important;
        border-bottom: 1px solid #ddd;
      }
      
      .simfit-root .two-row-tabs .nav-tabs > li {
        flex: 0 0 auto !important;
        float: none !important;
      }
      
      /* 确保标签页内容区域正常显示 */
      .simfit-root .two-row-tabs .tab-content {
        width: 100%;
        clear: both;
      }
      
      /* 确保标签页按钮在换行时正确对齐 */
      .simfit-root .two-row-tabs .nav-tabs::after {
        content: '';
        display: table;
        clear: both;
      }
      
      /* [新增] 嵌套标签页样式 - 确保子标签页正确显示 */
      .simfit-root .two-row-tabs .tab-content .tab-content {
        padding: 0;
      }
      
      /* 确保嵌套标签页的绘图区域正确渲染 */
      .simfit-root .two-row-tabs .tab-content .tab-content .shiny-plot-output {
        width: 100% !important;
        height: 100% !important;
      }
      
      /* 嵌套标签页的标签按钮样式 */
      .simfit-root .two-row-tabs .nav-tabs .nav-tabs {
        margin-bottom: 10px;
        border-bottom: 1px solid #ddd;
      }
      
      .simfit-root .two-row-tabs .nav-tabs .nav-tabs > li > a {
        padding: 8px 12px;
        font-size: 12px;
      }
      
      /* [新增] 响应式布局：在窄屏模式下调整参数快照位置 */
      @media (max-width: 768px) {
        /* 在窄屏时，让row变成flex容器 */
        .simfit-root .row {
          display: flex !important;
          flex-direction: column !important;
        }
        
        /* 关键：让左侧列容器使用 display: contents，使其子元素提升到row层级 */
        /* 这样plot-section和snapshot-section就可以和其他列一起参与order排序 */
        .simfit-root .left-column-container {
          display: contents !important;
        }
        
        /* 绘图区域保持在最前 */
        .simfit-root .plot-section {
          order: 1 !important;
          width: 100% !important;
        }
        
        /* 中栏排在绘图之后 */
        .simfit-root .middle-column {
          order: 2 !important;
        }
        
        /* 右栏排在中栏之后 */
        .simfit-root .right-column {
          order: 3 !important;
        }
        
        /* 参数快照移到最后 */
        .simfit-root .snapshot-section {
          order: 4 !important;
          width: 100% !important;
        }
      }
      
    "))
  ),
  
  tags$head(tags$title("ViaBind: Your Path, Your Model!")),
  
  div(
    class = "simfit-root",
    fluidRow(
    # ==========================================================
    # 1. 左栏 (Width 6): 主作图区 (原中间栏)
    # ==========================================================
    column(6, class="left-column-container",

           div(class="plot-section", style="position: relative;",
               uiOutput("plot_tabs")
           ),
           wellPanel(class="snapshot-section",
             div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;",
                 h4(uiOutput("section_param_snapshot_title")),
                 div(
                   column(6, uiOutput("snap_name_input")),
                   column(3, uiOutput("save_params_button")),
                   column(3, uiOutput("clear_params_button"))
                 )
             ),
             div(class = "table-scroll-container", DTOutput("param_table")),
            
             # [UI优化] 导出与导入参数按钮并排
             div(class = "flex-btn-row",
                 div(class = "flex-btn-item",
                     uiOutput("export_params_button")
                 ),
                 div(class = "flex-btn-item file-input-btn-style",
                     uiOutput("import_params_file_input")
                 )
             )
             
           )
    ),
    
    # ==========================================================
    # 2. 中栏 (Width 3): 路径构建和变量手调区
    # ==========================================================
    column(3, class="middle-column",
           wellPanel(
             # [UI微调] 标题与说明同一行
             div(style="margin-bottom: 10px; display:flex; justify-content:space-between; align-items:center;",
                 uiOutput("section_path_build_title"),
                 div(style="display:flex; gap:6px; align-items:center;",
                     uiOutput("sim_to_exp_button"),
                     uiOutput("rm_exp_button")
                 )
             ),
             
             # [新增] 表格头
             div(class = "model-header",
                 div(class="col-mod", uiOutput("model_header_model")),
                 div(class="col-par", uiOutput("model_header_param")),
                 div(class="col-sto", uiOutput("model_header_stoich")),
                 div(class="col-dsc", uiOutput("model_header_desc"))
             ),
             
             # [UI修改] 基础反应 (rxn_M) - 静态行，匹配表格样式
             # 移除 inline margin，使用全局 .checkbox 样式以保持一致
             div(class = "checkbox", style = "color: gray;",
                 tags$label(
                   tags$input(type = "checkbox", checked = NA, disabled = NA),
                   # [修复] 完全模拟 checkboxGroupInput 的 DOM 结构: input + span > div
                   tags$span(
                     div(class="model-row",
                          span(class="col-mod", "H+G=M"),
                          span(class="col-par", "K1 H1"),
                          span(class="col-sto", "H\u2081G\u2081"),
                          span(class="col-dsc", uiOutput("model_base_note"))
                     )
                   )
                 )
             ),
             br(),
             uiOutput("active_paths_checkbox")
           ),
           wellPanel(
             div(class = "header-action-row",
                 h4(uiOutput("section_manual_adjust_title")),
                 # [新增] 恢复默认按钮 (淡红色, btn-danger) 
                 uiOutput("reset_defaults_button")
             ),
             
             # 【修改】使用新的高容器包裹滑条
             div(class = "slider-scroll-container",
                 
                # --- 基础反应 M (始终显示) ---
                div(class="param-group", style="border-left-color: #3498db;",
                    strong(uiOutput("param_base_title", inline = TRUE)),
                    sliderInput("logK1", "logK1", 
                               min = PARAM_BOUNDS$logK["lower"], 
                               max = PARAM_BOUNDS$logK["upper"], 
                               value = DEFAULT_PARAMS$logK, 
                               step = 0.001, ticks = FALSE),
                    sliderInput("H1", "dH1", 
                               min = PARAM_BOUNDS$H["lower"], 
                               max = PARAM_BOUNDS$H["upper"], 
                               value = DEFAULT_PARAMS$H, 
                               step = 100, ticks = FALSE)
                ),
                 
                # --- Dimer (D) ---
                conditionalPanel("input.active_paths.includes('rxn_D')",
                                 div(class="param-group", style="border-left-color: #e67e22;",
                                     strong(uiOutput("param_stepwise_title", inline = TRUE)),
                                     sliderInput("logK2", "logK2", 
                                                min = PARAM_BOUNDS$logK["lower"], 
                                                max = PARAM_BOUNDS$logK["upper"], 
                                                value = DEFAULT_PARAMS$logK, 
                                                step = 0.001, ticks = FALSE),
                                     sliderInput("H2", "dH2", 
                                                min = PARAM_BOUNDS$H["lower"], 
                                                max = PARAM_BOUNDS$H["upper"], 
                                                value = DEFAULT_PARAMS$H, 
                                                step = 100, ticks = FALSE))
                ),
                 
                # --- Trimer (T) ---
                conditionalPanel("input.active_paths.includes('rxn_T')",
                                 div(class="param-group", style="border-left-color: #9b59b6;",
                                     strong(uiOutput("param_dimer_title", inline = TRUE)),
                                     sliderInput("logK3", "logK3", 
                                                min = PARAM_BOUNDS$logK["lower"], 
                                                max = PARAM_BOUNDS$logK["upper"], 
                                                value = DEFAULT_PARAMS$logK, 
                                                step = 0.001, ticks = FALSE),
                                     sliderInput("H3", "dH3", 
                                                min = PARAM_BOUNDS$H["lower"], 
                                                max = PARAM_BOUNDS$H["upper"], 
                                                value = DEFAULT_PARAMS$H, 
                                                step = 100, ticks = FALSE))
                ),
                 
                # --- Host Bind (B) ---
                conditionalPanel("input.active_paths.includes('rxn_B')",
                                 div(class="param-group", style="border-left-color: #f1c40f;",
                                     strong(uiOutput("param_reverse_title", inline = TRUE)),
                                     sliderInput("logK4", "logK4", 
                                                min = PARAM_BOUNDS$logK["lower"], 
                                                max = PARAM_BOUNDS$logK["upper"], 
                                                value = DEFAULT_PARAMS$logK, 
                                                step = 0.001, ticks = FALSE),
                                     sliderInput("H4", "dH4", 
                                                min = PARAM_BOUNDS$H["lower"], 
                                                max = PARAM_BOUNDS$H["upper"], 
                                                value = DEFAULT_PARAMS$H, 
                                                step = 100, ticks = FALSE))
                ),
                 
               # --- T+G (F) ---
               conditionalPanel("input.active_paths.includes('rxn_F')",
                                div(class="param-group", style="border-left-color: #1abc9c;",
                                    strong(uiOutput("param_oligomer_title", inline = TRUE)),
                                    sliderInput("logK5", "logK5", 
                                               min = PARAM_BOUNDS$logK["lower"], 
                                               max = PARAM_BOUNDS$logK["upper"], 
                                               value = DEFAULT_PARAMS$logK, 
                                               step = 0.001, ticks = FALSE),
                                    sliderInput("H5", "dH5", 
                                               min = PARAM_BOUNDS$H["lower"], 
                                               max = PARAM_BOUNDS$H["upper"], 
                                               value = DEFAULT_PARAMS$H, 
                                               step = 100, ticks = FALSE))
               ),
                
               # --- Bending (U) ---
               conditionalPanel("input.active_paths.includes('rxn_U')",
                                div(class="param-group", style="border-left-color: #e74c3c;",
                                    strong(uiOutput("param_bending_title", inline = TRUE)),
                                    sliderInput("logK6", "logK6", 
                                               min = PARAM_BOUNDS$logK["lower"], 
                                               max = PARAM_BOUNDS$logK["upper"], 
                                               value = DEFAULT_PARAMS$logK, 
                                               step = 0.001, ticks = FALSE),
                                    sliderInput("H6", "dH6", 
                                               min = PARAM_BOUNDS$H["lower"], 
                                               max = PARAM_BOUNDS$H["upper"], 
                                               value = DEFAULT_PARAMS$H, 
                                               step = 100, ticks = FALSE))
               ),
             ), # End slider-scroll-container
             
             tags$hr(),
             h4(uiOutput("param_correction_title")),
             # [响应式布局优化] 使用 Flexbox 替代 splitLayout，实现自动换行和自适应宽度
             div(style="display: flex; flex-wrap: wrap; gap: 5px;",
                div(style="flex: 1 0 30%; min-width: 85px;", uiOutput("factor_H_input")),
                div(style="flex: 1 0 30%; min-width: 85px;", uiOutput("factor_G_input")),
                div(style="flex: 1 0 30%; min-width: 85px;", uiOutput("V_init_input"))
            ),
             uiOutput("heat_offset_slider")
           )
    ),

    # ==========================================================
    # 3. 右栏 (Width 3): 实验数据与条件和拟合模块
    # ==========================================================
    column(3, class="right-column",
           wellPanel(
             # [UI优化] 标题与导入按钮并排显示，响应式布局
             div(class = "header-action-row",
                 h4(uiOutput("section_exp_data_title")),
                 div(class = "file-input-btn-style",
                     uiOutput("exp_file_input")
                 )
             ),
             uiOutput("exp_file_summary_ui"),
            splitLayout(
              numericInput("H_cell_0", "H, Cell (mM)",
                          value = UI_DEFAULTS$conc_cell_default,
                          min = UI_DEFAULTS$conc_cell_min,
                          max = UI_DEFAULTS$conc_cell_max), 
              numericInput("G_syringe", "G, Syr. (mM)",
                          value = UI_DEFAULTS$conc_syringe_default,
                          min = UI_DEFAULTS$conc_syringe_min,
                          max = UI_DEFAULTS$conc_syringe_max)
            ),
            splitLayout(
              numericInput("V_cell", "Cell V (mL)",
                          value = UI_DEFAULTS$v_cell_default,
                          min = UI_DEFAULTS$v_cell_min,
                          max = UI_DEFAULTS$v_cell_max), 
              numericInput("V_inj", "per Inj V (uL)",
                          value = UI_DEFAULTS$v_inj_default * 1000,  # 转换为 uL
                          min = UI_DEFAULTS$v_inj_min * 1000,
                          max = UI_DEFAULTS$v_inj_max * 1000)
            ),
            splitLayout(
              cellWidths = c("33%", "33%", "34%"),
              numericInput("n_inj", "N Inj",
                          value = UI_DEFAULTS$n_inj_default,
                          min = 2,  # 修复：限制最小注射次数为 2，避免单次注射的向量化边界问题
                          max = UI_DEFAULTS$n_inj_max),
              numericInput("V_pre", "V_inj#1 (uL)",
                          value = UI_DEFAULTS$v_pre_default, min=0, step=0.1),
              numericInput("Temp", "Temp. (K)",
                          value = UI_DEFAULTS$temp_default,
                          min = UI_DEFAULTS$temp_min,
                          max = UI_DEFAULTS$temp_max,
                          step = 0.1)
            )
           ),
           wellPanel(
             div(style="display:flex; justify-content:space-between; align-items:center;",
                 h4(uiOutput("section_fitting_title")), uiOutput("status_indicator")),
           uiOutput("dynamic_fit_params_ui"),
           uiOutput("fit_data_range_slider_label"),
           # [注意] 使用 UI_DEFAULTS$n_inj_default 作为初始最大值，启动时会根据实际 n_inj 更新
           sliderInput("fit_data_range", NULL, 
                      min = 1, 
                      max = UI_DEFAULTS$n_inj_default, 
                      value = c(2, UI_DEFAULTS$n_inj_default), 
                      step = 1, 
                      ticks = FALSE),
             div(style="display:flex; flex-wrap:wrap; gap:5px; margin-top:10px;",
                 uiOutput("fit_1_step_button"),
                 uiOutput("fit_10_step_button"),
                 uiOutput("fit_full_button"),
                 uiOutput("fit_global_button"),
                 uiOutput("report_button")
             ),
             
             # [UI优化] RSS 显示行
             div(class = "rss-container fit-rss-row",
                 h5(uiOutput("fit_rss_label")),
                 div(style="flex: 1;", # 容器包裹以确保 pre 标签正确拉伸
                     verbatimTextOutput("fit_status", placeholder = TRUE)
                 )
             ),

             div(class = "flex-btn-row fit-action-row",
                 div(class = "flex-btn-item",
                     uiOutput("data_to_plot_button")
                 ),
                 div(class = "flex-btn-item",
                     uiOutput("download_data_button")
                 )
             ),
             
             # [新增] 误差分析结果显示
             div(class = "fit-error-wrap", uiOutput("error_analysis_section"))
           )
    )
  )
)
)
