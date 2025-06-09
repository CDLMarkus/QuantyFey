## Server logic

modules_path <- "./Modules"
module_files <- list.files(modules_path, recursive = TRUE, full.names = TRUE, pattern = "module.*\\.r$")
lapply(module_files, source)

server <- function(input, output, session){

  observe({
    invalidateLater(100, session)
    session$sendCustomMessage("enable-blur-update", list("span_width", "quant_indicator", "qual_indicator", "IS_indicator"))
  })

  observe({
    invalidateLater(1000)
    req(input$file1)
    req(input$Compound != "")
    req(!is.null(rv$df_QQ))
    req("Quant.Name" %in% colnames(rv$df_QQ))
    req("Qual.Name" %in% colnames(rv$df_QQ))

    cond <- is.null(rv$df_QQ$Qual.Name[which(rv$df_QQ$Quant.Name == input$Compound)]) ||
            is.na(rv$df_QQ$Qual.Name[which(rv$df_QQ$Quant.Name == input$Compound)])

    if (cond) {
      shinyjs::hide(selector = "#data_visualization .nav-link[data-value='QQ_analysis']")
    } else {
      shinyjs::show(selector = "#data_visualization .nav-link[data-value='QQ_analysis']")
    }
  })

  observe({
    session$sendCustomMessage("registerDoubleClick", "quant")
  })



  # ======= Reactive Values ==========

  rv <- reactiveValues(
    orig = NULL,
    data = NULL,
    orig_RT = NULL,
    data_RT = NULL,
    bracketing_table = NULL,
    selection_table = NULL,
    selection_table_bracketing = NULL,
    selection_cals_table = NULL,
    quantitation_data = NULL,
    drift_corrected_data_temp = NULL,
    quantitate_temp = NULL,
    acc_table = NULL,
    results = NULL,
    LLOQs = NULL,
    settings_used = list(),
    compounds_analyzed = c(),
    regression_model = NULL,
    setup_cal = NULL,
    correction_factors = NULL,
    backup_data = NULL,
    corr_df = NULL,
    norm_data = NULL,
    Area_backup = NULL,
    area_for_quant = NULL,
    Classification_temp = NULL,
    Area = NULL,
    IS_ratio = NULL,
    dc_area = NULL,
    IS_table = NULL,
    IS_compound_data_for_correction = NULL,
    IS_plot = NULL,
    df_QQ = NULL,
    force_0 = FALSE,
    current_layout = NULL,
    current_layout_acc = NULL,
    current_layout_dc = NULL,
    current_layout_IS = NULL,
    weights = NULL,
    mod_ind = NULL,
    Class_ind = NULL,
    weights_ind = NULL,


    ## Plots
    p_blank = NULL,
    p_quan_qual = NULL,
    p_drift_correction = NULL,
    p_quantitate = NULL,
    p_IS_analysis = NULL,
    p_RT = NULL,
    templates = template_list
  )

  # =========== Data upload module ===============

  data_upload_module(input, output, session, rv)

  # ============ Configure Settings ================

  configure_settings_module(input, output, session, rv)

  ## ======== Compound Quantification ============

  compound_quantification_parameters_module(input, output, session, rv)

  # =========== Visualization Module =============

  visualization_module(input, output, session, rv)

  ## ============= Drift Correction ===============

  drift_correction_module(input, output, session, rv)

  # ========= IS Correction =========

  IS_correction_module(input, output, session, rv)

  # ========= Bracketing =============

  bracketing_module(input, output, session, rv)


  # ========= Individual Bracketing =============

  individual_bracketing_module(session, input, output, rv)

  # ============ Quantitate ===============

  quantification_module(input, output, session, rv)

  # ========== Results Tab ===============

  results_module(input, output, session, rv)


  # ========= Session End Function ========

  session$onSessionEnded(function() {
    stopApp()
  })
}
