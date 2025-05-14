
source("./Modules/Configure_settings/configure_settings_helper.r")
## Configure settings module

configure_settings_module <- function(input, output, session, rv) {

  observeEvent(input$quant_indicator, {
    req(input$file1)
    req(input$quant_indicator)

    tryCatch({
      observe_input_quant_indicator(input, rv, session)
    }, error = function(e) {
      showNotification("Error", "The selected quant pattern does not match the dataset. Please check that the pattern matches the Transition names exactly!", type = "error")
    })
  })

  observeEvent(input$IS_indicator, {
    req(input$file1)
    req(input$IS_indicator)

    tryCatch({
      observe_input_IS_indicator(input, rv, session)
    }, error = function(e) {
      showNotification("Error", "The selected IS pattern does not match the dataset. Please check that the pattern matches the Transition names exactly!", type = "error")
    })

  })

  observeEvent(input$mode, {
    req(input$file1)
    req(input$mode %in% names(rv$templates))
    req(rv$data)

    tryCatch({
      observe_input_mode(input, rv, session)
    }, error = function(e) {
      showNotification("Error", "Error in observe_input_mode: ", e$message, type = "error")
    })
  })

  # ----- Output -----

  output$setup_cal <- renderDT({
    req(rv$setup_cal)
    datatable(rv$setup_cal, editable = FALSE, options = list(lengthMenu = list(c(15, -1), c("15", "All")), dom = "tp"))
  })

  output$table_QQ <- renderDT({
    req(rv$data)
    req(input$quant_indicator)

    df_QQ <- get_qq_table(input, rv)

    
    datatable(df_QQ, options = list(dom = "tp"))

  })

  output$table_IS <- renderDT({
    req(rv$data)
    req(input$IS_indicator)

    is_cols <- colnames(rv$data)[grepl(input$IS_indicator, colnames(rv$data))]
    df_IS <- data.frame(IS.Names = is_cols, stringsAsFactors = FALSE)

    if(nrow(df_IS) == 0 | is.null(df_IS)) {
      showNotification("No IS columns found.")
      NULL
    } else {
      datatable(df_IS, options = list(dom = "tp"))
    }
    
  })

}
