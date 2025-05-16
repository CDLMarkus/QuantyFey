## Data upload module
source("Modules/Data_upload/data_upload_helper.r", local = TRUE)

data_upload_module <- function(input, output, session, rv) {

  observeEvent(input$file1, {

    req(input$file1)

    tryCatch({
      observe_input_file_1(input, rv, session)
    }, error = function(e) {
      showNotification(paste("Error reading file: ", e$message), type = "error")
    })

  })

  observeEvent(input$file_RT, {
    req(input$file_RT)

    df_temp <- read_file_safe(input$file_RT$datapath)
    req(df_temp)

    rv$orig_RT <- df_temp
    rv$data_RT <- rv$orig_RT

    updateCheckboxGroupInput(session, inputId = "RT_groups", choices = unique(rv$data$Sample.Type))

    rv$data_RT$Sample.Name <- as.character(rv$data_RT$Sample.Name)
  })

  observeEvent(input$reset, {
    showModal(modalDialog(
      title = "Confirm Reset",
      "Resetting will lose unsaved changes. Proceed?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Yes, reset", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_reset, {
    removeModal()
    reset_app(session, rv)
  })

  output$table1 <- renderDT({
    req(rv$data)
    datatable(rv$data[, 1:min(15, ncol(rv$data))], editable = FALSE, options = list(dom = "ltip" ,lengthMenu = list(c(15, -1), c("15", "All"))))
  })

  output$table_RT <- renderDT({
    req(rv$data_RT)
    datatable(rv$data_RT[, 1:min(15, ncol(rv$data_RT))], editable = FALSE, options = list(dom = "ltip", lengthMenu = list(c(15, -1), c("15", "All"))))
  })

}
