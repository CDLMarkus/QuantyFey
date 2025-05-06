# results module


results_module <- function(input, output, session, rv){

  output$Results <- renderDT({
    req(input$file1)

    if (is.null(rv$results)) {
      NULL
    } else {
      df_vis <- rv$results

      datatable(df_vis, options = list(lengthMenu = list(c(-1, 15), c("All", "15"))), editable = T)
    }
  })

  observeEvent(input$Results_cell_edit, {
    info <- input$Results_cell_edit

    rv$results[info$row, info$col] <- info$value
  })




}