# Bracketing_module


bracketing_module <- function(input, output, session, rv) {
  # ========= Bracketing Logic =============

observeEvent(input$dtable_cell_edit, {
  info <- input$dtable_cell_edit  # Get the cell edit info

  # Validate the info object
  if (!is.null(info) & 
      info[["row"]] <= nrow(rv$selection_table_bracketing) & 
      info[["col"]] <= ncol(rv$selection_table_bracketing)) {
    # Update the reactive value
    

    rv$selection_table_bracketing[info[["row"]], info[["col"]]] <- ifelse(info[["value"]] == 1, TRUE, FALSE)


        try({
      update_cals(input, rv, session)
    }, silent = T)


  } else {
    showNotification("Invalid cell edit!", type = "error")
  }
})

# ------- Output --------

output[["dtable"]] <- renderDataTable(
  {

    #print(rv$selection_table_bracketing)
    #cat(rv$bracketing_table, "\n")
    datatable(
      rv$bracketing_table,
      rownames = FALSE,
      escape = FALSE,
      editable = TRUE,  # Ensure editing is enabled
      selection = "none",
      callback = DT::JS(js("dtable", checkboxesColumns)),  # Use the correct JS function
      options = list(
        lengthMenu = list(c(-1, 10, 25, 50), c("All", "10", "25", "50")),
        dom = "ltip",
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = 0, width = "20%", nowrap = TRUE),
          list(targets = 1:(ncol(rv$bracketing_table) - 1), width = "50px", nowrap = FALSE, className = "dt-center")
        )
      )
    )
  },
  server = FALSE  # Ensure server-side processing is disabled
)

}
