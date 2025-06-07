# Bracketing_module

source("./Modules/Bracketing/bracketing_helper.r")

bracketing_module <- function(input, output, session, rv) {
  # ========= Bracketing Logic =============

observeEvent(input$dtable_cell_edit, {
  tryCatch({
    observe_input_cell_edit_bracketing(input, rv, session)
  }, error = function(e) {
    showNotification("Error in cell edit: " %||% e$message, type = "error")
  })
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
