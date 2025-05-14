

observe_input_cell_edit_bracketing <- function(input, rv, session) {
      info <- input$dtable_cell_edit  # Get the cell edit info

  # Validate the info object
  if (!is.null(info) & 
      info[["row"]] <= nrow(rv$selection_table_bracketing) & 
      info[["col"]] <= ncol(rv$selection_table_bracketing)) {
    # Update the reactive value
    

    rv$selection_table_bracketing[info[["row"]], info[["col"]]] <- ifelse(info[["value"]] == 1, TRUE, FALSE)


    tryCatch({
      update_cals(input, rv, session)
    }, error = function(e) {
        stop("Error in updating cals: " %||% e$message)
    })


  } else {
    stop("Invalid cell edit!")
  }
}