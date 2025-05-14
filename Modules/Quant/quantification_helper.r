observe_loq <- function(input, rv, session) {
    
    loq_input <- as.character(input$LOQ)

    if (grepl("\\.$", loq_input)) {
      loq_input <- paste0(loq_input, "0")
    }
    

    loq_value <- suppressWarnings(as.numeric(loq_input))
    
    if (is.na(loq_value)) {
      showNotification("Invalid input for LOQ. Please enter a numeric value.", type = "error")
      return(NULL)
    }
    
    if (!is.null(input$Block)) {
      blocks <- input$Block
      temp <- rv$selection_cals_table[[blocks]]
      
      if (!is.null(temp)) {
        for (i in seq_along(rv$selection_cals_table)) {
          if (all(unique(rv$selection_cals_table[[i]]$Classification) == unique(temp$Classification))) {
            blocks <- c(blocks, names(rv$selection_cals_table)[i])
          }
        }

        for (i in unique(blocks)) {
          rv$LLOQs[[i]] <- loq_value


        }
      }
    }
    
}