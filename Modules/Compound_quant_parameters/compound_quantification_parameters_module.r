### Compound Quantification Parameters logic

source("./Modules/Compound_quant_parameters/compound_quantification_parameters_helper.r")

compound_quantification_parameters_module <- function(input, output, session, rv){

  observeEvent(input$Compound_IS, {
    req(input$file1)
    req(input$Compound != "")

    if(is.null(rv$IS_table)){
      is_table <- data.frame(Sample.Type = unique(pd_temp(rv)), Correction.Factor = rep(1, length(unique(pd_temp(rv)))))
      rv$IS_table <- is_table
    }

    tryCatch({
      observe_input_compound_is(input, rv)
    }, error = function(e) {
      showNotification(paste("Error in IS compound setup:", e$message), type = "error")
    })
})

observeEvent(input$save_compound, {
  tryCatch({
    save_compound(input, rv, session)
  }, error = function(e) {
    message("Error in saving compound data: ", e$message, duration = 10)
  })
})

  
observeEvent(input$Compound, {
  req(input$file1)
  req(input$Compound != "")
  tryCatch({
    observe_input_compound(input, rv, session)
  }, error = function(e) {
    showNotification(paste("Error in compound setup:", e$message), type = "error")
  })
})




}
