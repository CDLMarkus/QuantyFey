#### Weighted Bracketing Module


source("Modules/Weighted Bracketing/weighted_bracketing_helper.r")

weighted_bracketing_module <- function(session, input, output, rv) {



output$bracketing_model_plot <- renderPlotly({


tryCatch({
  p <- suppressWarnings({get_plot_w_bracketing(input, rv)})


# Convert to interactive plot
p <- ggplotly(p, tooltip = c("Sample.Name", "Injection Position", "PeakArea"))

})

if(!is.null(p)){
  p
}else{NULL}

})








} 


