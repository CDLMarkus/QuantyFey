## Module Visualization for Compound Quantification

#source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "./Modules/Visualization/visualization_helper.r"))



    # --------- Visualization --------------
visualization_module <- function(input, output, session, rv) {



  


# ------ Output --------

output$Blank_plot <- renderPlotly({
    p <- suppressWarnings({get_plotly_blank(input, rv)})

    rv$p_blank <- p
    
    p <- ggplotly(p)

    p
})

output$qual_quant_plot <- renderPlotly({
    req(input$file1)

    p <- suppressWarnings({get_plotly_qual_quant(input, rv)})
    rv$p_quan_qual <- p

    
      
    suppressWarnings({
      p <- ggplotly(p, tooltip = c("ion_ratios", "Sample.Name", "Sequence.Position")) %>% layout(
      xaxis = list(
        mode = "category",
        autorange = TRUE,
        
        
        fixedrange = FALSE  # allow zoom
      ),
      yaxis = list(
        autorange = TRUE,
        autotick = TRUE,
        tickmode = "auto",
        fixedrange = FALSE
      )
      )

    })
    try({p$x$data <- lapply(p$x$data, function(trace) {
      if(trace$type == "box") {
      trace$marker <- modifyList(trace$marker, list(opacity = 0))  # hide outliers
      }
      return(trace)
    })}, silent = TRUE)
    # Remove outlier dots from the boxplot
    

    p

    
    

}) 


output$RT_plot <- renderPlotly({

    p <- suppressWarnings({get_plotly_RT(input, rv)})
    rv$p_RT <- p
   
    suppressWarnings({
        p <- ggplotly(p, tooltip = c("Retention.Time", "Sample.Name", "Sequence.Position")) %>% layout(
        xaxis = list(
        mode = "category",
        autorange = TRUE,
        
        
        fixedrange = FALSE  # allow zoom
      ),
        yaxis = list(
          autorange = TRUE,
          autotick = TRUE,
          tickmode = "auto",
          fixedrange = FALSE
        )
      )
    })
    try({p$x$data <- lapply(p$x$data, function(trace) {
      if(trace$type == "box") {
      trace$marker <- modifyList(trace$marker, list(opacity = 0))  # hide outliers
      }
      return(trace)
    })}, silent = TRUE)


    
    

    p
})

}
