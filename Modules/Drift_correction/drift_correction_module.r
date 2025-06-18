## Drift Correction Module

drift_correction_module <- function(input, output, session, rv) {
  observeEvent(input$model_drift, {
    req(input$file1)
    tryCatch({
      update_dc_data(input, rv)
    }, error = function(e) {
      showNotification(paste("Error in update_dc_data:", e$message), type = "error")
    })

    tryCatch({
      update_sel_cal_areas(input, rv)
    }, error = function(e) {
      showNotification(paste("Error in update_sel_cal_areas:", e$message), type = "error")
    })
  })

  observeEvent(input$files_for_correction, {
    req(input$file1)
    tryCatch({
      update_dc_data(input, rv)
      update_sel_cal_areas(input, rv)
    }, error = function(e) {
      showNotification(paste("Error in file correction:", e$message), type = "error")
    })
  })

  observeEvent(input$span_width, {
    req(input$file1)
    if (input$span_width < 0.4) {
      showNotification("Span width must be > 0.4.", type = "warning")
      # updateNumericInput(session, "span_width", min = 0.4, max = 2, value = 0.75, step = 0.05)
    }

    tryCatch({
      
      update_dc_data(input, rv)
     
      
    }, error = function(e) {
      showNotification(paste("Error in span width update:", e$message), type = "error")
      return(NULL)
    })
    update_cals(input, rv, session)
  })

  observeEvent(input$spline_df_dc, {
    req(input$file1)

    tryCatch({
      update_dc_data(input,rv)
    }, error = function(e){
      showNotification(type = "error", paste("An error occurred while updating the degree of freedom:", e$message, ". Please ensure that the degree is less than the number of available data points (files)."))
    })
    try({
      update_cals(inpt, rv, session)
    }, silent = T)
    

  })

# ------- Output ------

observeEvent(event_data("plotly_relayout", source = "dc"), {
  layout <- event_data("plotly_relayout", source = "dc")


  if (!is.null(layout)) {
    if (any(grepl("autorange", names(layout)))) {
      isolate({ rv$current_layout_dc <- NULL })
    } else {
      isolate({
        if (is.null(rv$current_layout_dc)) {
          rv$current_layout_dc <- layout
        } else {
          for (name in names(layout)) {
            if (name %in% names(rv$current_layout_dc)) {
              rv$current_layout_dc[[name]] <- layout[[name]]
            } else {
              rv$current_layout_dc[[name]] <- layout[[name]]
            }
          }
        }
      })
    }
  }
})

observeEvent(event_data("plotly_doubleclick", source = "dc"), {
  rv$current_layout_dc <- NULL
})



output$drift_output <- renderPlotly({

  suppressWarnings({
      tryCatch({
      update_dc_data(input, rv)
    }, error = function (e) {
       return(NULL)
          
    })
    
    try({
      p <- get_plotly_dc(input, rv)
    }, silent = TRUE)

  })

  
    

    # Convert ggplots to plotly objects
    suppressWarnings({
    g1 <- ggplotly(p[[1]], source = "dc") %>% event_register("plotly_relayout") 
  	g2 <- ggplotly(p[[2]], source = "dc") %>% event_register("plotly_relayout") 
    })


  # Manually adjust legend settings
  for(i in seq_along(g2$x$data)) {
    g2$x$data[[i]]$showlegend <- FALSE  # hide duplicate legend
    g2$x$data[[i]]$legendgroup <- g1$x$data[[i]]$legendgroup  # ensure same legend group
    }

    if(is.null(int_unit)) {
      ylab <- "Peak Area"
      ylab_corr <- "corrected Peak Area"
    } else {
      ylab <- paste("Peak Area [", int_unit, "]", sep = "")
      ylab_corr <- paste("corrected Peak Area [", int_unit, "]", sep = "")
    }

  

  # Combine with shared legend
  p <- subplot(g1, g2, nrows = 2, shareX = TRUE, shareY = FALSE) %>%
  layout(
    showlegend = TRUE,
    yaxis = list(title = ylab, tickformat = ".2e"),
    yaxis2 = list(title = ylab_corr, tickformat = ".2e"),
    xaxis2 = list(title = "Number of injection")
  )

  if(!is.null(rv$current_layout_dc)){
    p <- apply_current_layout(p, rv$current_layout_dc)

  }




    

  p
}) ## Render Drift Correction Plot

}

