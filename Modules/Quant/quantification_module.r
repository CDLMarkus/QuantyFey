## Quantification Module

# modules/quantification_module.R


quantification_module <- function(input, output, session, rv) {


 observeEvent(input$optimize, {
    tryCatch({
      optimize_model_metrics(input, rv, session)
    }, error = function(e) {
      showNotification(paste("An error occurred:", e$message), type = "error")
    }, finally = {
      showNotification("Optimization completed successfully.", type = "message")
    })
    
})

  observeEvent(input$regression_model, {
    rv$regression_model <- input$regression_model
  })

  observeEvent(input$apply_levels, {
  tryCatch({
    
    levels_not_used <- unique(rv$selection_cals_table[[input$Block]]$Sample.Name[!rv$selection_cals_table[[input$Block]]$used])

    for (i in 1:length(rv$selection_cals_table)) {
      rv$selection_cals_table[[i]]$used <- sapply(rv$selection_cals_table[[i]]$Sample.Name, FUN = function(x) {
        return(ifelse(x %in% levels_not_used, FALSE, TRUE))
      })
    }
    

  }, error = function(e) {
    showNotification(paste("An error occurred:", e$message), type = "error")
  }, finally = {
    showNotification("Levels were applied to all blocks successfully.", type = "message")
  })
  })

  observeEvent(input$apply_LLOQs, {
    tryCatch({
      for (i in 1:length(rv$LLOQs)) {
      rv$LLOQs[[i]] <- input$LOQ
    }
    }, error = function(e) {
      showNotification(paste("An error occurred:", e$message), type = "error")
    }, finally = {
      showNotification("LLOQs were applied to all blocks successfully.", type = "message")
    })
    
    
  })

  observeEvent(input$weight_method, {
    all_present <- all(rv$setup_cal$Cal.Name %in% rv$data$Sample.Name)
    req(all_present)

    rv$force_0 <- ifelse(input$weight_method %in% c("1/x force 0", "1/y force 0"), T, F)
    
    for (i in names(rv$selection_cals_table)) {

      rv$selection_cals_table[[i]] <- add_weights(weight_method = input$weight_method, data = rv$selection_cals_table[[i]])
      #rv$selection_cals_table[[i]]$weight <- get_weights(weight_method = input$weight_method, data = rv$selection_cals_table[[i]])

    }
  })

#observeEvent(input$A_dblclick, {
#  rv$current_layout <- NULL
#})

  observeEvent(input$Block, {
    LOQ <- rv$LLOQs[[input$Block]]

    suppressWarnings({
      LOQ_min <- min(rv$setup_cal$Concentration)
      LOQ_max <- max(rv$setup_cal$Concentration)
    })
    
    updateNumericInput(session, inputId = "LOQ", value = LOQ, min = ifelse(is.finite(LOQ_min), LOQ_min, 0), max = ifelse(is.finite(LOQ_max), LOQ_max, 0), step = LOQ_min)
    
  })




observeEvent(input$LOQ, {

  req(!all(is.na(concentrations(rv))))

  req(input$file1)
  req(input$Compound != "")
  req(all(rv$setup_cal$Cal.Name %in% rv$data$Sample.Name))
  req(all(rv$data$Sample.Name[pd_temp(rv) == "Cal"] %in% rv$setup_cal$Cal.Name))

  tryCatch({
    # Check if input$LOQ is a valid numeric value
    
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
  }, error = function(e) {
    # Catch any unexpected errors
    showNotification(paste("An error occurred:", e$message), type = "error")
  })
})




# ------- Output -------

observeEvent(event_data("plotly_doubleclick", source = "acc"), {
    rv$current_layout_acc <- NULL
  })


output$p_acc <- renderPlotly({ 
  tryCatch({
     try({
      p <- get_plotly_acc(rv)
    }, silent = TRUE)
    

    suppressWarnings({
      p <- ggplotly(p, source = "acc") 

      p <- event_register(p, "plotly_relayout") %>% 
          event_register("plotly_click") %>% 
          event_register("plotly_doubleclick")
      #

      if(!is.null(rv$current_layout_acc)){
        layout_vals <- isolate(rv$current_layout_acc)

    x_range <- if (all(c("xaxis.range[0]", "xaxis.range[1]") %in% names(layout_vals))) {
  as.numeric(c(layout_vals[["xaxis.range[0]"]], layout_vals[["xaxis.range[1]"]]))
    } else {
  NULL
    }

    y_range <- if (all(c("yaxis.range[0]", "yaxis.range[1]") %in% names(layout_vals))) {
       as.numeric(c(layout_vals[["yaxis.range[0]"]], layout_vals[["yaxis.range[1]"]]))
    } else {
       NULL
    }

        p <- layout(p,
  xaxis = if (!is.null(x_range)) list(range = x_range, autotick = TRUE, tickmode = "auto") else list(autorange = TRUE),
  yaxis = if (!is.null(y_range)) list(range = y_range, autotick = TRUE, tickmode = "auto") else list(autorange = TRUE)
      )
        
      } else {

        p <- p %>% layout(
        xaxis = list(
          autorange = TRUE,
          autotick = TRUE,
          tickmode = "auto",
          fixedrange = FALSE  # allow zoom
        ),
        yaxis = list(
          autorange = TRUE,
          autotick = TRUE,
          tickmode = "auto",
          fixedrange = FALSE
        )
      )


      }
  
      p
    })
  }, error = function(e) NULL)
   
})

  


observeEvent(event_data("plotly_relayout", source = "acc"), {
  layout <- event_data("plotly_relayout", source = "acc")

    
  if (!is.null(layout)) {
    if (any(grepl("autorange", names(layout)))) {
      isolate({rv$current_layout_acc <- NULL})
    } else if (!is.null(layout[["xaxis.range[0]"]]) || !is.null(layout[["yaxis.range[0]"]])) {
      isolate({rv$current_layout_acc <- layout})
    }
    #cat("Saved new zoom:", layout[["xaxis.range[0]"]], "-", layout[["xaxis.range[1]"]], "\n")
  }
  
})

observeEvent(event_data("plotly_click", source = "acc"), {
  click_info <- event_data("plotly_click", source = "acc")
  req(click_info)

  df <- rv$acc_table
  data <- df[df$Accuracy == click_info$y, ]





  for(i in 1:nrow(data)){
    click_info$y <- as.numeric(data$PeakArea[i])
    click_info$x <- as.numeric(data$Concentration[i])

  #cat("Click detected:", click_info$x, click_info$y, "\n")
  tryCatch({
      isolate({
    remove_point_from_click(click_info, input, rv)
  })

  }, error = function(e){
    showNotification(paste("An error occurred:", e$message), type = "error")
  })

  }
  

})




  output$quan_results_table <- renderDT({
    
    req(rv$selection_cals_table)
    
      df <- quantitate(input, rv)[[1]]
   
    

    req(is.data.frame(df))

    df <- df[df$Classification == input$Block, ]

    df$pred <- signif(df$pred, 3)
    df$PeakArea <- round(df$PeakArea, 1)
    df <- dplyr::select(df, Sample.Name, Classification, cals_used, PeakArea, pred)

    datatable(df, options = list(dom = "tp", lengthMenu = list(c(20, -1), c("20", "All"))))
  })

output$acc_table <- renderDT({
  tryCatch({
    df <- get_acc_table(input, rv)
    datatable(df, options = list(
      dom = "tp",
      lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "All")),
      pageLength = -1,  # Show all rows by default
      columnDefs = list(list(className = 'dt-center', targets = which(names(df) == "used")))
    ))
  }, error = function(e) {
    showNotification(paste("An error occurred:", e$message), type = "error")
  })
  }, error = function(e) {
    showNotification(paste("An error occurred:", e$message), type = "error")
  })




# ------------- Quant Plot ----------------

observeEvent(event_data("plotly_doubleclick", source = "quant"), {
    rv$current_layout <- NULL
  })


output$p_quant <- renderPlotly({ 
    


    if(!all(rv$setup_cal$Cal.Name %in% rv$data$Sample.Name) || !all(rv$data$Sample.Name[pd_temp(rv) == "Cal"] %in% rv$setup_cal$Cal.Name)){
      showNotification("Calibration data does not match the data. Please check and adjust the template and the data file!", type = "error")
      return(NULL)
    }
    req(rv$selection_cals_table)

    if(all(is.na(concentrations(rv)))){
      showNotification("Calibration Template does not match the data. Please check and adjust the template and the data file!", type = "error")
      return(NULL)
    }

    tryCatch({
      p <- (get_plotly_quant(input, rv)) 

    suppressWarnings({
      p <- ggplotly(p, source = "quant") 

      p <- event_register(p, "plotly_relayout") %>% 
          event_register("plotly_click") %>% 
          event_register("plotly_doubleclick")
      #session$sendCustomMessage("registerDoubleClick", "quant")

      if(!is.null(rv$current_layout)){
        layout_vals <- isolate(rv$current_layout)

    x_range <- if (all(c("xaxis.range[0]", "xaxis.range[1]") %in% names(layout_vals))) {
      as.numeric(c(layout_vals[["xaxis.range[0]"]], layout_vals[["xaxis.range[1]"]]))
    } else {
       NULL
    }

    y_range <- if (all(c("yaxis.range[0]", "yaxis.range[1]") %in% names(layout_vals))) {
      as.numeric(c(layout_vals[["yaxis.range[0]"]], layout_vals[["yaxis.range[1]"]]))
    } else {
       NULL
    }

        p <- layout(p,
      xaxis = if (!is.null(x_range)) list(range = x_range, autotick = TRUE, tickmode = "auto") else list(autorange = TRUE),
      yaxis = if (!is.null(y_range)) list(range = y_range, autotick = TRUE, tickmode = "auto") else list(autorange = TRUE)
      )
        
      } else {

        p <- p %>% layout(
        xaxis = list(
          autorange = TRUE,
          autotick = TRUE,
          tickmode = "auto",
          fixedrange = FALSE  # allow zoom
        ),
        yaxis = list(
          autorange = TRUE,
          autotick = TRUE,
          tickmode = "auto",
          fixedrange = FALSE
        )
      )


      }
  
      p
    })


    }, error = function(e) { NULL})
    
  })

observeEvent(event_data("plotly_relayout", source = "quant"), {
  layout <- event_data("plotly_relayout", source = "quant")

    
  if (!is.null(layout)) {
    if (any(grepl("autorange", names(layout)))) {
      isolate({rv$current_layout <- NULL})
    } else if (!is.null(layout[["xaxis.range[0]"]]) || !is.null(layout[["yaxis.range[0]"]])) {
      isolate({rv$current_layout <- layout})
    }
    #cat("Saved new zoom:", layout[["xaxis.range[0]"]], "-", layout[["xaxis.range[1]"]], "\n")
  }
  
})

observeEvent(event_data("plotly_click", source = "quant"), {
  click_info <- event_data("plotly_click", source = "quant")
  req(click_info)

  #cat("Click detected:", click_info$x, click_info$y, "\n")
  tryCatch({
    isolate({
    remove_point_from_click(click_info, input, rv)
  

  })
  
}, error = function(e){
    showNotification(paste("An error occurred:", e$message), type = "error")
  })

})

observeEvent(event_data("plotly_selected", source ="quant"), {
selected <- event_data("plotly_selected", source = "quant")
  
    if (is.null(selected)) {

   } else {

    shinyalert(
      title = "Modify Selection",
      text = "Do you want to toggle the selected points in the model?",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Toggle Selected",
      cancelButtonText = "Cancel",
      callbackR = function(value) {
        if (value) {

          # Toggle selected points in the model
          for (row in 1:nrow(selected)) {
            info <- selected[row, ]
            if(input$log_scale){
              info$y <- 10^(info$y)
            }
            isolate({
              blocks <- input$Block
              temp <- rv$selection_cals_table[[blocks]]
              for (i in 1:length(rv$selection_cals_table)) {
                if (all(unique(rv$selection_cals_table[[i]]$Classification) == unique(temp$Classification))) {
                  blocks <- c(blocks, names(rv$selection_cals_table)[i])
                }
              }
              # Toggle the 'used' status of the selected points
              temp[temp$PeakArea == info$y & temp$Concentration == info$x, c("used")] <- 
                !temp[temp$PeakArea == info$y & temp$Concentration == info$x, c("used")]
              for (i in unique(blocks)) {
                rv$selection_cals_table[[i]] <- temp
              }
            })
          }
        }
      }
    )
    }
})





  observeEvent(input$quantitation_method, {
    tryCatch({
      all_present <- all(rv$setup_cal$Cal.Name %in% rv$data$Sample.Name)
    if (input$quantitation_method == "Bracketing") {
    if (any(rowSums(rv$selection_table_bracketing[, -1]) == 0)) {
      shinyalert("Error in Bracketing Table", "Each Block in the bracketing table must have at least one Cal assigned to it.", type = "error")
      return(NULL)
      stop()
    }
    }

    req(all_present)

    req(input$file1)
    try({
      update_cals(input, rv, session)
    }, silent = T)
    


    })

    
  })





# ------------------------------------------
  output$HoR <- renderPlot({
    mods <- quantitate(input, rv)[3]

    mod_temp <- mods[[1]][[input$Block]]

    p <- gg_reshist(mod_temp) + theme_pubr()

    plot(p)
  })

  output$RFV <- renderPlot({
    mods <- quantitate(input, rv)[3]

    mod_temp <- mods[[1]][[input$Block]]

    p <- gg_resfitted(mod_temp) + theme_pubr()

    plot(p)
  })

  output$QQ <- renderPlot({
    mods <- quantitate(input, rv)[3]

    mod_temp <- mods[[1]][[input$Block]]

    p <- gg_qqplot(mod_temp) + theme_pubr()

    plot(p)
  })

  output$SLP <- renderPlot({
    mods <- quantitate(input, rv)[3]

    mod_temp <- mods[[1]][[input$Block]]

    
    p <- gg_scalelocation(mod_temp) + theme_pubr()
    #gg_scalelocation()
    plot(p)
  })

  output$CDP <- renderPlot({
    mods <- quantitate(input, rv)[3]

    mod_temp <- mods[[1]][[input$Block]]

    p <- gg_cooksd(mod_temp) + theme_pubr()
    plot(p)
  })



}

