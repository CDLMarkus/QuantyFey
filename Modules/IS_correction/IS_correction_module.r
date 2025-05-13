## IS correction module

IS_correction_module <- function(input, output, session, rv) {

  observeEvent(input[["IS_table_cell_edit"]], {
    req(input$file1)

    info <- input[["IS_table_cell_edit"]] # this input contains the info of the edit

    rv$IS_table[info$row, info$col] <- info$value

    for (pd in unique(rv$data$Sample.Type)) {
      rv$IS_compound_data_for_correction[rv$data$Sample.Type == pd] <- as.numeric(rv$data[rv$data$Sample.Type == pd, c(input$Compound_IS)]) * rv$IS_table$Correction.Factor[rv$IS_table$Sample.Type == pd]
    }


    med_IS <- median(rv$IS_compound_data_for_correction, na.rm = T)

    rv$IS_ratio <- as.numeric(rv$data[, input$Compound]) / sapply(as.numeric(rv$IS_compound_data_for_correction), FUN = function(x) {
      ifelse(x / med_IS < 0.01, med_IS, x)
    })
  })


  # ------ Output --------

  output$IS_table <- renderDT({
    req(input$file1)

    if (is.null(rv$IS_table)) {
      IS_correction_factors <- rep(1, length(unique(rv$data$Sample.Type)))

      rv$IS_table <- data.frame(Sample.Type = unique(rv$data$Sample.Type), Correction.Factors = IS_correction_factors)
    }

    dt <- rv$IS_table

    datatable(dt, editable = T, filter = "none", options = list(dom = "t"))
  })



observeEvent(event_data("plotly_relayout", source = "IS"), {
  layout <- event_data("plotly_relayout", source = "IS")


  if (!is.null(layout)) {
    if (any(grepl("autorange", names(layout)))) {
      isolate({ rv$current_layout_IS <- NULL })
    } else {
      isolate({
        if (is.null(rv$current_layout_IS)) {
          rv$current_layout_IS <- layout
        } else {
          for (name in names(layout)) {
            if (name %in% names(rv$current_layout_IS)) {
              rv$current_layout_IS[[name]] <- layout[[name]]
            } else {
              rv$current_layout_IS[[name]] <- layout[[name]]
            }
          }
        }
      })
    }
  }
})

observeEvent(event_data("plotly_doubleclick", source = "IS"), {
  rv$current_layout_IS <- NULL
})

  output$IS_plot <- renderPlotly({
    tryCatch({
      p <- get_plot_IS(input, rv)
    }, error = function(e) {print(e)})

    req(p)
    
    p1 <- p[[1]]
    p2 <- p[[2]]

    # Convert ggplots to plotly objects
    g1 <- ggplotly(p1, source = "IS") 
  	g2 <- ggplotly(p2, source = "IS")

    ylab <- if(is.null(int_unit)) {
      "Peak Area"
    } else {
      paste("Peak Area [", int_unit, "]", sep = "")
    }



    # Manually adjust legend settings
    for(i in seq_along(g2$x$data)) {
    g2$x$data[[i]]$showlegend <- FALSE  # hide duplicate legend
    g2$x$data[[i]]$legendgroup <- g1$x$data[[i]]$legendgroup  # ensure same legend group
    }

    # Combine with shared legend
    p <- subplot(g1, g2, nrows = 2, shareX = TRUE, shareY = FALSE) %>%
    layout(showlegend = TRUE,
    yaxis = list(title = ylab),
    yaxis2 = list(title = "IS Ratios"))

    if(!is.null(rv$current_layout_IS)){
      p <- apply_current_layout(p, rv$current_layout_IS)
    }

    #p <- subplot(ggplotly(p1), ggplotly(p2), nrows = 2, titleX = T, titleY = T)

    p
  })

}
