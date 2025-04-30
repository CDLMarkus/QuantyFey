## Configure settings module

configure_settings_module <- function(input, output, session, rv) {

  observeEvent(input$quant_indicator, {
    req(input$file1)
    req(input$quant_indicator)

    try({
      isolate({
        if (is.null(input$quant_indicator) || trimws(input$quant_indicator) == "") {
          return(NULL)
        }
        req(rv$data)
        req(is.data.frame(rv$data))

        is_exclude <- if (!is.null(input$IS_indicator)) input$IS_indicator else ""

        quant_columns <- names(rv$data)[
          grepl(input$quant_indicator, names(rv$data)) &
          !grepl(is_exclude, names(rv$data))
        ]

        if (length(quant_columns) < 2 || all(is.na(quant_columns)) || all(quant_columns == "")) {
          shinyalert("Warning", "The pattern for Quant Transition was not found in multiple columns.", type = "warning")

          updateSelectInput(session,
            inputId = "Compound",
            label = "Compound:",
            choices = character(0),
            selected = NULL
          )
          return(NULL)
        } else {
          updateSelectInput(session,
            inputId = "Compound",
            label = "Compound:",
            choices = quant_columns,
            selected = quant_columns[1]
          )
        }
      })
    }, silent = TRUE)
  })

  observeEvent(input$IS_indicator, {
    req(input$file1)
    req(input$IS_indicator)

    try({
      current_data <- tryCatch(data(input, rv), error = function(e) NULL)
      req(current_data)
      
      col_names <- names(current_data)
      req(col_names)

      is_columns <- col_names[sapply(col_names, function(x) grepl(input$IS_indicator, x))]

      if (length(is_columns) == 0 || all(is.na(is_columns)) || all(is.null(is_columns)) || is_columns == "none") {
        shinyalert("Warning", "No Internal Standard Transitions were observed.", type = "warning")
        
        if (!is.null(rv$data) && !is.null(input$Compound) && input$Compound %in% colnames(rv$data)) {
          rv$IS_ratio <- rv$data[[input$Compound]]
          rv$IS_compound_data_for_correction <- rep(1, length(rv$IS_ratio))
        }

        updateSelectInput(session, "Compound_IS", choices = character(0), selected = NULL)
        updateSelectInput(session, "quantitation_method",
          label = "Method for Quantification",
          choices = c("Drift Correction", "Bracketing", "Default"),
          selected = "Drift Correction"
        )

        if (!"none" %in% colnames(rv$data)) {
          rv$data <- dplyr::add_column(rv$data, none = 1)
        }

        is_columns <- "none"
      } else {
        if ("none" %in% colnames(rv$data)) {
          rv$data <- rv$data[, !grepl("none", colnames(rv$data))]
        }

        updateSelectInput(session, "quantitation_method",
          label = "Method for Quantification",
          choices = c("IS Correction", "Drift Correction", "Bracketing", "Default"),
          selected = "IS Correction"
        )

        updateSelectInput(session,
          inputId = "Compound_IS",
          label = "Internal Standard:",
          choices = is_columns,
          selected = is_columns[1]
        )
      }
    }, silent = TRUE)
  })

  observeEvent(input$mode, {
    req(input$file1)
    
    req(input$mode %in% names(rv$templates))
    req(rv$data)

    tryCatch({
      template <- rv$templates[[input$mode]]
    req(is.data.frame(template))

    template_cols <- setdiff(colnames(template), "Cal.Name")
    data_cols <- colnames(rv$data)

    if (all(template_cols %in% data_cols)) {
      rv$specific_setup <- template

      has_compound <- !is.null(input$Compound) && nzchar(input$Compound)
      compound_exists <- has_compound && input$Compound %in% colnames(rv$specific_setup)

      if (compound_exists) {
        rv$setup_cal <- data.frame(
          Cal.Name = rv$specific_setup$Cal.Name,
          Concentration = as.numeric(rv$specific_setup[[input$Compound]])
        )
      } else {
        rv$setup_cal <- data.frame(
          Cal.Name = rv$specific_setup$Cal.Name,
          Concentration = rep(NA, nrow(rv$specific_setup)))

          showNotification("The selected template does not contain the detected compound names. Please check the template or the quant transition pattern!", type = "warning")
        
      }
    } else if (ncol(template) == 2 && all(c("Cal.Name", "Concentration") %in% colnames(template))) {
      rv$setup_cal <- template
    } else {
      rv$setup_cal <- data.frame(
        Cal.Name = template$Cal.Name,
        Concentration = rep(NA, nrow(template))
      )
      showNotification("The selected template does not contain the detected compound names. Please check the template or the quant transition pattern!", type = "warning")
    }

    try({
      update_cals(input, rv, session)
    }, silent = TRUE)

    }, error = function(e) {
      #print(e)
      #shinyalert("Error", "The selected template does not match the dataset. Please check that the template matches the Transition names exactly!", type = "error")
      NULL
    })
    
  })

  observeEvent(input$sampleKey, {
    req(input$file1)
    req(input$file_RT)
    req(rv$data)

    updateCheckboxGroupInput(session, inputId = "RT_groups", choices = unique(rv$data$Sample.Type))
    updateCheckboxGroupInput(session, inputId = "IS_groups", choices = unique(rv$data$Sample.Type))
    updateCheckboxGroupInput(session, inputId = "quan_qual_groups", choices = unique(rv$data$Sample.Type))
  })

  # ----- Output -----

  output$setup_cal <- renderDT({
    req(rv$setup_cal)
    datatable(rv$setup_cal, editable = FALSE, options = list(lengthMenu = list(c(15, -1), c("15", "All")), dom = "tp"))
  })

  output$table_QQ <- renderDT({
    req(rv$data)
    req(input$quant_indicator)

    quant_cols <- colnames(rv$data)[grepl(input$quant_indicator, colnames(rv$data))]

    if(length(quant_cols) == 0 || all(is.null(quant_cols)) || all(is.na(quant_cols))) {
      showNotification("No quant columns found. Please adjust the patterns accordingly!", type = "warning")
      return(NULL)
    }

    df_QQ <- data.frame(Quant.Names = quant_cols, Qual.Names = NA_character_, stringsAsFactors = FALSE)

    quals <- colnames(rv$data)[grepl(input$qual_indicator, colnames(rv$data))]
    quals_split <- sapply(quals, function(x) strsplit(x, "_")[[1]][1])

    suppressWarnings({
      for (quant_name in df_QQ$Quant.Names) {
        quant_base <- strsplit(quant_name, "_")[[1]][1]
        matching_qual <- quals[quals_split == quant_base]
        if (length(matching_qual) > 0) {
          df_QQ$Qual.Names[df_QQ$Quant.Names == quant_name] <- matching_qual[1]
        }
      }
    })

    if(nrow(df_QQ) == 0) {
      message("No quant columns found. Please check the quant pattern")
    } else {
      
    rv$df_QQ <- df_QQ

    datatable(df_QQ, options = list(dom = "tp"))
    }

  })

  output$table_IS <- renderDT({
    req(rv$data)
    req(input$IS_indicator)

    is_cols <- colnames(rv$data)[grepl(input$IS_indicator, colnames(rv$data))]
    df_IS <- data.frame(IS.Names = is_cols, stringsAsFactors = FALSE)
    if(nrow(df_IS) == 0) {
      message("No IS columns found.")
    } else {
      datatable(df_IS, options = list(dom = "tp"))
    }
    
  })

}
