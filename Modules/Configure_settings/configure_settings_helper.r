


observe_input_quant_indicator <- function(input, rv, session) {


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
}

observe_input_IS_indicator <- function(input, rv, session) {
    
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
}

observe_input_mode <- function(input, rv, session) {
    
    tryCatch({
      template <- rv$templates[[input$mode]]
    req(is.data.frame(template))

    

    if(any(!(rv$data$Sample.Name[pd_temp(rv) == "Cal"] %in% template$Cal.Name))) {
      cal_names_in_data <- unique(rv$data$Sample.Name[pd_temp(rv) == "Cal"])
      cal_missing <- setdiff(cal_names_in_data, template$Cal.Name)



      # Ensure the new rows have the same columns as template (except Cal.Name)
      new_rows <- as.data.frame(matrix(0, nrow = length(cal_missing), ncol = ncol(template)))
      colnames(new_rows) <- colnames(template)
      new_rows$Cal.Name <- cal_missing
      template <- rbind(template, new_rows) %>% as.data.frame()
    }

    template_cols <- setdiff(colnames(template), "Cal.Name")
    data_cols <- colnames(rv$data)

    if (all(template_cols %in% data_cols)) {
      rv$specific_setup <- template

      has_compound <- !is.null(input$Compound) && nzchar(input$Compound)
      compound_exists <- has_compound && input$Compound %in% colnames(rv$specific_setup)

      if (compound_exists) {
        setup_cal <- data.frame(
          Cal.Name = rv$specific_setup$Cal.Name,
          Concentration = as.numeric(rv$specific_setup[[input$Compound]])
        )
      } else {
        setup_cal <- data.frame(
          Cal.Name = rv$specific_setup$Cal.Name,
          Concentration = rep(0, nrow(rv$specific_setup)))

          showNotification("The selected template does not contain the detected compound names. Please check the template or the quant transition pattern!", type = "warning")
        
      }
    } else if (ncol(template) == 2 && all(c("Cal.Name", "Concentration") %in% colnames(template))) {
      setup_cal <- template
    } else {
      setup_cal <- data.frame(
        Cal.Name = template$Cal.Name,
        Concentration = rep(NA, nrow(template))
      )
      showNotification("The selected template does not contain the detected compound names. Please check the template or the quant transition pattern!", type = "warning")
    }

    

    


    rv$setup_cal <- setup_cal



    try({
      update_cals(input, rv, session)
    }, silent = TRUE)

    }, error = function(e) {
      message("Error in observe_input_mode: ", e$message)
    })
    
}

get_qq_table <- function(input, rv) {
    quant_cols <- colnames(rv$data)[grepl(input$quant_indicator, colnames(rv$data))]
    quant_cols <- quant_cols[!grepl(input$IS_indicator, quant_cols)]

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
      stop("No quant columns found. Please check the quant pattern")
    } else {
    rv$df_QQ <- df_QQ
    return(df_QQ)
    }


}