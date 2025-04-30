### Compound Quantification Parameters logic

#source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "./Modules/Compound_quant_parameters/compound_quantification_parameters_helper.r"))

compound_quantification_parameters_module <- function(input, output, session, rv){

  observeEvent(input$Compound_IS, {
    req(input$file1)
    req(input$Compound != "")

    if(input$Compound_IS == "" | input$Compound_IS == "none" | is.null(input$Compound_IS) | is.na(input$Compound_IS)){

      rv$IS_ratio <- rv$data[, input$Compound]
    } else{



      tryCatch({
        rv$IS_compound_data_for_correction <- as.numeric(rv$data[, input$Compound_IS])
      }, error = function(e) {
        rv$IS_compound_data_for_correction <- rep(1, nrow(rv$data))
      })

    df <- data(input, rv)

    med_IS <- median(rv$IS_compound_data_for_correction, na.rm = T)

    rv$IS_ratio <- as.numeric(rv$data[, input$Compound]) / sapply(as.numeric(rv$IS_compound_data_for_correction), FUN = function(x) {
      ifelse(x / med_IS < 0.01, med_IS, x)
    })

    if (input$quantitation_method == "IS Correction") {
      sel_table <- rv$selection_cals_table

      tryCatch(
        {
          for (i in 1:length(sel_table)) {
            table <- sel_table[[i]]

            for (i_row in 1:nrow(table)) {
              table$PeakArea[i_row] <- rv$IS_ratio[rv$data$Sample.Name == table$Sample.Name[i_row] & rv$Classification_temp == table$Classification[i_row]]
            }

            sel_table[[i]] <- table
          }

          rv$selection_cals_table <- sel_table
        },
        error = function(e) {
          #print(e)
        }
      )
    }
    }
})

observeEvent(input$save_compound, {
  tryCatch({
    # If the results dataframe is empty, create a new one
    if (is.null(rv$results)) {
      
      setwd(results_directory(input))
      if (file.exists("results_quant.xlsx")) {
        timestamp <- format(Sys.Date(), "%Y%m%d")
        results_quant_name <- paste0("results_quant_", timestamp, ".xlsx")
      } else {
        results_quant_name <- "results_quant.xlsx"
      }

      rv$results <- rv$data[, c("Sample.Name", "Classification")]
      rv$results <- rbind(c("Comment", ""), rv$results)
    }

    setwd(script_path)

    # get quantitation results
    quant <- quantitate(input, rv)[[1]]
    quant <- rbind(c(input$Comment), data.frame(Concentration = signif(quant$pred, 3)))

    # Check if the compound name already exists in the results dataframe
    cpt_name <- input$Compound

    


    duplicated <- cpt_name %in% colnames(rv$results)
    n = 1
    while(duplicated){
        cpt_name <- paste("re", n, input$Compound, sep = "_")
        duplicated <- cpt_name %in% colnames(rv$results)
        n = n + 1
    }

    rv$results <- cbind(rv$results, quant)
    colnames(rv$results)[ncol(rv$results)] <- cpt_name


    if(!(cpt_name %in% colnames(rv$data))){
        rv$data <- add_column(rv$data, name_re2 = rv$data[, input$Compound], .after = input$Compound)
        colnames(rv$data)[colnames(rv$data) == "name_re2"] <- cpt_name

        rv$data_RT <- add_column(rv$data_RT, name_re2 = rv$data_RT[, input$Compound], .after = input$Compound)
        colnames(rv$data_RT)[colnames(rv$data_RT) == "name_re2"] <- cpt_name
    }


  # Overwrite values that are below LLOQ
    for (i in 1:length(rv$Classification_temp)) {
      block_temp <- rv$Classification_temp[i]

      LOQ_temp <- rv$LLOQs[[block_temp]]

      rv$results[(i + 1), cpt_name] <- ifelse(as.numeric(rv$results[(i + 1), cpt_name]) < LOQ_temp, paste("<", LOQ_temp, sep = " "), rv$results[(i + 1), cpt_name])
    }
    

    # Set working directory to the users documents folder
    setwd(results_directory(input))
    suppressWarnings({
      # Write the results to a CSV file
    if("Results_evaluation_interim.csv" %in% dir()){
    # Check if any files with the pattern "Results_evaluation_interim" exist
    interim_files <- list.files(pattern = "Results_evaluation_interim")

    # Order the files alphabetically
    interim_files <- sort(interim_files)

    # Initialize a flag to check if any file matches
    file_matched <- FALSE

    # Loop through each file
    for (file in interim_files) {
      # Read the file
      interim_data <- read.csv(file)

      # Remove the column "X" if it exists
      if ("X" %in% colnames(interim_data)) {
        interim_data <- interim_data[, !colnames(interim_data) %in% "X"]
      }
      
      # Check if all column names from interim_data are in rv$results
      if (all(colnames(interim_data) %in% colnames(rv$results))) {
        # Create a subset of rv$results with the same columns as interim_data
        subset_results <- rv$results[, colnames(interim_data)]
        
        # Check if the content matches 100%
        if (identical(subset_results, interim_data)) {
          # Overwrite the file with the new rv$results dataframe
          write.csv(rv$results, file)
          file_matched <- TRUE
          break
        }
      } else {
        next
      }
    }

    # If no file matched, create a new file with the timestamp

    if (!file_matched) {
      timestamp <- format(Sys.Date(), "%Y%m%d")
      results_filename <- paste0("Results_evaluation_interim_", timestamp, ".csv")
    } else {
      results_filename <- "Results_evaluation_interim.csv"
    }

    write.csv(rv$results, results_filename)

    } else {
      results_filename <- "Results_evaluation_interim.csv"

      write.csv(rv$results, results_filename)

    }

    })
    

    

    

    

    # write summary and results to an excel file 
    quantitate_output <- quantitate(input, rv)
    res_list <- list()
    quant <- quantitate_output[[1]]
    res_df <- data.frame(Sample.Name = rv$data$Sample.Name, Classification = rv$Classification_temp)
    res_list[["models"]] <- quantitate_output[[3]]

    
    if(input$quantitation_method == "Bracketing"){
        res_df$PeakArea <- rv$data[, cpt_name]
        res_df$RetentionTime <- rv$data_RT[, cpt_name]
        res_df$Concentration <- signif(quant$pred, 3)
        
        res_df$LLOQ <- sapply(res_df$Classification, FUN = function(x) {
            for (i in 1:length(rv$LLOQs)) {
            if (names(rv$LLOQs)[i] == x) {
                return(rv$LLOQs[[i]])
            }
            }
        })

        res_df$Function <- sapply(res_df$Classification, FUN = function(x) {
            model <- res_list[["models"]][[x]]
            coef <- signif(model$coefficients, 7)
            if (rv$regression_model == "linear") {
                paste0("y = ", coef[2], " * x + ", coef[1])
            } else if (rv$regression_model == "quadratic") {
                paste0("y = ", coef[1], " + ", coef[2], " * x + ", coef[3], " * x^2")
            } else if (rv$regression_model == "power") {
                paste0("y = ", exp(coef[1]), " * x^", coef[2])
            } else if (rv$regression_model == "exponential") {
                paste0("y = ", exp(coef[1]), " * e^(", coef[2], " * x)")
            } else if (rv$regression_model == "log(y)") {
                paste0("log(y) = ", coef[2], " * x + ", coef[1])
            }
        })

        res_df$Cals_used <- sapply(res_df$Classification, FUN = function(x){
            temp <- rv$selection_cals_table[[x]]
            cals_used <- unique(temp$Classification[temp$used])
            cals_used <- paste(cals_used, collapse = ", ")
            return(cals_used)
        }, USE.NAMES = F)

        res_df$Levels_used <- sapply(res_df$Classification, FUN = function(x){
            temp <- rv$selection_cals_table[[x]]
            cals_used <- temp$Sample.Name[temp$used]
            cals_used <- paste(cals_used, collapse = ", ")
            return(cals_used)
        }, USE.NAMES = F)

    } else if (input$quantitation_method == "IS Correction") {
        res_df$PeakArea <- rv$data[, cpt_name]
        res_df$IS_PeakArea <- rv$data[, input$Compound_IS]
        res_df$IS_Ratio <- rv$IS_ratio
        res_df$RetentionTime <- rv$data_RT[, cpt_name]
        res_df$Concentration <- signif(quant$pred, 3)
        
        res_df$` ` <- rep("", nrow(res_df))
        res_df$info_label <- rep("", nrow(res_df))
        res_df$info_value <- rep("", nrow(res_df))

        res_df$info_label[1] <- "Function"
        res_df$info_label[2] <- "LLOQ"

        coef <- signif(unique(res_list[["models"]])[[1]]$coefficients, 7)
        if (rv$regression_model == "linear") {
            coeficients <- paste0("y = ", coef[2], " * x + ", coef[1])
        } else if (rv$regression_model == "quadratic") {
            coeficients <- paste0("y = ", coef[1], " + ", coef[2], " * x + ", coef[3], " * x^2")
        } else if (rv$regression_model == "power") {
            coeficients <- paste0("y = ", exp(coef[1]), " * x^", coef[2])
        } else if (rv$regression_model == "exponential") {
            coeficients <- paste0("y = ", exp(coef[1]), " * e^(", coef[2], " * x)")
        } else if (rv$regression_model == "log(y)") {
            coeficients <- paste0("log(y) = ", coef[2], " * x + ", coef[1])
        }

        res_df$info_value[1] <- coeficients
        res_df$info_value[2] <- unique(unlist(rv$LLOQs))

        res_df$info_label[3] <- "IS Compound"
        res_df$info_label[4] <- "Correction Factors"
        
        res_df$info_value[3] <- input$Compound_IS
        res_df$info_value[4] <- paste(rv$IS_table$Sample.Type, rv$IS_table$Correction.Factors, sep = " = ", collapse = ", ")

    } else if (input$quantitation_method == "Drift Correction") {
        res_df$PeakArea <- rv$data[, cpt_name]

        res_df$corrected_area <- rv$dc_area

        res_df$RetentionTime <- rv$data_RT[, cpt_name]

        res_df$Concentration <- signif(quant$pred, 3)


        res_df$` ` <- rep("", nrow(res_df))
        res_df$info_label <- rep("", nrow(res_df))
        res_df$info_value <- rep("", nrow(res_df))

        res_df$info_label[1] <- "Function"
        res_df$info_label[2] <- "LLOQ"

        
        coef <- signif(unique(res_list[["models"]])[[1]]$coefficients, 7)
        if (rv$regression_model == "linear") {
            coeficients <- paste0("y = ", coef[2], " * x + ", coef[1])
        } else if (rv$regression_model == "quadratic") {
            coeficients <- paste0("y = ", coef[1], " + ", coef[2], " * x + ", coef[3], " * x^2")
        } else if (rv$regression_model == "power") {
            coeficients <- paste0("y = ", exp(coef[1]), " * x^", coef[2])
        } else if (rv$regression_model == "exponential") {
            coeficients <- paste0("y = ", exp(coef[1]), " * e^(", coef[2], " * x)")
        } else if (rv$regression_model == "log(y)") {
            coeficients <- paste0("log(y) = ", coef[2], " * x + ", coef[1])
        }


        res_df$info_value[1] <- coeficients
        res_df$info_value[2] <- unique(unlist(rv$LLOQs))



        res_df$info_label[3] <- "Drift Model"
        res_df$info_label[4] <- "Span Width"
        res_df$info_label[5] <- "Files for Correction"
        
        res_df$info_value[3] <- input$model_drift
        res_df$info_value[4] <- input$span_width
        res_df$info_value[5] <- input$files_for_correction



    } else if (input$quantitation_method == "Default") {

        res_df$PeakArea <- rv$data[, cpt_name]

        res_df$RetentionTime <- rv$data_RT[, cpt_name]

        res_df$Concentration <- signif(quant$pred, 3)

        res_df$` ` <- rep("", nrow(res_df))
        res_df$info_label <- rep("", nrow(res_df))
        res_df$info_value <- rep("", nrow(res_df))


        res_df$info_label[1] <- "Function"
        res_df$info_label[2] <- "LLOQ"

        coef <- signif(unique(res_list[["models"]])[[1]]$coefficients, 7)
        if (rv$regression_model == "linear") {
            coeficients <- paste0("y = ", coef[2], " * x + ", coef[1])
        } else if (rv$regression_model == "quadratic") {
            coeficients <- paste0("y = ", coef[1], " + ", coef[2], " * x + ", coef[3], " * x^2")
        } else if (rv$regression_model == "power") {
            coeficients <- paste0("y = ", exp(coef[1]), " * x^", coef[2])
        } else if (rv$regression_model == "exponential") {
            coeficients <- paste0("y = ", exp(coef[1]), " * e^(", coef[2], " * x)")
        } else if (rv$regression_model == "log(y)") {
            coeficients <- paste0("log(y) = ", coef[2], " * x + ", coef[1])
        }

        res_df$info_value[1] <- coeficients
        res_df$info_value[2] <- unique(unlist(rv$LLOQs))
    }


    setwd(results_directory(input))
    suppressWarnings({
      if (!file.exists("results_quant.xlsx")) {
      res_df <- list(cpt_name = res_df)
      names(res_df)[which(names(res_df) == "cpt_name")] <- cpt_name
      writexl::write_xlsx(res_df, "results_quant.xlsx")
    }else {
      suppressMessages({
        append_xlsx(existing_file = "results_quant.xlsx", new_data = res_df, sheet_name = cpt_name)

      })
        
    }

    })
    

    try({
    suppressWarnings({
      p <- get_plotly_blank(input, rv)
    rv$p_blank <- p
    p <- get_plotly_RT(input, rv)
    rv$p_RT <- p
    try({
       p <- get_plot_IS(input, rv)
    }, silent = T)
   
    rv$IS_plot <- p

      try({
        update_dc_data(input, rv)
      }, silent = T)
      
      p <- get_plotly_dc(input, rv)
    


    rv$plot_dc <- p
    p <- get_plotly_qual_quant(input, rv)
    rv$p_quan_qual <- p
    p <- get_plot_IS(input, rv)
    rv$IS_plot <- p



    })
    }, silent = T)
    

    plots_overview <- list(RT = rv$p_RT, QQ = rv$p_quan_qual, DC = rv$p_dc, IS = rv$p_IS_analysis, blank = rv$p_blank)
    cals_summary <- rv$selection_cals_table

    for_markdown <- list(
      cals_table = cals_summary, plots = plots_overview, compound = input$Compound, IS_compound = input$IS_compound,
      results_quant = quantitate(input, rv), results_summary = res_df,
      template = data.frame(Cal.Names = rv$setup_cal$Cal.Name, concentration = rv$setup_cal$Concentration),
      regression_model = rv$regression_model,
      pd = rv$data$Sample.Type
    )

    if(is.null(rv$IS_table)){
        IS_correction_factors <- rep(1, length(unique(rv$data$Sample.Type)))

        rv$IS_table <- data.frame(Sample.Type = unique(rv$data$Sample.Type), Correction.Factors = IS_correction_factors)
    }

    input_list <- reactiveValuesToList(input)
    rv_list <- reactiveValuesToList(rv)

    results_path <- results_directory(input)

    if (input$generate_report) {
      # Define the paths
      setwd(script_path)
      # script_path <- getwd()  # Current working directory where the R Markdown file is located
      result_directory <- results_path  # Directory where you want to save the PDF

      # Full path to the R Markdown file
      rmd_file <- file.path(script_path, "./Modules/Report/report_markdown.Rmd")

      output_file <- paste0("Report_", cpt_name, ".pdf")

      # Check if the output file already exists
      if (file.exists(file.path(result_directory, output_file))) {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        output_file <- paste0("Report_", cpt_name, "_", timestamp, ".pdf")
      }
      # Render the R Markdown file and save the PDF to the result directory
      suppressWarnings({
        rmarkdown::render(
        input = rmd_file,
        output_format = "pdf_document",
        output_dir = result_directory,
        output_file = output_file,
        intermediates_dir = result_directory
      )
        
      })
      
    }

   
    rv$compounds_analyzed <- c(rv$compounds_analyzed, cpt_name)

    rv$settings_used[[cpt_name]] <- list(
      file_for_correction = input$files_for_correction,
      selection_table = rv$selection_cals_table,
      LLOQ = rv$LLOQs,
      regression_model = rv$regression_model,
      comment = input$Comment,
      correction_model = input$model,
      weight_method = input$weight_method, ## check spelling
      quantitation_method = input$quantitation_method,
      IS_compound = input$Compound_IS,
      bracketing_table = rv$bracketing_table,
      span_width = input$span_width,
      IS_table = rv$IS_table,
      bracketing_sel_table = rv$selection_table_bracketing
    )

    

    rv$p_RT <- NULL
    rv$p_quan_qual <- NULL
    rv$p_blank <- NULL
    rv$p_IS_analysis <- NULL
    rv$p_dc <- NULL
    rv$LLOQs <- NULL

    updateSelectInput(session, inputId = "Compound", choices = names(rv$data)[grepl(input$quant_indicator, names(rv$data)) & !grepl(input$IS_indicator, names(rv$data))],
      selected = input$Compound)

    # Show a shiny notification if everything ran successfully
    shiny::showNotification(
  paste(
    "Output for compound", cpt_name, "was successfully generated.",
    "\nFolder:", results_path,
    "\n\nFiles generated or appended:",
    paste(
      if (isTRUE(input$generate_report)) {
        c(output_file, "results_quant.xlsx", results_filename)
      } else {
        c("results_quant.xlsx", results_filename)
      },
      collapse = "\n• "
    ) %>% paste0("• ", .)
  ),
  type = "message",
  duration = 5
)

  }, error = function(e) {
    message("Error in saving compound data: ", e$message)
  })
})

  
observeEvent(input$Compound, {
  req(input$file1)
  req(nzchar(input$Compound))  # Ensure it's not empty

  rv$current_layout <- NULL
  rv$current_layout_IS <- NULL
  rv$current_layout_dc <- NULL
  rv$current_layout_acc <- NULL  


    if (input$Compound %in% rv$compounds_analyzed) {
    
     

      
      settings <- rv$settings_used[[input$Compound]]
      #print(settings)
      updateSelectInput(session, inputId = "regression_model", choices = c("linear", "quadratic"), selected = settings$regression_model)
    
      updateNumericInput(session, inputId = "LOQ", value = settings$LLOQs[[input$Block]],min = min(concentrations(rv)) %||% 0, max = max(concentrations(rv)) %||% 0 ,  step = min(concentrations(rv)))
      rv$LLOQs <- settings$LLOQ
      
      rv$selection_table_bracketing <- settings$bracketing_sel_table
      #rv$bracketing_table <- settings$bracketing_table
      bracketing_table <- data.frame(Class = rv$selection_table_bracketing$Class)

      for (i in 1:(ncol(rv$selection_table_bracketing) - 1)) {
  values <- rv$selection_table_bracketing[[i + 1]]  # Skip Class column
  bracketing_table <- cbind(
    bracketing_table,
    checkboxColumn(len = nrow(rv$selection_table_bracketing), col = i + 1, values = values)
  )
  names(bracketing_table)[i + 1] <- names(rv$selection_table_bracketing)[i + 1]
}

rv$bracketing_table <- bracketing_table
      
      updateSelectInput(session, "Compound_IS", choices = unique(colnames(rv$data[, grepl(input$IS_indicator, colnames(rv$data))])), selected = settings$IS_Compound)


      updateSelectInput(session, "weight_method", choices = c("none", "1/x", "1/x2", "1/y", "1/y2", "1/x force 0", "1/y force 0"), selected = settings$weight_method)
      rv$selection_cals_table <- settings$selection_table
      
      if(input$Compound_IS == "none" | is.na(input$Compound_IS) | is.null(input$Compound_IS) | input$Compound_IS == ""){
        updateSelectInput(session, "quantitation_method", choices = c("Drift Correction", "Bracketing", "Default"), selected = settings$quantitation_method)
      } else {
        updateSelectInput(session, "quantitation_method", choices = c("IS Correction", "Drift Correction", "Bracketing", "Default"), selected = settings$quantitation_method)
      }


      updateTextInput(session, "Comment", value = settings$comment)
      updateRadioButtons(session, "model_drift", choices = c("lm", "loess"), selected = settings$model)
      updateSelectInput(session, "files_for_correction", choices = unique(rv$data$Sample.Name), selected = settings$file_for_correction)
      updateNumericInput(session, "span_width", value = settings$span_width, min = 0.4, max = 2, step = 0.05)

      rv$IS_table <- settings$IS_table
     
    }



  tryCatch({
    try({
      update_dc_data(input, rv)
    }, silent = T)
    

    # Internal standard correction
    rv$IS_compound_data_for_correction <- tryCatch({
      as.numeric(rv$data[[input$Compound_IS]])
    }, error = function(e) {
      rep(1, nrow(rv$data))
    })

    # Calculate area and ratios
    compound_data <- tryCatch(as.numeric(rv$data[[input$Compound]]), error = function(e) NA)
    rv$Area <- compound_data

    med_IS <- median(rv$IS_compound_data_for_correction, na.rm = TRUE)
    tryCatch({
      rv$IS_ratio <- compound_data / sapply(rv$IS_compound_data_for_correction, function(x) {
      ifelse(x / med_IS < 0.01, med_IS, x)
    })

    }, error = function(e){
      rv$IS_ratio <- compound_data
    })
    

    rv$dc_area <- tryCatch({
      as.numeric(rv$drift_corrected_data_temp$corrected.PeakArea)
    }, error = function(e) NA)

    # Classification logic
    if (input$quantitation_method == "Bracketing") {
      rv$Classification_temp <- rv$data$Classification
    } else {
      Classification_temp <- rv$data$Classification
      Classification_temp[!grepl("Cal", Classification_temp)] <- "all"
      rv$Classification_temp <- Classification_temp
    }

  
    # Update setup_cal if Compound exists
    if (input$Compound %in% colnames(rv$specific_setup)) {
      new_setup <- data.frame(
        Cal.Name = rv$specific_setup$Cal.Name,
        Concentration = rv$specific_setup[[input$Compound]]
      )
      if (!identical(rv$setup_cal, new_setup)) {
        rv$setup_cal <- new_setup
      }
    }

    rv$LLOQs <- create_classification_min_list(rv)

    


    # Update LOQ input
    min_LOQ <- suppressWarnings(min(rv$setup_cal$Concentration, na.rm = TRUE))
    max_LOQ <- suppressWarnings(max(rv$setup_cal$Concentration, na.rm = TRUE))

  #cat("Updating LOQ input. Block:", input$Block, 
  #  "Value:", rv$LLOQs[[input$Block]], "\n")

    

    updateNumericInput(session,
      inputId = "LOQ",
      label = "Limit of Quantification",
      value = rv$LLOQs[[input$Block]] %||% 0,
      min = ifelse(is.finite(min_LOQ), min_LOQ, 0),
      max = ifelse(is.finite(max_LOQ), max_LOQ, 0),
      step = ifelse(is.finite(min_LOQ), min_LOQ, 0.1)
    )

    #session$sendCustomMessage('force-blur', list(id = 'LOQ'))
   
    #cat("LLOQ was updated")

    # Data classification and quantitation method switching
    df <- data(input, rv)
    class <- unique(rv$Classification_temp)

if (is.null(rv$bracketing_table)) {
      bracketing_table <- data.frame(Class = unique(rv$data$Classification))
      selection_table_bracketing <- data.frame(Class = unique(rv$data$Classification))
      selection_table <- data.frame(Class = class)

      for (i in 1:length(class[grepl("Cal", class)])) {
        bracketing_table <- cbind(bracketing_table, checkboxColumn(len = nrow(bracketing_table), col = i + 1))
        selection_table_bracketing <- cbind(selection_table_bracketing, rep(FALSE, nrow(selection_table_bracketing)))
        selection_table <- cbind(selection_table, rep(FALSE, nrow(selection_table)))

        names(bracketing_table)[i + 1] <- paste("Cal", i, sep = " ")
        names(selection_table)[i + 1] <- paste("Cal", i, sep = " ")
        names(selection_table_bracketing)[i + 1] <- paste("Cal", i, sep = " ")
      }

      if (input$quantitation_method != "Bracketing") {
        selection_table[selection_table == F] <- T
      }

      rv$selection_table <- selection_table
      rv$bracketing_table <- bracketing_table
      rv$selection_table_bracketing <- selection_table_bracketing
    }

    selection_table <- if (input$quantitation_method == "Bracketing") {
      rv$selection_table_bracketing
    } else {
      rv$selection_table
    }

    updateSelectInput(inputId = "Block", choices = selection_table$Class)

    # Prepare table for calibration selection
    df_for_sel_cal_tab <- data.frame(
      Sample.Name = rv$data$Sample.Name,
      Classification = rv$Classification_temp
    )

    df_for_sel_cal_tab$PeakArea <- switch(
      input$quantitation_method,
      "Bracketing" = rv$Area,
      "Default" = rv$Area,
      "IS Correction" = rv$IS_ratio,
      "Drift Correction" = rv$dc_area,
      rv$Area
    )

    selection_cals_table <- create_block_sample_subsets(
      block_cal = selection_table,
      sample_data = df_for_sel_cal_tab
    )

    # Enrich calibration table
    if (all(rv$setup_cal$Cal.Name %in% selection_cals_table[[1]]$Sample.Name)) {
      for (i in seq_along(selection_cals_table)) {
        sel_temp <- selection_cals_table[[i]]

        sel_temp$Concentration <- sapply(sel_temp$Sample.Name, function(x) {
          rv$setup_cal$Concentration[rv$setup_cal$Cal.Name == x][1]
        })

        sel_temp <- add_weights(input$weight_method, sel_temp)

        if (is.null(sel_temp$used)) {
          sel_temp$used <- sel_temp$PeakArea != 0
        }

        selection_cals_table[[i]] <- subset(sel_temp, Concentration != 0)
      }

      rv$selection_cals_table <- selection_cals_table
    }

    updateTextInput(session, "Comment", value = "")

    # If compound was analyzed before, restore settings
       

  }, error = function(e) {
    showNotification(paste("Error in compound setup:", e$message), type = "error")
  })
})




}
