observe_input_compound_is <- function(input, rv) {
    
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
}
save_compound <- function(input, rv, session) {
    tryCatch({
        initialize_results(input, rv)
        tryCatch({
            quant <- get_quantitation_results(input, rv)
        }, error = function(e) {
            stop("Error in 'get_quantitation_results': ", e$message)
        })
        
        tryCatch({
            cpt_name <- handle_duplicate_compound_names(input, rv)
        }, error = function(e) {
            stop("Error in 'handle_duplicate_compound_names': ", e$message)
        })
        
        tryCatch({
            update_results_and_data(input, rv, quant, cpt_name)
        }, error = function(e) {
            stop("Error in 'update_results_and_data': ", e$message)
        })
        
        tryCatch({
            overwrite_below_LLOQ(rv, cpt_name)
        }, error = function(e) {
            stop("Error in 'overwrite_below_LLOQ': ", e$message)
        })
        
        tryCatch({
            results_filename <- save_interim_results(input, rv)
        }, error = function(e) {
            stop("Error in 'save_interim_results': ", e$message)
        })
        
        tryCatch({
            output_file <- save_final_results(input, rv, quant, cpt_name)
        }, error = function(e) {
            stop("Error in 'save_final_results': ", e$message)
        })
        
        tryCatch({
            generate_report_if_requested(input, rv, cpt_name)
        }, error = function(e) {
            stop("Error in 'generate_report_if_requested': ", e$message)
        })
        
        tryCatch({
            update_compound_analysis_state(input, rv, cpt_name, session)
        }, error = function(e) {
            stop("Error in 'update_compound_analysis_state': ", e$message)
        })
        
        show_success_notification(input, cpt_name, output_file, results_filename)
    }, error = function(e) {
        shiny::showNotification(
            paste("An error occurred in function 'save_compound' while saving the compound:", e$message),
            type = "error",
            duration = 10
        )
    })
}

initialize_results <- function(input, rv) {
    if (is.null(rv$results)) {
        setwd(results_directory(input))
        results_quant_name <- if (file.exists("Quant_summary.xlsx")) {
            paste0("results_quant_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
        } else {
            "Quant_summary.xlsx"
        }
        rv$results <- rv$data[, c("Sample.Name", "Classification")]
        rv$results <- rbind(c("Comment", ""), rv$results)
        setwd(script_path)
    }
}

get_quantitation_results <- function(input, rv) {
    tryCatch({
        quant <- quantitate(input, rv)[[1]]
        quant <- rbind(c(input$Comment), data.frame(Concentration = signif(quant$predicted.Concentration, 3)))
        return(quant)
    }, error = function(e) {
        stop("Failed to calculate quantitation results: ", e$message)
    })
}

handle_duplicate_compound_names <- function(input, rv) {
    cpt_name <- input$Compound
    duplicated <- cpt_name %in% colnames(rv$results)
    n <- 1
    while (duplicated) {
        cpt_name <- paste("re", n, input$Compound, sep = "_")
        duplicated <- cpt_name %in% colnames(rv$results)
        n <- n + 1
    }
    return(cpt_name)
}

update_results_and_data <- function(input, rv, quant, cpt_name) {
    tryCatch({
        rv$results <- cbind(rv$results, quant)
        colnames(rv$results)[ncol(rv$results)] <- cpt_name
        if (!(cpt_name %in% colnames(rv$data))) {
            rv$data <- add_column(rv$data, name_re2 = rv$data[, input$Compound], .after = input$Compound)
            colnames(rv$data)[colnames(rv$data) == "name_re2"] <- cpt_name
            rv$data_RT <- add_column(rv$data_RT, name_re2 = rv$data_RT[, input$Compound], .after = input$Compound)
            colnames(rv$data_RT)[colnames(rv$data_RT) == "name_re2"] <- cpt_name
        }
    }, error = function(e) {
        stop("Failed to update results and data: ", e$message)
    })
}

overwrite_below_LLOQ <- function(rv, cpt_name) {
    tryCatch({
        for (i in 1:length(rv$Classification_temp)) {
            block_temp <- rv$Classification_temp[i]
            LOQ_temp <- rv$LLOQs[[block_temp]]
            rv$results[(i + 1), cpt_name] <- ifelse(
                as.numeric(rv$results[(i + 1), cpt_name]) < LOQ_temp,
                paste("<", LOQ_temp, sep = " "),
                rv$results[(i + 1), cpt_name]
            )
        }
    }, error = function(e) {
        stop("Failed to overwrite values below LLOQ: ", e$message)
    })
}

save_interim_results <- function(input, rv) {
    tryCatch({
        setwd(results_directory(input))
        if ("Results_quant.csv" %in% dir()) {
            interim_files <- list.files(pattern = "Results_quant")
            interim_files <- sort(interim_files)
            file_matched <- FALSE
            for (file in interim_files) {
                interim_data <- read.csv(file)
                if ("X" %in% colnames(interim_data)) {
                    interim_data <- interim_data[, !colnames(interim_data) %in% "X"]
                }
                if (all(colnames(interim_data) %in% colnames(rv$results))) {
                    subset_results <- rv$results[, colnames(interim_data)]
                    if (identical(subset_results, interim_data)) {
                        write.csv(rv$results, file)
                        file_matched <- TRUE
                        break
                    }
                }
            }
            if (!file_matched) {
                timestamp <- format(Sys.Date(), "%Y%m%d")
                results_filename <- paste0("Results_quant_", timestamp, ".csv")
            } else {
                results_filename <- "Results_quant.csv"
            }
            write.csv(rv$results, results_filename)
        } else {
          results_filename <- "Results_quant.csv"
            write.csv(rv$results, results_filename)
        }
        if(is.null(results_filename)){
            results_filename <- "Results_quant.csv"
        }

        return(results_filename)
    }, error = function(e) {
        stop("Failed to save interim results: ", e$message)
    })
}

save_final_results <- function(input, rv, quant, cpt_name) {
    tryCatch({
        quantitate_output <- quantitate(input, rv)
        res_list <- list()
        quant <- quantitate_output[[1]]
        res_df <- data.frame(Sample.Name = rv$data$Sample.Name, Classification = rv$Classification_temp)
        res_list[["models"]] <- quantitate_output[[3]]
        res_df <- build_quant_results(res_df, input, rv, quant, res_list, cpt_name)
        setwd(results_directory(input))
        #
        write_quant_results(res_df, cpt_name)
    }, error = function(e) {
        stop("Failed to save final results: ", e$message)
    })
}

generate_report_if_requested <- function(input, rv, cpt_name) {
    if (input$generate_report) {
        tryCatch({
            setwd(script_path)
            rmd_file <- file.path(script_path, "./Modules/Report/report_markdown.Rmd")
            output_file <- paste0("Report_", cpt_name, ".pdf")
            if (file.exists(file.path(results_directory(input), output_file))) {
                timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
                output_file <- paste0("Report_", cpt_name, "_", timestamp, ".pdf")
            }
            rmarkdown::render(
                input = rmd_file,
                output_format = "pdf_document",
                output_dir = results_directory(input),
                output_file = output_file,
                intermediates_dir = results_directory(input)
            )
            return(output_file)
        }, error = function(e) {
            stop("Failed to generate report: ", e$message)
        })
    }
}

update_compound_analysis_state <- function(input, rv, cpt_name, session) {
    tryCatch({
        rv$compounds_analyzed <- c(rv$compounds_analyzed, cpt_name)
        rv$settings_used[[cpt_name]] <- list(
            file_for_correction = input$files_for_correction,
            file_for_bracketing = input$file_for_bracketing,
            selection_table = rv$selection_cals_table,
            LLOQ = rv$LLOQs,
            regression_model = rv$regression_model,
            comment = input$Comment,
            correction_model = input$model_drift,
            weight_method = input$weight_method,
            quantitation_method = input$quantitation_method,
            IS_compound = input$Compound_IS,
            bracketing_table = rv$bracketing_table,
            span_width = input$span_width,
            IS_table = rv$IS_table,
            bracketing_sel_table = rv$selection_table_bracketing,
            model_for_ind_bracketing = input$model_for_ind_bracketing,
            model_bracketing = input$model_bracketing,
            span_width_bracketing = input$span_width_bracketing,
            spline_df = input$spline_df


        )
        rv$p_RT <- NULL
        rv$p_quan_qual <- NULL
        rv$p_blank <- NULL
        rv$p_IS_analysis <- NULL
        rv$p_dc <- NULL
        #rv$LLOQs <- NULL
        updateSelectInput(session, inputId = "Compound", choices = names(rv$data)[grepl(input$quant_indicator, names(rv$data)) & !grepl(input$IS_indicator, names(rv$data))],
            selected = input$Compound)
    }, error = function(e) {
        stop("Failed to update compound analysis state: ", e$message)
    })
}

show_success_notification <- function(input, cpt_name, output_file, results_filename) {
    shiny::showNotification(
        paste(
            "Output for compound", cpt_name, "was successfully generated.",
            "\nFolder:", results_directory(input),
            "\n\nFiles generated or appended:",
            paste(
                if (isTRUE(input$generate_report)) {
                    c(output_file, "Quant_summary.xlsx", results_filename)
                } else {
                    c("Quant_summary.xlsx", results_filename)
                },
                collapse = "\n• "
            ) %>% paste0("• ", .)
        ),
        type = "message",
        duration = 5
    )
}

observe_input_compound <- function(input, rv, session) {
    
  rv$current_layout <- NULL
  rv$current_layout_IS <- NULL
  rv$current_layout_dc <- NULL
  rv$current_layout_acc <- NULL  


    if (input$Compound %in% rv$compounds_analyzed) {
     update_if_analyzed(input, rv, session)
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
    if (input$quantitation_method == "Custom Bracketing") {
      rv$Classification_temp <- rv$data$Classification
    } else if (input$quantitation_method == "Weighted Bracketing") {
      rv$Classification_temp <- update_Classification_ind(rv)
      

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
    min_LOQ <- suppressWarnings(min(rv$setup_cal$Concentration[rv$setup_cal$Concentration != 0], na.rm = TRUE))
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

      for (i in 1:length(class[grepl("^Cal", class)])) {
        bracketing_table <- cbind(bracketing_table, checkboxColumn(len = nrow(bracketing_table), col = i + 1))
        selection_table_bracketing <- cbind(selection_table_bracketing, rep(FALSE, nrow(selection_table_bracketing)))
        selection_table <- cbind(selection_table, rep(FALSE, nrow(selection_table)))

        names(bracketing_table)[i + 1] <- paste("Cal", i, sep = " ")
        names(selection_table)[i + 1] <- paste("Cal", i, sep = " ")
        names(selection_table_bracketing)[i + 1] <- paste("Cal", i, sep = " ")
      }

      if (input$quantitation_method != "Custom Bracketing") {
        selection_table[selection_table == F] <- T
      }

    
      rv$selection_table <- selection_table
      rv$bracketing_table <- bracketing_table
      rv$selection_table_bracketing <- selection_table_bracketing
    }

    selection_table <- if (input$quantitation_method == "Custom Bracketing") {
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
      "Custom Bracketing" = rv$Area,
      "Default Bracketing" = rv$Area,
      "Weighted Bracketing" = rv$Area,
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

    try({
      update_cals(input, rv, session)
    }, silent = TRUE)
    
       

  }, error = function(e) {
    showNotification(paste("Error in compound setup:", e$message), type = "error")
  })
}

update_if_analyzed <- function(input, rv, session) {
      settings <- rv$settings_used[[input$Compound]]

      #print(str(settings))
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
        updateSelectInput(session, "quantitation_method", choices = c("Drift Correction", "Custom Bracketing","Weighted Bracketing", "Default Bracketing"), selected = settings$quantitation_method)
      } else {
        updateSelectInput(session, "quantitation_method", choices = c("IS Correction", "Drift Correction","Custom Bracketing", "Weighted Bracketing", "Default Bracketing"), selected = settings$quantitation_method)
      }


      updateTextInput(session, "Comment", value = settings$comment)
      updateRadioButtons(session, "model_drift", choices = c("lm", "loess", "poly"), selected = settings$correction_model)

          # Update selection inputs
    unique_sample_names <- unique(rv$data$Sample.Name[rv$data$Sample.Type != "Blank"])
    sample_name_counts <- sapply(unique_sample_names, function(x) sum(rv$data$Sample.Name == x))
    unique_sample_names <- unique_sample_names[sample_name_counts > 2]
    sample_name_counts <- sample_name_counts[sample_name_counts > 2]
    display_choices <- paste(unique_sample_names, "(", sample_name_counts, ")", sep = "")

    updateSelectInput(session, inputId = "files_for_correction", choices = setNames(unique_sample_names, display_choices), selected = settings$file_for_correction)
    updateSelectInput(session, inputId = "file_for_bracketing", choices = setNames(unique_sample_names, display_choices),
    selected = settings$file_for_bracketing)
    
      updateNumericInput(session, "span_width", value = settings$span_width, min = 0.4, max = 2, step = 0.05)

      rv$IS_table <- settings$IS_table




updateSelectInput(session, inputId = "model_for_ind_bracketing", "Model for weighting:", choices = c("linear", "non linear (QC)"),
selected = settings$model_for_ind_bracketing)

 updateSelectInput(session, inputId = "model_bracketing", "Select Model:", choices = c("loess", "poly"), selected = settings$model_bracketing)

                                   
  # Loess-specific file input

      updateNumericInput(session,
        inputId = "span_width_bracketing",
        label = "Span Width:",
        min = 0.4, max = 10, step = 0.05, value = settings$span_width_bracketing
      )


    # Optional smoothing parameter or model tuning inputs (placeholders for now)
    
      updateNumericInput(session,
        inputId = "spline_df",
        label = "Degree:",
        value = settings$spline_df,
        min = 2,
        max = 20,
        step = 1
      )


                  
   

     
}