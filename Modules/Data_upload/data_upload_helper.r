observe_input_file_1 <- function(input, rv, session) {
  
    # Check if the uploaded file contains the required columns and valid separator
    file <- input$file1
    ext <- tools::file_ext(file$datapath)

    # Read the file with different separators to determine the correct one
    tryCatch({
      df_temp <- read_file_safe(file$datapath) %>% as.data.frame()
    }, error = function(e) {
      showNotification("Error reading the file. Please check the file format and content.", type = "error")
      return(NULL)
    })

    
    

    req(df_temp)

        # Check if required columns are present
    validate(need("Sample.Name" %in% colnames(df_temp), "The file must contain the 'Sample.Name' column."))
    validate(need("Sample.Type" %in% colnames(df_temp), "The file must contain the 'Sample.Type' column."))

    # Check for "Classification" and handle accordingly
    if (!("Classification" %in% colnames(df_temp))) {
      showNotification("The file does not contain the column 'Classification'. It will be added automatically.", type = "message")
      df_temp <- classify_samples(df_temp)
    } else {
      validate(need(any(grepl("Cal 1", df_temp$Classification)), 
                    "The 'Classification' column must contain at least one 'Cal 1'."))
    }

    # Reorder columns
    required_columns <- c("Sample.Name", "Sample.Type", "Classification")
    other_columns <- setdiff(colnames(df_temp), required_columns)
    df_temp <- df_temp[, c(required_columns, other_columns)]



      

      if (!is.null(rv$data)) {
        data_prev <- rv$data

        # Check if all columns from df_temp are present in rv$orig
        if (!all(colnames(df_temp) %in% colnames(data_prev))) {
        showModal(modalDialog(
          title = "Reset Session",
          "It appears that you want to upload a different dataset. Please reset the app before proceeding to prevent a crash. Would you like to reset now?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_reset", "Yes, reset", class = "btn-danger")
          ),
          easyClose = TRUE
        ))
        df_temp <- NULL
      } else {
    # Add any missing columns to df_temp (from rv$data), to ensure bind_rows doesn't fail
    missing_cols <- setdiff(colnames(data_prev), colnames(df_temp))
    for (col in missing_cols) {
      df_temp[[col]] <- NA
    }

    # Append the new rows
    combined_data <- cbind(df_temp, data_prev[, !(colnames(data_prev) %in% colnames(df_temp))])

    # Reorder columns to match the original rv$data column order
    combined_data <- combined_data[, colnames(data_prev)]

    # Append df_temp to rv$data
    df_temp <- combined_data
  }

     

    }  


     
     #stop("Works until here")


    

    req(df_temp)


    # Update reactive values
    

    rv$orig <- df_temp
    data_upload <- rv$orig
    data_upload[data_upload == "N/A"] <- 0
    data_upload[data_upload == "N/F"] <- 0
    data_upload[is.na(data_upload)] <- 0

    

    data_upload$Sample.Type[data_upload$Sample.Type == "Standard"] <- "Cal"
    data_upload <- as.list(data_upload)

    suppressWarnings({
      data_upload <- lapply(data_upload, function(x) {
        if (any(is.na(as.numeric(x)))) {
          return(x)
        } else {
          return(as.numeric(x))
        }
      })
    })

    data_upload <- as.data.frame(data_upload)

    

    # Remove columns if standards are missing
    col_rm <- c()
    req(input$quant_indicator)

    for (i in 1:ncol(data_upload)) {
      if (!grepl(input$quant_indicator, colnames(data_upload)[i])) next
      col_temp <- data_upload[grepl("^Cal", data_upload$Classification), i]
      if (sum(is.na(col_temp)) / length(col_temp) > 1) {
        col_rm <- c(col_rm, colnames(data_upload)[i])
      }
    }

    if (!all(data_upload$Sample.Type %in% 
             c("Cal", "Sample", "Standard", "Blank", "QC"))) {
      stop("The 'Sample.Type' column contains invalid entries. Ensure it only contains 'Cal', 'Sample', 'Standard', 'Blank', or 'QC'.")
    }

    
    rv$data <- data_upload[, !(colnames(data_upload) %in% col_rm)]

    if (length(col_rm) > 0) {
      shinyalert(
        title = "Columns Removed",
        text = paste("The following columns were removed:", paste(col_rm, collapse = ", ")),
        type = "info",
        closeOnClickOutside = TRUE
      )
    }

    rv$data$Sample.Name <- as.character(rv$data$Sample.Name)

    if (!("Classification" %in% colnames(rv$data))) {
      rv$data$Classification <- rep("N/A", nrow(rv$data))
      rv$data <- relocate(rv$data, Classification, after = Sample.ID)
    }

    # Sanitize column names
    for (i in 1:ncol(rv$data)) {
      name_temp <- colnames(rv$data)[i]
      colnames(rv$data)[i] <- regmatches(name_temp, gregexpr("[[:alnum:]]|[_.]+", name_temp)) %>% 
        unlist() %>% 
        paste(collapse = "", sep = "")
    }

    for (i in 1:nrow(rv$data)) {
      name_temp <- rv$data$Sample.Name[i]
      if (unlist(gregexpr("[0-9]", name_temp))[1] == 1) {
        name_temp <- paste0("X", name_temp)
      }
      rv$data$Sample.Name[i] <- name_temp
    }

    Class_temp <- rv$data$Classification

    if (input$quantitation_method != "Custom Bracketing" | input$quantitation_method != "Weighted Bracketing") {
      Class_temp[!grepl("^Cal", Class_temp)] <- "all"
    }

    rv$Classification_temp <- Class_temp

    

    # Update selection inputs
    unique_sample_names <- unique(rv$data$Sample.Name[rv$data$Sample.Type != "Blank"])
    sample_name_counts <- sapply(unique_sample_names, function(x) sum(rv$data$Sample.Name == x))
    unique_sample_names <- unique_sample_names[sample_name_counts > 2]
    sample_name_counts <- sample_name_counts[sample_name_counts > 2]
    display_choices <- paste(unique_sample_names, "(", sample_name_counts, ")", sep = "")

    updateSelectInput(session, inputId = "files_for_correction", choices = setNames(unique_sample_names, display_choices))
    updateSelectInput(session, inputId = "file_for_bracketing", choices = setNames(unique_sample_names, display_choices))
    
    updateCheckboxGroupInput(session, inputId = "RT_groups", choices = unique(rv$data$Sample.Type))
    updateCheckboxGroupInput(session, inputId = "IS_groups", choices = unique(rv$data$Sample.Type))

    updateSelectInput(session,
      inputId = "Compound", label = "Compound:",
      choices = names(rv$data)[grepl(input$quant_indicator, names(rv$data)) & !grepl(input$IS_indicator, names(rv$data))],
      selected = names(rv$data)[grepl(input$quant_indicator, names(rv$data)) & !grepl(input$IS_indicator, names(rv$data))][1]
    )

    updateSelectInput(session, inputId = "Block", label = "Block to Visualize", choices = unique(rv$Classification_temp), selected = unique(rv$Classification_temp)[1])

    # IS columns setup
    req(input$IS_indicator)
    is_columns <- names(data(input, rv))[grepl(input$IS_indicator, names(data(input, rv)))]

    if (length(is_columns) == 0 || all(is.null(is_columns)) || all(is.na(is_columns))) {
      shinyalert("Warning", "No Internal Standard Transitions observed.", type = "warning")
      updateSelectInput(session, inputId = "quantitation_method", label = "Method for Quantification",
        choices = c("Drift Correction", "Custom Bracketing","Weighted Bracketing", "Default Bracketing"), selected = "Drift Correction")
      rv$data <- add_column(rv$data, !!"none" := 1)
      is_columns <- "none"
    } else {
      updateSelectInput(session, inputId = "quantitation_method", label = "Method for Quantification",
        choices = c("IS Correction", "Drift Correction", "Custom Bracketing","Weighted Bracketing", "Default Bracketing"), selected = "IS Correction")
    }

    if (!"none" %in% colnames(rv$data)) {
      rv$data <- rv$data[, !grepl("none", colnames(rv$data))]
    }

    updateSelectInput(session,
      inputId = "Compound_IS", label = "Internal Standard:",
      choices = is_columns,
      selected = is_columns[1]
    )

    updateCheckboxGroupInput(session, inputId = "quan_qual_groups", choices = unique(rv$data$Sample.Type))

    req(input$mode)
    req(rv$templates[[input$mode]])

    

    if (all(colnames(dplyr::select(rv$templates[[input$mode]], -Cal.Name)) %in% colnames(rv$data))) {
      rv$specific_setup <- rv$templates[[input$mode]] %>% as.data.frame()

      if ((input$Compound) == "") {
        rv$setup_cal <- data.frame(Cal.Name = rv$specific_setup$Cal.Name, Concentration = rep(NA, nrow(rv$specific_setup)))
      } else {
        ret_temp <- rv$specific_setup$Cal.Name
        conc_temp <- as.numeric(rv$specific_setup[, input$Compound])
        rv$setup_cal <- data.frame(Cal.Name = ret_temp, Concentration = conc_temp)
      }
    } else {
      rv$setup_cal <- rv$templates[[input$mode]]
    }

}