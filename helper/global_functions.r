#### Global functions ----

# Function to get the script directory
get_script_directory <- function() {
  if ("rstudioapi" %in% installed.packages() && rstudioapi::isAvailable()) {
    return(dirname(rstudioapi::getActiveDocumentContext()$path))
  }
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  scriptPath <- sub("--file=", "", cmdArgs[grepl("--file=", cmdArgs)])
  if (length(scriptPath) > 0) {
    return(dirname(normalizePath(scriptPath, winslash = "/")))
  }
  return(getwd())
}

# Function to create block sample subsets
create_block_sample_subsets <- function(block_cal_map, sample_data) {
  block_sample_list <- list()
  unique_blocks <- unique(block_cal_map[, 1])
  for (block in unique_blocks) {
    block_row <- block_cal_map[block_cal_map[, 1] == block, ]
    if (nrow(block_row) == 0) next
    cals_for_block <- colnames(block_row)[which(as.logical(block_row[1, -1])) + 1]
    cals_ss <- subset(sample_data, Classification %in% cals_for_block)
    block_sample_list[[block]] <- cals_ss
  }
  return(block_sample_list)
}

# Function to get results directory

results_directory <- function(input = NULL) {
  if (.Platform$OS.type == "windows") {
    path <- file.path(Sys.getenv("USERPROFILE"), "Documents")
  } else if (Sys.info()["sysname"] == "Darwin") {
    path <- file.path(Sys.getenv("HOME"), "Documents")
  } else {
    path <- file.path(Sys.getenv("HOME"), "Documents")
  }
  path <- stringr::str_replace_all(path, "\\\\", "/")
  
  if (!dir.exists(path)) {
    stop("Error: Documents folder not found! Please check your system.")
  }
  
  # Set working directory to Documents
  setwd(path)
  #message("Working directory set to Documents folder.")
  
  # Create 'QuantyFey' folder if it doesn't exist
  quantyfey_path <- file.path(path, "QuantyFey")
  if (!dir.exists(quantyfey_path)) {
    dir.create(quantyfey_path)
    message("Created 'QuantyFey' folder.")
  }
  setwd(quantyfey_path)
  #message("Working directory set to 'QuantyFey' folder.")
  
  # Create 'Results_YYYYMMDD' folder
  today_date <- format(Sys.Date(), "%Y%m%d")

  if(!is.null(input)){
    results_folder <- file.path(quantyfey_path, input$project_name)
  } else {
    results_folder <- file.path(quantyfey_path, paste0("Results_", today_date))
  }
  


  if (!dir.exists(results_folder)) {
    dir.create(results_folder)
    message(paste("Created results folder:", results_folder))
  }
  return(results_folder)
  
}


checkboxColumn <- function(len, col, values = NULL, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    checked <- if (!is.null(values)) values[i] else FALSE
    inputs[i] <- as.character(checkboxInput(paste0("checkb_", col, "_", i), label = NULL, value = checked, ...))
  }
  inputs
}


js <- function(dtid, cols, ns = identity) {
  code <- vector("list", length(cols))
  for (i in seq_along(cols)) {
    col <- cols[i]
    code[[i]] <- c(
      sprintf("$(document).on('click', '[id^=checkb_%d_]', function() {", col),
      "  var id = this.getAttribute('id');",
      sprintf("  var i = parseInt(/checkb_%d_(\\d+)/.exec(id)[1]);", col),
      "  var value = $(this).prop('checked');",
      sprintf("  var info = [{row: i, col: %d, value: value}];", col),
      sprintf("  Shiny.setInputValue('%s_cell_edit', info);", dtid),  # Match the input name
      "});"
    )
  }
  do.call(c, code)
}

create_cals_list <- function(block_cal_map, sample_data) {
  
  # 1. Initialize the output list
  block_sample_list <- list()
  
  # 2. Get the unique blocks from the block_cal_map
  unique_blocks <- unique(block_cal_map[, 1])
  
  # 3. Iterate over each block
  for (block in unique_blocks) {
    
    # 4. Get the row corresponding to the block from block_cal_map
    block_row <- block_cal_map[block_cal_map[, 1] == block, ]
    
    if (nrow(block_row) == 0) {
      next # Skip this block if no data is found
    }
    
    # 5. Find the Cals that are marked as TRUE for this block
    cals_for_block <- colnames(block_row)[which(as.logical(block_row[1, -1])) + 1]
    
    # 6. Subset the selected Cals from the data
    cals_ss <- subset(sample_data, Classification %in% cals_for_block)
    
    # 7. Add the subset into the list with the name of that block
    block_sample_list[[block]] <- cals_ss
    
  }
  
  # 10. Return the list of subsets
  return(block_sample_list)
}



# Function to append data to an existing Excel file
append_xlsx <- function(existing_file, new_data, sheet_name) {
    # Check if existing_file is a string that ends with .xlsx
    if (!is.character(existing_file) || !grepl("\\.xlsx$", existing_file)) {
        stop("existing_file must be a string that ends with .xlsx")
    }

    # Check if new_data is a data frame
    if (!is.data.frame(new_data)) {
        stop("new_data must be a data frame")
    }

    # Check if sheet_name is a string
    if (!is.character(sheet_name) || length(sheet_name) != 1) {
        stop("sheet_name must be a single string")
    }

    # Check if the file exists

    if (file.exists(existing_file)) {

    # get all sheets from the existing file

    existing_sheets <- readxl::excel_sheets(existing_file)

    # create empty list to store dataframes

    existing_data_list <- list()

    # create list of dataframes from the existing sheets
    for (sheet in existing_sheets) {

      # read the data from each sheet into a list
      suppressWarnings(
         sheet_data <- readxl::read_xlsx(existing_file, sheet = sheet)
      )
     
      # Rename the column `...8` to an empty string if it exists
      colnames(sheet_data)[colnames(sheet_data) == "...8"] <- ""

      existing_data_list[[sheet]] <- sheet_data
    }
    n = 1
    while(sheet_name %in% existing_file){
      sheet_name <- paste0(n, "_", sheet_name)
      n =+ 1
    }


    # Combine existing data and new data into a new list

    existing_data_list[[sheet_name]] <- new_data
    
    # write new version of the file
    tryCatch({
        writexl::write_xlsx(existing_data_list, existing_file)
    }, error = function(e) {
        if (grepl("open", e$message, ignore.case = TRUE)) {
            stop("The file is likely open. Please close the file and try again.")
        } else {
            stop(e)
        }
    })
    
  } else {
    # If the file doesn't exist, create it with the new data
    writexl::write_xlsx(new_data, existing_file, sheet = sheet_name)
  }
}


add_weights <- function(weight_method = c("1/x", "1/x2", "1/y", "1/y2", "1/x force 0", "1/y force 0", "none"), data) {
          
            # Validate input
            if (!all(c("Concentration", "PeakArea") %in% colnames(data))) {
              stop("The data must contain the columns 'Concentration' and 'PeakArea'.")
          }

            
            # Calculate weights based on the specified method
          weights <- switch(weight_method,
              "1/x" = ifelse(data$PeakArea != 0 & !is.na(data$PeakArea), 1 / data$Concentration, 0),
              "1/x2" = ifelse(data$PeakArea != 0 & !is.na(data$PeakArea), 1 / data$Concentration^2, 0),
              "1/y" = ifelse(data$PeakArea != 0 & !is.na(data$PeakArea), 1 / data$PeakArea, 0),
              "1/y2" = ifelse(data$PeakArea != 0 & !is.na(data$PeakArea), 1 / data$PeakArea^2, 0),
              "none" = ifelse(data$PeakArea != 0 & !is.na(data$PeakArea), 1, 0),
              "1/x force 0" = {
                ifelse(data$PeakArea != 0 & !is.na(data$PeakArea), 1 / data$Concentration, 0)
                
              },
              "1/y force 0" = {
                ifelse(data$PeakArea != 0 & !is.na(data$PeakArea), 1 / data$PeakArea, 0)
                
              },

              stop("Invalid weight method specified.")
          )

          data$weights <- weights
            
          return(data)
        }

## Get models Function
quantyFey <- function(data = NULL, xfunction = c("linear", "quadratic", "power", "exponential", "logarithmic(y)"), weight_method = c("1/x", "1/x2", "1/y", "1/y2", "1/x force 0", "1/y force 0", "none")) {

  # Check if data is a dataframe with the required columns
  required_columns <- c("Sample.Name", "Classification", "Concentration", "PeakArea")
  if (!is.data.frame(data) || !all(required_columns %in% colnames(data))) {
    stop("Error: 'data' must be a dataframe with the following columns: Sample.Name, Classification, Concentration, PeakArea")
  }

  # Check if the 'weight' column is missing and add it using the add_weight function
  if (!"weights" %in% colnames(data)) {
    data <- add_weights(data = data, weight_method = weight_method)
  }

  # Check if the 'used' column is missing and add it
  if (!"used" %in% colnames(data)) {
    data$used <- !(data$PeakArea == 0 | data$Concentration == 0)
  }

  data$used <- as.logical(data$used)

  ## select only necessary columns
  data <- dplyr::select(data, Sample.Name, Classification, Concentration, PeakArea, weights, used)

  force_0 <- ifelse(weight_method %in% c("1/x force 0", "1/y force 0"), TRUE, FALSE)

  data$used[data$PeakArea == 0] <- FALSE

  if (force_0) {
    data <- rbind(data, data.frame(
      Sample.Name = c("Force 0", "Force 0", "Force 0"),
      Classification = c("Force 0", "Force 0", "Force 0"),
      Concentration = c(0, 0, 0),
      PeakArea = c(0, 0, 0),
      weights = c(1E10, 1E10, 1E10),
      used = c(TRUE, TRUE, TRUE)
    ))
  }

  # Create models based on the specified function type
  if (xfunction == "linear") {
    mods <- lm(PeakArea ~ Concentration, data = data[data$used, ], weights = data$weight[data$used])
  } else if (xfunction == "quadratic") {
    data$Concentration2 <- data$Concentration^2
    mods <- lm(PeakArea ~ Concentration + Concentration2, data = data[data$used, ], weights = data$weight[data$used])
  } else if (xfunction == "power") {
    mods <- lm(log(PeakArea) ~ log(Concentration), data = data[data$used, ], weights = data$weight[data$used])
  } else if (xfunction == "exponential") {
    mods <- lm(log(PeakArea) ~ Concentration, data = data[data$used, ], weights = data$weight[data$used])
  } else if (xfunction == "logarithmic(y)") {
    mods <- lm(PeakArea ~ log(Concentration), data = data[data$used, ], weights = data$weight[data$used])
  } else {
    stop("Error: Unsupported function type specified.")
  }

  return(mods)
}




predict_concentrations <- function(data, model, method = c("linear", "quadratic", "power", "exponential", "logarithmic(y)")){
  # Check if data is a data.frame with the column "PeakArea"
  if (is.data.frame(data) && "PeakArea" %in% colnames(data)) {
    temp <- data.frame(PeakArea = data$PeakArea)
  } 
  # Check if data is a vector
  else if (is.vector(data)) {
    temp <- data.frame(PeakArea = data)
  } 
  # If neither, throw an error
  else {
    stop("Error: 'data' must be either a data.frame with a 'PeakArea' column or a vector.")
  }

  if (method == "linear") {
    coefficients <- coef(model)
    k <- coefficients[2]  # Slope
    d <- coefficients[1]  # Intercept
    temp$Concentration <- (temp$PeakArea - d) / k
    return(temp$Concentration)
  } else if (method == "quadratic") {

    coefficients <- coef(model)
    a <- coefficients[3]  # Quadratic term
    b <- coefficients[2]  # Linear term
    c <- coefficients[1]  # Intercept
    temp$Concentration <- sapply(temp$PeakArea, function(y) {
      discriminant <- b^2 - 4 * a * (c - y)
      if (discriminant < 0) {
      return(NA)  # No real solution
      }
      x1 <- (-b + sqrt(discriminant)) / (2 * a)
      x2 <- (-b - sqrt(discriminant)) / (2 * a)
      x_max <- -b / (2 * a)
      return(x1)
    })
    return(temp$Concentration)
  } else if (method == "power") {

    coefficients <- coef(model)
      a <- coefficients[2]  # Exponent
      b <- exp(coefficients[1])  # Base
      temp$Concentration <- exp((log(temp$PeakArea) - log(b)) / a)
      return(temp$Concentration)
    # Code for power method
  } else if (method == "exponential") {

    coefficients <- coef(model)
    a <- coefficients[2]  # Exponent
    b <- exp(coefficients[1])  # Base
    temp$Concentration <- log(temp$PeakArea / b) / a
    return(temp$Concentration)
    # Code for exponential method
  } else if (method == "logarithmic(y)") {
    coefficients <- coef(model)
    a <- coefficients[2]  # Slope
    b <- coefficients[1]  # Intercept
    temp$Concentration <- exp((temp$PeakArea - b) / a)
    return(temp$Concentration)
    # Code for logarithmic(y) method
  } else {
    stop("Error: Unsupported method specified.")
  }

}


# generate the quantification ggplot
generate_quantitate_ggplot <- function(data, regression_model, regression_method = c("linear", "quadratic", "power", "exponential", "log(y)")){
  # Check if the required columns are present in the data
  required_columns <- c("Sample.Name", "Classification", "Concentration", "PeakArea", "weights", "used")
  if (!all(required_columns %in% colnames(data))) {
    stop("Error: The data must contain the following columns: Cal.Name, Classification, Concentration, PeakArea, weights, used")
  }
  # Validate that regression_model is an object resulting from the lm function
  if (!inherits(regression_model, "lm")) {
    stop("Error: 'regression_model' must be an object resulting from the lm function.")
  }
  data$Concentration2 <- data$Concentration^2

  df_mods <- data.frame(Concentration = seq(0, max(data$Concentration[(data$used)]), length.out = 300))
  data$used <- factor(data$used, levels = c(T, F), labels = c("used", "not used"))
  suppressWarnings({
      p <- ggplot() +
      geom_point(data = data, mapping = aes(x = Concentration, y = PeakArea, label = Sample.Name, col = used), size = 1.5) +
      scale_colour_manual(values = c("used" = "black","not used" = "lightgrey")) +
      labs(col = "")
  })
  

     

      df_mods$Concentration2 <- df_mods$Concentration^2
      df_mods$PeakArea <- predict(regression_model, df_mods)

      
      p <- p + geom_line(data = df_mods, mapping = aes(x = Concentration, y = PeakArea), col = "black", linewidth = 1)

    p <- p + theme_pubclean(base_size = 17)

    if (regression_method == "quadratic") {
      a <- signif(regression_model$coefficients[3], 5)
      b <- signif(regression_model$coefficients[2], 5)
      c <- signif(regression_model$coefficients[1], 5)

      p <- p + ggtitle(label = paste(
        paste(
          "y = ", ifelse(a > 0, paste("+", a, sep = " "), paste("-", abs(a), sep = " ")), "x<sup>2</sup>",
          ifelse(b > 0, paste("+", b, sep = " "), paste("-", abs(b), sep = " ")), "x",
          ifelse(c > 0, paste("+", c, sep = " "), paste("-", abs(c), sep = " "))
        ),
        paste("R<sup>2</sup> =", signif(summary(regression_model)$r.squared, 4)),
        sep = "\n"
      ))
    } else if (regression_method == "linear") {
      a <- signif(regression_model$coefficients[2], 5)
      b <- signif(regression_model$coefficients[1], 5)

      p <- p + ggtitle(label = paste(
        paste(
          "y = ", ifelse(a > 0, paste("+", a, sep = " "), paste("-", abs(a), sep = " ")), "x",
          ifelse(b > 0, paste("+", b, sep = " "), paste("-", abs(b), sep = " "))
        ),
        paste("R<sup>2</sup> =", signif(summary(regression_model)$r.squared, 4)),
        sep = "\n"
      ))
    } else if (regression_method == "power") {
      a <- signif(regression_model$coefficients[2], 5)
      b <- signif(exp(regression_model$coefficients[1]), 5)

      p <- p + ggtitle(label = paste(
        paste(
          "y = ", ifelse(b > 0, paste(b, sep = " "), paste("-", abs(b), sep = " ")), "x<sup>", a, "</sup>"
        ),
        paste("R<sup>2</sup> =", signif(summary(regression_model)$r.squared, 4)),
        sep = "\n"
      ))
    } else if (regression_method == "exponential") {
      a <- signif(regression_model$coefficients[2], 5)
      b <- signif(exp(regression_model$coefficients[1]), 5)

      p <- p + ggtitle(label = paste(
        paste(
          "y = ", ifelse(b > 0, paste(b, sep = " "), paste("-", abs(b), sep = " ")), "e<sup>", a, "x</sup>"
        ),
        paste("R<sup>2</sup> =", signif(summary(regression_model)$r.squared, 4)),
        sep = "\n"
      ))
    } else if (regression_method == "log(y)") {
      a <- signif(regression_model$coefficients[2], 5)
      b <- signif(regression_model$coefficients[1], 5)

      p <- p + ggtitle(label = paste(
        paste(
          "log(y) = ", ifelse(a > 0, paste("+", a, sep = " "), paste("-", abs(a), sep = " ")), "x",
          ifelse(b > 0, paste("+", b, sep = " "), paste("-", abs(b), sep = " "))
        ),
        paste("R<sup>2</sup> =", signif(summary(regression_model)$r.squared, 4)),
        sep = "\n"
      ))
    }

    return(p)

}


classify_samples <- function(df) {
  # Ensure the data frame has the required column
  if (!"Sample.Type" %in% colnames(df)) {
    stop("The data frame must contain a 'Sample.Type' column.")
  }
  
  # Add a new column for classification
  df <- dplyr::mutate(df, Classification = NA)
  
  # Initialize variables
  cal_counter <- 1
  first_block <- TRUE
  sample_counter <- 1

  i <- 2  # Start from the second row to allow checking prior and next rows
  
  while (i <= nrow(df) - 1) {
    # Get the prior, current, and next Sample.Type
    prior <- df$Sample.Type[i - 1]
    current <- df$Sample.Type[i]
    xnext <- df$Sample.Type[i + 1]
    
    # Check if all three are "^Cal" or "Standard"
    if (all(c(prior, current, xnext) %in% c("Cal", "Standard"))) {
      if(!first_block){
        sample_counter <- sample_counter + 1
      }
      
      first_block <- FALSE
      

      # Enter the "Cal mode"
      while (i <= nrow(df) && df$Sample.Type[i] %in% c("Cal", "Standard")) {
        df$Classification[i - 1] <- paste0("Cal ", cal_counter)
        df$Classification[i] <- paste0("Cal ", cal_counter)
        #df$Classification[i + 1] <- paste0("Cal ", cal_counter)
        i <- i + 1
      }
      # Increment the counter for the next group
      cal_counter <- cal_counter + 1
    } else {
      if(i == 2){
        df$Classification[1] <- ifelse(first_block, "Pre 1", paste0("Block ", sample_counter))
    }
      df$Classification[i] <- ifelse(first_block, "Pre 1", paste0("Block ", sample_counter))

      # Move to the next row if the condition is not met
      i <- i + 1
    }
  }
  
  return(df)
}

process_peak_table <- function(peak_table){
  # Check if the input is a data.frame
  if (!is.data.frame(peak_table)) {
    stop("The peak table must be a data.frame.")
  }

  # Check for required column names
  required_columns <- c("Sample.Name", "Sample.Type", "Classification")
  missing_columns <- setdiff(required_columns, colnames(peak_table))
  if (length(missing_columns) > 0) {
    stop(paste("The peak table is missing the following required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Validate Sample.Type values
  valid_sample_types <- c("Cal", "Standard", "QC", "Sample", "Blank")
  if (!all(peak_table$Sample.Type %in% valid_sample_types)) {
    stop("The 'Sample.Type' column contains invalid values. Allowed values are: Cal, Standard, QC, Sample, or Blank.")
  }

  # Check for at least one "Cal 1" value in Classification
  if (!any(peak_table$Classification == "Cal 1")) {
    stop("The 'Classification' column must contain 'Cal 1'.")
  }

  # swap N/A, N/F and NA to 0

  peak_table[peak_table %in% c("N/F", "N/A")] <- 0
  peak_table[is.na(peak_table)] <- 0

  peak_table$Sample.Type[peak_table$Sample.Type == "Standard"] <- "Cal"

  

  suppressWarnings({
        peak_table <- lapply(as.list(peak_table), FUN = function(x) {
        if (any(is.na(as.numeric(x)))) {
          return(x)
        } else {
          return(as.numeric(x))
        }
      }) %>% as.data.frame()

    })
  
  # Remove Columns where all Standards are NA
  col_rm <- c()
  df_rm <- peak_table[, setdiff(colnames(peak_table), c("Sample.Name", "Sample.Type", "Classification"))]
  for (i in 1:ncol(df_rm)) {
    
    col_temp <- df_rm[peak_table$Sample.Type == "Cal", i]

    if (sum(is.na(col_temp) / length(col_temp)) == 1) {
      col_rm <- c(col_rm, colnames(data_upload)[i])
      if (length(col_rm) > 0) {
        peak_table <- peak_table[, !(colnames(peak_table) %in% col_rm)]
        warning(paste("The following columns have been removed due to all values from Standards being NA:", paste(col_rm, collapse = ", ")))
      }
    }
  }
  
  #remove any special characters from Sample.Names
  peak_table$Sample.Name <- sapply(peak_table$Sample.Name, FUN = remove_special_characters)
  

  return(peak_table)
}


remove_special_characters <- function(name){

  names_vector_clean <- regmatches(name, gregexpr("[[:alnum:]]|[_. ]+", name)) %>% unlist() %>% paste(collapse = "", sep = "")

  if (grepl("^[0-9]", names_vector_clean)) {
    names_vector_clean <- paste0("X", names_vector_clean)
  }

  return(names_vector_clean)
}

update_sel_cal_areas <- function(input, rv){


  if (!is.null(rv$selection_cals_table)) {
    df <- data.frame(Sample.Name = rv$data$Sample.Name, Classification = rv$Classification_temp)
    
    df$Area <- sapply(input$quantitation_method, FUN = function(x) {
      if (x == "Custom Bracketing" | x == "Default Bracketing" | x == "Weighted Bracketing") {
        return(rv$Area)
      } else if (x == "IS Correction") {
        return(rv$IS_ratio)
      } else if (x == "Drift Correction") {
        return(rv$dc_area)
      }
    }, USE.NAMES = F)
    
    selection_cals_table <- rv$selection_cals_table
    
    for (i_table in 1:length(selection_cals_table)) {
      sel_table <- selection_cals_table[[i_table]]
      for (i_row in 1:nrow(sel_table)) {
        Class_temp <- sel_table$Classification[i_row]
        Sample_temp <- sel_table$Sample.Name[i_row]
        
        sel_table$PeakArea[i_row] <- df$Area[df$Sample.Name == Sample_temp & df$Classification == Class_temp]
      }
      
      selection_cals_table[[i_table]] <- sel_table
    }
    
    rv$selection_cals_table <- selection_cals_table
  }


}

labels <- function(rv){
  return(rv$setup_cal$Cal.Name)
}

  concentrations <- function(rv){
      return(rv$setup_cal$Concentration)
  }

  pd_temp <- function(rv){
    pd <- rv$data$Sample.Type
    return(pd)
  }

  get_cals_list <- function(input, rv){
    all_present <- all(rv$setup_cal$Cal.Name %in% rv$data$Sample.Name)
    req(all_present)

    df <- data()[c("Sample.Name", "Classification", input$Compound)]

    selection <- rv$selection_table

    colnames(df)[colnames(df) == input$Compound] <- "PeakArea"

    cals_list <- create_cals_list(block_cal_map = selection, sample_data = df)

    for (i in 1:length(cals_list)) {
      cal_temp <- cals_list[[i]]

      cal_temp$Concentration <- sapply(cal_temp$Sample.Name, function(x, label, concentration) {
        for (i in 1:length(label)) {
          if (grepl(label[i], x)) {
            return(concentration[i])
          }
        }
      },
      label = rv$setup_cal$Cal.Name,
      concentration = rv$setup_cal$Concentration
      )

      cal_temp <- subset(cal_temp, Concentration != 0)
      #rv$force_0 <- ifelse(input$weight_method %in% c("1/x force 0", "1/y force 0"), T, F)
      cal_temp <- add_weights(weight_method = input$weight_method, data = cal_temp)
      #cal_temp$weight <- get_weights(weight_method = input$weight_method, data = cal_temp)

      if (is.null(cal_temp$used)) {

        cal_temp$used <- ifelse(cals_temp$PeakArea == 0, FALSE, TRUE)
        #cal_temp$used <- rep(TRUE, nrow(cals_temp))
      }

      cals_list[[i]] <- cal_temp
    }

    return(cals_list)
  }


  update_cals <- function(input, rv, session)({

    all_present <- all(rv$setup_cal$Cal.Name %in% rv$data$Sample.Name)
    req(all_present)

    req(input$file1)


    if (input$quantitation_method == "Custom Bracketing") {
      selection <- rv$selection_table_bracketing

      

      rv$Classification_temp <- rv$data$Classification

      rv$selection_cals_table <- list()



      updateSelectInput(session, inputId = "Block", choices = selection$Class)

    } else if (input$quantitation_method == "Weighted Bracketing"){
      rv$Classification_temp <- update_Classification_ind(rv)
      selection <- data.frame(Class = unique(rv$Classification_temp))
      for(i in unique(rv$Classification_temp[grepl("^Cal", rv$Classification_temp)])){
        selection <- cbind(selection, new = rep(T, nrow(selection)))
        colnames(selection)[colnames(selection) == "new"] <- i
      }
      selection[selection == FALSE] <- TRUE

      rv$selection_cals_table <- list()

      updateSelectInput(session, inputId = "Block", choices = rv$Classification_temp)

    } else if (input$quantitation_method == "Default Bracketing") {
      selection <- rv$selection_table
      selection[selection == FALSE] <- TRUE

      rv$Classification_temp[!grepl("^Cal", rv$Classification_temp)] <- "all"

      rv$selection_cals_table <- list()

      updateSelectInput(session, inputId = "Block", choices = rv$Classification_temp)
    } else if (input$quantitation_method == "IS Correction") {
      selection <- rv$selection_table
      selection[selection == FALSE] <- TRUE

      rv$Classification_temp[!grepl("^Cal", rv$Classification_temp)] <- "all"

      rv$selection_cals_table <- list()

      updateSelectInput(session, inputId = "Block", choices = rv$Classification_temp)
    } else if (input$quantitation_method == "Drift Correction") {
      selection <- rv$selection_table
      selection[selection == FALSE] <- TRUE

      rv$Classification_temp[!grepl("^Cal", rv$Classification_temp)] <- "all"

      rv$selection_cals_table <- list()

      updateSelectInput(session, inputId = "Block", choices = rv$Classification_temp)
    }

    rv$LLOQs <- list()

    for (block in unique(rv$Classification_temp)) {
      rv$LLOQs[[block]] <- min(rv$setup_cal$Concentration)
    }
    

    updateNumericInput(session, inputId = "LOQ", value = rv$LLOQs[[input$Block]], min = min(rv$setup_cal$Concentration), max = max(rv$setup_cal$Concentration))
    
   
    
    df <- data.frame(Sample.Name = rv$data$Sample.Name, Classification = rv$Classification_temp)

    df$PeakArea <- sapply(input$quantitation_method, FUN = function(x) {
      if (x == "Custom Bracketing" | x == "Default Bracketing" | x == "Weighted Bracketing") {
        return(rv$Area)
      } else if (x == "Drift Correction") {
        return(rv$dc_area)
      } else if (x == "IS Correction") {
        return(rv$IS_ratio)
      }
    }, USE.NAMES = F)

    for (i in 1:nrow(selection)) {
      sel <- as.vector(unlist(selection[i, 2:ncol(selection)]))
      col_used <- colnames(selection[, 2:ncol(selection)])[sel]

      if (all(!sel)) {
        cals_temp <- NULL

        rv$selection_cals_table[[selection$Class[i]]] <- cals_temp
      } else {
        cals_temp <- df[df$Classification %in% col_used, ]

        cals_temp$Concentration <- sapply(cals_temp$Sample.Name,
          FUN = function(x, label, concentration) {
            for (i in 1:length(label)) {
              if (grepl(label[i], x)) {
                return(concentration[i])
              }
            }
          }, label = rv$setup_cal$Cal.Name,
          concentration = rv$setup_cal$Concentration
        )

        colnames(cals_temp)[colnames(cals_temp) == input$Compound] <- "PeakArea"

        cals_temp$Concentration <- unlist(cals_temp$Concentration)

        cals_temp$Concentration[is.null(cals_temp$Concentration)] <- 0

        #rv$force_0 <- ifelse(input$weight_method %in% c("1/x force 0", "1/y force 0"), T, F)
        cals_temp <- add_weights(weight_method = input$weight_method, data = cals_temp)
        #cals_temp$weight <- get_weights(weight_method = input$weight_method, data = cals_temp)

        if (is.null(cals_temp$used)) {
          cals_temp$used <- ifelse(cals_temp$PeakArea == 0, FALSE, TRUE)
          #cals_temp$used <- rep(TRUE, nrow(cals_temp))
        }
        cals_temp <- cals_temp[cals_temp$Concentration != 0, ]

        rv$selection_cals_table[[selection$Class[i]]] <- cals_temp
      }

      #print(str(rv$selection_cals_table))

    }

  })
  quantitate <- function(input, rv){

    all_present <- all(rv$setup_cal$Cal.Name %in% rv$data$Sample.Name)
    req(all_present)
    req(input$file1)

    

    selection_cals_table <- update_weights_ind(input, rv)

    

    df <- data.frame(Sample.Name = rv$data$Sample.Name, Classification = rv$Classification_temp)

    df$PeakArea <- sapply(input$quantitation_method, FUN = function(method) {
      if (method == "Custom Bracketing" | method == "Default Bracketing" | method == "Weighted Bracketing") {
        return(rv$Area)
      } else if (method == "Drift Correction") {
        return(rv$dc_area)
      } else if (method == "IS Correction") {
        return(rv$IS_ratio)
      }
    }, USE.NAMES = F)

    mods <- list()
    preds <- list()
    cals <- list()

    
    for (block in unique(rv$Classification_temp)) {
      block_temp <- df[df$Classification == block, ]
      cals_temp <- selection_cals_table[[block]]

      mods[[block]] <- quantyFey(data = cals_temp, xfunction = rv$regression_model, weight_method = input$weight_method)
 

      block_temp$pred <- predict_concentrations(data = block_temp, model = mods[[block]], method = rv$regression_model)


      block_temp$cals_used <- rep(paste(unique(cals_temp$Classification), collapse = ", "))

      preds[[block]] <- block_temp
      cals[[block]] <- cals_temp

      for (i in 1:length(preds)) {
        if (i == 1) {
          results <- preds[[i]]
        } else {
          results <- rbind(results, preds[[i]])
        }
      }
    }

    results <- results[order(as.numeric(row.names(results))), ]

    return(list(results, cals, mods))
  }
areas <- function(input, reactive){
  areas <- sapply(input$quantitation_method, FUN = function(x) {
      if (x == "Custom Bracketing" | x == "Default Bracketing" | x == "Weighted Bracketing") {
        return(rv$Area)
      } else if (x == "IS Correction") {
        return(rv$IS_ratio)
      } else if (x == "Drift Correction") {
        return(rv$dc_area)
      }
    }, USE.NAMES = F)

  return(areas)

}

update_select_plots <- function(rv) {
    # Updates the plots from the reactive values

    rv$p_RT <- get_plotly_RT(input, rv)
    rv$p_quan_qual <- get_plotly_qual_quant(input, rv)
    rv$p_blank <- get_plotly_blank(input, rv)
    #rv$p_IS_analysis <- get_isboxplot
    rv$p_dc <- get_plotly_dc(input, rv)[[1]]
}

  data <- function(input, rv){
    req(input$file1)

    df <- rv$data

    return(df)
  }

    
  update_dc_data <- function(input, rv){
    req(input$file1)
    
    input_not_blank <- input$Compound != ""
    req(input_not_blank)

    input_numeric <- all(sapply(rv$data[, input$Compound], is.numeric))
    req(input_numeric)


    df <- data.frame(Sample.Name = rv$data$Sample.Name, PeakArea = rv$data[, input$Compound])

    df$inj <- 1:nrow(df) %>% as.numeric()
    pd <- rv$data$Sample.Type

    if (!all(is.na(df$PeakArea))) {
      df_cals <- df

      cal_dc <- input$files_for_correction

      df_cals <- df_cals[df_cals$Sample.Name == cal_dc, ]
      cal_cd <- input$files_for_correction
      suppressWarnings(
        {rm(mod_temp)}
      )
      

      tryCatch(
        {
          suppressWarnings({
            if (input$model_drift == "lm") {
            mod_temp <- lm(formula = PeakArea ~ inj, data = df_cals)
          } else if (input$model_drift == "loess") {
            mod_temp <- loess(formula = PeakArea ~ inj, data = df_cals, span = input$span_width)
          } else if(input$model_drift == "spline") {
            
            if (!"splines" %in% .packages(all.available = TRUE)) {
              install.packages("splines")
            }
            library(splines)

            #print(ns(inj, df = input$spline_df))
            #print(df_cals)

            mod_temp <- lm(formula = PeakArea ~ ns(inj, df = input$spline_df), data = df_cals)
            
          }

          df$pred.PeakArea <- predict(mod_temp, df)

          
      req(mod_temp)

      mean_temp <- mean(df$pred.PeakArea, na.rm = T)
      start_pa <- na.omit(df)$pred.PeakArea[1]
      end_pa <- na.omit(df)$pred.PeakArea[length(na.omit(df)$pred.PeakArea)]

      start <- T
      for (i in 1:nrow(df)) {
        if (is.na(df$pred.PeakArea[i])) {
          if (start) {
            df$pred.PeakArea[i] <- start_pa
          } else {
            df$pred.PeakArea[i] <- end_pa
          }
        } else {
          start <- F
        }
      }
      df$corrected.PeakArea <- df$PeakArea / df$pred.PeakArea * mean_temp

      rv$drift_corrected_data_temp <- df

      rv$dc_area <- df$corrected.PeakArea

          })
          
        },
        error = function(e) {

          df$pred.PeakArea <- rep(1, nrow(df))
          rv$drift_corrected_data_temp <- df
          rv$dc_area <- df$PeakArea

          stop("Error: Model could not be generated, data is not valid. Adjust model before using Drift Correction!")

        }
      )

    }
  }

get_peak_area_by_method <- function(method, rv) {
  switch(method,
         "Custom Bracketing" = rv$Area,
         "Default Bracketing" = rv$Area,
         "Weighted Bracketing" = rv$Area,
         "IS Correction" = rv$IS_ratio,
         "Drift Correction" = rv$dc_area,
         rep(NA, nrow(rv$data))
  )
}

read_file_safe = function(file_name, seps = c(":", ";", ",", " ", "\t"), allowed_formats = c("csv", "txt", "xlsx")) {

        # Extract the file extension
        ext <- tools::file_ext(file_name)

        # Check if the file format is allowed
        if (!ext %in% allowed_formats) {
          shinyalert("Error", paste("The file format is invalid. Allowed formats are:", paste(allowed_formats, collapse = ", ")), type = "error")
          return(NULL)
        }


        
        # Read the file based on its format
        if (ext == "xlsx") {
          if (!requireNamespace("readxl", quietly = TRUE)) {
            shinyalert("Error", "The 'readxl' package is required to read Excel files. Please install it.", type = "error")
            return(NULL)
          }
          data <- readxl::read_excel(file_name) %>% as.data.frame()
          #print(data)
        } else {
          valid_sep <- NULL
          df_temp <- NULL

          text <- readLines(file_name, warn = F)

          sep_counts <- sapply(seps, function(sep) {
          tryCatch({

            n <- sum(sapply(text, FUN = function(x){
            n <- str_count(x, pattern = sep)

            return(n)

           }))

              return(n)
            }, error = function(e) {
              0
            })
          })
          
        valid_sep <- seps[which.max(sep_counts)]
        
        if(is.null(valid_sep)){
          error("No valid separator was found. Make sure, only ',', ';', ':', ' ', '\t' are used.")
        }

        data <- read.csv(file_name, sep = valid_sep)
        }

        colnames(data) <- gsub("[^a-zA-Z0-9._]", ".", colnames(data))
        
      
      return(data)

      
}



  get_plotly_blank <- function(input, rv){
    req(input$file1)

    df <- data.frame(`Peak.Area` = get_peak_area_by_method(method = "Default Bracketing", rv), Classification = rv$Classification_temp, Sample.Name = rv$data$Sample.Name)

    pd <- rv$data$Sample.Type

    df[is.na(df)] <- 0

    mean_blank <- sapply(as.numeric(df$Peak.Area)[pd == "Blank"], FUN = mean, na.rm = T)

    max_sample <- sapply(as.numeric(df$Peak.Area)[pd == "Sample"], FUN = max, na.rm = T)

    df$blank_ratio <- df$Peak.Area / ifelse(mean_blank == 0 | is.na(mean_blank), 1, mean_blank)

    df$Sample.Type <- pd

    df <- subset(df, Sample.Type == "Sample" | Sample.Type == "Blank")

    df <- df[, c("Sample.Name", "Peak.Area", "blank_ratio", "Sample.Type")]

    p <- ggplot(df, aes(x = Sample.Type, y = blank_ratio, fill = Sample.Type)) +
      geom_boxplot(width = 0.5) +
      theme_pubclean(base_size = 17) + scale_fill_manual(values = c("Cal" = "navy", "Sample" = "lightblue3", "Blank" = "grey70", "QC" = "purple3")) +
        labs(x = "Sample Type", y = "Blank Ratio", fill = "Sample Type", color = "Sample Type")

    return(p)
  }

  get_plotly_qual_quant <- function(input, rv){
    req(input$file1)
    req(rv$df_QQ)

    df_QQ <- rv$df_QQ
    
    quant_name <- input$Compound
    qual_name <- df_QQ$Qual.Name[which(df_QQ$Quant.Name == quant_name)]

    df <- data(input, rv)
    pd <- rv$data$Sample.Type


    
        df <- df[,c("Sample.Name", "Classification", "Sample.Type", quant_name, qual_name)]
        df$Sequence.Position <- 1:nrow(df) %>% as.numeric()
    
    


    if (!exists("df")) {
        return(ggplot() + theme_void())
    } else {
        df[is.na(df)] <- 0
        

        df$ion_ratios <- as.numeric(df[[qual_name]]) / as.numeric(df[[quant_name]])
        df$ion_ratios[is.na(df$ion_ratios) | is.infinite(df$ion_ratios)] <- 0

        ion_ratios_standards <- median(df$ion_ratios[df$Sample.Type == "Cal" & df$ion_ratios != 0], na.rm = T)

        df_ss <- subset(df, Sample.Type %in% input$quan_qual_groups)
        



        if (nrow(df_ss) == 0) {
            p <- ggplot() + 
          scale_y_continuous(limits = c(0, 1.5)) + 
          labs(x = "Sample Type", y = "Ion Ratios") + 
          theme_pubclean(base_size = 17)

        } else {
          df_ss$Sequence.Position <- paste0("Sequence Position: ", df_ss$Sequence.Position)

        suppressWarnings({
        p <- ggplot(df_ss, aes(x = Sample.Type, y = ion_ratios, fill = Sample.Type, label = Sample.Name, text = Sequence.Position)) + geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) + 
        geom_jitter(width = 0.2, size = 2) + theme_pubclean(base_size = 17) + 
        geom_hline(yintercept = ion_ratios_standards) + geom_hline(yintercept = (ion_ratios_standards * 1.5), linetype = "dotted") + 
        geom_hline(yintercept = (ion_ratios_standards * 0.5), linetype = "dotted") + scale_fill_manual(values = c("Cal" = "navy", "Sample" = "lightblue3", "Blank" = "grey70", "QC" = "purple3")) +
        labs(x = "Sample Type", y = "Ion Ratio", fill = "Sample Type", color = "Sample Type", text = "Sequence Position")

        })
    }
    return(p)
    }

    
  }

  get_plotly_RT <- function(input, rv){
    req(input$file_RT)

    df <- data_RT(input, rv)


    df <- df[, c("Sample.Name", "Sample.Type", input$Compound)]
    df$Sequence.Position <- 1:nrow(df) %>% as.numeric()
    
    df[df == "N/A"] <- NA

    df[[input$Compound]] <- as.numeric(df[[input$Compound]])

    means_cal <- median(df[[input$Compound]][rv$data$Sample.Type == "Cal"], na.rm = T)
    
    df$Sample.Type <- rv$data$Sample.Type
    df_ss <- df[rv$data$Sample.Type %in% input$RT_groups,]
    
    colnames(df_ss)[which(colnames(df_ss) == input$Compound)] <- "Retention.Time"
    

    ylab <- if(is.null(rt_unit)) {
      "Retention Time"
    } else {
      paste("Retention Time [", rt_unit, "]", sep = "")
    }

    if (nrow(df_ss) == 0) {
      suppressWarnings({
        p <- ggplot() + 
        scale_y_continuous(limits = c(0, 1.5)) + 
        labs(x = "Sample.Type", y = ylab) + 
        theme_pubclean(base_size = 17)
      })
    } else {
      suppressWarnings({
        df_ss$Sequence.Position <- paste0("Sequence Position: ", df_ss$Sequence.Position)


        p <- ggplot(df_ss, aes(y = `Retention.Time`, x = Sample.Type, label = Sample.Name, text = Sequence.Position)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5, mapping = aes(fill = Sample.Type)) +
        geom_jitter(size = 2, width = 0.2, aes(fill = Sample.Type), color = "black") +
        geom_hline(yintercept = (means_cal + 0.05), linetype = "dotted", color = "black") +
        geom_hline(yintercept = (means_cal - 0.05), linetype = "dotted", color = "black") +
        geom_hline(yintercept = means_cal, color = "black", linetype = "solid") +
        labs(
          x = "Sample Type",
          y = ylab,
          fill = "Sample Type",
          color = "Sample Type"
        ) +
        theme_pubclean(base_size = 17) + scale_fill_manual(values = c("Cal" = "navy", "Sample" = "lightblue3", "Blank" = "grey70", "QC" = "purple3"))
        

      })
 
    }

    return(p)
    }
    data_RT <- function(input, rv){
    req(input$file_RT)

    df <- rv$data_RT

    df[df == "N/A"] <- NA

    df <- as.list(df)

    df <- lapply(df, FUN = function(x) {
      if (any(is.na(as.numeric(x)))) {
        return(x)
      } else {
        return(as.numeric(x))
      }
    })

    df <- as.data.frame(df)

    return(df)
  }


  get_plotly_dc <- function(input, rv){
    req(input$file1)

    tryCatch({
      update_dc_data(input, rv)
    }, error = function(e){ return(e)})
    
    cal_dc <- input$files_for_correction

    df <- data.frame(Sample.Name = rv$data$Sample.Name, PeakArea = rv$Area, corrected.PeakArea = rv$dc_area)

    df$inj <- 1:nrow(df) %>% as.numeric()
    pd <- rv$data$Sample.Type

    df$Sample.Type <- rv$data$Sample.Type

    df$Sample.Type[df$Sample.Name == cal_dc] <- "Model"



    suppressWarnings({
         p1 <- ggplot(data = df, aes(x = inj, y = PeakArea, label = Sample.Name, fill = Sample.Type))+ geom_bar(stat = "identity", col = "black", width = 0.7) +
      scale_fill_manual(values = c("Model" = "red3", "Sample" = "lightblue3", "Cal" = "navy", "Blank" = "grey70", "QC" = "purple3")) +
      theme_pubclean(base_size = 17) +
      theme(legend.position = "bottom") +
      labs(x = NULL, y = "Peak Area") 
      #theme(legend.position = "bottom")+

      if(input$model_drift == "loess"){
        p1 <- p1 + stat_smooth(data = df[df$Sample.Name == cal_dc, ], mapping = aes(y = PeakArea, x = inj), formula = y ~ x, method = input$model_drift, span = input$span_width, col = "red3", se = F)
      } else if(input$model_drift == "lm") {
        p1 <- p1 + stat_smooth(data = df[df$Sample.Name == cal_dc, ], mapping = aes(y = PeakArea, x = inj), formula = y ~ x, method = input$model_drift, col = "red3", se = F)
      } else if (input$model_drift == "spline") {
  library(splines)
  
  
  df_sub <- df[df$Sample.Name == cal_dc, ]
  
  # Fit spline model
  spline_mod <- lm(PeakArea ~ ns(inj, df = input$spline_df), data = df_sub)
  
  # Create new data for prediction (for smooth line)
  inj_seq <- seq(min(df_sub$inj, na.rm = TRUE), max(df_sub$inj, na.rm = TRUE), length.out = 200)
  pred_df <- data.frame(inj = inj_seq)
  pred_df$PeakArea <- predict(spline_mod, newdata = pred_df)
  
  # Add fitted spline line
  p1 <- p1 + geom_line(
    data = pred_df,
    aes(x = inj, y = PeakArea),
    col = "red3"
  )
}
      


    df$corrected.PeakArea <- rv$dc_area


    p2 <- ggplot(data = df, aes(x = inj, y = corrected.PeakArea, label = Sample.Name, fill = Sample.Type))+ geom_bar(stat = "identity", col = "black", width = 0.7) +
      scale_fill_manual(values = c("Model" = "red3", "Sample" = "lightblue3", "Cal" = "navy", "Blank" = "grey70", "QC" = "purple3")) +
      theme_pubclean(base_size = 17) +
      labs(x = "Number of injection", y = "corrected Peak Area") +
      theme(legend.position = "bottom")+
      stat_smooth(data = df[df$Sample.Name == cal_dc, ], mapping = aes(y = corrected.PeakArea, x = inj), formula = y ~ x, method = "lm", col = "red3", se = F)
    })
 
    p <- list(p1, p2)

    rv$plot_dc <- p

    return(p)
  } 

  get_plot_IS <- function(input, rv){
    req(input$file1)

    # Check for valid column name
    if (!(input$Compound %in% colnames(rv$data))) {
      stop("Invalid column name in input$Compound")
    }

    # Extract data
    #IS_Areas <- ifelse(length(rv$IS_compound_data_for_correction) == 0, rep(1, nrow(rv$data)), as.numeric(rv$IS_compound_data_for_correction))
    IS_Areas <- rv$IS_compound_data_for_correction
    Areas <- as.numeric(rv$data[, c(input$Compound)])
    IS_ratios <- rv$IS_ratio

    


    # Check for matching lengths
    if (!all(length(IS_Areas) == length(Areas), length(Areas) == length(IS_ratios))) {
      stop("Vector lengths do not match")
    }

    # Create data frame
    df <- data.frame(Sample.Name = rv$data$Sample.Name, IS_Area = IS_Areas, Areas = Areas, IS_ratios = IS_ratios, Sample.Type = rv$data$Sample.Type)
    df$inj <- 1:nrow(df)

    
    # Create plots
    suppressWarnings({
      p1 <- ggplot(df)+
    geom_bar(aes(y =Areas, x = inj, label = Sample.Name, fill = Sample.Type), stat = "identity", col = "black") +
    geom_point(aes(x = inj, y = IS_Area, label = Sample.Name), col = "red3") +
    theme_pubclean(base_size = 17) +
    scale_fill_manual(values = c("Sample" = "lightblue3", "Cal" = "navy", "Blank" = "grey70", "QC" = "purple3"))

 

    p2 <- ggplot(data = df, aes(x = inj, y = IS_ratios, label = Sample.Name, fill = Sample.Type)) +
      geom_bar(stat = "identity", col = "black") +
      theme_pubclean(base_size = 17) +
      labs(y = "IS Ratios", x = "Number of injection")+
      scale_fill_manual(values = c("Sample" = "lightblue3", "Cal" = "navy", "Blank" = "grey70", "QC" = "purple3"))
    })
    

    
    if(input$log_scale_IS){
      suppressWarnings({
        p1 <- p1 + scale_y_log10()
      })
    }

    p <- list(p1, p2)

    rv$IS_plot <- p

    return(p)
  }


  optimize_model_metrics <- function(input, rv, session){
    req(input$file1)

    cal_tables <- rv$selection_cals_table

    areas <- get_peak_area_by_method(input$quantitation_method, rv)

    message("Step 1: Retrieved calibration tables and areas.")

    if(all(rv$setup_cal$Cal.Name %in% rv$data$Sample.Name)){

      for(i in 1:length(cal_tables)){
        cal_temp <- cal_tables[[i]]

        areas_samples <- areas[rv$data$Sample.Type == "Sample"]

        mod_lin <- quantyFey(data = cal_temp, xfunction = "linear", weight_method = input$weight_method)
        mod_quad <- quantyFey(data = cal_temp, xfunction = "quadratic", weight_method = input$weight_method)
        
        loftest <- anova(mod_quad, mod_lin)

        better_model <- ifelse(loftest$`Pr(>F)`[2] <= 0.05, "quad", "lin")

        message("Step 2: Determined better model (linear or quadratic) for calibration data.")
      }
      
      cal_n <- 2
      try({
        while (better_model == "quad") {
          max_sample <- max(areas_samples)
          min_concentration <- min(cal_temp$PeakArea[which(cal_temp$Concentration == unique(cal_temp$Concentration)[order(unique(cal_temp$Concentration), decreasing = T)[cal_n]])])

          if (max_sample < min_concentration) {
            cal_temp$used[which(cal_temp$Concentration == unique(cal_temp$Concentration)[order(unique(cal_temp$Concentration), decreasing = T)[(cal_n - 1)]])] <- FALSE
          }

          mod_lin <- quantyFey(data = cal_temp, xfunction = "linear", weight_method = input$weight_method)
          mod_quad <- quantyFey(data = cal_temp, xfunction = "quadratic", weight_method = input$weight_method)

          loftest <- anova(mod_quad, mod_lin)

          better_model <- ifelse(loftest$`Pr(>F)`[2] <= 0.05, "quad", "lin")

          cal_n <- cal_n + 1

          if (cal_n == 4) {
            break
          }
        }
        message("Step 3: Optimized calibration model by iteratively testing quadratic and linear fits.")
      })

      if(better_model == "quad"){
        model_temp <- mod_quad
      } else {
        model_temp <- mod_lin
      }

      cal_temp$pred <- predict_concentrations(data = cal_temp, model = model_temp, method = ifelse(better_model == "quad", "quadratic", "linear"))
      cal_temp$accuracy <- cal_temp$pred / cal_temp$Concentration * 100

      message("Step 4: Predicted concentrations and calculated accuracy for calibration data.")

      good_or_not <- sapply(unique(cal_temp$Sample.Name), FUN = function(x) {
        mean <- cal_temp$accuracy[cal_temp$Sample.Name == x] %>% mean()
        sd <- ifelse(length(cal_temp$accuracy[cal_temp$Sample.Name == x]) == 1, 0, cal_temp$accuracy[cal_temp$Sample.Name == x] %>% sd())
        rsd <- sd / mean
        return(ifelse(between(mean, 70, 130) & rsd < 0.5, TRUE, FALSE))
      })

      df_gon <- data.frame(Sample.Name = names(good_or_not), good_or_not = good_or_not)

      cal_temp_sorted <- cal_temp[order(cal_temp$Concentration, decreasing = F),]

      for(level in unique(cal_temp$Concentration)){
        sample_to_predict <- areas[rv$data$Sample.Type == "Sample"]
        level_name <- cal_temp$Sample.Name[cal_temp$Concentration == level] %>% unique()
        sample_pred <- predict_concentrations(data = sample_to_predict, model = model_temp, method = ifelse(better_model == "quad", "quadratic", "linear"))
        min_sample <- min(sample_pred, na.rm = T)
        next_level <- cal_temp_sorted$Concentration[cal_temp_sorted$Concentration > level][1]

        if(min_sample > next_level & !df_gon$good_or_not[df_gon$Sample.Name == level_name]){
          cal_temp$used[cal_temp$Sample.Name == cal_temp_sorted$Sample.Name[cal_temp_sorted$Concentration == level]] <- FALSE
        } else {
          break
        }
      }

      message("Step 5: Adjusted calibration levels based on sample predictions and accuracy.")

      rv$LLOQs <- list()


      good_or_not[is.na(good_or_not)] <- FALSE

      for (block in unique(rv$Classification_temp)) {
        if(!all(good_or_not)){
          rv$LLOQs[[block]] <- min(unique(cal_temp$Concentration[cal_temp$Sample.Name %in% names(good_or_not)[good_or_not]]))
        } else {
          rv$LLOQs[[block]] <- min(concentrations(rv))
        }
      }

      

      if(is.finite(rv$LLOQs[[input$Block]])){
        updateNumericInput(session, inputId = "LOQ", label = "Limit of Quantification", value = rv$LLOQs[[input$Block]], step = min(concentrations(rv)), min = min(concentrations(rv)), max = max(concentrations(rv)))
        message("Step 6: Updated Limit of Quantification (LOQ) based on calibration data.")
      } else {
        showNotification(
          "LLOQ was not set, due to bad accuracy of the model! Please revise and adjust your regression model.",
          type = "error",
          duration = 5
        )
      }

      cals_to_remove <- cal_temp$Sample.Name[!cal_temp$used]

      if (better_model == "quad") {
        updateSelectInput(session, inputId = "regression_model", choices = c("linear", "quadratic"), selected = "quadratic")
      } else {
        updateSelectInput(session, inputId = "regression_model", choices = c("linear", "quadratic"), selected = "linear")
      }

      rv$selection_cals_table <- lapply(rv$selection_cals_table, FUN = function(x) {
        df_temp <- x
        df_temp$used[df_temp$Sample.Name %in% cals_to_remove] <- FALSE
        return(df_temp)
      })

      message("Step 7: Updated calibration tables and regression model selection.")
    }   
  }


reset_app <- function(session, rv) {
  # File inputs can't be reset directly, can hide/show or reload page if needed
  shinyjs::reset("file1")
  shinyjs::reset("file_RT")

  updateCheckboxInput(session, "change_project_name", value = FALSE)
  updateTextInput(session, "project_name", value = paste0("Results_", format(Sys.Date(), "%Y%m%d")))
  
  updateSelectInput(session, "mode", selected = Template_name)
  updateCheckboxInput(session, "change_patterns", value = TRUE)
  updateTextInput(session, "quant_indicator", value = quant_pattern)
  updateTextInput(session, "qual_indicator", value = qual_pattern)
  updateTextInput(session, "IS_indicator", value = IS_pattern)

  updateSelectInput(session, "Compound", selected = "")
  updateSelectInput(session, "Compound_IS", selected = "")
  updateTextInput(session, "Comment", value = "")
  updateCheckboxInput(session, "generate_report", value = FALSE)

  updateCheckboxGroupInput(session, "RT_groups", selected = character(0))
  updateCheckboxGroupInput(session, "quan_qual_groups", selected = character(0))
  
  updateRadioButtons(session, "model_drift", selected = "lm")
  updateSelectInput(session, "files_for_correction", selected = NULL)
  updateSelectInput(session, "file_for_bracketing", selected = NULL)
  updateNumericInput(session, "span_width", value = 0.75)

  updateCheckboxInput(session, "use_correction", value = FALSE)
  updateCheckboxInput(session, "log_scale_IS", value = FALSE)

  updateSelectInput(session, "regression_model", selected = "linear")
  updateNumericInput(session, "LOQ", value = NULL)
  updateSelectInput(session, "weight_method", selected = "1/x")
  updateSelectInput(session, "quantitation_method", selected = "IS Correction")
  updateCheckboxInput(session, "show_samples", value = TRUE)
  updateSelectInput(session, "Block", selected = NULL)
  updateCheckboxInput(session, "log_scale", value = FALSE)

    rv$orig = NULL
    rv$data = NULL
    rv$orig_RT = NULL
    rv$data_RT = NULL
    rv$bracketing_table = NULL
    rv$selection_table = NULL
    rv$selection_table_bracketing = NULL
    rv$selection_cals_table = NULL
    rv$quantitation_data = NULL
    rv$drift_corrected_data_temp = NULL
    rv$quantitate_temp = NULL
    rv$acc_table = NULL
    rv$results = NULL
    rv$LLOQs = NULL
    rv$settings_used = list()
    rv$compounds_analyzed = c()
    rv$regression_model = "linear"
    rv$setup_cal = NULL
    rv$correction_factors = NULL
    rv$backup_data = NULL
    rv$corr_df = NULL
    rv$norm_data = NULL
    rv$Area_backup = NULL
    rv$area_for_quant = NULL
    rv$Classification_temp = NULL
    rv$Area = NULL
    rv$IS_ratio = NULL
    rv$dc_area = NULL
    rv$IS_table = NULL
    rv$IS_compound_data_for_correction = NULL
    rv$IS_plot = NULL
    rv$df_QQ = NULL
    rv$force_0 = F

        ## Plots
    rv$p_blank = NULL
    rv$p_quan_qual = NULL
    rv$p_drift_correction = NULL
    rv$p_quantitate = NULL
    rv$p_IS_analysis = NULL
    rv$p_RT = NULL
    rv$templates = template_list

}


get_plotly_acc <- function(rv) {

  req(rv$acc_table)
  # Check if the required data is available
  if (is.null(rv$acc_table) || is.null(rv$setup_cal$Cal.Name)) {
    stop("Error: Required data (acc_table or setup_cal$Cal.Name) is missing.")
  }

  df <- rv$acc_table

  # Check if the required columns are present in the data
  required_columns <- c("Sample.Name", "Accuracy", "Classification", "used")
  missing_columns <- setdiff(required_columns, colnames(df))
  if (length(missing_columns) > 0) {
    stop(paste("Error: The following required columns are missing in acc_table:", paste(missing_columns, collapse = ", ")))
  }

  # Ensure Sample.Name is a factor with levels from setup_cal$Cal.Name
  df$Sample.Name <- factor(df$Sample.Name, levels = unique(rv$setup_cal$Cal.Name))

  # Add accuracy classification
  df$accuracy <- factor(ifelse(between(df$Accuracy, 70, 130), "70 - 130 %", "< 70 & > 130 %"))

  # Ensure 'used' column is a factor
  if (!"used" %in% colnames(df)) {
    stop("Error: 'used' column is missing in acc_table.")
  }
  df$used <- factor(df$used, levels = c(TRUE, FALSE), labels = c("used", "not used"))

  # Generate the plot
  suppressWarnings({
    tryCatch({
      p <- ggplot(df, aes(y = Accuracy, x = Sample.Name, fill = accuracy, label = Classification, alpha = used)) +
        geom_point() +
        geom_hline(yintercept = 130, linetype = "dotted") +
        geom_hline(yintercept = 70, linetype = "dotted") +
        scale_alpha_manual(values = c("used" = 1, "not used" = 0.3)) +
        labs(fill = NULL, used = NULL, y = "Accuracy [%]", x = "Sample Name") +
        theme_pubclean(base_size = 17)

      # Adjust fill colors based on accuracy classification
      if (all(df$accuracy == "< 70 & > 130 %")) {
        p <- p + scale_fill_manual(values = c("red3"))
      } else {
        p <- p + scale_fill_manual(values = c("70 - 130 %" = "green3", "< 70 & > 130 %" = "red3"))
      }

      return(p)
    }, error = function(e) {
      stop(paste("Error generating plot:", e$message))
    })
  })
}



remove_point_from_click <- function(info, input, rv) {
  tryCatch({
    if (!is.null(info)) {
      if (isTRUE(input$log_scale)) {
        info$y <- 10^(info$y)
      }
      isolate({
        blocks <- input$Block
        validate(need(blocks %in% names(rv$selection_cals_table), "Selected block not found in data."))
        
        temp <- rv$selection_cals_table[[blocks]]
        
        for (i in seq_along(rv$selection_cals_table)) {
          if (all(unique(rv$selection_cals_table[[i]]$Classification) == unique(temp$Classification))) {
            blocks <- c(blocks, names(rv$selection_cals_table)[i])
          }
        }
        
        idx <- which(temp$PeakArea == info$y & temp$Concentration == info$x)
        if (length(idx) > 0) {
          temp[idx, "used"] <- !temp[idx, "used"]
          for (i in unique(blocks)) {
            rv$selection_cals_table[[i]] <- temp
          }
        }
      })
    }
  }, error = function(e) {
    cat("Error in remove_point_from_click:", e$message, "\n")
  })
}

get_plotly_quant <- function(input, rv) {
  req(input$file1)

  q_result <- quantitate(input, rv)
  quant_data <- q_result[[1]]
  mods <- q_result[[3]]
  mod <- mods[[input$Block]]

  df <- rv$selection_cals_table[[input$Block]]
  quant_data_block <- quant_data[quant_data$Classification == input$Block, ]

  p <- generate_quantitate_ggplot(data = df, regression_model = mod, regression_method = rv$regression_model)

  if (input$show_samples) {
    suppressWarnings({
      p <- p + geom_point(
        data = quant_data_block,
        mapping = aes(x = pred, y = PeakArea, label = Sample.Name),  # removed label
        color = "orange"
      )
    })
  }

  ylab <- if(input$quantitation_method == "IS Correction") {
    "IS Ratio"
  } else if (is.null(int_unit)) {
    "Peak Area"
  } else {
    paste("Peak Area [", int_unit, "]", sep = "")
  }

  xlab <- if(is.null(conc_unit)) {
    "Concentration"
  } else {
    paste("Concentration [", conc_unit, "]", sep = "")
  }

  p <- p + {
    if (input$log_scale) {
      scale_y_log10() + labs(x = xlab, y = "Peak Area (log)")
    } else {
      labs(x = xlab, y = ylab)
    }
  }

  

  return(p)
}


create_classification_min_list <- function(rv) {
  tryCatch({
    # Check necessary components exist
    req(rv)
    req(rv$Classification_temp)
    
    concs <- tryCatch({
      concentrations(rv)
    }, error = function(e) {
      warning("Error getting concentrations:", e$message)
      return(NA)
    })
    
    min_conc <- suppressWarnings(min(concs, na.rm = TRUE))
    if (is.infinite(min_conc) || is.na(min_conc)) {
      min_conc <- 0
    }

    # Find unique classifications
    classifications <- unique(rv$Classification_temp)
    validate(need(length(classifications) > 0, "No classifications found."))

    # Build named list
    classification_min_list <- setNames(
      as.list(rep(min_conc, length(classifications))),
      classifications
    )

    
    return(classification_min_list)
    
  }, error = function(e) {
    cat("Error creating classification min list:", e$message, "\n")
    showNotification(
      paste("Error creating classification min list:", e$message),
      type = "error",
      duration = 8
    )
    return(NULL)
  })
}

apply_current_layout <- function(plot, current_layout){
  p <- plot
  
  if(!is.null(current_layout)){
    layout_vals <- isolate(current_layout)
    
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
    
    y2_range <- if (all(c("yaxis2.range[0]", "yaxis2.range[1]") %in% names(layout_vals))) {
      as.numeric(c(layout_vals[["yaxis2.range[0]"]], layout_vals[["yaxis2.range[1]"]]))
    } else {
      NULL
    }
    
    # Now apply layout
    p <- layout(
      p,
      xaxis = if (!is.null(x_range)) list(range = x_range, autotick = TRUE, tickmode = "auto", fixedrange = FALSE, autorange = FALSE) else list(autorange = TRUE, fixedrange = FALSE),
      yaxis = if (!is.null(y_range)) list(range = y_range, autotick = TRUE, tickmode = "auto", fixedrange = FALSE, autorange = FALSE) else list(autorange = TRUE, fixedrange = FALSE),
      yaxis2 = if (!is.null(y2_range)) list(range = y2_range, autotick = TRUE, tickmode = "auto", fixedrange = FALSE, autorange = FALSE) else list(autorange = TRUE, fixedrange = FALSE)
    )
    
  } else {
    # No current layout -- default autorange for everything
    p <- layout(
      p,
      xaxis = list(autorange = TRUE, autotick = TRUE, tickmode = "auto", fixedrange = FALSE),
      yaxis = list(autorange = TRUE, autotick = TRUE, tickmode = "auto", fixedrange = FALSE),
      yaxis2 = list(autorange = TRUE, autotick = TRUE, tickmode = "auto", fixedrange = FALSE)
    )
  }
  
  return(p)
}

get_acc_table <- function(input, rv)  {
  req(input$Block)
  req(rv$selection_cals_table)
  req(rv$setup_cal)

  sel_cal_table <- rv$selection_cals_table[[input$Block]]

  if (is.null(sel_cal_table)) {
    return(NULL)
  }



  quantitate_object <- quantitate(input, rv)
  mod_temp <- quantitate_object[[3]][[input$Block]]
  sel_cal_table$pred <- predict_concentrations(data = sel_cal_table, model = mod_temp, method = rv$regression_model)
  sel_cal_table$Accuracy <- sel_cal_table$pred / sel_cal_table$Concentration * 100


suppressWarnings({
  if(any(pd_temp(rv) == "Cal" & rv$Classification_temp == input$Block & rv$data$Sample.Name %in% rv$setup_cal$Cal.Name[rv$setup_cal$Concentration != 0])){
  Standard_QC <- quantitate_object[[1]][pd_temp(rv) == "Cal" & rv$Classification_temp == input$Block, ]

   Standard_QC <- Standard_QC[Standard_QC$Sample.Name %in% rv$setup_cal$Cal.Name[rv$setup_cal$Concentration != 0], ]
    conc_temp <- concentrations(rv)[labels(rv) == Standard_QC$Sample.Name]
    sel_cal_table <- rbind(sel_cal_table, data.frame(Sample.Name = Standard_QC$Sample.Name, Classification = Standard_QC$Classification, PeakArea  = Standard_QC$PeakArea, Concentration = conc_temp, weights = 0, used = FALSE, pred = Standard_QC$pred, Accuracy = Standard_QC$pred / conc_temp * 100))

}

})

  

  rv$acc_table <- sel_cal_table

  sel_cal_table$pred <- signif(sel_cal_table$pred, digits = 3)

  if(input$quantitation_method == "IS Correction"){
    sel_cal_table$PeakArea <- signif(sel_cal_table$PeakArea, digits = 3)
  } else {
    sel_cal_table$PeakArea <- round(sel_cal_table$PeakArea, digits = 0)
  }
  #sel_cal_table$PeakArea <- ifelse(input$quantitation_method == "IS correction", signif(sel_cal_table$PeakArea, 3),round(sel_cal_table$PeakArea, digits = 0))
  sel_cal_table$Accuracy <- round(sel_cal_table$Accuracy, digits = 2)
  sel_cal_table$weights <- ifelse(sel_cal_table$weights > 9999, 
                                  round(sel_cal_table$weights, digits = nchar(as.character(floor(sel_cal_table$weights)))), 
                                  round(sel_cal_table$weights, digits = 4))
  sel_cal_table$used <- ifelse(sel_cal_table$used, "\u2713", "")


  return(sel_cal_table)




}


build_quant_results <- function(res_df, input, rv, quant, res_list, cpt_name){
  
    if(input$quantitation_method == "Custom Bracketing"){
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

        res_df$R2 <- sapply(res_df$Classification, FUN = function(x) {
            model <- res_list[["models"]][[x]]
            return(summary(model)$r.squared)
            
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
        res_df$info_label[2] <- "R2"
        res_df$info_label[3] <- "LLOQ"

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
        res_df$info_value[2] <- summary(unique(res_list[["models"]])[[1]])$r.squared
        res_df$info_value[3] <- unique(unlist(rv$LLOQs))

        res_df$info_label[4] <- "IS Compound"
        res_df$info_label[5] <- "Correction Factors"
        
        res_df$info_value[4] <- input$Compound_IS
        res_df$info_value[5] <- paste(rv$IS_table$Sample.Type, rv$IS_table$Correction.Factors, sep = " = ", collapse = ", ")

    } else if (input$quantitation_method == "Drift Correction") {
        res_df$PeakArea <- rv$data[, cpt_name]

        res_df$corrected_area <- rv$dc_area

        res_df$RetentionTime <- rv$data_RT[, cpt_name]

        res_df$Concentration <- signif(quant$pred, 3)


        res_df$` ` <- rep("", nrow(res_df))
        res_df$info_label <- rep("", nrow(res_df))
        res_df$info_value <- rep("", nrow(res_df))

        res_df$info_label[1] <- "Function"
        res_df$info_label[2] <- "R2"
        res_df$info_label[3] <- "LLOQ"

        
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
        res_df$info_value[2] <- summary(unique(res_list[["models"]])[[1]])$r.squared
        res_df$info_value[3] <- unique(unlist(rv$LLOQs))



        res_df$info_label[4] <- "Drift Model"
        res_df$info_label[5] <- "Span Width"
        res_df$info_label[6] <- "Files for Correction"
        
        res_df$info_value[4] <- input$model_drift
        res_df$info_value[5] <- input$span_width
        res_df$info_value[6] <- input$files_for_correction



    } else if (input$quantitation_method == "Default Bracketing") {

        res_df$PeakArea <- rv$data[, cpt_name]

        res_df$RetentionTime <- rv$data_RT[, cpt_name]

        res_df$Concentration <- signif(quant$pred, 3)

        res_df$` ` <- rep("", nrow(res_df))
        res_df$info_label <- rep("", nrow(res_df))
        res_df$info_value <- rep("", nrow(res_df))


        res_df$info_label[1] <- "Function"
        res_df$info_label[2] <- "LLOQ"
        res_df$info_label[2] <- "R2"

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
        res_df$info_value[3] <- summary(unique(res_list[["models"]])[[1]])$r.squared
    } else if(input$quantitation_method == "Weighted Bracketing") {

      
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
        

        res_df$R2 <- sapply(res_df$Classification, FUN = function(x) {
            model <- res_list[["models"]][[x]]
            return(summary(model)$r.squared)
            
        })


        for (i in 1:ncol(rv$weights)) {

          print(rv$weights)

          if(!grepl("Cal", colnames(rv$weights)[i])){
            next
          }
    
            weights <- sapply(
                res_df$Classification,
                FUN = function(x) rv$weights[rv$weights$Block == x, i],
                USE.NAMES = FALSE
            )
    
            weights <- data.frame(weights = weights)

            
            if (ncol(weights) > 0 && length(names(rv$weights)) >= i) {
                colnames(weights)[1] <- paste(
                    names(rv$weights)[i],
                    "weights",
                    sep = " "
                )
            }
            print(weights)
    
            res_df <- cbind(res_df, weights)


          

        }

        

        

    }

    if((input$quantitation_method !="Custom Bracketing")){
      acc_table <- get_acc_table(input, rv)
   
    # Add two empty columns to res_df before combining
    res_df$` ` <- ""
    res_df$`  ` <- ""

    # Adjust acc_table to match the number of rows in res_df
    if (nrow(acc_table) < nrow(res_df)) {
      acc_table <- rbind(as.matrix(acc_table), matrix("", nrow = nrow(res_df) - nrow(acc_table), ncol = ncol(acc_table)))
      colnames(acc_table) <- colnames(rv$acc_table)
    }
    

    # Combine res_df and acc_table
    res_df <- cbind(res_df, acc_table)
    
    # Ensure empty columns do not appear as column names when written
    colnames(res_df)[colnames(res_df) %in% c(" ", "  ")] <- ""
    
    }
    
    return(res_df)

}

write_quant_results <- function(res_df, cpt_name){
  suppressWarnings({
      if (!file.exists("Quant_summary.xlsx")) {
      res_df <- list(cpt_name = res_df)
      names(res_df)[which(names(res_df) == "cpt_name")] <- cpt_name
      writexl::write_xlsx(res_df, "Quant_summary.xlsx")
    }else {
      suppressMessages({
        append_xlsx(existing_file = "Quant_summary.xlsx", new_data = res_df, sheet_name = cpt_name)

      })
        
    }

    })
}
