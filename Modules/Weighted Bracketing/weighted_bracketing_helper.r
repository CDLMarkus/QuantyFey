update_Classification_ind <- function(rv){
  ind_classification <- rv$data$Classification
  sample_names <- rv$data$Sample.Name
  # Find indices of "Cal x" classes
  cal_indices <- grep("^Cal \\d+$", ind_classification)
  new_classification <- ind_classification

  if (length(cal_indices) > 1) {
    for (i in seq_along(cal_indices)[-length(cal_indices)]) {
      start_idx <- cal_indices[i]
      end_idx <- cal_indices[i + 1]
      # Assign "Sample [row] (Sample.Name)" to indices between Cal x's (exclusive)
      if ((end_idx - start_idx) > 0) {
        between_idx <- (start_idx + 1):(end_idx - 1)
        if (length(between_idx) > 0) {
          for (j in between_idx) {
            # Only assign if not already a "Cal n"
            if (!grepl("^Cal \\d+$", ind_classification[j])) {
              new_classification[j] <- paste0(j, " (", sample_names[j], ")")
            }
          }
        }
      }
    }
  }
  return(new_classification)
}



update_weights_ind <- function(input, rv) {
  
    selection_cals_table <- rv$selection_cals_table

    
    for(i in 1:length(selection_cals_table)){
        selection_cals_table[[i]] <- add_weights(weight_method = input$weight_method, data = selection_cals_table[[i]])
    }

    



  # Prevent infinite reactive loops: do not update rv$selection_cals_table inside a reactive context
  if (input$quantitation_method == "Weighted Bracketing") {
    weights <- weights_ind(input, rv)
    
    # To avoid infinite reactivity, update rv$selection_cals_table outside of a reactive context
    # Return the weights table and let the caller update rv$selection_cals_table if needed
    weights_result <- list()
    
    for (i in seq_len(nrow(weights))) {
      block_name <- weights$Block[i]
      if (!is.null(selection_cals_table[[block_name]])) {
        tab <- selection_cals_table[[block_name]]
        for (class in unique(tab$Classification)) {
          tab$weights[tab$Classification == class] <-
            tab$weights[tab$Classification == class] *
            weights[weights$Block == block_name, class]
        }
        weights_result[[block_name]] <- tab
      }
    }

    #rv$weights <- weights
    rv$selection_cals_table <- weights_result
    }

  rv$weights <- weights
  return(rv$selection_cals_table)
}



weights_ind <- function(input, rv){
  

    weights <- data.frame(Block = unique(rv$Classification_temp))
    cal_blocks <- unique(
      rv$Classification_temp[grepl("^Cal", rv$Classification_temp)]
    )
    for (idx in seq_along(cal_blocks)) {
      i <- cal_blocks[idx]
      weights <- cbind(weights, data.frame(new = rep(0, nrow(weights))))
      colnames(weights)[colnames(weights) == "new"] <- i
      if (idx == 1) {
        # For the first Cal, set everything before and including to 1
        weights[1:which(weights$Block == i), i] <- 1
      } else if (idx == length(cal_blocks)) {
        # For the last Cal, set everything after and including to 1
        weights[which(weights$Block == i):nrow(weights), i] <- 1
      } else {
        # For other Cals, set only the matching row to 1
        weights[weights$Block == i, i] <- 1
      }
    }

    if (input$model_for_ind_bracketing == "linear") {

      # Find indices of all "^Cal" blocks
      cal_indices <- which(grepl("^Cal", weights$Block))
      # Only consider samples between the first and last "^Cal" blocks
      valid_range <- cal_indices[1]:cal_indices[length(cal_indices)]
      # selected_blocks <- weights$Block[valid_range] # Not used

      # Count samples between each pair of consecutive "^Cal" blocks
      between_counts <- integer(length(cal_indices) - 1)
      for (i in seq_along(between_counts)) {
        start_idx <- cal_indices[i]
        end_idx <- cal_indices[i + 1]
        # Exclude the "^Cal" blocks themselves
        between_counts[i] <- sum(
          !grepl(
            "^Cal",
            weights$Block[(start_idx + 1):(end_idx - 1)]
          )
        )
      }

      # Iterate through each pair of consecutive Cal blocks
      for (i in seq_len(length(cal_indices) - 1)) {
        start_idx <- cal_indices[i]
        end_idx <- cal_indices[i + 1]
        cal_start <- weights$Block[start_idx]
        cal_end <- weights$Block[end_idx]
        # Indices between the two Cal blocks (exclusive)
        between_idx <- (start_idx + 1):(end_idx - 1)
        n_between <- length(between_idx)
        if (n_between > 0) {
          # Linear interpolation: from 1 to 0 for cal_start, 0 to 1 for cal_end
          weights[between_idx, cal_start] <-
            seq(
              1 - 1 / (n_between + 1),
              1 / (n_between + 1),
              length.out = n_between
            )
          weights[between_idx, cal_end] <-
            seq(
              1 / (n_between + 1),
              1 - 1 / (n_between + 1),
              length.out = n_between
            )
        }
      }

      rv$mod_ind <- list(data.frame(Sample.Name = rv$data$Sample.Name, PeakArea = rv$Area, inj = 1:nrow(rv$data), Classification = rv$Classification_temp))

    } else if (input$model_for_ind_bracketing == "non linear (QC)") {
        
      if (!is.null(input$file_for_bracketing)) {

        
        

        df <- data.frame(Sample.Name = rv$data$Sample.Name, PeakArea = rv$Area, inj = 1:nrow(rv$data), Classification = rv$Classification_temp) 
        df_ss <- df %>% subset(Sample.Name == input$file_for_bracketing)
        if (input$model_bracketing == "loess") {
  mod <- tryCatch(
    loess(PeakArea ~ inj, data = df_ss, span = input$span_width_bracketing),
    error = function(e) {
      showNotification(
        "The loess model could not be fitted. Please review and adjust the parameters to ensure proper model performance.",
        type = "error"
      )
      return(NULL)
    }
  )

} else if (input$model_bracketing == "poly") {

  mod <- tryCatch(
    {
      lm(PeakArea ~ poly(inj, degree = input$spline_df), data = df_ss)
    },
    error = function(e) {
      showNotification("Polynomial model fitting failed.", type = "error")
      return(NULL)
    })

}  




                df$pred <- predict(mod, df)

                rv$mod_ind <- list(df)
          
           


           # Fill NAs at the beginning with the first non-NA value, and at the end with the last non-NA value
           if (anyNA(df$pred)) {
             first_non_na <- which(!is.na(df$pred))[1]
             last_non_na <- tail(which(!is.na(df$pred)), 1)
             if (!is.na(first_non_na) && first_non_na > 1) {
               df$pred[1:(first_non_na - 1)] <- df$pred[first_non_na]
             }
             if (!is.na(last_non_na) && last_non_na < nrow(df)) {
               df$pred[(last_non_na + 1):nrow(df)] <- df$pred[last_non_na]
             }
           }






           # Calculate the numerical derivative of the predicted values with respect to injection position
           df$deriv <- c(0, diff(df$pred) / diff(df$inj))

            # Assign the derivative values to the weights data frame for each block
            weights$deriv <- 0  # Initialize with 0 for all blocks

            # Find indices of Cal blocks
            cal_idx <- which(grepl("^Cal \\d+$", weights$Block))

            # For blocks between Cal blocks, assign the corresponding df$deriv value
            for (i in seq_along(cal_idx)[-length(cal_idx)]) {
                start_idx <- cal_idx[i]
                end_idx <- cal_idx[i + 1]
                between_idx <- (start_idx + 1):(end_idx - 1)
                if (length(between_idx) > 0) {
                    # Map the derivative from df to the weights for these blocks
                    # Find the corresponding rows in df for these blocks
                    block_names <- weights$Block[between_idx]
                    for (j in seq_along(block_names)) {
                        block_name <- block_names[j]
                        # Find the first matching row in df for this block
                        df_row <- which(df$Classification == block_name)[1]
                        if (!is.na(df_row)) {
                            weights$deriv[between_idx[j]] <- df$deriv[df_row]
                        }
                    }
                }
            }
         

# Find indices of all "^Cal" blocks
cal_indices <- which(grepl("^Cal", weights$Block))

# Only consider samples between the first and last "^Cal" blocks
valid_range <- cal_indices[1]:cal_indices[length(cal_indices)]

# Iterate through each pair of consecutive Cal blocks
for (i in seq_len(length(cal_indices) - 1)) {
  start_idx <- cal_indices[i]
  end_idx <- cal_indices[i + 1]
  cal_start <- weights$Block[start_idx]
  cal_end <- weights$Block[end_idx]

  # Indices between the two Cal blocks (exclusive)
  between_idx <- (start_idx + 1):(end_idx - 1)
  n_between <- length(between_idx)

  if (n_between > 0) {
    # Use derivatives to guide weight shift
    derivs <- weights$deriv[between_idx]
    derivs[is.na(derivs)] <- 0

    # Cumulative sum of derivatives
    cum_deriv <- cumsum(derivs)

    # Normalize to [offset, 1 - offset] instead of [0, 1]
    offset <- 1 / (n_between + 1)  # e.g. 1/6 if 5 points
    range_cum <- max(cum_deriv) - min(cum_deriv)
    if (range_cum > 0) {
      norm_cum_deriv <- (cum_deriv - min(cum_deriv)) / range_cum
      norm_cum_deriv <- norm_cum_deriv * (1 - 2 * offset) + offset
    } else {
      norm_cum_deriv <- rep(0.5, n_between)  # flat weight in case of zero slope
    }

    # Apply weights: smooth transition between calibrants
    weights[between_idx, cal_start] <- norm_cum_deriv
    weights[between_idx, cal_end]   <- 1 - norm_cum_deriv

    # Ensure all other Cal blocks are zeroed
    other_cals <- setdiff(weights$Block[cal_indices], c(cal_start, cal_end))
    if (length(other_cals) > 0) {
      for (other in other_cals) {
        weights[between_idx, other] <- 0
      }
    }
  }
}


      }
    }

    return(weights)

}




weights_ind_for_plot <- function(input, rv){

  classification <- update_Classification_ind(rv)  

    weights <- data.frame(Block = unique(classification))
    cal_blocks <- unique(
      classification[grepl("^Cal", classification)]
    )
    for (idx in seq_along(cal_blocks)) {
      i <- cal_blocks[idx]
      weights <- cbind(weights, data.frame(new = rep(0, nrow(weights))))
      colnames(weights)[colnames(weights) == "new"] <- i
      if (idx == 1) {
        # For the first Cal, set everything before and including to 1
        weights[1:which(weights$Block == i), i] <- 1
      } else if (idx == length(cal_blocks)) {
        # For the last Cal, set everything after and including to 1
        weights[which(weights$Block == i):nrow(weights), i] <- 1
      } else {
        # For other Cals, set only the matching row to 1
        weights[weights$Block == i, i] <- 1
      }
    }

    if (input$model_for_ind_bracketing == "linear") {

      # Find indices of all "^Cal" blocks
      cal_indices <- which(grepl("^Cal", weights$Block))
      # Only consider samples between the first and last "^Cal" blocks
      valid_range <- cal_indices[1]:cal_indices[length(cal_indices)]
      # selected_blocks <- weights$Block[valid_range] # Not used

      # Count samples between each pair of consecutive "^Cal" blocks
      between_counts <- integer(length(cal_indices) - 1)
      for (i in seq_along(between_counts)) {
        start_idx <- cal_indices[i]
        end_idx <- cal_indices[i + 1]
        # Exclude the "^Cal" blocks themselves
        between_counts[i] <- sum(
          !grepl(
            "^Cal",
            weights$Block[(start_idx + 1):(end_idx - 1)]
          )
        )
      }

      # Iterate through each pair of consecutive Cal blocks
      for (i in seq_len(length(cal_indices) - 1)) {
        start_idx <- cal_indices[i]
        end_idx <- cal_indices[i + 1]
        cal_start <- weights$Block[start_idx]
        cal_end <- weights$Block[end_idx]
        # Indices between the two Cal blocks (exclusive)
        between_idx <- (start_idx + 1):(end_idx - 1)
        n_between <- length(between_idx)
        if (n_between > 0) {
          # Linear interpolation: from 1 to 0 for cal_start, 0 to 1 for cal_end
          weights[between_idx, cal_start] <-
            seq(
              1 - 1 / (n_between + 1),
              1 / (n_between + 1),
              length.out = n_between
            )
          weights[between_idx, cal_end] <-
            seq(
              1 / (n_between + 1),
              1 - 1 / (n_between + 1),
              length.out = n_between
            )
        }
      }

      rv$mod_ind <- list(data.frame(Sample.Name = rv$data$Sample.Name, PeakArea = rv$Area, inj = 1:nrow(rv$data), Classification =classification))

    } else if (input$model_for_ind_bracketing == "non linear (QC)") {
        
      if (!is.null(input$file_for_bracketing)) {

        
        

        df <- data.frame(Sample.Name = rv$data$Sample.Name, PeakArea = rv$Area, inj = 1:nrow(rv$data), Classification = classification) 
        df_ss <- df %>% subset(Sample.Name == input$file_for_bracketing)
        if (input$model_bracketing == "loess") {
  mod <- tryCatch(
    loess(PeakArea ~ inj, data = df_ss, span = input$span_width_bracketing),
    error = function(e) {
      showNotification(
        "The loess model could not be fitted. Please review and adjust the parameters to ensure proper model performance.",
        type = "error"
      )
      return(NULL)
    }
  )

} else if (input$model_bracketing == "poly") {

  mod <- tryCatch(
    {
      lm(PeakArea ~ poly(inj, degree = input$spline_df), data = df_ss)
    },
    error = function(e) {
      showNotification("Polynomial model fitting failed.", type = "error")
      return(NULL)
    })

}  




                df$pred <- predict(mod, df)

                rv$mod_ind <- list(df)
          
           


           # Fill NAs at the beginning with the first non-NA value, and at the end with the last non-NA value
           if (anyNA(df$pred)) {
             first_non_na <- which(!is.na(df$pred))[1]
             last_non_na <- tail(which(!is.na(df$pred)), 1)
             if (!is.na(first_non_na) && first_non_na > 1) {
               df$pred[1:(first_non_na - 1)] <- df$pred[first_non_na]
             }
             if (!is.na(last_non_na) && last_non_na < nrow(df)) {
               df$pred[(last_non_na + 1):nrow(df)] <- df$pred[last_non_na]
             }
           }






           # Calculate the numerical derivative of the predicted values with respect to injection position
           df$deriv <- c(0, diff(df$pred) / diff(df$inj))

            # Assign the derivative values to the weights data frame for each block
            weights$deriv <- 0  # Initialize with 0 for all blocks

            # Find indices of Cal blocks
            cal_idx <- which(grepl("^Cal \\d+$", weights$Block))

            # For blocks between Cal blocks, assign the corresponding df$deriv value
            for (i in seq_along(cal_idx)[-length(cal_idx)]) {
                start_idx <- cal_idx[i]
                end_idx <- cal_idx[i + 1]
                between_idx <- (start_idx + 1):(end_idx - 1)
                if (length(between_idx) > 0) {
                    # Map the derivative from df to the weights for these blocks
                    # Find the corresponding rows in df for these blocks
                    block_names <- weights$Block[between_idx]
                    for (j in seq_along(block_names)) {
                        block_name <- block_names[j]
                        # Find the first matching row in df for this block
                        df_row <- which(df$Classification == block_name)[1]
                        if (!is.na(df_row)) {
                            weights$deriv[between_idx[j]] <- df$deriv[df_row]
                        }
                    }
                }
            }
         

# Find indices of all "^Cal" blocks
cal_indices <- which(grepl("^Cal", weights$Block))

# Only consider samples between the first and last "^Cal" blocks
valid_range <- cal_indices[1]:cal_indices[length(cal_indices)]

# Iterate through each pair of consecutive Cal blocks
for (i in seq_len(length(cal_indices) - 1)) {
  start_idx <- cal_indices[i]
  end_idx <- cal_indices[i + 1]
  cal_start <- weights$Block[start_idx]
  cal_end <- weights$Block[end_idx]

  # Indices between the two Cal blocks (exclusive)
  between_idx <- (start_idx + 1):(end_idx - 1)
  n_between <- length(between_idx)

  if (n_between > 0) {
    # Use derivatives to guide weight shift
    derivs <- weights$deriv[between_idx]
    derivs[is.na(derivs)] <- 0

    # Cumulative sum of derivatives
    cum_deriv <- cumsum(derivs)

    # Normalize to [offset, 1 - offset] instead of [0, 1]
    offset <- 1 / (n_between + 1)  # e.g. 1/6 if 5 points
    range_cum <- max(cum_deriv) - min(cum_deriv)
    if (range_cum > 0) {
      norm_cum_deriv <- (cum_deriv - min(cum_deriv)) / range_cum
      norm_cum_deriv <- norm_cum_deriv * (1 - 2 * offset) + offset
    } else {
      norm_cum_deriv <- rep(0.5, n_between)  # flat weight in case of zero slope
    }

    # Apply weights: smooth transition between calibrants
    weights[between_idx, cal_start] <- norm_cum_deriv
    weights[between_idx, cal_end]   <- 1 - norm_cum_deriv

    # Ensure all other Cal blocks are zeroed
    other_cals <- setdiff(weights$Block[cal_indices], c(cal_start, cal_end))
    if (length(other_cals) > 0) {
      for (other in other_cals) {
        weights[between_idx, other] <- 0
      }
    }
  }
}


      }
    }

    return(weights)

}

