## helper functions for set up calibration tables

assign_concentration_to_sample <- function(sample_name, cal_df) {
  matched_row <- cal_df[apply(cal_df, 1, function(row) grepl(row["Cal.Name"], sample_name)), ]

  if (nrow(matched_row) > 0) {
    return(as.numeric(matched_row$Concentration[1]))  # Return the first match
  }

  return(0)
}

get_peak_area <- function(method, rv) {
  switch(method,
         "Bracketing" = rv$Area,
         "Default" = rv$Area,
         "Drift Correction" = rv$dc_area,
         "IS Correction" = rv$IS_ratio,
         rep(NA, length(rv$data$Sample.Name)))  # Fallback
}

process_calibration_block <- function(class_name, selection_row, df, setup_cal, compound, weight_method) {
  sel <- as.logical(selection_row)
  col_used <- names(selection_row)[sel]

  if (all(!sel)) return(NULL)

  cals_temp <- df[df$Classification %in% col_used, , drop = FALSE]

  cals_temp$Concentration <- sapply(
    cals_temp$Sample.Name,
    assign_concentration_to_sample,
    cal_df = setup_cal
  )

  # Rename to "PeakArea" if needed
  if (!"PeakArea" %in% names(cals_temp)) {
    colnames(cals_temp)[colnames(cals_temp) == compound] <- "PeakArea"
  }

  cals_temp$Concentration <- as.numeric(cals_temp$Concentration)
  cals_temp <- cals_temp[cals_temp$Concentration != 0, , drop = FALSE]

  cals_temp <- add_weights(weight_method = weight_method, data = cals_temp)

  if (is.null(cals_temp$used)) {
    cals_temp$used <- cals_temp$PeakArea != 0
  }

  return(cals_temp)
}
