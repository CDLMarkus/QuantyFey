#### Weighted Bracketing Module


source("Modules/Weighted Bracketing/weighted_bracketing_helper.r")

weighted_bracketing_module <- function(session, input, output, rv) {



output$bracketing_model_plot <- renderPlotly({





    weights <- weights_ind_for_plot(input, rv)
    
    class <- update_Classification_ind(rv)

    df_weight <- data.frame(Classification = class)
    # For each column in weights (except "Block"), add a new column to df with the sum for each Classification
    weight_cols <- setdiff(names(weights), "Block")
    for (col in weight_cols) {
        df_weight[[paste0("Weight_", col)]] <- sapply(df_weight$Classification, function(class_val) {
            sum(weights[weights$Block == class_val, col], na.rm = TRUE)
        })
    }



    
    df <- rv$mod_ind[[1]]
    #df$pred <- predict(mod, df)

    df <- cbind(df, select(df_weight, -Classification))
 


# Ensure column names are safe
names(df) <- gsub(" ", "_", names(df))

# Identify calibration weight columns
cal_cols <- grep("Cal", names(df), value = TRUE)

# Prepare weights in long format
if (length(cal_cols) > 0) {
  df_long <- pivot_longer(df, cols = all_of(cal_cols), names_to = "Calibration", values_to = "Weight")

  # Manual color palette (colorblind-friendly, professional)
# Define a base palette (can be extended)
base_palette <- c(
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a",
  "#66a61e", "#e6ab02", "#a6761d", "#666666"
)

# Repeat or trim the palette to match the number of cal_cols
color_palette <- rep(base_palette, length.out = length(cal_cols))

  # Build base plot
  p <- ggplot() +
    # Raw intensities as bars
    geom_bar(data = df, aes(x = inj, y = PeakArea), stat = "identity", fill = "grey80", alpha = 0.7) +

    # Model prediction as line
    #geom_line(data = df, aes(x = inj, y = pred), color = "black", linewidth = 1) +

    # Calibrant weights on secondary axis (0–1 scale)
    geom_line(
      data = df_long,
      aes(x = inj, y = Weight * max(df$PeakArea, na.rm = TRUE), color = Calibration),  # scale to match y
      linetype = "dashed",
      linewidth = 1
    ) +

    # Y axes
    scale_y_continuous(
      name = "Peak Area",
      sec.axis = sec_axis(~ . / max(df$PeakArea, na.rm = TRUE), name = "Calibration Weights (0–1)")
    ) +

    # X-axis
    scale_x_continuous(name = "Number of injection") +

    # Color for calibrants
    scale_color_manual(values = color_palette) +

    # Clean, minimal theme
    theme_minimal(base_size = 14) +
    theme(
      axis.title.y.right = element_text(color = "steelblue"),
      axis.title.y.left = element_text(color = "black"),
      legend.position = "top",
      legend.title = element_blank()
    )
}

# Convert to interactive plot
p <- ggplotly(p)




})








} 


