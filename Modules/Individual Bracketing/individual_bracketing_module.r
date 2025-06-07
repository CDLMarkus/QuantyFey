#### Individual Bracketing Module


source("Modules/Individual Bracketing/individual_bracketing_helper.r")

individual_bracketing_module <- function(session, input, output, rv) {



output$bracketing_model_plot <- renderPlotly({

    weights <- weights_ind(input, rv)
    class <- update_Classification_ind(rv)

    df_weight <- data.frame(Classification = class)
    # For each column in weights (except "Block"), add a new column to df with the sum for each Classification
    weight_cols <- setdiff(names(weights), "Block")
    for (col in weight_cols) {
        df_weight[[paste0("Weight_", col)]] <- sapply(df_weight$Classification, function(class_val) {
            sum(weights[weights$Block == class_val, col], na.rm = TRUE)
        })
    }



    mod <- rv$mod_ind[[1]]
    df <- rv$mod_ind[[2]]
    df$pred <- predict(mod, df)

    df <- cbind(df, select(df_weight, -Classification))
    print(df)
 

    p <- ggplot() + geom_bar(data = df, aes(x = inj, y = PeakArea), stat = "identity")+geom_line(data = df, aes(x  = inj, y = pred))
    names(df) <- gsub(pattern = " ", replacement = "_", names(df))
    cal_cols <- grep("Cal", names(df), value = TRUE)

    print(df)
    # Assign a unique color to each calibration column
    colors <- scales::hue_pal()(length(cal_cols))
    # Use ggplot2's sec.axis for a secondary y-axis
    # We'll need to reshape the data for easier plotting
    library(tidyr)
    if (length(cal_cols) > 0) {
        df_long <- pivot_longer(df, cols = all_of(cal_cols), names_to = "Calibration", values_to = "CalValue")
        p <- ggplot() +
            geom_bar(data = df, aes(x = inj, y = PeakArea), stat = "identity") +
            geom_line(data = df, aes(x = inj, y = pred)) +
            geom_line(
                data = df_long,
                aes(x = inj, y = CalValue, color = Calibration),
                linetype = "dashed",
                size = 1
            ) +
            scale_y_continuous(
                name = "PeakArea / pred",
                sec.axis = sec_axis(~ ., name = "Calibration Values")
            ) +
            scale_color_manual(values = colors)
    }



 

    p <- ggplotly(p)

    p





})






} 


