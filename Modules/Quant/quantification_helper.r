observe_loq <- function(input, rv, session) {
    
    loq_input <- as.character(input$LOQ)

    if (grepl("\\.$", loq_input)) {
      loq_input <- paste0(loq_input, "0")
    }
    

    loq_value <- suppressWarnings(as.numeric(loq_input))
    
    if (is.na(loq_value)) {
      showNotification("Invalid input for LOQ. Please enter a numeric value.", type = "error")
      return(NULL)
    }
    
    if (!is.null(input$Block)) {
      blocks <- input$Block
      temp <- rv$selection_cals_table[[blocks]]
      
      if (!is.null(temp)) {
        for (i in seq_along(rv$selection_cals_table)) {
          if (all(unique(rv$selection_cals_table[[i]]$Classification) == unique(temp$Classification))) {
            blocks <- c(blocks, names(rv$selection_cals_table)[i])
          }
        }

        for (i in unique(blocks)) {
          rv$LLOQs[[i]] <- loq_value


        }
      }
    }
    
}

qq_with_ci <- function(model, conf = 0.95, sims = 1000) {
  set.seed(123)  # for reproducibility
  
  # Get standardized residuals
  resid <- rstandard(model)
  n <- length(resid)
  ord_resid <- sort(resid)
  theo_q <- qnorm(ppoints(n))

  # Simulate envelopes
  sim_resid <- replicate(sims, sort(rnorm(n)))
  lower <- apply(sim_resid, 1, quantile, probs = (1 - conf) / 2)
  upper <- apply(sim_resid, 1, quantile, probs = 1 - (1 - conf) / 2)

  df <- data.frame(
    Theoretical = theo_q,
    Sample = ord_resid,
    Lower = lower,
    Upper = upper
  )

  ggplot(df, aes(x = Theoretical, y = Sample)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.3) +
    geom_point(color = "grey20", size = 1.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Normal Q-Q Plot with 95% Confidence Envelope",
      x = "Theoretical Quantiles",
      y = "Standardized Residuals"
    ) +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5))
}

gg_reshist_with_guides <- function(model) {
  resid <- residuals(model)
  df <- data.frame(resid = resid)

  # Compute histogram manually as density
  hist_data <- hist(resid, breaks = 30, plot = FALSE)
  hist_df <- data.frame(
    x = hist_data$mids,
    y = hist_data$density
  )

  # Normal density, scaled to match max of histogram
  x_vals <- seq(min(hist_data$breaks), max(hist_data$breaks), length.out = 500)
  normal_density <- dnorm(x_vals, mean = mean(resid), sd = sd(resid))
  scale_factor <- max(hist_df$y) / max(normal_density)

  norm_df <- data.frame(
    x = x_vals,
    y = normal_density * scale_factor
  )

  ggplot() +
    geom_bar(data = hist_df, aes(x = x, y = y), stat = "identity",
             width = diff(hist_data$breaks)[1], fill = "grey80", color = "black") +
    geom_line(data = norm_df, aes(x = x, y = y), color = "blue", size = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Histogram of Residuals with Scaled Normal Curve",
      x = "Residuals",
      y = "Density"
    ) +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5))
}


gg_resfitted_with_limits <- function(model) {
  df <- data.frame(
    Fitted = fitted(model),
    StdResiduals = rstandard(model)
  )

  threshold <- 2  # Common threshold for "large" residuals

  ggplot(df, aes(x = Fitted, y = StdResiduals)) +
    geom_point(color = "black", size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_hline(yintercept = c(-threshold, threshold), linetype = "dotted", color = "grey40") +
    labs(
      title = "Residuals vs. Fitted Values",
      x = "Fitted Values",
      y = "Standardized Residuals"
    ) +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5))
}


gg_scalelocation_with_limits <- function(model) {
  sqrt_stdres <- sqrt(abs(rstandard(model)))
  df <- data.frame(
    Fitted = fitted(model),
    SqrtStdRes = sqrt_stdres
  )

  median_val <- median(sqrt_stdres)
  upper_limit <- quantile(sqrt_stdres, 0.75)

  ggplot(df, aes(x = Fitted, y = SqrtStdRes)) +
    geom_point(color = "black", size = 1.5) +
    geom_hline(yintercept = median_val, linetype = "dashed", color = "red") +
    geom_hline(yintercept = upper_limit, linetype = "dotted", color = "grey40") +
    labs(
      title = "Scale-Location Plot",
      x = "Fitted Values",
      y = "√|Standardized Residuals|"
    ) +
    theme_pubr() +
    theme(plot.title = element_text(hjust = 0.5))
}


gg_cooksd_with_threshold <- function(model) {
  cooks <- cooks.distance(model)
  n <- length(cooks)
  df <- data.frame(Obs = seq_along(cooks), Cook = cooks)

  ggplot(df, aes(x = Obs, y = Cook)) +
    geom_bar(stat = "identity", fill = "grey70", width = 0.1) +
    geom_hline(yintercept = 4/n, linetype = "dashed", color = "red") +
    labs(title = "Cook's Distance Plot", x = "Observation", y = "Cook's Distance") +
    theme_pubr()
}

gg_resleverage_with_cook <- function(model) {
  infl <- influence.measures(model)
  lev <- hatvalues(model)
  stdres <- rstandard(model)
  cooks <- cooks.distance(model)

  df <- data.frame(
    Leverage = lev,
    StandardizedResiduals = stdres,
    Cook = cooks
  )

  n <- nrow(df)
  p <- length(coef(model))

  ggplot(df, aes(x = Leverage, y = StandardizedResiduals)) +
    geom_point(aes(size = Cook), alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dotted") +
    labs(title = "Residuals vs. Leverage", x = "Leverage", y = "Standardized Residuals") +
    theme_pubr()
}