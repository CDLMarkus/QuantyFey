## Additional functions for optimize and save all compounds with one click

## Additional functions for optimize and save all compounds with one click

optimize_save_compounds <- function(input, rv, session) {
  cpts <- colnames(rv$data[, grepl(input$quant_indicator, colnames(rv$data)) & !grepl(input$IS_indicator, colnames(rv$data))])
  cpts_IS <- colnames(rv$data[, grepl("IS", colnames(rv$data))])

  cpt_with_IS <- c(
    "X1.Met.His_deriv_305.1_124.03_quant.must",
    "X3.Met.His_deriv_305.1_168.04_quant.must",
    "Putrescine_deriv_359.1_266.01_quant",
    "t4.OH.Pro_deriv_267.1_132_quant",
    "Trp_deriv_340.2_188.03_quant",
    "b.Ala_deriv_225_90.01_quant",
    "Sarcosine_deriv_225_90_quant",
    "Anserine_deriv_376.1_241.03_quant",
    "Met.SO_deriv_301.2_237_quant",
    "Met_deriv_285.1_150_quant"
  )
  #cpts <- cpts[1:5]

  run_for_cpt <- function(i) {
    if (i > length(cpts)) return()
    cpt <- cpts[i]
    updateSelectInput(session, "Compound", choices = cpts, selected = cpt)

    if (cpt %in% cpt_with_IS) {
      IS_name <- switch(cpt,
        "X1.Met.His_deriv_305.1_124.03_quant.must" = "IS_L.Histidine_296.1_203.1_CE15_quant",
        "X3.Met.His_deriv_305.1_168.04_quant.must" = "IS_L.Histidine_296.1_203.1_CE15_quant",
        "Putrescine_deriv_359.1_266.01_quant" = "IS_Putrescin_363.1_270.1_CE15_quant",
        "t4.OH.Pro_deriv_267.1_132_quant" = "IS_DL.Proline_258.1_123.1_CE18_quant",
        "Trp_deriv_340.2_188.03_quant" = "IS_Tryptophan_348.1_195.1_CE25_quant",
        "b.Ala_deriv_225_90.01_quant" = "IS_Alanine_228.1_93.1_CE15_quant",
        "Sarcosine_deriv_225_90_quant" = "IS_Alanine_228.1_93.1_CE15_quant",
        "Anserine_deriv_376.1_241.03_quant" = "IS_Alanine_228.1_93.1_CE15_quant",
        "Met.SO_deriv_301.2_237_quant" = "IS_Methionine_292.1_157.1_CE15_quant",
        "Met_deriv_285.1_150_quant" = "IS_Methionine_292.1_157.1_CE15_quant",
        cpt
      )
      updateSelectInput(session, "Compound_IS", choices = cpts_IS, selected = IS_name)
      meths <- c("IS Correction", "Drift Correction", "Custom Bracketing", "Default Bracketing", "Weighted Bracketing")
    } else {
      meths <- c("Drift Correction", "Custom Bracketing", "Default Bracketing", "Weighted Bracketing")
    }

    run_for_method <- function(j) {
      if (j > length(meths)) {
        shinyjs::delay(500, run_for_cpt(i + 1))
        return()
      }
      meth <- meths[j]
      updateSelectInput(session, "quantitation_method", choices = meths, selected = meth)
      shinyjs::delay(500, {
        #tryCatch({
        #  optimize_model_metrics(input, rv, session)
        #}, error = function(e) {
        #  showNotification(paste("An error occurred:", e$message), type = "error")
        #}, finally = {
        #  showNotification("Optimization completed successfully.", type = "message")
        #})

        updateTextInput(session, "Comment", label = "Comment:", value = paste(cpt, meth, "autom", sep = "_"))

        tryCatch({
          save_compound(input, rv, session)
        }, error = function(e) {
          message("Error in saving compound data: ", e$message, duration = 10)
        })

        run_for_method(j + 1)
      })
    }
    run_for_method(1)
  }
  run_for_cpt(1)
}