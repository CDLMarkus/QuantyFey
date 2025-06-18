ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = NULL,
    primary = "#005a9c",
    base_font = font_google("Inter"),
    font_scale = 1.1
  ),

  tags$style(HTML("
    body {
      background-color: white;
    }

    .navbar, .navbar-default {
      background-color: white !important;
      border-bottom: 1px solid #ccc;
    }

    .titlePanel {
      background-color: white;
      color: #005a9c;
      padding: 10px 20px;
      font-size: 32px;
      display: flex;
      align-items: center;
    }

    .my-special-select .selectize-input {
      height: auto;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }

    .titlePanel .app-name {
      margin-left: 10px;
      font-weight: bold;
    }

    .titlePanel .app-icon {
      font-size: 36px;
      color: #005a9c;
    }

    .navbar-default .navbar-brand {
      color: #005a9c !important;
    }

    .nav-pills > li > a {
      color: #005a9c !important;
      background-color: transparent !important;
    }

    .nav-pills > li.active > a,
    .nav-pills > li.active > a:focus,
    .nav-pills > li.active > a:hover,
    .nav-pills > li > a.active,
    .nav-pills > li > a.nav-link.active {
      background-color: #005a9c !important;
      color: #ffffff !important;
      border-radius: 5px;
    }

    .btn {
      background-color: #005a9c !important;
      color: #ffffff !important;
    }

    .btn-primary {
      background-color: #005a9c !important;
      border-color: #005a9c !important;
    }

    .btn-secondary {
      background-color: #337ab7 !important;
      border-color: #2e6da4 !important;
    }

    .dataTables_wrapper {
      font-size: 14px;
    }

    @media (max-width: 1200px) {
      .titlePanel {
        font-size: 24px;
        padding: 8px 16px;
      }
      .dataTables_wrapper {
        font-size: 13px;
      }
    }

    @media (max-width: 768px) {
      .titlePanel {
        font-size: 20px;
        flex-direction: column;
        text-align: center;
      }
      .app-icon {
        font-size: 28px;
      }
      .dataTables_wrapper {
        font-size: 12px;
      }
      table.dataTable {
        width: 100% !important;
      }
    }

    Shiny.addCustomMessageHandler('registerDoubleClick', function(sourceId) {
      var el = document.querySelector('[data-source=\"' + sourceId + '\"]');
      if (!el) return;

      el.on('plotly_doubleclick', function() {
        Shiny.setInputValue(sourceId + '_dblclick', new Date().getTime());
      });
    });
  ")),

    tags$script(HTML("
    Shiny.addCustomMessageHandler('enable-blur-update', function(ids) {
      ids.forEach(function(id) {
        var input = document.querySelector('#' + id + ' input, #' + id);
        if (!input) return;
        var container = input.closest('.form-group');
        if (container) Shiny.unbindAll(container);
        input.addEventListener('blur', function() {
          var newValue = input.type === 'number' ? input.valueAsNumber : input.value;
          var currentShinyValue = Shiny.shinyapp.$inputValues[id];
          if (newValue !== currentShinyValue) {
            Shiny.setInputValue(id, newValue, {priority: 'event'});
          }
        });
      });
    });

    Shiny.addCustomMessageHandler('force-blur', function(data) {
  var input = document.querySelector('#' + data.id + ' input, #' + data.id);
  if (input) {
    input.blur();
  }
});
  ")),

  titlePanel(
    div(
      class = "titlePanel",
      tags$img(src = icon_data, height = "64px"),
      span(class = "app-name", "Quanty Fey")
    )
  ),

navset_pill(

    # Data Upload Panel
    nav_panel("Data Upload",
      icon = icon("cloud-upload"),
      fluidRow(
        column(
          12,
          card(
            title = "Upload Metabolomics Data",
            page_sidebar(
              sidebar = sidebar(
                fileInput("file1", "Upload Peak Area Data:", buttonLabel = "Choose File", placeholder = "No file selected", accept = c(".txt", ".csv", ".xlsx")),
                conditionalPanel(
  condition = "output.fileUploaded",
  fileInput("file_RT", "Upload Retention Time Data:",
            buttonLabel = "Choose File",
            placeholder = "No file selected",
            accept = c(".txt", ".csv", ".xlsx"))
),
                
                checkboxInput("change_project_name", "Use Project Name", value = F),
                conditionalPanel(condition = 'input.change_project_name == true',
                  textInput("project_name", label = "Project Name:", value = paste0("Results_", format(Sys.Date(), "%Y%m%d")))  
                ),
                 useShinyjs(),
                actionButton(inputId = "reset", label = "Reset App")
              ),
              card(
                navset_pill(
                  nav_panel("Peak Table Preview",
                    card(
                      title = "Preview Area Data",
                      DTOutput("table1"),
                      height = "800px"
                    )
                  ),
                  nav_panel("Retention Time Table Preview",
                    card(
                      title = "Preview RT Data",
                      DTOutput("table_RT"),
                      height = "800px"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),

    # Configure Settings Panel
    nav_panel("Configure Settings",
      icon = icon("sliders"),
      fluidRow(
      column(
        12,
        card(
        title = "Settings",
        page_sidebar(
          sidebar = sidebar(
          selectInput("mode", "Template:", choices = templates_names, selected = Template_name),
          checkboxInput(inputId = "change_patterns", "Change Patterns", value = FALSE),
          conditionalPanel(condition = 'input.change_patterns == true',
            textInput("quant_indicator", "Pattern for Quant Transition:", value = quant_pattern),
            textInput("qual_indicator", "Pattern for Qual Transition:", value = qual_pattern),
            textInput("IS_indicator", "Pattern for IS Transitions:", value = IS_pattern)
          )
          ),
            card(
            title = "Preview",
            layout_columns(
            card(title = "Calibration Preview", DTOutput("setup_cal", width = "490px"), width = 3),
            card(title = "Quant Transitions Preview", DTOutput("table_QQ", width = "1010px"), width = 6),
            card(title = "IS Transitions Preview", DTOutput("table_IS", width = "490px"), width = 3), col_widths = c(3, 6, 3), height = "400px"
            )
            )
        )
        )
      )
      )
    ),

    # Compound Quantification Panel
    nav_panel("Compound Quantification",
      icon = icon("clipboard-check"),
      fluidRow(
        column(
          12,
          card(
            title = "Compound Quantification Settings",
            page_sidebar(
              sidebar = sidebar(
                div(class = "my-special-select",
                      selectInput(
                      inputId  = "Compound",
                      label    = "Compound:",
                      choices  = ""
                    )),
                textInput("Comment", "Comment:"),
                actionButton("save_compound", label = "Save", class = "btn-primary"),
                checkboxInput(inputId = "generate_report", label = "Generate Report", value = FALSE),
                 #checkboxInput(inputId = "show_dev", label = "Show Development Options", value = FALSE),
                conditionalPanel(condition = "false",
                  actionButton(inputId = "optimize_save", label = "Optimize and Save all Compounds")
              )),
              accordion(
                accordion_panel(
                  title = "Data Visualization",
                  
                  icon = bs_icon("grid"),
                  navset_pill(
                    id = "data_visualization",
                    nav_panel("RT Analysis", layout_columns(
                      card("Settings", checkboxGroupInput("RT_groups", "Groups to Include:", choices = NULL)),
                      card("Plot", plotlyOutput("RT_plot", height = "800px")),
                      col_widths = c(2, 10)
                    )),
                    nav_panel("Qual/Quant Ion Ratio Analysis", value = "QQ_analysis", layout_columns(
                      card("Settings", checkboxGroupInput("quan_qual_groups", "Groups to Include:", choices = NULL)),
                      card("Plot", plotlyOutput("qual_quant_plot", height = "800px")),
                      col_widths = c(2, 10)
                    )),
                    nav_panel("Blank Analysis", 
                      card("Plot", plotlyOutput("Blank_plot", height = "800px"))
                      )
                  )
                ),
                accordion_panel(
                  title = "IS Correction",
                  icon = bs_icon("columns"),
                  layout_columns(
                    card(
                div(class = "my-special-select",
                    
                      selectInput(
                      inputId  = "Compound_IS",
                      label    = "Internal Standard:",
                      choices  = ""
                      )
                  ),

                    ),
                    card(
                    title = "IS Analysis Plot",
                    plotlyOutput("IS_plot", height = "800px")
                  ),
                  card(
                    checkboxInput(inputId = "use_correction", label = "Correction Factors", value = FALSE),
                    checkboxInput(inputId = "log_scale_IS", label = "Scale log", value = FALSE),
                    title = "Correction Factors Table",
                    conditionalPanel(
                    condition = "input.use_correction == true",
                    DTOutput("IS_table")
                    )
                  ),
                  
                  col_widths = c(2, 10, 4)
                  )
                ),
                accordion_panel(
                  title = "Drift Correction",
                  icon = bs_icon("graph-down"),
                  layout_columns(
                    card("Settings",
                      radioButtons("model_drift", "Choose Model", choices = c("lm", "loess", "poly")),
                      selectInput("files_for_correction", "Choose Sample for Drift Correction:", choices = NULL),
                      conditionalPanel(condition = "input.model_drift == 'loess'",
                        numericInput(inputId = "span_width", label = "Span Width for loess", min = 0.4, max = 2, step = 0.05, value = 0.75)),
                        conditionalPanel(condition = "input.model_drift == 'poly'",
                        numericInput("spline_df_dc", "Degree:", value = 4, min = 1, max = 20)

                      
                  )),
                    card("Plot", plotlyOutput("drift_output", height = "800px")), col_widths = c(2, 10)
                    
                  )
                ),
                accordion_panel("Custom Bracketing",
                  icon = bsicons::bs_icon("crop"),
                  card(
                  DTOutput("dtable", width = "800px"),
                  height = "800px"
                  )
                ),
                accordion_panel("Weighted Bracketing",
                  icon = bsicons::bs_icon("graph-down"),
                  layout_columns(card(
                  selectInput(inputId = "model_for_ind_bracketing", "Model for weighting:", choices = c("linear", "non linear (QC)"), selected = "linear"),
                  conditionalPanel(condition = "input.model_for_ind_bracketing == 'non linear (QC)'",
                                   selectInput(inputId = "file_for_bracketing", label = "Select file for trend prediction", choices = NULL),
                                   selectInput(inputId = "model_bracketing", "Select Model:", choices = c("loess", "poly")),


  # Loess-specific file input
    conditionalPanel(
      condition = "input.model_bracketing == 'loess'",
      numericInput(
        inputId = "span_width_bracketing",
        label = "Span Width:",
        min = 0.4, max = 10, step = 0.05, value = 0.75
      )
    ),

    # Optional smoothing parameter or model tuning inputs (placeholders for now)
    conditionalPanel(
      condition = "input.model_bracketing == 'poly'",
      numericInput(
        inputId = "spline_df",
        label = "Degree:",
        value = 4,
        min = 2,
        max = 20,
        step = 1
      )
    ),

                  
                  )),
                  card(plotlyOutput("bracketing_model_plot")), col_widths = c(2, 10))
                ),
                accordion_panel(
                  title = "Quantification",
                  icon = bs_icon("stars"),
                  layout_columns(
                    card(
                      selectInput(inputId = "regression_model", label = "Regression Model:", choices = c("linear", "quadratic"), selected = "linear"),
                      numericInput(inputId = "LOQ", label = "Limit of Quantification", value = NULL),
                      selectInput(inputId = "weight_method", "Method for Weighing:", choices = c("none", "1/x", "1/x2", "1/y", "1/y2", "1/x force 0", "1/y force 0"), selected = "1/x"),
                      selectInput(inputId = "quantitation_method", "Method for Quantification", choices = c("IS Correction", "Drift Correction", "Custom Bracketing","Weighted Bracketing", "Default Bracketing")),
                      checkboxInput(inputId = "show_samples", label = "Show Samples", value = T),
                      conditionalPanel(
                      condition = "input.quantitation_method == 'Custom Bracketing' | input.quantitation_method == 'Weighted Bracketing'",
                      fluidRow(
                      column(
                      12,
                      selectInput(
                      inputId = "Block",
                      label = "Block to Visualize",
                      choices = NULL,
                      width = "100%"
                      )
                      )
                      ),
                      fluidRow(
                      column(
                      6,
                      actionButton(
                      inputId = "apply_levels",
                      label = "Apply Levels",
                      class = "btn-primary",
                      style = "width: 100%;"
                      )
                      ),
                      column(
                      6,
                      actionButton(
                      inputId = "apply_LLOQs",
                      label = "Apply LLOQ",
                      class = "btn-secondary",
                      style = "width: 100%;"
                      )
                      )
                      )
                      ),
                      actionButton(inputId = "optimize", label = "Optimize Model", class = "btn-primary btn-lg")
                    ),
                    card(
                      navset_pill(
                        nav_panel("Plots", card(plotlyOutput("p_quant", height = "800px"), 
                        conditionalPanel(condition = "true",
                        verbatimTextOutput("click"))), card(checkboxInput(inputId = "log_scale", label = "Show log scale", value = F))),
                        nav_panel("Accuracy", card(plotlyOutput("p_acc", height = "600px"), DTOutput("acc_table"), verbatimTextOutput("click_acc"), verbatimTextOutput("select_info_acc"), height = "1400px")),
                        nav_panel("Model Diagnostics", card(navset_pill(
                          nav_panel("Histogram of Residuals", plotOutput("HoR", height = "800px")),
                          nav_panel("Residuals vs. Fitted Values", plotOutput("RFV", height = "800px")),
                          nav_panel("Normal-QQ Plot", plotOutput("QQ", height = "800px")),
                          nav_panel("Scale-Location Plot", plotOutput("SLP", height = "800px")),
                          nav_panel("Cook's Distance Plot", plotOutput("CDP", height = "800px"))
                        ))),
                        nav_panel("Results", card(DTOutput("quan_results_table", width = "600px"), height = "900px"))
                      )
                    ),
                    col_widths = c(2, 10)
                  )
                ),
                multiple = F
              )
            )
          )
        )
      )
    ),

    # Results Panel
    nav_panel("Results",
      icon = bs_icon("text-center"),
      DTOutput("Results")
    )
  )
) # End of UI