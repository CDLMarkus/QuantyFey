#### Shiny App QuantyFey Version 1.0.0
#### Author Markus Aigensberger
#### Funding Christian Doppler Forschungsgesellschaft
#### Project CDL-LiveGut
#### Date 2025-06-26

#### Display copyright and license information

#### QuantyFey  Copyright (C) 2025  Markus Aigensberger
#### This program comes with ABSOLUTELY NO WARRANTY; see the LICENCE file for details.
#### This is free software, and you are welcome to redistribute it
#### under the terms of the GNU General Public License v3.0; see the LICENCE file for details.



## Load necessary libraries ----


# Check if required files are present in the working directory
required_files <- c("./Modules/Package_installation/install_packages.r", "./helper/global_functions.r", "./Modules/Report/report_markdown.rmd", "./Dependencies/templates.xlsx")
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(paste("The following required files are missing in the working directory:", paste(missing_files, collapse = ", ")))
}

# Source the script to install required packages
source("./Modules/Package_installation/install_packages.r")


# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(tinytex)
  library(DT)
  library(tidyverse)
  library(plotly)
  library(ggplot2)
  library(ggpubr)
  library(shinyFiles)
  library(reshape2)
  library(tibble)
  library(gridExtra)
  library(dplyr)
  library(tidyr)
  library(rmarkdown)
  library(bslib)
  library(readxl)
  library(lindia)
  library(ggtext)
  library(extrafont)
  library(kableExtra)
  library(knitr)
  library(bsicons)
  library(rstudioapi)
  library(shinyalert)
  library(pandoc)
  library(renv)
  library(patchwork)
  library(cowplot)
  library(writexl)
  library(shinyjs)
})


source("./helper/global_functions.r")
source("./helper/dev_functions.r")
source("default_settings.r")

# Check if the file exists in the current working directory

templates_names <- excel_sheets("./Dependencies/templates.xlsx")

template_list <- list()

for (template in templates_names) {
  df_temp <- read_xlsx("./Dependencies/templates.xlsx", sheet = template) %>% as.data.frame()
  colnames(df_temp) <- gsub("[^a-zA-Z0-9._]", ".", colnames(df_temp))

  template_list[[template]] <- df_temp
}


for (i in 1:length(template_list)) {
      name_temp <- names(template_list)[i]
      template_temp <- template_list[[i]]

      if (!("Cal.Name" %in% colnames(template_temp))) {
        stop(paste("The sheet", name_temp, "does not contain the column 'Cal.Name'."))
      } 
    }

    # Check if the default template name is found in the template list
    if (!(Template_name %in% names(template_list))) {
      stop("The default template name is not found in the template.xlsx file. Please adjust the template.xlsx or the default template name.")
    }

checkboxesColumns <- c(1:15)


# Source global functions

#source("./helper/helper_data_upload.r")
source("server.r")

icon_data <- base64enc::dataURI(file = "Dependencies/icon.png", mime = "image/png")

source("./Modules/UI/ui.r")
script_path <- get_script_directory()
source("./helper/global_functions.r")
# Create and run the Shiny app
app <- shinyApp(server = server, ui = ui)



cat("parsing args")
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 2){
    launch = tolower(args[1]) == "true"
    port = as.integer(args[2])
}else{
    launch = TRUE
    port = 3000
}
cat(sprintf("Launching QuantyFey on port %d", launch, port))
runApp(app, launch.browser = launch, port = port)

