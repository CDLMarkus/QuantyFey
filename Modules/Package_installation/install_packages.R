# Print the current library being used
current_library <- .libPaths()[1]
message("Current user library: ", current_library)

# Function to check and install packages if missing
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing package: ", pkg)
      install.packages(pkg, lib = current_library, repos = "https://cran.rstudio.com/")
    } else {
      message("Package already installed: ", pkg)
    }
  }
}

# Check and install required packages
required_packages <- c("renv", "here", "rstudioapi")
install_if_missing(required_packages)

# Load renv to check for lock file
library(renv)

start <- Sys.time()
# Check if renv.lock file exists in the current working directory
if (file.exists("renv.lock")) {
  message("Lock file found. Restoring environment...")
  renv::restore()
} else {
  message("No lock file found. Skipping environment restoration.")
}
end <- Sys.time()

message("Time taken for renv restore: ", round(difftime(end, start, units = "secs"), 2), " seconds")
# Check and install additional packages (tinytex and shiny)
additional_packages <- c("tinytex", "shiny")
install_if_missing(additional_packages)

# Final message
message("Script execution complete. All required packages are installed.")