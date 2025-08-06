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

# Get the major.minor version of R
r_version <- paste0(R.version$major, ".", strsplit(R.version$minor, "\\.")[[1]][1])
lockfile_name <- paste0("renv.lock.", r_version)

# Check and copy version-specific lockfile if it exists
start <- Sys.time()
if (file.exists(lockfile_name)) {
  message("Found lockfile for R version ", r_version, ": ", lockfile_name)
  file.copy(lockfile_name, "renv.lock", overwrite = TRUE)
  message("Copied ", lockfile_name, " to renv.lock. Restoring environment...")
  renv::restore()
} else if (file.exists("renv.lock")) {
  message("Default renv.lock found. Restoring environment...")
  renv::restore()
} else {
  message("No lockfile found for R version ", r_version, ". Skipping environment restoration.")
}
end <- Sys.time()

message("Time taken for renv restore: ", round(difftime(end, start, units = "secs"), 2), " seconds")

# Check and install additional packages
additional_packages <- c("tinytex", "shiny")
install_if_missing(additional_packages)

# Final message
message("Script execution complete. All required packages are installed.")
