#!/bin/bash

# Display copyright and license information
echo
echo "QuantyFey  Copyright (C) 2025  Markus Aigensberger"
echo "This program comes with ABSOLUTELY NO WARRANTY; refer to the LICENCE file for details."
echo "This is free software, and you are welcome to redistribute it"
echo "under the terms of the GNU General Public License v3.0; refer to the LICENCE file for details."
echo



# Set the script directory
SCRIPT_DIR=$(dirname "$0")

# Set the path to the portable R executable
R_PATH="/usr/bin/R"

# Check if Rscript exists
if [ ! -x "$R_PATH" ]; then
    echo "ERROR: Rscript not found in $R_PATH"
    echo "Consider installing it with 'sudo apt install r-base'"
    exit 1
else
    echo "R path: $R_PATH"
fi

# Set library path for the portable R version
R_LIBS_USER=$(realpath "$SCRIPT_DIR/library")
echo "R_LIBS_USER: $R_LIBS_USER"

# Create R_LIBS_USER directory if it doesn't exist
mkdir -p "$R_LIBS_USER"

# Set Pandoc Path
RSTUDIO_PANDOC=$(realpath "$SCRIPT_DIR/pandoc-3.6.3")

# Check if Pandoc exists
if [ ! -x "$RSTUDIO_PANDOC" ]; then
    echo "ERROR: Pandoc not found in $RSTUDIO_PANDOC"
    echo "Consider installing it with 'sudo apt install pandoc'"
    exit 1
else
    echo "RSTUDIO_PANDOC: $RSTUDIO_PANDOC"
fi

# Check if cmake exists
if [ ! -x "cmake" ]; then
    echo "ERROR: cmake not found"
    echo "Consider installing it with 'sudo apt install cmake'"
    exit 1
else
    echo "cmake found"
fi

# Ensure renv is installed
echo 
"$R_PATH" -e "if (!requireNamespace('renv', quietly = TRUE)){ cat('Installing renv package...'); install.packages('renv', repos = 'https://cloud.r-project.org/'); }"
if [ $? -ne 0 ]; then
    echo "ERROR: Failed to install renv package!"
    exit 1
else
    echo "renv package available"
fi

# Run the R script
echo "Running R Script... $SCRIPT_DIR"
"${R_PATH}script" --vanilla "$SCRIPT_DIR/QuantyFey-Script.R"

echo "Script completed."

