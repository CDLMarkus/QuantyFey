#!/bin/bash

# Display copyright and license information
echo
echo "QuantyFey  Copyright (C) 2025  Markus Aigensberger"
echo "This program comes with ABSOLUTELY NO WARRANTY; refer to the LICENCE file for details."
echo "This is free software, and you are welcome to redistribute it"
echo "under the terms of the GNU General Public License v3.0; refer to the LICENCE file for details."
echo


echo "=== DEBUGGING INFORMATION ==="
echo "Current Directory: $(pwd)"
echo "Script Location: $(dirname "$0")"

# Set the path to the portable R executable
R_PATH="$(dirname "$0")/Dependencies/R-Portable/bin/Rscript"
echo "R_PATH: $R_PATH"

# Check if Rscript exists
if [ ! -f "$R_PATH" ]; then
    echo "ERROR: Rscript not found in $(dirname "$0")/Dependencies/R-Portable/bin"
    exit 1
fi

# Set library path for the portable R version
export R_LIBS_USER="$(dirname "$0")/Dependencies/portable R/library"
echo "R_LIBS_USER: $R_LIBS_USER"

# Set Pandoc Path
export RSTUDIO_PANDOC="$(dirname "$0")/Dependencies/pandoc-3.6.3"
echo "RSTUDIO_PANDOC: $RSTUDIO_PANDOC"

# Verify Pandoc
"$RSTUDIO_PANDOC/pandoc" --version
if [ $? -ne 0 ]; then
    echo "ERROR: Pandoc is not found or not working!"
    exit 1
fi

# Run the R script
echo "Running R Script..."
"$R_PATH" --vanilla "$(dirname "$0")/app.R"

# Pause to check output
read -p "Press any key to continue..."