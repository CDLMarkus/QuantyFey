#!/bin/bash

# Display copyright and license information
echo
echo "QuantyFey  Copyright (C) 2025  Markus Aigensberger"
echo "This program comes with ABSOLUTELY NO WARRANTY; for details type \"show w\"."
echo "This is free software, and you are welcome to redistribute it"
echo "under the terms of the GNU General Public License v3.0; type \"show c\" for details."
echo

# Implement 'show w' and 'show c' commands
while true; do
    read -p 'Type "show w" for warranty, "show c" for license, or press Enter to continue: ' LICENSE_CMD
    case "$LICENSE_CMD" in
        "show w"|"SHOW W"|"Show W")
            echo
            echo "NO WARRANTY: This program is distributed in the hope that it will be useful,"
            echo "but WITHOUT ANY WARRANTY; without even the implied warranty of"
            echo "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the"
            echo "GNU General Public License for more details."
            echo
            ;;
        "show c"|"SHOW C"|"Show C")
            echo
            echo "LICENSE: This program is free software: you can redistribute it and/or modify"
            echo "it under the terms of the GNU General Public License as published by"
            echo "the Free Software Foundation, either version 3 of the License, or"
            echo "(at your option) any later version."
            echo
            echo "You should have received a copy of the GNU General Public License"
            echo "along with this program.  If not, see https://www.gnu.org/licenses/."
            echo
            ;;
        "")
            break
            ;;
        *)
            ;;
    esac
done
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