@echo off
setlocal
:: Display copyright and license information
echo.
echo QuantyFey  Copyright (C) 2025  Markus Aigensberger
echo This program comes with ABSOLUTELY NO WARRANTY; for details type "show w".
echo This is free software, and you are welcome to redistribute it
echo under the terms of the GNU General Public License v3.0; type "show c" for details.
echo.

:: Implement 'show w' and 'show c' commands
:license_prompt
set /p LICENSE_CMD=Type "show w" for warranty, "show c" for license, or press Enter to continue: 
if /i "%LICENSE_CMD%"=="show w" (
    echo.
    echo NO WARRANTY: This program is distributed in the hope that it will be useful,
    echo but WITHOUT ANY WARRANTY; without even the implied warranty of
    echo MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    echo GNU General Public License for more details.
    echo.
    goto license_prompt
) else if /i "%LICENSE_CMD%"=="show c" (
    echo.
    echo LICENSE: This program is free software: you can redistribute it and/or modify
    echo it under the terms of the GNU General Public License as published by
    echo the Free Software Foundation, either version 3 of the License, or
    echo (at your option) any later version.
    echo.
    echo You should have received a copy of the GNU General Public License
    echo along with this program.  If not, see https://www.gnu.org/licenses/.
    echo.
    goto license_prompt
)
echo.


echo === DEBUGGING INFORMATION ===
echo Current Directory: %CD%
echo Script Location: %~dp0

:: Set the path to the portable R executable
set "R_PATH=%~dp0Dependencies\R-Portable\bin\Rscript.exe"
echo R_PATH: %R_PATH%

:: Check if Rscript.exe exists
if not exist "%R_PATH%" (
    echo ERROR: Rscript.exe not found in %~dp0R-Portable\bin
    pause
    exit /b
)

:: Set library path for the portable R version
set "R_LIBS_USER=%~dp0Dependencies\R-Portable\library"
echo R_LIBS_USER: %R_LIBS_USER%

:: Create R_LIBS_USER directory if it doesn't exist
if not exist "%R_LIBS_USER%" mkdir "%R_LIBS_USER%"

:: Set Pandoc Path
set "RSTUDIO_PANDOC=%~dp0Dependencies\pandoc-3.6.3"
echo RSTUDIO_PANDOC: %RSTUDIO_PANDOC%

:: Verify Pandoc
"%RSTUDIO_PANDOC%\pandoc.exe" --version
if %ERRORLEVEL% NEQ 0 (
    echo ERROR: Pandoc is not found or not working!
    pause
    exit /b
)

:: Run the R script
echo Running R Script...
"%R_PATH%" --vanilla "%~dp0app.R"

:: Pause to check output
pause