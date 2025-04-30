@echo off
setlocal

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