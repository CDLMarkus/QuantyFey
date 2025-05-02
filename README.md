<table style="border-collapse: collapse; border: none; margin: 0; padding: 0;">
  <tr>
    <td style="border: none; padding: 0; vertical-align: middle;">
      <img src="Dependencies/icon.ico" alt="Icon" width="100">
    </td>
    <td style="border: none; padding: 0; vertical-align: middle;">
      <h1 style="font-size: 100px; margin: 0;">QuantyFey</h1>
    </td>
  </tr>
</table>

# **Project Overview**

**QuantyFey** is a Shiny application for the **visualization, analysis, and quantification** of **mass spectrometry (MS) data** using **external calibration**.  
It is specifically designed to address **intensity drifts** in datasets, offering multiple **correction strategies** to ensure accurate quantification.  
QuantyFey is compatible with **Windows** operating systems only.

## **Target Audience**

QuantyFey is intended for users with a basic understanding of mass spectrometry and data analysis, including:  
- **Analytical chemists** conducting MS data quantification.  
- **Laboratory technicians** processing MS results in research or industrial settings.

## **Key Features**

QuantyFey provides:
- commonly applied **drift correction** methods:
    - **Internal Standard (IS) correction**
    - **Drift Correction** using statistical models
    - **Bracketing** Quantification
- **Interactive Regression Model Optimization** - manual adjustment of model, weights, standard levels etc.
- **Automatic Optimization Module** - automatic selection of **linear** or **quadratic** regression model, and selection of appropriate standards.

---
# **Getting Started**

## **Prerequisites**
### **Windows**:
- **RTools**: Version 4.2 must be installed.
  - **Option A**: Use the [official installer](https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html).
  - **Option B**: From the included R Portable:
    - Run `R.exe` in the `R-portable/bin/` folder.
    - Execute the following commands:
      ```r
      install.packages("installr", repos = "https://cloud.r-project.org/")
      installr::install.Rtools()
      ```
    - Follow the installer instructions.
    - **Note:** An error may occur during finalization. Dismiss the error and proceed with the installation.
 
### **Linux**

To make sure `QuantyFey` runs smoothly on Ubuntu, please make sure the following dependencies are installed:
1. Install `r-base`, `pandoc`, and `cmake` by running the following command in your terminal:
   ``` bash
   sudo apt install r-base pandoc cmake
   ```
2. Once these dependencies are installed, you can proceed with using or setting up `QuantyFey` as described in the rest of the documentation.


### **Mac**

Install Homebrew (if not already installed): Open a terminal and run:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Use Homebrew to install the required dependencies:

``` bash
brew install r pandoc cmake
```
Once these dependencies are installed, you can proceed with using or setting up QuantyFey as described in the rest of the documentation.
**Note:** Does not currently work..!


## **Installation**

1. Download the zip file download [QuantyFey.zip](release/v0.0.1windows) (or for [mac version](release/v0.0.1mac) or [linux version](release/v0.0.1linux))
2. Extract the folder to a desired location.
3. Navigate to the `QuantyFey` folder and execute `QuantyFey.bat` (approval may be required).
4. The console will open, and all required packages will be installed automatically (this process may take up to 20 minutes).
5. Once installation is complete, the application will launch in your default web browser.

---

# **Using the App**

Discription of the app and tutorials on how to effectively use it can be found in the tutorial folder.
The app provides the user the tools for versatile and interactive quantification of targeted Mass Spectrometry Data using external calibration. Especially when Intensity Drift is observed during the Measurement, the app provides the user with tools to effectively handle these drift. This app works as an extra software to already integrated mass spectrometry data, and does not provide any integration cabailities, but rather offers the user an interactive tool for efficient quantification.

We hope, this app can help save a lot of nerves during your Quantification efforts!

<p align="left">
  <img src="images/keep_calm.png" alt="Keep Calm and QuantyFey!" width="300">
</p>




