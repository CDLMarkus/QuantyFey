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
    - **Custom Bracketing** Quantification (Assigning calibration data to predefined blocks of the sequence).
    - **Weighted Bracketing** Weighting of calibration curves based on the position of the samples between calibration curves.
- **Interactive Regression Model Optimization** - manual adjustment of model, weights, standard levels etc.
- **Automatic Optimization Module** - automatic selection of **linear** or **quadratic** regression model, and selection of appropriate standards.

---
# **Quick Installation Guide**

## **Prerequisites**
### **Windows**:
- **RTools**: Version 4.2 must be installed.

### **Linux**
multiple prerequisites - please refer to the [`tutorial`](tutorial/tutorial.md) for more information.

---

> Refer to the [`tutorial`](tutorial/tutorial.md) for a more information on installing and using **QuantyFey**.


## **Installation**

1. Download the zip file download [QuantyFey.zip](https://github.com/CDLMarkus/QuantyFey/releases/)
2. Extract the folder to a desired location.
3. Navigate to the `QuantyFey` folder and execute `QuantyFey.bat` (Windows only)(approval may be required).
4. The console will open, and all required packages will be installed automatically (this process may take up to 10 minutes).
5. Once installation is complete, the application will launch in your default web browser.

## **Apptainer Version Installation**

A standalone version based on **Apptainer** is available for download.
This comes with mutliple batch files for installation of wsl, apptainer and other required packages - as well as a launch script (as shell for Linux or batch for Windows).

1. Download and unzip [QuantyFey_apptainer.zip](https://github.com/CDLMarkus/QuantyFey/releases/)
2. Windows: install wsl and apptainer - use `install_wsl.bat` (admin rights required) and `setup.bat`.
3. Launch the app using `launch_windows.bat` or `launch_linux.sh`

> **Note:** For Windows the standalone version is preferred - as writing output is not as easily done with the apptainer version.

---

## **Overview**

![Overview of the QuantyFey Application](tutorial/images/workflow.png)

---

# **Using the App**

Discription of the app and tutorials on how to effectively use it can be found in the [tutorial](tutorial/tutorial.md).
The app provides the user the tools for versatile and interactive quantification of targeted Mass Spectrometry Data using external calibration. Especially when Intensity Drift is observed during the Measurement, the app provides the user with tools to effectively handle these drift. This app works as an additional software to already integrated mass spectrometry data, and does not provide any integration cabailities, but rather offers the user an interactive tool for efficient quantification.

![Graphical Abstract of the App](tutorial/images/graphical_abstract.png)


