## Tutorial for Apptainer Version

This file is additional file for the correct installation of apptainer for Windows, Linux, and MacOS

> **Note:** The apptainer Version currently only works with the provided Datasets. Efforts are put into it, to make the Templates adjustable, and the PDF-report available. Please use other forms of installation for using QuantyFey as intended.

---

## Windows

This guide will walk you through setting up all necessary components to run QuantyFey on Windows using WSL (Windows Subsystem for Linux) and Apptainer.

## Prerequisites

- Windows 10 version 2004 and higher (Build 19041 and higher) or Windows 11
- At least 8 GB of RAM (16 GB recommended)
- Administrator rights on your Windows machine
- At least 10 GB of free disk space

## Step 1: Install Windows Subsystem for Linux (WSL)

1. Open PowerShell or Windows Command Prompt as Administrator
2. Run the following command to install WSL with Ubuntu:
   ```powershell
   wsl --install
   ```
3. Restart your computer when prompted
4. After restart, a Ubuntu window will open automatically
5. Create a UNIX username and password when prompted

If the automatic installation doesn't work, follow these manual steps:

1. Enable WSL by running in PowerShell (Admin):
   ```powershell
   dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
   dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart
   ```
2. Restart your computer
3. Download and install the [WSL2 Linux kernel update package](https://wslstorestorage.blob.core.windows.net/wslblob/wsl_update_x64.msi)
4. Set WSL 2 as default:
   ```powershell
   wsl --set-default-version 2
   ```
5. Install Ubuntu from the Microsoft Store

## Step 2: Set Up Ubuntu in WSL

1. Open Ubuntu from the Start menu
2. Wait for initial setup to complete
3. Update the package system:
   ```bash
   sudo apt update && sudo apt upgrade -y
   ```

## Step 3: Install Apptainer Dependencies

1. In your Ubuntu terminal, install necessary dependencies:
   ```bash
   sudo apt install -y \
       build-essential \
       libseccomp-dev \
       pkg-config \
       squashfs-tools \
       cryptsetup \
       curl \
       wget \
       git
   ```

## Step 4: Install Go (Required for Apptainer)

1. Install Go using the following commands:
   ```bash
   export GOLANG_VERSION=1.19.9
   wget https://dl.google.com/go/go${GOLANG_VERSION}.linux-amd64.tar.gz
   sudo tar -C /usr/local -xzf go${GOLANG_VERSION}.linux-amd64.tar.gz
   rm go${GOLANG_VERSION}.linux-amd64.tar.gz
   ```

2. Add Go to your PATH by adding these lines to your `~/.bashrc`:
   ```bash
   echo 'export PATH=/usr/local/go/bin:$PATH' >> ~/.bashrc
   echo 'export PATH=$PATH:$HOME/go/bin' >> ~/.bashrc
   source ~/.bashrc
   ```

## Step 5: Install Apptainer

1. Clone and install Apptainer:
   ```bash
   git clone https://github.com/apptainer/apptainer.git
   cd apptainer
   git checkout v1.1.9
   ./mconfig
   make -C builddir
   sudo make -C builddir install
   ```

2. Verify the installation:
   ```bash
   apptainer --version
   ```

## Step 6: Set Up QuantyFey

1. Create a directory for QuantyFey (in Windows):
   ```powershell
   mkdir "%USERPROFILE%\Documents\QuantyFey"
   ```

2. Copy the following files to your Documents\QuantyFey folder:
   - `QuantyFey.sif`
   - `launch_quantyfey.bat`
   - `launch_quantyfey.sh`

## Running QuantyFey

1. Open File Explorer and navigate to your QuantyFey folder
2. Double-click `launch_quantyfey.bat`
3. The script will:
   - Create an overlay file if it doesn't exist
   - Set up the necessary directories
   - Launch QuantyFey with your Documents folder mounted

## Troubleshooting

### Common Issues and Solutions

1. **WSL not installed properly:**
   ```powershell
   wsl --status
   ```
   Should show WSL 2 as the default version.

2. **Apptainer not found:**
   ```bash
   which apptainer
   ```
   If not found, try reinstalling Apptainer.

3. **Permission denied errors:**
   - Ensure you're using `--fakeroot` flag (already included in scripts)
   - Check file permissions in WSL:
   ```bash
   ls -la ~/Documents
   ```

4. **Overlay creation fails:**
   - Ensure you have enough disk space
   - Try running with administrator privileges

### Getting Help

If you encounter any issues:
1. Check the error message in the terminal
2. Ensure all prerequisites are properly installed
3. Verify WSL is running in version 2:
   ```powershell
   wsl --list --verbose
   ```

## Additional Notes

- The Windows Defender firewall might ask for permission when first running the container. Allow the connection when prompted.
- WSL might need to be restarted occasionally: `wsl --shutdown` in PowerShell
- Keep your WSL Ubuntu system updated: `sudo apt update && sudo apt upgrade`
- The overlay file provides persistence between sessions and is created automatically

## Uninstallation

To completely remove the installation:

1. Delete the QuantyFey folder
2. (Optional) Uninstall WSL:
   ```powershell
   wsl --unregister Ubuntu
   wsl --uninstall
   ```
