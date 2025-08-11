## Tutorial for Apptainer Version

This file is additional file for the correct installation of apptainer for Windows, Linux, and MacOS

---
### Prerequisites
#### **MacOS**
- **Homebrewer**:
- **Linux Virtual Machine (VM)** (Lima)
- [**Apptainer**](https://apptainer.org/docs/admin/main/installation.html) in the Lima VM

#### **Windows**
- **Windows Subsystem for Linux (WSL)**
- [**Apptainer**](https://apptainer.org/docs/admin/main/installation.html) in WSL

---
## Installation
## Linux:

1. install apptainer as described on their [official website](https://apptainer.org/docs/admin/main/installation.html#install-from-github-release-rpms)
2. Launch the apptainer by using the `launch_quantyfey.sh`-file.


## Windows
This guide sets up **WSL2** with Debian, installs Apptainer inside it, and runs a prebuilt `.sif` file.
> Note: This version is not recommanded, as the setup is a bit more complicated. The standalone version is recommanded for windows systems.

> Works on Wiundows 10 (2004+) and Windows 11.
> Make sure you're using WSL2, not WSL1.

1. Enable WSL2
Open **PowerShell** as Administrator:
``` powershell
wsl --install
```
Restart if prompted.
Check your WSL version:
``` powershell
wsl --version
```
If it shows "Version 2" or higher, you're fine.
If not, update to WSL2:
``` powershell
wsl --set-default-version 2
```

2. Install Debian in WSL2
From PowerShell:
``` powershell
wsl --install -d Debian
```
Check it's running under WSL2:
``` powershell
wsl --list --verbose
```

It should say `Debian` and `VERSION 2`

3. Start Debian
``` powershell
wsl -d Debian
```

4. Install Apptainer inside Debian
``` bash
sudo apt install -y wget
wget https://github.com/apptainer/apptainer/releases/download/v1.4.1/apptainer_1.4.1_amd64.deb
sudo apt install -y ./apptainer_1.4.1_amd64.deb
rm -f ./apptainer_1.4.1_amd64.deb
apptainer version
```

5. Bind and run the Apptainer Version
Run the `launch_quantyfey.bat` file in the downloaded `QuantyFey_apptainer` folder.


# #MacOS

On macOS a small Linux VM with [Lima] and run the already built `QuantyFey.sif` container.
> **Note:** This version is not recommanded, as the VR completely needs to emulate a Linux machine, which is inefficient.
> The launch using R terminal, or RStudio is recommanded for MacOS systems. Referr to the [tutorial.md](/tutorial.md) for more information.

> Works on Apple Silicon and Intel Macs. 
> If perfomrance is slow, use the vz config on Apple Silicon or Intel. Use the QEMU config only if uv is not available.

## Requirements
- macOS 12 or newer
- terminal
- Admin rights
- `QuantyFey.sif` file

1. Install **Homebrew** and **Lima**
Open Terminal and run
``` bash
# Install Homebrew (skip if you already have it)
 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Make sure brew is in your PATH (Apple Silicon)
 eval "$(/opt/homebrew/bin/brew shellenv)" 2>/dev/null || true

# Install Lima and the extra guest agents
 brew update
 brew install lima lima-additional-guestagents
```

> **Note:** Homebrew is the package manager we use. Lima runs a Linux VM on your Mac. The "additional guest agents" are needed for x86_64 VMs on Apple Silicon.

2. Create a Linux VM configuration
Create a file that tells Lima what to start. This VM uses Ubuntu 22.04 (Jammy) x86_64 and forwards port 3000.

``` bash
cat > /tmp/apptainer-qemu-x86_64.yaml <<'YAML'
vmType: "qemu"
arch: "x86_64"
images:
  - location: "https://cloud-images.ubuntu.com/jammy/current/jammy-server-cloudimg-amd64.img"
    arch: "x86_64"

mounts:
  - location: "~"
    writable: true
  - location: "/tmp/lima"
    writable: true

# Make http://localhost:3000 on the VM available on your Mac
portForwards:
  - guestPort: 3000
    hostPort: 3000
YAML
```

3. Create and start the VM
``` bash
limactl create --name apptainer-vm /tm/apptainer-qemu-x86_64.yaml
limactl start apptainer-vm
```
 > **Note:** if you see an error about a missing guest agent, install `lima-additional-guestagens` (done in step 1), then delete and recreate the VM:
``` bash
limactl delete -f apptainer-vm
limactl create --name apptainer-vm /tmp/apptainer-qemu-x86_64.yaml
limactl start apptainer-vm
```

4. Open a shell into the VM
``` bash
limactl shell apptainer-vm
```
Your macOS home folder will appear inside the VM at the same path. For example, your Mac's `~/Documents` is `/Users/<your-mac-username>/Documents` inside the VM too.

5. Install Apptainer inside the VM
Run these **inside the VM**:
``` bash
sudo apt-get update
sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:apptainer/ppa
sudo apt-get update
sudo apt-get install -y apptainer uidmap fuse-overlayfs squashfs-tools
apptainer --version
```
The official Apptainer PPA proveds ready-made packages for Ubuntu 22.04 on both amd64 and arm64.

6. Run your `.sif` file
Still **inside the VM**, change to the folder that has your `.sif` on your Mac, then run it.

``` bash
# Example: if your SIF is in ~/Downloads/QuantyFey_apptainer on your Mac
cd /Users/<your-mac-username>/Downloads/QuantyFey_apptainer

# Bind your Documents folder into the container at /user_host_home (adjust if needed)
apptainer run --bind "/Users/<your-mac-username>/Documents:/user_host_home" ./QuantyFey.sif
```
If the browser doesn't open automatically open a browser and go to `https://localhost:3000`.
