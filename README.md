The following instructions have been successfully used to build a shiny servers for use with this program on 05 June 2025.

# Build a Ubuntu 24.04.02 LTS server.
- Download the iso file for Ubuntu 24.04.02 LTS server
	- https://ubuntu.com/download/server
- Burn this iso file to a USB device with something like Rufus.
	- https://portableapps.com/apps/utilities/rufus-portable
- Use the USB device to boot into the installation process.  Leave all setting as default with the following exceptions.
	- An admin username and password will be required.  Clearly mark the server with this information.
	- Select the option to "Installed OpenSSH server" so that the steps in the following sections and be applied via "copy-and-paste" from a remote computer.
- After the installation process finished the server will initiate a reboot. Remove the USB device when prompted.
- Use the admin username and password to log into the system to obtain the IP address with the following command.
	- hostname -I
- Log out.
	- exit
- Execute the remaining steps via SSH from a remote computer.

# Install software
- sudo apt-get install r-base
- sudo su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
- sudo apt-get install gdebi-core
- wget https://download3.rstudio.org/ubuntu-20.04/x86_64/shiny-server-1.5.23.1030-amd64.deb
- sudo gdebi shiny-server-1.5.23.1030-amd64.deb
- sudo apt-get install texlive-latex-extra

# Install R packages
- sudo su - -c "R -e \"install.packages('rmarkdown')\""
- sudo su - -c "R -e \"install.packages('knitr')\""
- sudo su - -c "R -e \"install.packages('nleqslv')\""
- sudo su - -c "R -e \"install.packages('deSolve')\""
- sudo su - -c "R -e \"install.packages('tinytex')\""
- sudo su - -c "R -e \"tinytex::install_tinytex()\""

# Install the COHb program
- cd /srv/shiny-server
- sudo git clone https://github.com/SaltLakeTechnicalCenter/COHb.git
- sudo chmod 777 /srv/shiny-server/COHb