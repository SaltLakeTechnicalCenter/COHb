The following instructions have been successfully tested for installation of this program on a DOL computer as of 11 October 20222.

# Install R and RStudio
- Locate the “Service Central” icon on your desktop.
- Find “Software Install”.
- Use the following information to request R and R Studio 2022 to be installed on your computer:
    - Computer name of the laptop or desktop you would like the software installed on? (This is the name of your computer. It is the first line of information displayed on the top, right corner of your desktop.)
    - Select Software: (R and R Studio 2022)
    - Does your agency already have a software license purchased? (No)
- Press the “Request” button at the bottom of the page
- You will get a notification through teams informing you that RStudio is now available in Software Center for you to install
- Find “Software Center” on your desktop and use it to install the latest version of RStudio (R-4.1.2_And_RStudio-2021.09.1-372)

# Get the COHb software
- Available at https://github.com/SaltLakeTechnicalCenter/COHb
- Use the green “Code” button to “Download Zip”.
- This zip file should download to your “Downloads” file.
- Extract these this folder of files to somewhere that makes sense. I recommend placing them in a folder inside of you “Documents” folder (“C:\Users\your_user_name\COHb”) since this is the default place for RStudio to look for things.

# Install R packages
- Open RStudio and locate the “Console” to the left. Copy and paste the following commands into the console:
	- install.packages("knitr")
	- install.packages("nleqslv")
	- install.packages("deSolve")
	- install.packages("shiny")
	- install.packages('tinytex')
	- tinytex::install_tinytex()

# Run the program
- Open RStudio and locate the “Files” window. Navigate to where you extracted the COHb files.
- Open the server.R file.
- Locate and press the “Run App” button that appears at the top of the new “server.R” window.

# Generate a report
- To generate a report you must first enable the “Perform Monte Carlo simulations” under the “Parameters” tab. (It is recommended that 1000 simulations be used for the final calculation.)
- The “Generate Report” button is found under the “Summary” tab.
- The first time you generate a report it will take a long time (approximately 7 minutes). Subsequent reports should generate nearly instantaneously.
