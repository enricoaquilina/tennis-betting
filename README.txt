----------------------------------------------------------------------------------------------
Required software to be installed on your system:
----------------------------------------------------------------------------------------------
1. p7zip-rar(sudo apt-get install p7zip-rar)
2. plyr CRAN package(install.packages(pkgs='plyr'))
3. data.table CRAN package(install.packages('data.table'))
----------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------
Instructions on how to run the scripts:
----------------------------------------------------------------------------------------------
The housekeeping.R script accepts 2 arguments. The first one is the current directory where the scripts are and the second is the year you want to extract tennis matches for. Please make sure that the directory you specify has a folder called 'data', with the raw zipped files. Also make sure that the specified argument directory does not include any slashes at the end! The extracted files will eventually be placed in a created folder called 'processed'. I amalgamated the files together to extract 1 file per year.
----------------------------------------------------------------------------------------------
Command used to run the first script
----------------------------------------------------------------------------------------------
Rscript housekeeping.R /home/enrico/Desktop/sds 2012
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

To run the second script make sure that there is a folder with 2 folders inside called 'Tennis-Data' and 'Betfair'. The first command-line argument is the directory of where these 2 folders reside. The second argument is the year you want to process.

Command used to run the second script
----------------------------------------------------------------------------------------------
Rscript strategy.R /home/enrico/Desktop/sds/extracted 2012


