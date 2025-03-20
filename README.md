
# Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an "as is" basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.


## Data sources

 -  responseData.RData
 -  land.RData
 -  oceanSSTData.RData
 -  oceanSSHData.RData (this one is too large to upload, so will have to be created by the user)
 
To update the data files, you will need to download the source data files, store them in the data folder (use /SST and /SSH subfolders for SST and SSH data), and run the code in createStoredObjects.R

SST data are from ERSST (https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/).
But I obtained them where they have been reposted in a slightly different format here:
https://psl.noaa.gov/data/gridded/data.noaa.ersst.v5.html.

ERSST is a 2x2 degree gloabal dataset

Reference: Huang et al, 2017: Extended Reconstructed Sea Surface Temperatures Version 5 (ERSSTv5): Upgrades, Validations, and Intercomparisons. Journal of Climate, https://doi.org/10.1175/JCLI-D-16-0836.1

Use acknowledgment: The NOAA Extended Reconstructed Sea Surface Temperature (ERSST) data are provided by the NOAA National Centers for Environmental Information(NCEI)

SSH Data from: https://psl.noaa.gov/data/gridded/data.godas.html and is 0.33x1 degree

## Running CMISST

R/run_CMISST.R contains all the code needed to run the model.

I would suggest running this code line by line.  The top portion of this script sets the parameters to use and sources functions.  The middle of the script runs the model initially.  The bottom of the script is to plot results.

Once the functions have been defined, you can rerun updateCMISST() with the same parameters or after altering parameters.

