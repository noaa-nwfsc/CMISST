# Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an "as is" basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## Files

-   createStoredObjects.R - Code to create or update RData files used by CMISST; this only needs to be run if the data files do not exist or need to be updated
-   crossValidation.R - Defines a function that runs cross-validate of the CMISST model; called by runCMISST.R
-   get_index.R - Creates a function that calculates the CMISST index, given a response dataset and the SST or SSH data to use
-   getOceanData.R - Creates a function that will extract SST or SSH data to be used by CMISST for the user-defined spatial and temporal extents
-   makePlots.R - Creates several functions that allow the user to plot CMISST results or create a table of results
-   runCMISST.R - Primary user file for running CMISST; see notes below
-   updateCMISST.R - Creates a function that creates the CMISST index and optionally runs crossvalidation, based on user-defined parameters; typically called by runCMISST.R, but can be called manually as well

## Data sources

-   responseData.RData
-   land.RData
-   oceanSSTData.RData
-   oceanSSHData.RData (this file is too large to upload to GitHub - it will have to be created by the user)

To update the data files, you will need to download the source data files, store them in the data folder (use data/SST/ and data/SSH/ subfolders for SST and SSH data), and run the code in createStoredObjects.R

Response data all come from Columbia Basin Research's Data Access in Real Time (DART) website. Adult counts were obtained here: <https://www.cbr.washington.edu/dart/query/adult_daily> and Smolt to Adult Survival (SAR) data were obtained here: <https://www.cbr.washington.edu/dart/query/pit_sar_esu>. The code does not enforce this, but it's best to use a log link for counts and a logit link for SARs.

SST data are from ERSST (<https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/>). However, I obtained them where they have been reposted in a slightly different format here: <https://psl.noaa.gov/data/gridded/data.noaa.ersst.v5.html>. This includes the long-term mean (sst.mon.ltm.1991-2020.nc) and the annual values (sst.mnmean.nc - this is a large file containing data from 1854 to 2025)

ERSST is a 2x2 degree global dataset

Reference: Huang et al, 2017: Extended Reconstructed Sea Surface Temperatures Version 5 (ERSSTv5): Upgrades, Validations, and Intercomparisons. Journal of Climate, <https://doi.org/10.1175/JCLI-D-16-0836.1>

Use acknowledgment: The NOAA Extended Reconstructed Sea Surface Temperature (ERSST) data are provided by the NOAA National Centers for Environmental Information (NCEI)

SSH Data from: <https://psl.noaa.gov/data/gridded/data.godas.html>, under the heading "sea surface height relative to geoid", which includes the long-term mean file (sshg.mon.ltm.1991-2020.nc) and the annual SSH files (sshg.YYYY.nc). The resolution of these data is 0.33x1 degree global dataset

## Running CMISST

R/run_CMISST.R contains all the code needed to run the model.

I would suggest running this code line by line. The top portion of this script sets the parameters to use and sources functions. The middle of the script runs the model initially. The bottom of the script is to plot results.

Once the functions have been defined, you can rerun updateCMISST() with the same parameters or after altering parameters.
