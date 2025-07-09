# Extract data from global data sets
# User sets extent and return object type

# To run this code, you will need to download the original data files
# and store them in data/SST/ or data/SSH
#  This includes teh monthly data as well as the long term mean

# SST data are from ERSST (https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/)
# But I obtained them where they have been reposted in a slightly different format here:
#   https://psl.noaa.gov/data/gridded/data.noaa.ersst.v5.html
# ERSST is a 2x2 degree gloabal dataset

# references: Huang et al, 2017: Extended Reconstructed Sea Surface Temperatures Version 5 (ERSSTv5): Upgrades, Validations, and Intercomparisons. Journal of Climate, https://doi.org/10.1175/JCLI-D-16-0836.1
# climatology: Climatology is based on 1971-2000 SST, Xue, Y., T. M. Smith, and R. W. Reynolds, 2003: Interdecadal changes of 30-yr SST normals during 1871.2000. Journal of Climate, 16, 1601-1612.
# acknowledgment: The NOAA Extended Reconstructed Sea Surface Temperature (ERSST) data are provided by the NOAA National Centers for Environmental Information(NCEI)

# SSH Data from: https://psl.noaa.gov/data/gridded/data.godas.html

library(ncdf4)

#***************************************************************
# Create the function
#***************************************************************
getOceanData<-function(dataSet='ERSST',
                 min.lon=158, max.lon=246,
                 min.lat=10, max.lat=62,
                 years=seq(1980, 2020, 1), months=seq(1,12,1)) {

  # Check conditions
  if (min.lon < 0 | min.lon > 360 | max.lon < 0 | max.lon > 360) return (cat("Longitude is in degrees east (Tokyo is 139.8, Seattle is 237.6)"))
  if (min.lat < -90 | min.lat > 90 | max.lat < -90 | max.lat > 90) return (cat("Latitude is in degrees from the equator (should not exceed 90)"))

  year_mo<-data.frame(year=rep(years, each=length(months)), month=rep(months, length(years)),
                      label=paste(rep(years, each=length(months)), rep(months, length(years)), sep = "_"))
  
  #***************************************************************
  # Extract the location data
  #***************************************************************

  # open a (any) netCDF file to extract lats and longs
  if (dataSet == 'ERSST') {
    ncfname <- "data/SST/sst.mon.ltm.1991-2020.nc"
    #ncfname <- "data/SST/sst.mon.ltm.1981-2010.nc"
    ncin <- nc_open(ncfname)
    lons <- ncvar_get(ncin,"lon")
    lats <- ncvar_get(ncin,"lat",verbose=F)
    sst.ltm <- ncvar_get(ncin,"sst")
    nc_close(ncin)
  }
  if (dataSet == 'SSH') {
    ncfname <- "data/SSH/sshg.mon.ltm.1991-2020.nc"
    ncin <- nc_open(ncfname) # open it
    lons <- ncvar_get(ncin,"lon")
    lats <- ncvar_get(ncin,"lat",verbose=F)
    ssh.ltm <- ncvar_get(ncin,"sshg")
    nc_close(ncin)
  }
  #print(ncin) # metadata (file must be opened)
  
  #***************************************************************
  # Define the grid of interest
  #***************************************************************
  
  # Index the locations in the file
  lon.index<-which(lons >= min.lon & lons < max.lon) 
  lat.index<-which(lats > min.lat & lats < max.lat)
  lon.subset <- lons[lon.index]
  lat.subset <- lats[lat.index]

  # Loop over files, extract the anomaly data, and store them in a single array
  returnData<-array(rep(x = NaN, nrow(year_mo) * length(lon.subset) * length(lat.subset)),
                  dim = c(length(lon.subset), length(lat.subset), nrow(year_mo)),
                  dimnames = list(lon.subset, lat.subset, year_mo$label))
  
  if (dataSet == 'ERSST') {
    # All of the sst data are in one file
    ncfname <- "data/SST/sst.mnmean.nc"
    ncin <- nc_open(ncfname)
    sst <- ncvar_get(ncin,"sst") #float sst[lon,lat,time]
    psl.time <- ncvar_get(ncin,"time")
    dates <- format(as.Date("1800-01-01") + psl.time, "%Y_%m")
    dates <- sub("(.+)_0?(.+)", "\\1_\\2", dates)
    dimnames(sst) <- list(seq(0,358,2), seq(-88,88,2), dates)
    # get anomaly, if requested
    for (mo in 1:nrow(year_mo)) {
      returnData[, , mo] <- sst[,, dates == year_mo$label[mo]] - sst.ltm[,, year_mo$month[mo]]
    }
    nc_close(ncin)
    returnData <- returnData[,89:1,] # I think this would fix the reversed latitude
  }
  if (dataSet == 'SSH') {
    # Loop over files (one per year) and get ssh data
    for (ym in 1:nrow(year_mo)) {
      if (year_mo$month[ym]==1) {
        ncfname <- paste0("data/SSH/sshg.",year_mo$year[ym],".nc")
        ncin <- nc_open(ncfname)
        ssh.temp <- ncvar_get(ncin,"sshg")
      }
      # get anomaly, if requested
      ssh.temp[,, year_mo$month[ym]]<-ssh.temp[,, year_mo$month[ym]] - ssh.ltm[,, year_mo$month[ym]]
      returnData[,,ym]<-ssh.temp[lon.index, lat.index, year_mo$month[ym]]
      if (year_mo$month[ym]==12) nc_close(ncin)
    }
  }
    
  return(returnData)
}
