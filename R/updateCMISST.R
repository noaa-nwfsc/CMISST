# This script defines the function that updates CMISST with given parameters
#
#  Created by Brian Burke, NOAA Fisheries, NWFSC
#    brian.burke@noaa.gov
#


#************************************
# Function to calculate the index and return results
#************************************
updateCMISST <- function() {
  min.lon = input.long[1]
  max.lon = input.long[2]
  min.lat = input.lat[1]
  max.lat = input.lat[2]
  years = seq(input.years[1], input.years[2], 1)

  response.tmp <- response
  # Lag the response variable
  response.tmp$year <- response.tmp$year - as.numeric(input.lag)
  # refine to just the years asked for and just the requested response variable
  response.tmp <- response.tmp[response.tmp$year %in% years, c('year', input.stock)]
  colnames(response.tmp) <- c('year', 'val')
  # sometimes the response starts after the input.years, so limit the years in the ocean data 
  #years <- years[years %in% response.tmp$year]
  years <- years[years >= min(response.tmp$year)]
  
  # Log (if requested) and scale the response variable
  if(input.log) response.tmp$val <- log(response.tmp$val)
  response.tmp$val.scl <- scale(response.tmp$val)

  # Which years are being fit to?
  years.fit <- years[years %in% response.tmp$year]
  
  # Calculate the CMISST index
  cmisst <- get_CMISST_index(response = response.tmp[,c("year","val.scl")],
                             oceanData = oceanData,
                             min.lon = min.lon, max.lon = max.lon,
                             min.lat = min.lat, max.lat = max.lat,
                             years = years, years.fit = years.fit,
                             months = months)

  if (input.loocv) {
    loocv <- LOO_CV(response = response.tmp[,c("year","val.scl")],
                    oceanData = oceanData, loocvYears = loocvYears,
                    min.lon = min.lon, max.lon = max.lon,
                    min.lat = min.lat, max.lat = max.lat,
                    years = years.fit,
                    months = months)
    return(append(cmisst, loocv))
  } else return(cmisst)
  # Returns index as a list
  #  cmisst[[1]] contains 7 columns (as one list item): year, 4 seasonal indices,
  #              the scaled response, and the original response
  #  cmisst[[2]] winter spatial covariance values (for maps)
  #  cmisst[[3]] spring spatial covariance values (for maps)
  #  cmisst[[4]] summer spatial covariance values (for maps)
  #  cmisst[[5]] autumn spatial covariance values (for maps)
  #  cmisst[[6]] lat, long min and max, 1 list item
  #  cmisst[[7]] loocv, if requested
}

