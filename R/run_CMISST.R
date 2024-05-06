# This is the main script to run
#
#  Created by Brian Burke, NOAA Fisheries, NWFSC
#    brian.burke@noaa.gov
#

library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(doBy)

#************************************
# Load any data files
#************************************

# land geo file for plotting the map
load("data/land.Rdata")
# Bonneville Dam Counts
load('data/responseData.RData')
# ERSST data
load('data/oceanSSTData.RData')
oceanData <- oceanData_ERSST

source('R/get_index.R')
source('R/crossValidation.R')

# set parameters (for most users, leave these as they are)
months=seq(1,12,1)
removeBering=FALSE
returnDataType='anom'
returnObjectType='array'


#************************************
#       Define Parameters
#************************************
input.spatialData = "ERSST"

# Input: Choose a stock ----
input.stock = "spCK"
#input.stock = "faCK"
#input.stock = "steel"

# Input: log response? ----
input.log = TRUE

# Input: lag response? ----
input.lag= 2

# Input: Latitude range ----
input.lat = c(10, 62)

# Input: Longitude range ----
input.long= c(158, 246)

# Input: Ocean Years ----
#  For salmon, this would be the year of ocean entry
#  Years after the most recent year in the response will be predicted
input.years= c(1980, 2023)

# Prediction years (ocean years)
#  These years will not be included in calculating the CMISST index,
#  but will be in the index output for use in a predictive model
#input.years.pred=c(2020)
input.years.pred=NA

#************************************
#  For Leave One Out Cross-validation
#************************************

# The script will leave only the most recent years out,
#   emulating a forecasting scenario.  How many years should be included?
#   E.g., 5 will only test the 5 most recent years, and using the full
#   time series length will remove every data point (one at a time)
loocvYears=5 # the most recent X years to include in the LOO CV

# Input: MAE LOO CV? ----
input.loocv= TRUE
# Do we want each individual year's prediction output (TRUE),
#   or a mean and se per year (FALSE)
pred_out = TRUE


#************************************
# Function to calculate the index and return results
#************************************
updateCMISST <- function() {
  min.lon = input.long[1]
  max.lon = input.long[2]
  min.lat = input.lat[1]
  max.lat = input.lat[2]
  years = seq(input.years[1], input.years[2], 1)
  years = sort(unique(c(years, input.years.pred)))
  
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
  if (!is.na(input.years.pred[1])) {
    years.fit<-years[!years %in% input.years.pred & years %in% response.tmp$year] # will be needed to calculate the covariance
  } else years.fit <- years[years %in% response.tmp$year]
  
  # Calculate the CMISST index
  cmisst <- get_CMISST_index(response = response.tmp[,c("year","val.scl")],
                             oceanData = oceanData, years.pred = input.years.pred,
                             min.lon = min.lon, max.lon = max.lon,
                             min.lat = min.lat, max.lat = max.lat,
                             years = years, years.fit = years.fit,
                             months = months,
                             returnDataType = returnDataType,
                             removeBering = removeBering)

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

# Call the function
cmisst <- updateCMISST()

#************************************
# Things below here should be moved to a new script for plotting
#************************************

source("R/makePlots.R")

# Input: What map to plot
input.season = "spr"

# Make the covariance map
makeCovarianceMap(input.season = input.season, cmisst = cmisst)

# Biplot with response
makeBiplot(input.season = input.season, cmisst = cmisst)

# Make Time series Plot
makeTimeSeriesPlot(input.season = 'spr', cmisst = cmisst,
                   ylab = "Counts", yaxis_scaler = 1000)

# Output: Index time series
makeIndexPlot(cmisst = cmisst)
  
# Output: Observed and predicted time series from the LOO
makeLOOplot(cmisst = cmisst, season = "spr")

# Print the sesonal indices and the scaled response variable
makeTable(cmisst = cmisst)
