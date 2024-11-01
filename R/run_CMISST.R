# This is the main script to run
#
#  Created by Brian Burke, NOAA Fisheries, NWFSC
#    brian.burke@noaa.gov
#

library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(doBy)


# Data --------------------------------------------------------------------


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

# define the updateCMISST() function
source('R/updateCMISST.R')
# calculate CMISST index, used by updateCMISST function
source('R/get_index.R')
# Define LOO_CV(), used by updateCMISST function
source('R/crossValidation.R')

# set parameters (for most users, leave these as they are)
months=seq(1,12,1)
removeBering=FALSE

# Parameters --------------------------------------------------------------

#************************************
#       Define Parameters
#************************************
input.spatialData = "ERSST"

# Input: Choose a stock 
input.stock = "Sp_Chinook"
#input.stock = "Fa_Chinook"
#input.stock = "Steelhead"

# Input: log response?
input.log = TRUE

# Input: lag response? 
input.lag= 2

# Input: Latitude range 
input.lat = c(10, 62)

# Input: Longitude range 
input.long= c(158, 246)

# Ocean Years 
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

# MAE LOO CV? 
input.loocv= TRUE
# Do we want each individual season and year's prediction output (TRUE),
#   or a mean and se per season (FALSE)
pred_out = TRUE


# Run ---------------------------------------------------------------------


#************************************
# Function to calculate the index and return results
#************************************

# This uses the values of the parameters in memory
cmisst <- updateCMISST()


# Plot --------------------------------------------------------------------


#************************************
#   Plot Results
#************************************

source("R/makePlots.R")

# Input: What map to plot
input.season = "spr"

# Make the covariance map
makeCovarianceMap(input.season = input.season, cmisst = cmisst)

# Biplot with response
makeBiplot(input.season = input.season, cmisst = cmisst)

# Make Time series Plot
makeTimeSeriesPlot(input.season = input.season, cmisst = cmisst,
                   ylab = "Counts", yaxis_scaler = 1000)

# Output: Index time series
makeIndexPlot(cmisst = cmisst)
  
# Output: Observed and predicted time series from the LOO
makeLOOplot(cmisst = cmisst, season = input.season)

# Print the sesonal indices and the scaled response variable
makeTable(cmisst = cmisst)
