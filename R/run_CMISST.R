# This is the main script to run
#
#  Created by Brian Burke, NOAA Fisheries, NWFSC
#    brian.burke@noaa.gov
#

library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(doBy)
library(dplyr)

#************************************
#   ---- Load data files & functions ----
#************************************

# land geo file for plotting the map
load("data/land.Rdata")

# Bonneville Dam Counts
load('data/responseData.RData')

# Snake Sp/Su Chinook SAR
load('data/responseDataSnake.RData')
response <- merge(response, Snake_SAR, all = TRUE)
response <- response %>% rename(Snake_SAR = meanSAR)

# ERSST data
load('data/oceanSSTData.RData')

# define the updateCMISST() function
source('R/updateCMISST.R')

# calculate CMISST index, used by updateCMISST function
source('R/get_index.R')

# Define LOO_CV(), used by updateCMISST function
source('R/crossValidation.R')

# Define the plotting functions
source("R/makePlots.R")


#************************************
#    ---- Define Parameters ----
# set parameters (for most users, leave these as they are)
#************************************
input.spatialData = "ERSST"
if (input.spatialData == "ERSST") oceanData <- oceanData_ERSST
#if (input.spatialData == "SSH") oceanData <- oceanData_SSH

# Input: Choose a stock 
input.stock = "Sp_Chinook"
#input.stock = "Fa_Chinook"
#input.stock = "Steelhead"
#input.stock = "Snake_SAR"

# Input: log response?
if (input.stock == "Snake_SAR") {
  input.link = "logit"
} else {
  input.link = "log"
}
#input.link = "None"

# Input: lag response? 
if (input.stock == "Snake_SAR") {
  input.lag = 0 # SAR data are already based on year of ocean entry
} else {
  input.lag = 2
}

# Input: Latitude range 
input.lat = c(10, 62)

# Input: Longitude range 
input.long = c(158, 246)

# Ocean Years 
#  For salmon, this would be the year of ocean entry
#  Years after the most recent year in the response will be predicted
#  if the SST / SSH data are available (2025 is not fully available yet)
input.years = c(1980, 2024)

months = seq(1,12,1)


#************************************
#  ---- Leave One Out Cross-validation ----
#************************************
# Calculate Mean Absolute Error from a LOO CV? 
input.loocv = TRUE

# The script will leave only the most recent years out,
#   emulating a forecasting scenario.  How many years should be included?
#   E.g., 5 will only test the 5 most recent years, and using the full
#   time series length will remove every data point (one at a time)
loocvYears = 10 # the most recent X years to include in the LOO CV

# Do we want each individual season and year's prediction output (TRUE),
#   or a mean and se per season (FALSE)
pred_out = TRUE




#************************************
#    ---- Run the model -----
# Function to calculate the index and return results
#************************************

# This uses the values of the parameters in memory
cmisst <- updateCMISST()




#************************************
#     ---- Plot  Results ----
#************************************

# Input: What map to plot (spr, sum, aut, win)
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
