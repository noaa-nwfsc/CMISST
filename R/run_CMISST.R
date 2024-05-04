# This is the main script to run
#
#  Created by Brian Burke, NOAA Fisheries, NWFSC
#    brian.burke@noaa.gov
#

library(sf)

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
dataSet='ERSST'
months=seq(1,12,1)
removeBering=FALSE
returnDataType='anom'
returnObjectType='array'

#************************************
#  For Leave One Out Cross-validation
#************************************

# The script will leave only the most recent years out,
#   emulating a forecasting scenario.  How many years should be included?
#   E.g., 5 will only test the 5 most recent years, and using teh full
#   time series length will remove every data point (one at a time)
loocvYears=5 # the most recent X years to include in the LOO CV


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
input.years= c(1980, 2023)

# Prediction years
#  These years will not be included in calculating the CMISST index,
#  but will be in the index output for use in a predictive model
#input.years.pred=c(2021,2022)
input.years.pred=NA

# Input: MAE LOO CV? ----
#input.loocv= FALSE
input.loocv= TRUE
# Do we want each individual year's prediction output (TRUE), or a mean and se per year (FALSE)
pred_out=TRUE

# Input: What map to plot
input.season = "spr"


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
  colnames(response.tmp) <- c('year','val')
  # sometimes the response starts after the input.years, so limit the years in the ocean data too
  years <- years[years %in% response.tmp$year]
  
  # Log (if requested) and scale the response variable
  if(input.log) response.tmp$val <- log(response.tmp$val)
  response.tmp$val.scl <- scale(response.tmp$val)

  # Calculate the CMISST index
  cmisst <- get_CMISST_index(response = response.tmp[,c("year","val.scl")],
                             oceanData = oceanData_ERSST, years.pred = input.years.pred,
                             min.lon = min.lon, max.lon = max.lon,
                             min.lat = min.lat, max.lat = max.lat,
                             years = years, months = months,
                             returnDataType = returnDataType,
                             removeBering = removeBering)

  if (input.loocv) {
    loocv <- LOO_CV(response = response.tmp[,c("year","val.scl")],
                                   oceanData = oceanData, loocvYears = loocvYears,
                                   min.lon = min.lon, max.lon = max.lon,
                                   min.lat = min.lat, max.lat = max.lat,
                                   years = years, months = months)
    return(append(cmisst, loocv))
  } else return(cmisst)
  # Returns index as a list
  #  cmisst[[1]] contains 6 columns (as one list item): 4 seasonal indices, year, response
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



# For the manuscript Table 1
#cmisst[[7]]

# Covariance Map
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
season <- switch(input.season,
               win = 2,
               spr = 3,
               sum = 4,
               aut = 5)
myTitle <- switch(input.season,
                 win = "Winter",
                 spr = "Spring",
                 sum = "Summer",
                 aut = "Autumn")
covMap<-cmisst[[season]]
lmt<-max(abs(covMap), na.rm=TRUE)
limits<-c(-lmt, lmt)
extent <- cmisst[[6]] # min, max of lat, long

gg <- ggplot() + ggtitle(myTitle) +
  geom_raster(data = melt(covMap), aes(x = Var1, y = Var2, fill=value)) +
  geom_sf(data=land, color="black", fill="grey", linewidth=0.25) +
  xlim(extent[3], extent[4]) + ylim(extent[1], extent[2]) +
  scale_fill_gradientn(colours = myPalette(100),limits=limits,name="Covariance", na.value = "white") +
  theme_classic() + theme(panel.border = element_rect(colour = "grey", fill=NA)) +
  labs(x = "Longitude", y = "Latitude")
gg


# Biplot with response
index <- cmisst[[1]]
season <- switch(input.season,
                 win = 1,
                 spr = 2,
                 sum = 3,
                 aut = 4)
index$ind <- index[,season]
myTitle <- switch(input.season,
                  win = "Winter",
                  spr = "Spring",
                  sum = "Summer",
                  aut = "Autumn")
plot(index$ind, index$val, pch=20, cex=2, xlab=paste(myTitle, "CMISST Index"),
     ylab="Scaled (Z-score) Response", main=myTitle)
lm1 <- lm(index$val~index$ind)
abline(lm1)
text(bquote(~ R^2 == .(round(summary(lm1)$adj.r.squared, 2))),
     x = par("usr")[1]*0.8, y=par("usr")[4]*0.80, cex=1.6, col="blue")
if (input.loocv) {
  mae <- cmisst[[7]]
  text(paste("MAE =", round(mae[season,"mae"], 2)),
     x = par("usr")[1]*0.75, y=par("usr")[4]*0.60, cex=1.6, col="blue")
}

# cmisst[[7]][cmisst[[7]]$season=="spr",]

# Time series plot in normal space
# To get back to normal space
reverse_scale <- function(x, center = NULL, scale = NULL) {
  if (!is.null(attr(x, "scaled:scale"))) {
    x <- x * attr(x, "scaled:scale")
  } else { x <- x * scale }
  if (!is.null(attr(x, "scaled:center"))) {
    x <- x + attr(x, "scaled:center")
  } else { x <- x + center }
  x
}
response.tmp <- response
response.tmp$year <- response.tmp$year - as.numeric(input.lag)
response.tmp <- response.tmp[response.tmp$year %in% seq(input.years[1], input.years[2], 1), c('year', input.stock)]
colnames(response.tmp) <- c('year','val')
if(input.log) response.tmp$val <- log(response.tmp$val)
response.tmp$val.scl <- scale(response.tmp$val)
reverse_scale(response.tmp$val.scl)

index <- cmisst[[1]]
season <- switch(input.season,
                 win = 1, spr = 2, sum = 3, aut = 4)
index$ind <- index[,season]
index$counts <- reverse_scale(index$val, attr(response.tmp$val.scl, "scaled:center"), attr(response.tmp$val.scl, "scaled:scale"))
if (input.log) index$counts <- exp(index$counts)
myTitle <- switch(input.season,
                  win = "Winter", spr = "Spring", sum = "Summer", aut = "Autumn")
lm1 <- lm(index$val~index$ind)
preds<-predict(lm1, newdata = index, interval = "confidence")
preds<-reverse_scale(preds, attr(response.tmp$val.scl, "scaled:center"), attr(response.tmp$val.scl, "scaled:scale"))
if (input.log) preds<-exp(preds)
# Use prediction interval for last point
preds_last<-predict(lm1, newdata = index, interval = "prediction")
preds_last<-reverse_scale(preds_last, attr(response.tmp$val.scl, "scaled:center"), attr(response.tmp$val.scl, "scaled:scale"))
if (input.log) preds_last<- exp(preds_last)
preds[nrow(preds),]<-preds_last[nrow(preds),]

preds<-data.frame(preds)
index$year_return <- index$year + input.lag
preds$year_return<-index$year_return
# Plot for SOEM talk in 2024
scaler<-1
ggplot() +
  geom_line(data = index, aes(x=year_return, y=counts/scaler)) +
  geom_point(data = index, aes(x=year_return, y=counts/scaler)) +
  theme_classic() +
  #ylab("Spring Chinook Counts at Bonneville Dam") + xlab("") +
  #ylab("Bongo Biomass") + xlab("") +
  ylab("IGF") + xlab("") +
  geom_line(data=preds, aes(x=year_return, y=fit/scaler), color="deepskyblue2", linewidth=1.3) +
  geom_point(data=preds, aes(x=year_return, y=fit/scaler), color="deepskyblue2") +
  geom_ribbon(data=preds, aes(x=year_return, ymin = lwr/scaler, ymax = upr/scaler), fill = "deepskyblue2", alpha = 0.2)

# Output: Index time series
index <- cmisst[[1]]
plot(index$year, index$win.cov, type='b', pch=20, col="red4",
     xlab="", ylab="CMISST Index",
     ylim=c(min(index[,c("win.cov","spr.cov","sum.cov","aut.cov")], na.rm=TRUE),
            max(index[,c("win.cov","spr.cov","sum.cov","aut.cov")], na.rm=TRUE)))
points(index$year, index$spr.cov, type='b', pch=20, col="blue")
points(index$year, index$sum.cov, type='b', pch=20, col="green3")
points(index$year, index$aut.cov, type='b', pch=20, col="purple")
legend("topleft", legend = c("Win","Spr","Sum","Aut"), bty='n',
       col = c("red4","blue","green3","purple"), pch = 20, lty=1)

  
# Output: Observed and predicted time series from the LOO
index <- cmisst[[7]]
index<-index[index$season=="spr" & index$model=="cmisst",]
plot(index$year, index$mae, type='b', pch=20, col="red4", xlab="", ylab="LOO")

#index <- cmisst[[1]] # This gets us the whole time series, whereas item 7 is only the LOO results
#plot(index$year, index$val, type='b', pch=20, cex=2, col="black", xlab="", ylab="Scaled Response", main = input.stock)
index <- cmisst[[7]]
index<-index[index$season=="spr" & index$model=="cmisst",]
plot(index$year, index$response, type='b', pch=20, cex=2, col="black", xlab="", ylab="Scaled Response", main = input.stock)
abline(0,0, lty=2)
index <- cmisst[[7]]
index2<-index[index$season=="spr" & index$model=="cmisst",]
lines(index2$year, index2$pred, lwd=3, col="deepskyblue2")
text(labels = paste("LOO MAE CMISST =", round(mean(index2$mae),2)),
     x = par("usr")[1]+9, y=par("usr")[4]*0.80, cex=1.2, col="deepskyblue2")
index3<-index[index$season=="spr" & index$model=="pdo",]
lines(index3$year, index3$pred, lwd=3, col="tomato3")
text(labels = paste("LOO MAE PDO =", round(mean(index3$mae),2)),
x = par("usr")[1]+8, y=par("usr")[4]*0.65, cex=1.2, col="tomato3")




# Output: Table
out<-cmisst[[1]]
out$year <- as.integer(out$year)
#out <- out[,c(5,6,1:4)]
colnames(out)[colnames(out)=="val"] <- "response"
cbind(out)
