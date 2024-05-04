# Run LOO cross-validation, removing each year sequentially and predicting that year

# library(ncdf4)
# library(RColorBrewer)
# library(sp)
# library(maptools)
# library(reshape2)
# library(ggplot2)
# library(sdmTMB)
# library(ggeffects)
# library(visreg)
# library(doBy)


#*************************************************************
#  Create a large loop right here for cross-validation
#*************************************************************
LOO_CV <- function(response = response,
                   oceanData = oceanData, loocvYears = 5,
                   min.lon = min.lon, max.lon = max.lon,
                   min.lat = min.lat, max.lat = max.lat,
                   years = years, months = months,
                   includePDO = FALSE, includePC1 = FALSE) {

  # Verify that the response is what we expect
  if (ncol(response)!=2) { print("incorrect data - requires a 2-column data frame with year and the response"); return(NA) }
  colnames(response)<-c("year","val")
  
  year_mo<-data.frame(year=rep(years, each=length(months)), month=rep(months, length(years)),
                      label=paste(rep(years, each=length(months)), rep(months, length(years)), sep = "_"))
  
  # Index the locations in the file
  lons <- as.numeric(dimnames(oceanData)[[1]])
  lats <- as.numeric(dimnames(oceanData)[[2]])
  yr_mo <- dimnames(oceanData)[[3]]
  lon.index<-which(lons >= min.lon & lons <= max.lon) 
  lat.index<-which(lats >= min.lat & lats <= max.lat)
  yr_mo.index<-which(yr_mo %in% year_mo$label)
  oceanData <- oceanData[lon.index, lat.index, yr_mo.index]

  # Container for results
  mae<-data.frame(model=as.character(), season=as.character(), year=as.numeric(),
                    response=as.numeric(), pred=as.numeric(),
                    mae=as.numeric(), stringsAsFactors = FALSE)
  n<-length(years)
  for (this_year in years[(n-(loocvYears-1)):n]) { # just the last "loocvYears" years
    years.fit<-years[!years %in% this_year]
    years.pred<-years[years %in% this_year]
    
    
    #************************************************
    # Create seasonal datasets
    #************************************************
    
    createSeasonalData_LOOCV<-function(oceanData, years = years, months = months, year_mo=year_mo, season=1) {

      seasonal<-array(NA, c(dim(oceanData)[1], dim(oceanData)[2], length(years)), dimnames = list(dimnames(oceanData)[[1]], dimnames(oceanData)[[2]], years))
      for (yy in 1:length(years)) {
        if (season==1) seasonal[,,yy]<-(oceanData[,,year_mo$month == 1 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 2 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 3 & year_mo$year==years[yy]])/3
        if (season==2) seasonal[,,yy]<-(oceanData[,,year_mo$month == 4 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 5 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 6 & year_mo$year==years[yy]])/3
        if (season==3) seasonal[,,yy]<-(oceanData[,,year_mo$month == 7 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 8 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 9 & year_mo$year==years[yy]])/3
        if (season==4) seasonal[,,yy]<-(oceanData[,,year_mo$month == 10 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 11 & year_mo$year==years[yy]]+oceanData[,,year_mo$month == 12 & year_mo$year==years[yy]])/3
      } 
      # This scales (Z-score) the data cell-wise
      # The aperm is needed because for some reason the apply function returns the third dimension (time) as the first dimension
      oceanData.scl <- aperm(apply(seasonal, 1:2, scale), c(2,3,1))
      dimnames(oceanData.scl)[[3]]<-years
      return(oceanData.scl)
    }
    
    # Create the data by calling our function (returns scaled sst array and full dataset with fish)
    oceanData.s1.scl <- createSeasonalData_LOOCV(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 1)
    oceanData.s2.scl <- createSeasonalData_LOOCV(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 2)
    oceanData.s3.scl <- createSeasonalData_LOOCV(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 3)
    oceanData.s4.scl <- createSeasonalData_LOOCV(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 4)
    
    # Get covariance between each cell's temperature and survival
    covs1<-apply(oceanData.s1.scl[,,as.character(eval(years.fit))], 1:2, function(x) cov(x,response$val[years %in% years.fit], use="pairwise.complete.obs"))
    covs2<-apply(oceanData.s2.scl[,,as.character(eval(years.fit))], 1:2, function(x) cov(x,response$val[years %in% years.fit], use="pairwise.complete.obs"))
    covs3<-apply(oceanData.s3.scl[,,as.character(eval(years.fit))], 1:2, function(x) cov(x,response$val[years %in% years.fit], use="pairwise.complete.obs"))
    covs4<-apply(oceanData.s4.scl[,,as.character(eval(years.fit))], 1:2, function(x) cov(x,response$val[years %in% years.fit], use="pairwise.complete.obs"))
    
    #********************************************************************
    # Create the index (how similar is each year to the covariance map)
    #   The covariance maps were created without the holdout year
    #   but we want to create the index value for all years, so we can forecast
    #********************************************************************
    coefs_cov<-NULL
    options(na.action="na.omit")
    for (tt in 1:dim(oceanData.s1.scl)[3])
      coefs_cov<-rbind(coefs_cov, c(lm(as.vector(oceanData.s1.scl[,,tt]) ~ -1 + as.vector(covs1))$coef,
                                    lm(as.vector(oceanData.s2.scl[,,tt]) ~ -1 + as.vector(covs2))$coef,
                                    lm(as.vector(oceanData.s3.scl[,,tt]) ~ -1 + as.vector(covs3))$coef,
                                    lm(as.vector(oceanData.s4.scl[,,tt]) ~ -1 + as.vector(covs4))$coef))
    coefs_cov<-data.frame(coefs_cov)
    coefs_cov$year<-years
    index_cov<-cbind(coefs_cov,response$val)
    colnames(index_cov)<-c("win.cov","spr.cov","sum.cov","aut.cov","year","val")

    #*****************************************
    # Calculate MAE
    #*****************************************
    
    index.fit<-index_cov[index_cov$year %in% years.fit,]
    index.pred<-index_cov[index_cov$year %in% years.pred,]

    # Need to loop over seasons
    for(season in c("win","spr","sum","aut")) {
      
      index.fit$var <- index.fit[,paste0(season,".cov")]
      index.pred$var <- index.pred[,paste0(season,".cov")]
      mdl<-lm(val~var, data=index.fit)
      pred<-predict(mdl, newdata = index.pred)
      #cov_mae<-sqrt(mean((pred - index.pred$val)^2)) # This is RMSEP
      cov_mae<-mean(abs(pred - index.pred$val)) # This is MAE
      # Assign it to the data.frame
      mae<-rbind(mae, data.frame(model="cmisst", season=season, year=this_year,
                                     response=index.pred$val, pred=pred,
                                     mae=cov_mae, stringsAsFactors = FALSE))
    } # End looping over season
  } # End looping over years
  
  se <- function(x) { return(sd(x)/sqrt(length(x))) }
  mae$season<-factor(mae$season, levels = c("win","spr","sum","aut"))
  # Trying this out - if I leave this line out, I might get all the individual predictions
  if(exists("pred_out") & !pred_out) mae<-summaryBy(mae ~ model+season, data = mae, FUN = c(mean, se))
  return(list(mae))
}
