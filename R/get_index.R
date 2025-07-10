# This is the main function that creates the CMISST index, given a response variable time series
#  and a user-specified spatial ocean variable

get_CMISST_index <- function(response, oceanData=oceanData_ERSST,
                             years=NA, years.fit=years.fit,
                             months=1:12,
                             min.lon=158, max.lon=246,
                             min.lat=10, max.lat=62) {
  # year in the response variable has already been lagged, so it represents the year of ocean entry
  
  # Verify that the response is what we expect
  if (ncol(response)!=2) { print("incorrect data - requires a 2-column data frame with year and the response"); return(NA) }
  colnames(response)<-c("year","val")
  
  # 'years' will be considered 'all years'  If we need fit or pred, we can access them
  year_mo <- data.frame(year=rep(years, each=length(months)), month=rep(months, length(years)),
                      label=paste(rep(years, each=length(months)), rep(months, length(years)), sep = "_"))
  
  #***************************************************************
  # Extract and scale the spatial data
  #***************************************************************
  
  # Index the locations in the file
  lons <- as.numeric(dimnames(oceanData)[[1]])
  lats <- as.numeric(dimnames(oceanData)[[2]])
  yr_mo <- dimnames(oceanData)[[3]]
  lon.index <- which(lons >= min.lon & lons <= max.lon) 
  lat.index <- which(lats >= min.lat & lats <= max.lat)
  yr_mo.index <- which(yr_mo %in% year_mo$label)
  # Subset the ocean data with user-defined extent
  oceanData <- oceanData[lon.index, lat.index, yr_mo.index]
  
  # Create the function to calculate seasonal averages
  createSeasonalData <- function(oceanData,
                               years = years, months = months, year_mo=year_mo, season=1) {
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
    dimnames(oceanData.scl)[[3]] <- years
    return(oceanData.scl)
  }
  
  # Create the data by calling our function (returns scaled sst array and full dataset with fish)
  oceanData.s1.scl <- createSeasonalData(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 1)
  oceanData.s2.scl <- createSeasonalData(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 2)
  oceanData.s3.scl <- createSeasonalData(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 3)
  oceanData.s4.scl <- createSeasonalData(oceanData = oceanData, years = years, months = months, year_mo=year_mo, season = 4)

  # Get covariance between each cell's temperature and survival (only for fit years!!)
  covs1 <- apply(oceanData.s1.scl[,,as.character(years.fit)], 1:2, function(x) cov(x, response$val[response$year %in% years.fit], use="pairwise.complete.obs"))
  covs2 <- apply(oceanData.s2.scl[,,as.character(years.fit)], 1:2, function(x) cov(x, response$val[response$year %in% years.fit], use="pairwise.complete.obs"))
  covs3 <- apply(oceanData.s3.scl[,,as.character(years.fit)], 1:2, function(x) cov(x, response$val[response$year %in% years.fit], use="pairwise.complete.obs"))
  covs4 <- apply(oceanData.s4.scl[,,as.character(years.fit)], 1:2, function(x) cov(x, response$val[response$year %in% years.fit], use="pairwise.complete.obs"))

  #********************************************************************
  # Create the index (how similar is each year to the covariance map)
  #********************************************************************
  coefs_cov<-NULL
  options(na.action="na.omit")
  for (tt in 1:dim(oceanData.s1.scl)[3])
    coefs_cov <- rbind(coefs_cov, c(tryCatch(lm(as.vector(oceanData.s1.scl[,,tt]) ~ -1 + as.vector(covs1))$coef,error=function(e){NA}),
                                  tryCatch(lm(as.vector(oceanData.s2.scl[,,tt]) ~ -1 + as.vector(covs2))$coef,error=function(e){NA}),
                                  tryCatch(lm(as.vector(oceanData.s3.scl[,,tt]) ~ -1 + as.vector(covs3))$coef,error=function(e){NA}),
                                  tryCatch(lm(as.vector(oceanData.s4.scl[,,tt]) ~ -1 + as.vector(covs4))$coef,error=function(e){NA})))
  coefs_cov <- data.frame(coefs_cov)
  coefs_cov$year <- years
  index_cov <- merge(coefs_cov, response[response$year %in% years.fit,], all.x=TRUE)
  colnames(index_cov)<-c("year","win.cov","spr.cov","sum.cov","aut.cov","val")

  # Returns index as a list
  #  cmisst[[1]] contains 6 columns (as one list item): year, 4 seasonal indices, response
  #  cmisst[[2]] winter spatial covariance values (for maps)
  #  cmisst[[3]] spring spatial covariance values (for maps)
  #  cmisst[[4]] summer spatial covariance values (for maps)
  #  cmisst[[5]] autumn spatial covariance values (for maps)
  #  cmisst[[6]] lat, long min and max, as 1 list item
  return(list(index_cov, covs1, covs2, covs3, covs4, c(min.lat, max.lat, min.lon, max.lon)))
}
