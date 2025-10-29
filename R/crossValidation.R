# Run LOO cross-validation, removing each year sequentially and predicting that year

#*************************************************************
#  Create a loop for cross-validation
#   The only years that should be sent here are the fit years
#*************************************************************
LOO_CV <- function(response = response,
                   oceanData = oceanData, loocvYears = 5,
                   min.lon = min.lon, max.lon = max.lon,
                   min.lat = min.lat, max.lat = max.lat,
                   years = years.fit, months = months,
                   includePDO = FALSE, includePC1 = FALSE) {

  # Container for results
  mae <- data.frame(model=as.character(), season=as.character(), year=as.numeric(),
                    response=as.numeric(), pred=as.numeric(),
                    mae=as.numeric(), stringsAsFactors = FALSE)
  
  # Loop over just the last "loocvYears" years
  n <- length(years)
  for (this_year in years[(n-(loocvYears-1)):n]) { # just the last "loocvYears" years
    years.fit <- years[!years %in% this_year]
    years.pred <- years[years %in% this_year]
    
    # Get the index
    cmisst_loo <- get_CMISST_index(response = response,
                               oceanData = oceanData,
                               min.lon = min.lon, max.lon = max.lon,
                               min.lat = min.lat, max.lat = max.lat,
                               years = years, years.fit = years.fit,
                               months = months)
    

    # output
    index_cov <- cmisst_loo[[1]]
    index.fit <- index_cov[index_cov$year %in% years.fit,]
    index.pred <- index_cov[index_cov$year %in% years.pred,]

    # Calculate MAE
    # Need to loop over seasons
    for(season in c("win","spr","sum","aut")) {
      
      index.fit$var <- index.fit[,paste0(season,".cov")]
      index.pred$var <- index.pred[,paste0(season,".cov")]
      mdl <- lm(val~var, data=index.fit)
      pred <- predict(mdl, newdata = index.pred)
      # Assign it to the data.frame
      mae <- rbind(mae, data.frame(model="cmisst", season=season, year=this_year,
                                   response=response$val.scl[response$year == this_year],
                                   pred=pred, stringsAsFactors = FALSE))
    } # End looping over season
  } # End looping over years
  
  mae$season <- factor(mae$season, levels = c("win","spr","sum","aut"))
  return(mae)
}
