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

    # This is for the manuscript
    if (includePDO) {
      index_other<-merge(otherIndicators, response, all.x=TRUE)
      otherIndicators.fit<-index_other[index_other$year %in% years.fit,]
      otherIndicators.pred<-index_other[index_other$year %in% years.pred,]
    }
    
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
      
      if (includePDO) {
        # Optionally, include PDO
        otherIndicators.fit$var <- otherIndicators.fit[,paste0("pdo.", season)]
        otherIndicators.pred$var <- otherIndicators.pred[,paste0("pdo.", season)]
        mdl<-lm(val.scl~var, data=otherIndicators.fit)
        pred<-predict(mdl, newdata = otherIndicators.pred)
        mae<-rbind(mae, data.frame(model="pdo", season=season, year=this_year,
                                   response=otherIndicators.pred$val.scl, pred=pred,
                                   stringsAsFactors = FALSE))
        # NPGO
        otherIndicators.fit$var <- otherIndicators.fit[,paste0("npgo.", season)]
        otherIndicators.pred$var <- otherIndicators.pred[,paste0("npgo.", season)]
        mdl<-lm(val.scl~var, data=otherIndicators.fit)
        pred<-predict(mdl, newdata = otherIndicators.pred)
        mae<-rbind(mae, data.frame(model="npgo", season=season, year=this_year,
                                   response=otherIndicators.pred$val.scl, pred=pred,
                                   stringsAsFactors = FALSE))
        # # ONI
        otherIndicators.fit$var <- otherIndicators.fit[,paste0("oni.", season)]
        otherIndicators.pred$var <- otherIndicators.pred[,paste0("oni.", season)]
        mdl<-lm(val.scl~var, data=otherIndicators.fit)
        pred<-predict(mdl, newdata = otherIndicators.pred)
        mae<-rbind(mae, data.frame(model="oni", season=season, year=this_year,
                                   response=otherIndicators.pred$val.scl, pred=pred,
                                   stringsAsFactors = FALSE))
        # # SSTarc
        otherIndicators.fit$var <- otherIndicators.fit[,paste0("arc.", season)]
        otherIndicators.pred$var <- otherIndicators.pred[,paste0("arc.", season)]
        mdl<-lm(val.scl~var, data=otherIndicators.fit)
        pred<-predict(mdl, newdata = otherIndicators.pred)
        mae<-rbind(mae, data.frame(model="SSTarc", season=season, year=this_year,
                                   response=otherIndicators.pred$val.scl, pred=pred,
                                   stringsAsFactors = FALSE))
      }
      
    } # End looping over season
  } # End looping over years
  
  mae$season <- factor(mae$season, levels = c("win","spr","sum","aut"))
  return(mae)
}
