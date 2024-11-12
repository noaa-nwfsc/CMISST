# Make a few plots from the results

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


makeCovarianceMap <- function(input.season = input.season, cmisst = cmisst) {
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
}


makeBiplot <- function(input.season = input.season, cmisst = cmisst) {
  # Biplot with response
  index <- cmisst[[1]]
  season <- switch(input.season,
                   win = 2,
                   spr = 3,
                   sum = 4,
                   aut = 5)
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
    colnames(mae)[colnames(mae)=="mae.mean"] <- "mae"
    text(paste("MAE =", round(mean(abs(mae[mae$season==input.season,"mae"])), 2)),
         x = par("usr")[1]*0.75, y=par("usr")[4]*0.60, cex=1.6, col="blue")
  }
}

makeTimeSeriesPlot <- function(input.season = input.season, cmisst = cmisst,
                               ylab="", yaxis_scaler=1) {
  # Time series plot in normal space
  response.tmp <- response
  response.tmp$year <- response.tmp$year - as.numeric(input.lag)
  response.tmp <- response.tmp[response.tmp$year %in% seq(input.years[1], input.years[2], 1), c('year', input.stock)]
  colnames(response.tmp) <- c('year','val')
  if(input.log) response.tmp$val <- log(response.tmp$val)
  response.tmp$val.scl <- scale(response.tmp$val)
  #reverse_scale(response.tmp$val.scl)
  
  index <- cmisst[[1]]
  season <- switch(input.season,
                   win = 2, spr = 3, sum = 4, aut = 5)
  index$ind <- index[,season]
  index$counts <- reverse_scale(index$val, attr(response.tmp$val.scl, "scaled:center"), attr(response.tmp$val.scl, "scaled:scale"))
  if (input.log) index$counts <- exp(index$counts)
  myTitle <- switch(input.season,
                    win = "Winter", spr = "Spring", sum = "Summer", aut = "Autumn")
  lm1 <- lm(index$val~index$ind)
  preds<-predict(lm1, newdata = index, interval = "confidence")
  preds<-reverse_scale(preds, attr(response.tmp$val.scl, "scaled:center"), attr(response.tmp$val.scl, "scaled:scale"))
  if (input.log) preds<-exp(preds)
  # Use prediction interval for predicted points
  preds_new<-predict(lm1, newdata = index, interval = "prediction")
  preds_new<-reverse_scale(preds_new, attr(response.tmp$val.scl, "scaled:center"), attr(response.tmp$val.scl, "scaled:scale"))
  if (input.log) preds_new<- exp(preds_new)
  # replace just the ones that were not used during fitting
  #preds[index$year %in% input.years.pred,]<-preds_new[index$year %in% input.years.pred,]
  preds[is.na(index$val),]<-preds_new[is.na(index$val),]
  
  preds<-data.frame(preds)
  # unlag the year to show the plot in return year
  index$year_return <- index$year + input.lag
  preds$year_return <- index$year_return
  # Plot for SOEM talk in 2024
  ggplot() +
    geom_line(data = index, aes(x=year_return, y=counts/yaxis_scaler)) +
    geom_point(data = index, aes(x=year_return, y=counts/yaxis_scaler)) +
    theme_classic() +
    ylab(label = ylab) + xlab("Response Year") +
    geom_line(data=preds, aes(x=year_return, y=fit/yaxis_scaler), color="deepskyblue2", linewidth=1.3) +
    geom_point(data=preds, aes(x=year_return, y=fit/yaxis_scaler), color="deepskyblue2") +
    geom_ribbon(data=preds, aes(x=year_return, ymin = lwr/yaxis_scaler, ymax = upr/yaxis_scaler), fill = "deepskyblue2", alpha = 0.2)
}

makeIndexPlot <- function(cmisst = cmisst) {
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
}


makeLOOplot <- function(cmisst = cmisst, season = "spr") {
  # Output: Observed and predicted time series from the LOO
  index <- cmisst[[1]] # This gets us the whole time series
  plot(index$year, index$val, type='b', pch=20, cex=2, col="black", xlab="", ylab="Scaled Response", main = input.stock)
  abline(0,0, lty=2)
  index <- cmisst[[7]] # this is just the loo results
  index2<-index[index$season==season & index$model=="cmisst",]
  lines(index2$year, index2$pred, lwd=3, col="deepskyblue2")
  text(labels = paste("LOO MAE CMISST =", round(mean(abs(index2$mae)),2)),
       x = par("usr")[1]+9, y=par("usr")[4]*0.80, cex=1.0, col="deepskyblue2")
}

makeTable <- function(cmisst = cmisst) {
  # Time series plot in normal space
  response.tmp <- response
  response.tmp$year <- response.tmp$year - as.numeric(input.lag)
  response.tmp <- response.tmp[response.tmp$year %in% seq(input.years[1], input.years[2], 1), c('year', input.stock)]
  colnames(response.tmp) <- c('year','val')
  if(input.log) response.tmp$val <- log(response.tmp$val)
  response.tmp$val.scl <- scale(response.tmp$val)
  index <- cmisst[[1]]
  index$response <- reverse_scale(index$val, attr(response.tmp$val.scl, "scaled:center"), attr(response.tmp$val.scl, "scaled:scale"))
  if (input.log) index$response <- exp(index$response)

  # Output: Table
  out<-cmisst[[1]]
  out$year <- as.integer(out$year)#out <- out[,c(5,6,1:4)]
  out$response <- index$response
  out
}
