# For the manuscript, this file runs the models needed to make Figure 4

# Run the models----------------------------------------------------------------

# Determine the sensitivity of dataset length and spatial extent, using best lag and season
source('R/run_CMISST.R') # This sets everything up, then we can make changes
stockList <- c("Sp_Chinook","Fa_Chinook","Steelhead")
response.orig <- response
input.years= c(1980, 2022)
input.years.orig <- input.years
input.loocv=TRUE
loocvYears=10 # Sequentially removes each of the 10 last years in the LOO

# Store sensitivity results
durationResults <- data.frame(data=as.character(), stock=as.character(),
                              index=as.character(), season=as.character(),
                              lag=as.numeric(), startYearData=as.numeric(),
                              looYear=as.numeric(),
                              #startLat=as.numeric(), startLong=as.numeric(),
                              response=as.numeric(), pred=as.numeric(), 
                              stringsAsFactors = FALSE)

# Loop over variables
for (input.stock in stockList) {
  response<-response.orig[,c("year",input.stock)]
  
  # Set best parameters (taken from Figure 3)
  # (all seasons are done at the same time, so that will be selected later)
  if (input.stock == "Sp_Chinook") {
    input.lag <- 2
    input.spatialData == "ERSST"
  } else if (input.stock == "Fa_Chinook") {
    input.lag <- 3
    input.spatialData == "SSH"
  } else if (input.stock == "Steelhead") {
    input.lag <- 3
    input.spatialData == "ERSST"
  }
  
  for (startYear in input.years.orig[1]:(input.years.orig[2]-9)) {
    input.years[1] <- startYear
    cmisst <- updateCMISST()
    loo <- cmisst[[7]]
    if (input.spatialData == "SSH") loo$model <- "cmissh"
    durationResults <- rbind(durationResults, data.frame(data = input.spatialData,
                                                         stock = rep(input.stock, nrow(loo)),
                                                         index=loo$model, season=loo$season,
                                                         lag=input.lag, startYearData=startYear,
                                                         looYear=loo$year,
                                                         response=loo$response, pred=loo$pred,
                                                         stringsAsFactors = FALSE))
  }
}

durationResults$lag <- factor(durationResults$lag)
durationResults$stock <- factor(durationResults$stock, levels = c("Sp_Chinook","Fa_Chinook","Steelhead"))
durationResults$index <- factor(durationResults$index, levels = c("cmisst","cmissh"))

# Save results
save(x = "durationResults", file = 'manuscriptFigures/durationResults')

# Make the figure---------------------------------------------------------------
library(patchwork)

load(file = 'manuscriptFigures/durationResults')
# Summarize
durationSummary <- durationResults %>%
  group_by(stock, index, season, lag, startYearData) %>%
  summarise(mae = mae_func(pred, response),
            rmse = rmse_func(pred, response),
            kge = hydroGOF::KGE(pred, response, method = "2021"), .groups = 'keep')

extent <- cmisst[[6]] # min, max of lat, long
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdBu")), space="Lab")
seasonPalette <- c("#56B4E9", "#009E73", "#E69F00", "#F0E442")

# Make the inset maps
for (input.stock in stockList) {
  response<-response.orig[,c("year",input.stock)]
  
  # Set best parameters (taken from Figure 3)
  # (all seasons are done at the same time, so that will be selected later)
  if (input.stock == "Sp_Chinook") {
    input.lag <- 2
    input.spatialData == "ERSST"
  } else if (input.stock == "Fa_Chinook") {
    input.lag <- 3
    input.spatialData == "SSH"
  } else if (input.stock == "Steelhead") {
    input.lag <- 3
    input.spatialData == "ERSST"
  }
  
  for (startYear in c(1980, 1995, 2010)) {
    input.years[1] <- startYear
    cmisst <- updateCMISST()
    covMap <- cmisst[[3]] # spring covariances
    gg <- ggplot() +
      geom_raster(data = melt(covMap), aes(x = Var1, y = Var2, fill=value)) +
      geom_sf(data=land, color="black", fill="grey", linewidth=0.25) +
      xlim(extent[3], extent[4]) + ylim(extent[1], extent[2]) +
      scale_fill_gradientn(colours = myPalette(100), limits=c(-1,1), name="Covariance", na.value = "white") +
      theme_classic() +
      theme(panel.border = element_rect(colour = "grey", fill=NA),
            axis.text.x=element_blank(), axis.text.y=element_blank(),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.ticks = element_blank(), legend.position = "none",
            plot.margin = unit(c(0,0,0,0), "cm"))
    assign(paste("gg", startYear, sep = ""), gg)
  }

  # Make the line plots
  gg <- ggplot() + ggtitle(switch(input.stock, Sp_Chinook="Spring Chinook", Fa_Chinook="Fall Chinook", Steelhead="Steelhead")) +
  geom_line(data=durationSummary[durationSummary$stock == input.stock,], aes(x=startYearData, y=rmse, col=season)) +
  ylab("Root Mean Squared Error\n") + 
  theme_classic() +
  scale_color_discrete(name = "", labels = c("Winter","Spring","Summer","Autumn"), palette = seasonPalette)
  if (input.stock == stockList[1]) gg <- gg +
    theme(axis.title.x = element_text(color = "white"),
          axis.title.y = element_text(color = "white")) +
    xlab("")
  if (input.stock == stockList[2]) gg <- gg +
    theme(axis.title.x = element_text(color = "white")) +
    xlab("")
  if (input.stock == stockList[3]) gg <- gg +
    theme(axis.title.y = element_text(color = "white")) +
    xlab("Start Year")

  gg_inset <- gg +
    inset_element(gg1980, left = 0.05, bottom = 0.65, right = .3, top = 1) +
    inset_element(gg1995, left = 0.375, bottom = 0.65, right = .625, top = 1) +
    inset_element(gg2010, left = 0.7, bottom = 0.65, right = 0.95, top = 1)

  # Assign the results to an object
  if (input.stock==stockList[1]) gg_inset_sp <- gg_inset
  if (input.stock==stockList[2]) gg_inset_fa <- gg_inset
  if (input.stock==stockList[3]) gg_inset_st <- gg_inset
}

design <- "1\n1\n1\n2\n2\n2\n3\n3\n3\n4"
gg_final <- gg_inset_sp + gg_inset_fa + gg_inset_st + guide_area() +
  plot_layout(ncol = 1, guides = "collect", design = design) &
  theme(legend.direction = 'horizontal', legend.text = element_text(size=7))
gg_final

ggsave(filename = "manuscriptFigures/Fig 5 duration.tiff",
       plot = gg_final, width = 90, height = 160, units = "mm", dpi = 500)

