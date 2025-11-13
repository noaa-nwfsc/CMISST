# For the manuscript, this file runs the models needed to make Figure 5

# Run the models----------------------------------------------------------------

# Determine the sensitivity of dataset length and spatial extent, using best lag and season
source('R/run_CMISST.R') # This sets everything up, then we can make changes
stockList <- c("Sp_Chinook","Fa_Chinook","Steelhead")
response.orig <- response
input.years= c(1980, 2022)
input.lat.orig <- input.lat
input.long.orig <- input.long
input.loocv=TRUE
loocvYears=43 # Sequentially removes each year in the LOO

# Store sensitivity results
spatialResults <- data.frame(data=as.character(), stock=as.character(),
                              index=as.character(), season=as.character(),
                              lag=as.numeric(),
                              looYear=as.numeric(),
                              startLat=as.numeric(), startLong=as.numeric(),
                              response=as.numeric(), pred=as.numeric(), 
                              stringsAsFactors = FALSE)

# Loop over variables
for (input.stock in stockList) {
  response<-response.orig[,c("year",input.stock)]
  
  # Set best parameters (taken from Figure 3)
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
  
  # Loop over Latitudes
  for (startLat in seq(-40, 50, 10)) {
    input.lat[1] <- startLat
    cmisst <- updateCMISST()
    loo<-cmisst[[7]]
    spatialResults <- rbind(spatialResults,
                            data.frame(data = input.spatialData,
                                       stock= input.stock, index=loo$model,
                                       season=loo$season, lag=input.lag,
                                       looYear=loo$year,
                                       startLat=startLat, startLong=input.long.orig[1],
                                       response=loo$response, pred=loo$pred,
                                       stringsAsFactors = FALSE))
  }
  input.lat <- input.lat.orig # reset

  # Loop over Longitudes
  for (startLong in seq(100, 220, 10)) {
    input.long[1] <- startLong
    cmisst <- updateCMISST()
    loo<-cmisst[[7]]
    spatialResults <- rbind(spatialResults,
                            data.frame(data = input.spatialData,
                                       stock= input.stock, index=loo$model,
                                       season=loo$season, lag=input.lag,
                                       looYear=loo$year,
                                       startLat=input.lat.orig[1], startLong=startLong,
                                       response=loo$response, pred=loo$pred,
                                       stringsAsFactors = FALSE))
  }
  input.long <- input.long.orig # reset
}

spatialResults$lag <- factor(spatialResults$lag)
spatialResults$stock <- factor(spatialResults$stock, levels = c("Sp_Chinook","Fa_Chinook","Steelhead"))
spatialResults$index <- factor(spatialResults$index, levels = c("cmisst","cmissh"))

# Save results
save(x = "spatialResults", file = 'manuscriptFigures/spatialResults')


# Make the figure---------------------------------------------------------------
library(patchwork)

load(file = 'manuscriptFigures/spatialResults')
# Summarize
spatialSummary <- spatialResults %>%
  group_by(stock, index, season, lag, startLat, startLong) %>%
  summarise(mae = mae_func(pred, response),
            rmse = rmse_func(pred, response),
            kge = hydroGOF::KGE(pred, response, method = "2021"), .groups = 'keep')

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
seasonPalette <- c("#56B4E9", "#009E73", "#E69F00", "#F0E442")
pretty_labels <- c("Sp_Chinook" = "Spring Chinook",
                   "Fa_Chinook" = "Fall Chinook",
                   "Steelhead" = "Steelhead")

# Starting Latitude plot
gg1 <- ggplot() + theme_classic() +
  geom_line(data=spatialSummary[spatialSummary$startLong == input.long.orig[1],], aes(x=startLat, y=rmse, col=season)) +
  ylab("Root Mean Squared Error") + xlab("Start Latitude")  +
  scale_color_discrete(name = "Season", labels = c("Winter","Spring","Summer","Autumn"), palette = seasonPalette) +
  facet_grid(cols = vars(stock),
             scales = "free_y",
             labeller = labeller(stock = pretty_labels), ) +
  theme(
    strip.text = element_text(size = 14),
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines")
  )
gg1

gg2 <- ggplot() + theme_classic() +
  geom_line(data=spatialSummary[spatialSummary$startLat == input.lat.orig[1],], aes(x=startLong, y=rmse, col=season)) +
  ylab("Root Mean Squared Error") + xlab("Start Longitude")  +
  scale_color_discrete(name = "Season", labels = c("Winter","Spring","Summer","Autumn"), palette = seasonPalette) +
  facet_grid(cols = vars(stock),
             scales = "free_y",
             labeller = labeller(stock = pretty_labels)) +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines")
  )
gg2


# Run all the lat and long code first
gg_spatial <- gg1 + gg2 +
  plot_layout(ncol = 1, guides = "collect", axes = "collect_y") 
gg_spatial

ggsave(filename = "manuscriptFigures/Fig 6 latlong.tiff",
       plot = gg_spatial, width = 190, height = 100, units = "mm", dpi = 500)
