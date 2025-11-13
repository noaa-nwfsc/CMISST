# Create Figure 1, the basic covariance maps
source('R/run_CMISST.R') # This sets everything up, then we can make changes
stockList <- c("Sp_Chinook","Fa_Chinook","Steelhead")
response.orig <- response
input.years= c(1980, 2022)
input.loocv=FALSE

for (input.stock in c("Sp_Chinook", "Fa_Chinook", "Steelhead")) {

  response<-response.orig[,c("year",input.stock)]
  cmisst <- updateCMISST()
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  season <- switch(input.season, win = 2, spr = 3, sum = 4, aut = 5)
  myTitle <- switch(input.season, win = "Winter", spr = "Spring", sum = "Summer", aut = "Autumn")
  covMap<-cmisst[[season]]
  lmt <- 0.8; limits<-c(-lmt, lmt)
  extent <- cmisst[[6]] # min, max of lat, long
  
  gg <- ggplot() +
    ggtitle(switch(input.stock, Sp_Chinook="Spring Chinook", Fa_Chinook="Fall Chinook", Steelhead="Steelhead")) +
    geom_raster(data = melt(covMap), aes(x = Var1, y = Var2, fill=value)) +
    geom_sf(data=land, color="black", fill="grey", linewidth=0.25) +
    xlim(extent[3], extent[4]) + ylim(extent[1], extent[2]) +
    scale_fill_gradientn(colours = myPalette(100),limits=limits,name="Covariance", na.value = "white") +
    theme_classic() + theme(panel.border = element_rect(colour = "grey", fill=NA)) +
    #theme(legend.position = "none") +
    labs(x = "Longitude", y = "Latitude")
  assign(paste('ggCM_', input.stock, sep=''), gg)
}

ggCM <- ggCM_Sp_Chinook + ggCM_Fa_Chinook + ggCM_Steelhead + guide_area() +
  plot_layout(ncol = 2, guides = "collect")

ggsave(filename = "manuscriptFigures/Fig 1 covMaps.tiff",
       plot = ggCM, width = 190, height = 120, units = "mm", dpi = 500)
