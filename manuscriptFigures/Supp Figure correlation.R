# For the manuscript, this file runs the models needed to make the correlation plot

library(corrplot)

# Run the models----------------------------------------------------------------

source('R/run_CMISST.R') # This sets everything up, then we can make changes
stockList <- c("Sp_Chinook","Fa_Chinook","Steelhead")
response.orig <- response
input.years= c(1980, 2022)
input.loocv=FALSE


# Loop over variables
tiff("manuscriptFigures/Supp Fig corrplot.tiff",
     width = 200, height = 200, units = "mm", res = 500)
op <- par(mfrow = c(2,2), oma=c(2, 0, 2, 0))

for (input.stock in stockList) {
  response<-response.orig[,c("year",input.stock)]
  
  # Set best parameters (taken from Figure 3)
  if (input.stock == "Sp_Chinook") {
    input.lag <- 2
    input.spatialData == "ERSST"
    input.season <- "spr"
  } else if (input.stock == "Fa_Chinook") {
    input.lag <- 3
    input.spatialData == "SSH"
    input.season <- "aut"
  } else if (input.stock == "Steelhead") {
    input.lag <- 3
    input.spatialData == "ERSST"
    input.season <- "aut"
  }
  
  # Update CMISST and get the index
  cmisst <- updateCMISST()
  cmi <- cmisst[[1]]
  cmi_vals <- data.frame(year = cmi$year,
                         response = cmi$val,
                         cmi = cmi[,paste0(input.season,".cov")],
                         stringsAsFactors = FALSE)
  
  # Add in the other indiocators
  cmi_vals <- merge(cmi_vals, otherIndicators[,c("year",
                                     paste0("pdo.", input.season),
                                     paste0("npgo.", input.season),
                                     paste0("oni.", input.season),
                                     paste0("arc.", input.season))],
        by="year")
  colnames(cmi_vals) <- c("Year", "log(Counts)", "CMI","PDO","NPGO","ONI","SSTarc")
  
  # Make the correlation matrix and the plot
  mat <- cor(cmi_vals[,-1], use = "pairwise.complete.obs")
  corrplot(mat, method = "circle", diag = FALSE, type = 'lower', mar = c(2,0,2,0),
           tl.col = "black", tl.srt = 45, addCoef.col = "black",
           title = switch(input.stock, Sp_Chinook="Spring Chinook", Fa_Chinook="Fall Chinook", Steelhead="Steelhead"))
  #assign(paste('cp_', input.stock, sep=''), recordPlot())
}
par(op)
dev.off()

