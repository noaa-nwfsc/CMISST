# For the manuscript, this file runs the models needed to make Figure 3

# Run the models----------------------------------------------------------------
source('R/run_CMISST.R') # This sets everything up, then we can make changes
stockList <- c("Sp_Chinook","Fa_Chinook","Steelhead")
response.orig <- response
input.years= c(1980, 2022)
includePDO = TRUE # this will also include ONI NPGO and SSTarc
input.loocv=TRUE
loocvYears=43 # Sequentially removes each of the 43 years in the LOO

looResults <- data.frame(data=as.character(), stock=as.character(),
                         index=as.character(), season=as.character(),
                         lag=as.numeric(),
                         looYear=as.numeric(), 
                         response=as.numeric(), pred=as.numeric(), 
                         stringsAsFactors = FALSE)

# Loop over combinations
for (input.spatialData in c("ERSST","SSH")) {
  for (input.stock in stockList) {
    response<-response.orig[,c("year",input.stock)]
    
    for (input.lag in 2:3) {
      cmisst <- updateCMISST()
      loo <- cmisst[[7]]
      if (input.spatialData == "SSH") loo$model <- "cmissh"
      looResults <- rbind(looResults, data.frame(data = input.spatialData,
                                                 stock = rep(input.stock, nrow(loo)),
                                                 index=loo$model, season=loo$season,
                                                 lag=input.lag, looYear=loo$year,
                                                 response=loo$response, pred=loo$pred,
                                                 stringsAsFactors = FALSE))
    }
  }
  # when moving to SSH, don't bother doing these other things - they've already been run
  includePDO = FALSE
}

looResults$lag <- factor(looResults$lag)
looResults$stock <- factor(looResults$stock, levels = c("Sp_Chinook","Fa_Chinook","Steelhead"))
looResults$index <- factor(looResults$index, levels = c("cmisst","cmissh","pdo","oni","SSTarc","npgo"))

# Save results
save(x = "looResults", file = 'manuscriptFigures/looResults.RData')

# Summarize
looSummary <- looResults %>%
  group_by(stock, index, season, lag) %>%
  summarise(mae = mae_func(pred, response),
            rmse = rmse_func(pred, response),
            kge = hydroGOF::KGE(pred, response, method = "2021"), .groups = 'keep')

# Make the figure---------------------------------------------------------------
library(lemon)

load(file = 'manuscriptFigures/looResults.RData')

# Summarize
looResults <- looResults %>%
  group_by(stock, index, season, lag) %>%
  summarise(mae = mae_func(pred, response),
            rmse = rmse_func(pred, response),
            kge = hydroGOF::KGE(pred, response, method = "2021"), .groups = 'keep')
looResults$index <- toupper(looResults$index)
looResults$index[looResults$index == "SSTARC"] <- "SSTarc"
looResults$index <- factor(looResults$index, levels = c("CMISST","CMISSH","PDO","ONI","NPGO","SSTarc"))

# Points by stock
pretty_labels <- c("Sp_Chinook" = "Spring Chinook",
                   "Fa_Chinook" = "Fall Chinook",
                   "Steelhead" = "Steelhead")
pretty_x_axis <- c(win = "Winter",
                   spr = "Spring",
                   sum = "Summer",
                   aut = "Autumn")
cbbPalette <- c("#56B4E9", "#009E73", "#E69F00", "#F0E442", "#0072B2", "#CC79A7")
dw<-0.4
gg_mae <- ggplot(data = looResults) +
  geom_point(position=position_dodge(width = dw),
             aes(x = season, y = rmse, group=index, col=index, pch=lag), cex=3) +
  scale_colour_manual(values=cbbPalette) +
  scale_x_discrete(labels = pretty_x_axis) +
  theme_classic() +
  labs(y="Root Mean Squared Error", x=NULL) +
  facet_rep_wrap(~stock, nrow = 3, strip.position = "left", scales = 'free_x',
                 labeller = labeller(stock=pretty_labels), repeat.tick.labels='top') +
  theme(strip.placement = "outside", strip.background = element_blank(),
        axis.text.x = element_text(size=12), strip.text = element_text(size=12),
        axis.title.y = element_text(size=14))
#facet_wrap(vars(stock), nrow = 3)
gg_mae

ggsave(filename = "manuscriptFigures/Fig 3 compare2pdo.tiff",
       plot = gg_mae, width = 140, height = 160, units = "mm", dpi = 500)
