# Create the LOO results tables for the Supplement

library(kableExtra)
library(knitr)
library(dplyr)

source('R/run_CMISST.R') # This sets everything up, then we can make changes

load(file = 'manuscriptFigures/looResults.RData')
looResults$stock <- as.character(looResults$stock)
looResults$stock[looResults$stock=="Sp_Chinook"] <- "Spring Chinook"
looResults$stock[looResults$stock=="Fa_Chinook"] <- "Fall Chinook"
looResults$stock[looResults$stock=="Steelhead"] <- "Steelhead"
looResults$stock <- factor(looResults$stock, levels = c("Spring Chinook","Fall Chinook","Steelhead"))

# Summarize
looSummary <- looResults %>%
  group_by(stock, index, season, lag) %>%
  summarise(MAE = mae_func(pred, response),
            RMSE = rmse_func(pred, response),
            KGE = hydroGOF::KGE(pred, response, method = "2021"), .groups = 'keep')
looSummary$index <- toupper(looSummary$index)
looSummary$index[looSummary$index == "SSTARC"] <- "SSTarc"
looSummary$index <- factor(looSummary$index, levels = c("CMISST","CMISSH","PDO","ONI","NPGO","SSTarc"))

colnames(looSummary) <- c("Stock","Index","Season","Lag","MAE","RMSE","KGE")

kbl(looSummary[looSummary$Season=="spr" & looSummary$Lag==2,]) %>%
  kable_classic_2(bootstrap_options = c("striped", "condensed"),
                  full_width = F, font_size = 15)

# alternative version, used in the manuscript
l2 <- looSummary[looSummary$Lag==2,]
l3 <- looSummary[looSummary$Lag==3,]
wideData <- merge(l2, l3, by=c("Stock","Index","Season"))
# reorder columns (and remove lags)
wideData <- wideData[,c("Stock","Index","Season","MAE.x","MAE.y",
                        "RMSE.x","RMSE.y","KGE.x","KGE.y")]
wideData <- wideData[order(wideData$Stock, wideData$Index),]
wideData[,4:9] <- round(wideData[,4:9], 2)

write.csv(wideData, "manuscriptFigures/SupplementTable.csv", row.names = FALSE)

kbl(wideData,
    col.names = c(
      "Stock","Index","Season","Lag 2","Lag 3",
      "Lag 2","Lag 3","Lag 2","Lag 3"
    ), row.names = FALSE) %>%
  add_header_above(c(" " = 3, "MAE" = 2, "RMSE" = 2, "KGE" = 2)) %>%
  kable_classic_2(bootstrap_options = c("striped", "condensed"),
                  full_width = F, font_size = 15, html_font = "arial")

# estimate the proportional improvement in RMSE provided by CMI
for (ss in unique(looSummary$Stock)) {
  bestCMI <- min(looSummary$RMSE[looSummary$Stock == ss & looSummary$Index %in% c("CMISST","CMISSH")])
  bestNonCMI <- min(looSummary$RMSE[looSummary$Stock == ss & !looSummary$Index %in% c("CMISST","CMISSH")])
  cat(ss, ": ", round((bestNonCMI - bestCMI) / bestNonCMI, 2), "\n")
}
