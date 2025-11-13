# Create Figure 2, which is a simple time series of salmon returns
# and an example CMISST for each

source('R/run_CMISST.R') # This sets everything up, then we can make changes
stockList <- c("Sp_Chinook","Fa_Chinook","Steelhead")
response.orig <- response[,c("year", stockList)]
input.years= c(1980, 2022)
input.loocv=FALSE
response_long <- melt(response[,c("year", stockList)],
                      id.vars = "year", variable.name = "stock")
response_long$stock <- factor(response_long$stock, levels = stockList)

g1 <- ggplot(data = response_long[response_long$year>=input.years[1],]) +
  geom_line(aes(x = year, y = value/1000, col=stock), linewidth=1.1) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.2, 0.8)) + ylab("Adult Returns (x1000)") +
  scale_color_discrete(name = "", labels = c("Spring Chinook","Fall Chinook","Steelhead"))

# Make the correlation plots
cols <- c("#F8766D", "#00BA38", "#619CFF") # ggplot's defaults
colIndex<-1
for (input.stock in stockList) {
  response<-response.orig[,c("year",input.stock)]
  cmisst <- updateCMISST()
  index<-cmisst[[1]]
  cat(input.stock, "r =", round(cor(index$spr.cov, index$val, use = "pairwise.complete.obs"),2), "\n")
  gg <- ggplot(data = index) +
    geom_point(aes(x = spr.cov, y = val), size=1.5, col=cols[colIndex]) +
    theme_classic() +
    theme(plot.margin = margin(0.5, 0.01, 0.01, 0.01)) +
    xlab(paste(switch(input.stock, Sp_Chinook="Spring Ch.", Fa_Chinook="Fall Ch.", Steelhead="Steelhead"), "CMISST")) +
    ylab("Adult Returns\n(Scaled)")
  if (input.stock == "Fa_Chinook" | input.stock == "Steelhead") gg <- gg +
    theme(axis.title.y = element_text(color = "white"))
  label <- paste("r =", round(cor(index$spr.cov, index$val, use = "pairwise.complete.obs"),2), "\n")
  gg <- gg + geom_text(data = NULL, x = -1.5, y = 1.6, size=2.8, label = label)
  assign(paste("gg_",input.stock,"_cor", sep = ""), gg)
  colIndex=colIndex+1
}  

ggFig2 <- g1 + gg_Sp_Chinook_cor + gg_Fa_Chinook_cor + gg_Steelhead_cor + 
  plot_layout(design = "AAA\n###\nBCD", heights=c(6,0.5,4))
ggFig2

ggsave(filename = "manuscriptFigures/Fig 2 salmonData.tiff", plot = ggFig2,
       width = 190, height = 120, units = "mm", dpi = 500)
