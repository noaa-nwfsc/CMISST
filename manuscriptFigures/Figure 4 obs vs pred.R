# For the manuscript, this file runs the models needed to make Figure 3

# Load model results------------------------------------------------------------
if (file.exists('manuscriptFigures/looResults.RData')) {
  load(file = 'manuscriptFigures/looResults.RData')
} else {
  print("Please run code in Figure 3.R before running this script")
}
looResults$index <- toupper(looResults$index)
looResults$index[looResults$index == "SSTARC"] <- "SSTarc"
looResults$index <- factor(looResults$index, levels = c("CMISST","CMISSH","PDO","ONI","NPGO","SSTarc"))

# Make the figure---------------------------------------------------------------

#cbbPalette <- c("#56B4E9", "#009E73", "#E69F00", "#F0E442", "#0072B2", "#CC79A7")
cols <- c("#F8766D", "#00BA38", "#619CFF") # ggplot's defaults
pretty_labels <- c("Sp_Chinook" = "Spring Chinook",
                   "Fa_Chinook" = "Fall Chinook",
                   "Steelhead" = "Steelhead")

# Refine to just the best lag and season for each stock (from Figure 3)
looSummary <- looResults %>%
  filter((stock=="Sp_Chinook" & lag==2 & season=="spr") |
           (stock=="Fa_Chinook" & lag==3 & season=="aut") |
           (stock=="Steelhead" & lag==3 & season=="aut"))

gg_obs_pred <- ggplot(data = looSummary, aes(x = response, y = pred, color = stock)) +
  geom_point(cex=0.7) +
  scale_colour_manual(values=cols) +
  theme_bw() +
  labs(y="Leave One Out Predictions", x="Observed") +
  facet_grid(cols = vars(stock), rows = vars(index),
             switch = "y", scales = "free_x",
             labeller = labeller(stock = pretty_labels)) +
  theme(
    strip.background = element_blank(),  # Remove the box around strip labels
    strip.placement = "outside",         # Place strips outside the plot area
    #strip.text.y.left = element_text(angle = 0),  # Make y-strips readable
    legend.position="none", panel.grid = element_blank()
  )
gg_obs_pred

ggsave(filename = "manuscriptFigures/Fig 4 obs vs pred.tiff",
       plot = gg_obs_pred, width = 90, height = 160, units = "mm", dpi = 500)
