# Read data
################################################################################

# Packages
library(plyr)
library(dplyr)
library(shape) # colorlegend()
library(RColorBrewer)
library(fields) # colorbar.plot()
library(datalimited2)
library(ggplot2)
library(ggrepel)
library(scales)

source("analysis/plot_perf.R")

# Define directories
# Define directories
# datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/paper/data"
# plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/paper/figures"
datadir <- "data"
plotdir <- "figures"

# Read prediction data
preds.sim <- read.csv(paste(datadir, "com_status_predictions_simstocks.csv", sep="/"), as.is=T)


# Build data
################################################################################

# Methods
bbmsy_cols <- c("super_bbmsy", "comsir_bbmsy", "cmsy17_bbmsy", "cmsy13_bbmsy",
                "sscom_bbmsy", "ocom_bbmsy", "zbrt_bbmsy", "mprm_bbmsy")
method_abbrevs <- c("Super", "COM-SIR", "cMSY-17", "cMSY-13",
                    "SSCOM", "OCOM", "zBRT", "mPRM")

# Exploitation dynamics scenarios
eds <- c("ED0", "ED0.6", "OW", "RC")
ed.names <- c("(a) Constant", "(b) Biomass-coupled", "(c) Increasing", "(d) Roller coaster")

# Empty dataframe
ed.data <- data.frame(scenario=sort(rep(eds, length(method_abbrevs))),
                      method=rep(method_abbrevs,4),
                      accuracy=NA, ranking=NA, bias=NA)

# Loop through scenarios
for(i in 1:length(eds)){

  # Subset scenario data
  ed1 <- eds[i]
  sdata <- subset(preds.sim, ed==ed1)

  # Loop through methods
  for(j in 1:length(bbmsy_cols)){

    # Data
    obs <- sdata$true_bbmsy
    preds <- sdata[,bbmsy_cols[j]]

    # Calculate performance statistics
    # Proportional error = (predicted - observed) / (|observed|)
    # Bias: median proportional error
    # Accuracy: median absolute proportional error
    prop.error <- (preds-obs) / abs(obs)
    abs.prop.error <- abs(prop.error)
    bias <- median(prop.error, na.rm=T)
    accuracy <- median(abs.prop.error, na.rm=T)
    ranking <- cor(obs, preds, method="spearman", use="pairwise.complete.obs")

    # Record data
    method <- method_abbrevs[j]
    ed.data$accuracy[ed.data$scenario==ed1 & ed.data$method==method] <- accuracy
    ed.data$ranking[ed.data$scenario==ed1 & ed.data$method==method] <- ranking
    ed.data$bias[ed.data$scenario==ed1 & ed.data$method==method] <- bias

  }

}

b_range <- range(ed.data$bias)
ylim <- range(ed.data$ranking)
xlim <- range(ed.data$accuracy)

labs <- tibble(scenario = eds, scenario_clean = ed.names)

g <- left_join(ed.data, labs) %>%
  rename(inaccuracy = accuracy) %>%
  plot_perf(b_range = b_range, xlim = xlim, ylim = ylim, y = "ranking",
    point.padding = grid::unit(0.41, "lines"),
    min.segment.length = 0.3, force = 4) +
  facet_wrap(~scenario_clean) +
  theme(legend.position = c(0.44, 0.86)) +
  theme(strip.text = element_text(angle = 0, hjust = 0, size = rel(1)))
# g

ggsave(paste0(plotdir, "/fig3.pdf"), width = 6.75, height = 5.9)

# # Plot data
# ################################################################################
#
# # Bias distributions
# hist(ed.data$bias) # -1.0, 2.0
#
# # Bias colors
# cols <- brewer.pal(11, "RdBu")
# ed.data$bias_bin <- cut(ed.data$bias, breaks=seq(-2,2,length.out=11))
# ed.data$bias_color <- cols[ed.data$bias_bin]
#
# # Setup figure
# figname <- "Fig3_com_performance_effort_final.png"
# png(paste(plotdir, figname, sep="/"), width=5, height=5, units="in", res=600)
# par(mfrow=c(2,2), mar=c(2,2,0.5,0.5), mgp=c(2.5,0.8,0), oma=c(2,2,0,0), xpd=NA)
#
# # Positions
# # GBM, COM-SIR, cMSY-17, cMSY-13, SSCOM, OCOM, BRT, mPRM
# positions <- matrix(c(4,3,1,2,1,1,3,3,
#                       4,4,3,1,4,3,2,4,
#                       4,2,3,4,3,3,2,4,
#                       2,2,4,2,3,4,1,2), ncol=8, byrow=T)
#
# # Loop through ED scenarios and plot
# for(i in 1:length(eds)){
#
#   # Plot ED scenario
#   sdata <- subset(ed.data, scenario==eds[i])
#   plot(ranking ~ accuracy, sdata, bty="n", las=1,
#        pch=21, bg=sdata$bias_color, col="black", cex=1.5,
#        xlim=c(0,2), ylim=c(-0.2,0.6),
#        xlab="", ylab="", cex.axis=0.8)
#   text(x=sdata$accuracy, y=sdata$ranking, labels=sdata$method, cex=0.75, pos=4, xpd=NA)
#
#   # ED name
#   text(x=2.05, y=0.58, pos=2, font=2, labels=ed.names[i], cex=0.9)
#
# }
#
# # Add axis labels
# mtext("Inaccuracy (MAPE)", outer=T, side=1, adj=0.52, line=0.7, cex=0.9)
# mtext("Rank correlation", outer=T, side=2, adj=0.52, line=0.7, cex=0.9)
#
# # Off
# dev.off()
# graphics.off()
#
#
#
#
#
#
#
#
#
#
