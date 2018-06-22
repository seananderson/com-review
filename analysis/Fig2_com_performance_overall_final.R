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
# datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/paper/data"
# plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/paper/figures"
datadir <- "data"
plotdir <- "figures"

# Read prediction data
ram <- read.csv(paste(datadir, "com_status_predictions_ramldb.csv", sep="/"), as.is=T)
sim <- read.csv(paste(datadir, "com_status_predictions_simstocks.csv", sep="/"), as.is=T)

# Build data
################################################################################

# Continuous performance
bbmsy_cols <- c("true_bbmsy",
  "super_bbmsy", "comsir_bbmsy", "cmsy17_bbmsy", "cmsy13_bbmsy",
  "sscom_bbmsy", "ocom_bbmsy", "zbrt_bbmsy", "mprm_bbmsy")
method_abbrevs <- c("Super", "COM-SIR", "cMSY-17", "cMSY-13",
  "SSCOM", "OCOM", "zBRT", "mPRM")
bbmsy_ram <- na.omit(ram[,bbmsy_cols])
bbmsy_sim <- na.omit(sim[,bbmsy_cols])
cont_ram <- performance(bbmsy_ram, method_abbrevs)
cont_sim <- performance(bbmsy_sim, method_abbrevs)

b_range <- range(c(cont_ram$bias, cont_sim$bias))
ylim <- range(c(cont_ram$rank, cont_sim$rank))
xlim <- range(c(cont_ram$inaccuracy, cont_sim$inaccuracy))
g2 <- plot_perf(cont_ram, b_range = b_range, xlim = xlim, ylim = ylim) +
  ylab("") + theme(legend.position = c(0.15, 0.70)) +
  ggtitle("(b) RAMLDB; continuous performance")
g1 <- plot_perf(cont_sim, b_range = b_range, xlim = xlim, ylim = ylim) +
  guides(fill = FALSE) + ggtitle("(a) Simulated; continuous performance")


# Categorical performance
status_cols <- c("true_status",
  "ssp02_status", "ssp13_status", "orcs_status",
  "super_status", "comsir_status", "cmsy17_status", "cmsy13_status",
  "sscom_status", "ocom_status", "zbrt_status", "mprm_status")
method_abbrevs <- c("SSP-02", "SSP-13", "rORCS", "Super", "COM-SIR", "cMSY-17", "cMSY-13",
  "SSCOM", "OCOM", "zBRT", "mPRM")
status_cols1 <- c("true_status",
  "ssp02_status", "ssp13_status",
  "super_status", "comsir_status", "cmsy17_status", "cmsy13_status",
  "sscom_status", "ocom_status", "zbrt_status", "mprm_status")
method_abbrevs1 <- c("SSP-02", "SSP-13", "Super", "COM-SIR", "cMSY-17", "cMSY-13",
  "SSCOM", "OCOM", "zBRT", "mPRM")
status_ram <- na.omit(ram[,status_cols])
status_sim <- na.omit(sim[,status_cols1])
catg_ram <- performance(status_ram, method_abbrevs)
catg_sim <- performance(status_sim, method_abbrevs1)

ylim <- range(c(catg_ram$kappa, catg_sim$kappa))
xlim <- range(c(catg_ram$accuracy, catg_sim$accuracy))
lab_x = xlim[2] + 0.01
g4 <- catg_ram %>%
  rename(inaccuracy = accuracy) %>%
  mutate(bias = 1) %>%
  plot_perf(y = "kappa", xlim = xlim, ylim = ylim) + ylab("Cohen's kappa") +
  scale_fill_continuous(low = "grey65", high = "grey65") +
  guides(fill = FALSE) + ylab("") + xlab("Accuracy") +
  geom_hline(yintercept = 0.4, col = "grey70", lty = 2) +
  geom_hline(yintercept = 0.2, col = "grey70", lty = 2) +
  annotate("text", x = lab_x, y = 0.45, label = "good", hjust = 1, col = "grey80") +
  annotate("text", x = lab_x, y = 0.25, label = "fair", hjust = 1, col = "grey80") +
  annotate("text", x = lab_x, y = 0.15, label = "poor", hjust = 1, col = "grey80") +
  ggtitle("(d) RAMLDB; categorical performance")

g3 <- catg_sim %>%
  rename(inaccuracy = accuracy) %>%
  mutate(bias = 1) %>%
  plot_perf(y = "kappa", xlim = xlim, ylim = ylim) + ylab("Cohen's kappa") +
  scale_fill_continuous(low = "grey65", high = "grey65") +
  guides(fill = FALSE) + xlab("Accuracy") +
  geom_hline(yintercept = 0.4, col = "grey70", lty = 2) +
  geom_hline(yintercept = 0.2, col = "grey70", lty = 2) +
  annotate("text", x = lab_x, y = 0.45, label = "good", hjust = 1, col = "grey80") +
  annotate("text", x = lab_x, y = 0.25, label = "fair", hjust = 1, col = "grey80") +
  annotate("text", x = lab_x, y = 0.15, label = "poor", hjust = 1, col = "grey80") +
  ggtitle("(c) Simulated; categorical performance")


g <- cowplot::plot_grid(g1, g2, g3, g4, nrow = 2,
  # labels = c("(a)", "(b)", "(c)", "(d)"),
  label_x = 0.15, label_y = 0.88, label_size = 10)
ggsave(paste0(plotdir, "/fig2.pdf"), width = 6.75, height = 5.9)

# # Plot data
# ################################################################################
#
# # Bias colors
# bvals <- c(cont_ram$bias, cont_sim$bias)
# bmin <- floor(min(bvals) / 0.1) * 0.1 # lower val
# bmax <- ceiling(max(bvals) / 0.1) * 0.1 # upper val
# blim <- c(bmin, bmax)
# babs <- blim[which.max(abs(blim))] # which value is largest?
# blim1 <- c(babs*-1, babs) # create - and + version of largest value so color pal is even
# b_breaks <- seq(blim1[1], blim1[2], length.out=11)
# cont_ram$col_bin <- cut(cont_ram$bias, breaks=b_breaks)
# cont_ram$col <- RColorBrewer::brewer.pal(11, "RdBu")[cont_ram$col_bin]
# cont_sim$col_bin <- cut(cont_sim$bias, breaks=b_breaks)
# cont_sim$col <- RColorBrewer::brewer.pal(11, "RdBu")[cont_sim$col_bin]
#
# # Setup figure
# figname <- "Fig2_com_performance_overall_final.png"
# png(paste(plotdir, figname, sep="/"), width=5, height=5, units="in", res=600)
# par(mfrow=c(2,2), mar=c(3,2,0.5,0.5), mgp=c(2.5,0.8,0), oma=c(0,2,0.8,0), xpd=NA)
#
# # Continuous
# #######################
#
# # A. Simulated stocks
# plot(rank ~ inaccuracy, cont_sim, bty="n", las=1,
#   pch=21, col="black", bg=cont_sim$col, cex=1.4,
#   xaxt="n", xlim=c(0.3, 0.6), ylim=c(0,0.8), cex.axis=0.9, xlab="", ylab="Rank correlation")
# axis(1, at=seq(0.3, 0.6, 0.1), cex.axis=0.9)
# title("Simulated stocks (n=5491)", cex.main=0.9, line=0.2)
# pos <- c(4,1,3,3,2,2,1,2)
# text(x=cont_sim$inaccuracy, y=cont_sim$rank, labels=cont_sim$method, cex=0.75, pos=pos, xpd=NA)
# text(x=0.3-(0.6-0.3)*0.05, y=0.8-(0.8-0.0)*0.05, pos=4, labels="Continuous performance", cex=0.9, font=4)
#
# # B. RAMLDB stocks
# plot(rank ~ inaccuracy, cont_ram, bty="n", las=1,
#   pch=21, col="black", bg=cont_ram$col, cex=1.4,
#   xaxt="n", xlim=c(0.3, 0.6), ylim=c(0,0.8), cex.axis=0.9, xlab="", ylab="")
# axis(1, at=seq(0.3, 0.6, 0.1), cex.axis=0.9)
# title("RAMLDB stocks (n=135)", cex.main=0.9, line=0.2)
# pos <- c(4,1,3,3,2,2,1,2)
# text(x=cont_ram$inaccuracy, y=cont_ram$rank, labels=cont_ram$method, cex=0.75, pos=pos, xpd=NA)
#
#
# # Categorical
# #######################
#
# # C. Simulated stocks
# plot(kappa ~ accuracy, catg_sim, bty="n", las=1,
#   pch=21, col="black", bg="grey80", cex=1.4,
#   xlim=c(0.3, 0.8), ylim=c(-0.1,0.6), cex.axis=0.9, xlab="", ylab="Cohen's kappa")
# lines(x=c(0.3, 0.8), y=c(0.2,0.2), lty=3, lwd=1.4, col="grey70")
# lines(x=c(0.3, 0.8), y=c(0.4,0.4), lty=3, lwd=1.4, col="grey70")
# pos <- c(4,1,3,3,2,2,1,2)
# text(x=catg_sim$accuracy, y=catg_sim$kappa, labels=catg_sim$method, cex=0.75, pos=pos, xpd=NA)
# # text(x=0.82, y=0.17, pos=2, labels="poor", cex=0.9, col="grey70", xpd=NA)
# # text(x=0.82, y=0.23, pos=2, labels="fair", cex=0.9, col="grey70", xpd=NA)
# # text(x=0.82, y=0.43, pos=2, labels="good", cex=0.9, col="grey70", xpd=NA)
# text(x=0.3-(0.8-0.3)*0.05, y=0.6-(0.6--0.1)*0.05, pos=4, labels="Categorical performance", cex=0.9, font=4)
#
# # D. RAMLDB stocks
# plot(kappa ~ accuracy, catg_ram, bty="n", las=1,
#   pch=21, col="black", bg="grey80", cex=1.4,
#   xlim=c(0.3, 0.8), ylim=c(-0.1,0.6), cex.axis=0.9, xlab="", ylab="")
# lines(x=c(0.3, 0.8), y=c(0.2,0.2), lty=3, lwd=1.4, col="grey70")
# lines(x=c(0.3, 0.8), y=c(0.4,0.4), lty=3, lwd=1.4, col="grey70")
# text(x=catg_ram$accuracy, y=catg_ram$kappa, labels=catg_ram$method, cex=0.75, pos=pos, xpd=NA)
# text(x=0.82, y=0.17, pos=2, labels="poor", cex=0.9, col="grey70", xpd=NA)
# text(x=0.82, y=0.23, pos=2, labels="fair", cex=0.9, col="grey70", xpd=NA)
# text(x=0.82, y=0.43, pos=2, labels="good", cex=0.9, col="grey70", xpd=NA)
#
# # Add x-axis labels
# mtext("Accuracy", outer=T, side=1, adj=0.53, line=-1.2, cex=0.8)
# mtext("Inaccuracy (MAPE)", outer=T, side=1, adj=0.53, line=-15.8, cex=0.8)
#
# # Off
# dev.off()
# graphics.off()
