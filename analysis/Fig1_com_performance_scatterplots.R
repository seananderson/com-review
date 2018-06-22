# READ DATA
################################################################################

# Define directories
# datadir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/paper/data"
# plotdir <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/paper/figures"
datadir <- "data"
plotdir <- "figures"

# Read prediction data
preds.ram.full <- read.csv(paste(datadir, "com_status_predictions_ramldb.csv", sep="/"), as.is=T)
preds.sim.full <- read.csv(paste(datadir, "com_status_predictions_simstocks.csv", sep="/"), as.is=T)

line_col <- "grey35"
# line_col <- "red"

# HELPER FUNCTIONS
################################################################################

# Plot functions
# obs <- preds.sim$true_bbmsy; ests <- preds.sim$cmsy17_bbmsy; xaxt<-T, yaxt<-T
plot_data <- function(obs, ests, xaxt, yaxt){

  # Plot point density
  tck <- -0.02
  smoothScatter(obs, ests,
                nrpoint=0, nbin=50,
                las=0, cex.axis=0.7, tck=-0.02,
                xlim=c(0,5), ylim=c(0,5), xaxt="n", yaxt="n", xlab="", ylab="")
  if(xaxt==T){axis(1, at=0:5, labels=T, cex.axis=0.8, tck=tck, col.ticks = line_col, col = line_col)}else{axis(1, at=0:5, labels=F, tck=tck, col.ticks = line_col, col = line_col)}
  if(yaxt==T){axis(2, at=0:5, labels=T, las = 1, cex.axis=0.8, tck=tck, col.ticks = line_col, col = line_col)}else{axis(2, at=0:5, labels=F, tck=tck, col.ticks = line_col, col = line_col)}

  # Calculate performance statistics
  # Proportional error = (predicted - observed) / (|observed|)
  # Bias: median proportional error
  # Accuracy: median absolute proportional error
  prop.error <- (ests-obs) / abs(obs)
  abs.prop.error <- abs(prop.error)
  bias <- median(prop.error, na.rm=T)
  accuracy <- median(abs.prop.error, na.rm=T)
  ranking <- cor(obs, ests, method="spearman", use="pairwise.complete.obs")

  # Print correlation
  rtext <- paste0("r=", format(round(ranking, 2), nsmall=2))
  mpe.text <- paste0("MPE=", format(round(bias, 2), nsmall=2))
  mape.text <- paste0("MAPE=", format(round(accuracy, 2), nsmall=2))
  stat.text <- paste(rtext, mpe.text, mape.text, sep="\n")
  text(labels=stat.text, pos=2, x=5.2, y=4.3, cex=0.75)

  # Add marker lines
  lines(x=c(1,1), y=c(0,5), lwd=mline.lwd, lty=3) # vertical
  lines(x=c(0,5), y=c(1,1), lwd=mline.lwd, lty=3) # horizontal
  lines(x=c(0,3.8), y=c(0,3.8), lwd=mline.lwd, lty=3) # diagonal

}

# PLOT DATA
################################################################################

# Params
mline.lwd <- 0.4

# B/BMSY predictions to evaluate
bbmsy_cols1 <- c("super_bbmsy", "comsir_bbmsy", "cmsy17_bbmsy", "cmsy13_bbmsy")
bbmsy_cols2 <- c("sscom_bbmsy", "ocom_bbmsy", "zbrt_bbmsy", "mprm_bbmsy")
bbmsy_methods1 <- c("Superensemble", "COM-SIR", "cMSY-2017", "cMSY-2013")
bbmsy_methods2 <- c("SSCOM", "OCOM", "zBRT", "mPRM")

# Reduce to complete datasets
bbmsy_cols <- c(bbmsy_cols1, bbmsy_cols2)
nconverge.ram <- apply(preds.ram.full[, bbmsy_cols], 1, function(x) sum(!is.na(x)))
nconverge.sim <- apply(preds.sim.full[, bbmsy_cols], 1, function(x) sum(!is.na(x)))
preds.ram <- preds.ram.full[nconverge.ram==8,]
preds.sim <- preds.sim.full[nconverge.sim==8,]

# Setup figure
figname <- "fig1.pdf"
pdf(paste(plotdir, figname, sep="/"), width=6, height=6)
gap.ht <- 0.04 # percentage of figure height occupied by gap
reg.ht <- (1-gap.ht)/4
layout(matrix(data=c(1,3,5,7,
                     2,4,6,8,
                     9,9,9,9,
                     10,12,14,16,
                     11,13,15,17), ncol=4, byrow=T), heights=c(reg.ht, reg.ht, gap.ht, reg.ht, reg.ht))
par(mar=c(1, 1, 0.5, 0.5), mgp=c(2,0.6,0), oma=c(2,2,1,0))

par(col.axis = line_col, col.lab = line_col, col = line_col)

# Plot top section
for(i in 1:length(bbmsy_cols1)){

  # Which method
  bbmsy_col <- bbmsy_cols1[i]
  bbmsy_method <- bbmsy_methods1[i]

  # Plot RAMLDB performance
  xaxt <- F
  yaxt <- ifelse(i==1, T, F)
  plot_data(obs=preds.ram$true_bbmsy, ests=preds.ram[,bbmsy_col], xaxt=xaxt, yaxt=yaxt)
  if(i==1){mtext("RAMLDB\nstocks", side=3, adj=0.05, line=-2, cex=0.6, font=2)}
  mtext(bbmsy_method, side=3, adj=0.5, line=0.2, cex=0.7, font=2)

  # Plot simstock performance
  plot_data(obs=preds.sim$true_bbmsy, ests=preds.sim[,bbmsy_col], xaxt=xaxt, yaxt=yaxt)
  if(i==1){mtext("Simulated\nstocks", side=3, adj=0.05, line=-2, cex=0.6, font=2)}

}

# Add space
plot.new()

# Plot bottom section
for(i in 1:length(bbmsy_cols2)){

  # Which method
  bbmsy_col <- bbmsy_cols2[i]
  bbmsy_method <- bbmsy_methods2[i]

  # Plot RAMLDB performance
  xaxt <- F
  yaxt <- ifelse(i==1, T, F)
  plot_data(obs=preds.ram$true_bbmsy, ests=preds.ram[,bbmsy_col], xaxt=xaxt, yaxt=yaxt)
  if(i==1){mtext("RAMLDB\nstocks", side=3, adj=0.05, line=-2, cex=0.6, font=2)}
  mtext(bbmsy_method, side=3, adj=0.5, line=0.2, cex=0.7, font=2)

  # Plot simstock performance
  xaxt <- T
  plot_data(obs=preds.sim$true_bbmsy, ests=preds.sim[,bbmsy_col], xaxt=xaxt, yaxt=yaxt)
  if(i==1){mtext("Simulated\nstocks", side=3, adj=0.05, line=-2, cex=0.6, font=2)}

}

# Axis labels
xlabel <- expression(paste("B/B"["MSY"], " observed"))
ylabel <- expression(paste("B/B"["MSY"], " predicted"))
mtext(xlabel, outer=T, side=1, line=0.8, adj=0.51, cex=0.7)
mtext(ylabel, outer=T, side=2, line=0.4, adj=0.5, cex=0.7)

# Off
dev.off()
graphics.off()
