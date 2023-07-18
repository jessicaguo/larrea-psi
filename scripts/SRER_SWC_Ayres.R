# SRER soil moisture time series from Feb-March 2023

library(neonUtilities)

# Read in the data
swc <- loadByProduct(dpID="DP1.00094.001", site = "SRER", startdate="2023-02", enddate="2023-03", package="expanded", timeIndex="30", check.size=F)

# Identify shallowest measurement level rows
d1 <- which(swc$SWS_30_minute$verticalPosition == "501")

# Identify soil plot rows
p1d1 <- intersect(which(swc$SWS_30_minute$horizontalPosition == "001"), d1)
p2d1 <- intersect(which(swc$SWS_30_minute$horizontalPosition == "002"), d1)
p3d1 <- intersect(which(swc$SWS_30_minute$horizontalPosition == "003"), d1)
p4d1 <- intersect(which(swc$SWS_30_minute$horizontalPosition == "004"), d1)
p5d1 <- intersect(which(swc$SWS_30_minute$horizontalPosition == "005"), d1)

# Identify data that passed the QA/QC tests
goodRows <- which(swc$SWS_30_minute$VSWCFinalQF == 0)

# Identify max soil water content for the shallowest measurement level
maxD1 <- max(swc$SWS_30_minute$VSWCMean[intersect(d1, goodRows)], na.rm=T)

# Create a time series plot of the shallowest sensor in each soil plot using unflagged data only
# png("C:/Users/eayres/Downloads/SRER_SWC.png", width = 6, height = 6, units = "in",res = 300)
plot(swc$SWS_30_minute$startDateTime[intersect(p1d1, goodRows)], swc$SWS_30_minute$VSWCMean[intersect(p1d1, goodRows)], ylab="soil water content (m3/m3)", xlab="", ylim=c(0, maxD1), pch=".", main="SRER")
points(swc$SWS_30_minute$startDateTime[intersect(p2d1, goodRows)], swc$SWS_30_minute$VSWCMean[intersect(p2d1, goodRows)], pch=".", col="red")
points(swc$SWS_30_minute$startDateTime[intersect(p3d1, goodRows)], swc$SWS_30_minute$VSWCMean[intersect(p3d1, goodRows)], pch=".", col="orange")
points(swc$SWS_30_minute$startDateTime[intersect(p4d1, goodRows)], swc$SWS_30_minute$VSWCMean[intersect(p4d1, goodRows)], pch=".", col="green")
points(swc$SWS_30_minute$startDateTime[intersect(p5d1, goodRows)], swc$SWS_30_minute$VSWCMean[intersect(p5d1, goodRows)], pch=".", col="blue")
legend("topleft", legend=c("Plot 1", "Plot 2", "Plot 3", "Plot 4", "Plot 5"), lty=1, col=c("black", "red", "orange", "green", "blue"), bty="n")
# dev.off()



# Assign water content final quality flag based on alpha and beta quality metrics, temp test, and science review flags
goodRowsHighAlpha <- intersect(intersect(
  intersect(intersect(intersect(which(swc$SWS_30_minute$VSWCAlphaQM < 30), 
                                which(swc$SWS_30_minute$VSWCBetaQM < 20)), 
                      c(which(swc$SWS_30_minute$VSWCFinalQFSciRvw == 0), 
                        which(is.na(swc$SWS_30_minute$VSWCFinalQFSciRvw), arr.ind=T))), 
            which(swc$SWS_30_minute$verticalPosition < 504)), 
  which(swc$SWS_30_minute$tempPassQM > 0)), 
  which(swc$SWS_30_minute$tempFailQM == 0))

# Create a time series plot of the shallowest sensor in each soil plot using unflagged data only but with a higher Alpha metric threshold
# png("C:/Users/eayres/Downloads/SRER_SWC_highAlpha.png", width = 6, height = 6, units = "in",res = 300)
plot(swc$SWS_30_minute$startDateTime[intersect(p1d1, goodRowsHighAlpha)], swc$SWS_30_minute$VSWCMean[intersect(p1d1, goodRowsHighAlpha)], ylab="soil water content (m3/m3)", xlab="", ylim=c(0, maxD1), pch=".", main="SRER: High Alpha metric")
points(swc$SWS_30_minute$startDateTime[intersect(p2d1, goodRowsHighAlpha)], swc$SWS_30_minute$VSWCMean[intersect(p2d1, goodRowsHighAlpha)], pch=".", col="red")
points(swc$SWS_30_minute$startDateTime[intersect(p3d1, goodRowsHighAlpha)], swc$SWS_30_minute$VSWCMean[intersect(p3d1, goodRowsHighAlpha)], pch=".", col="orange")
points(swc$SWS_30_minute$startDateTime[intersect(p4d1, goodRowsHighAlpha)], swc$SWS_30_minute$VSWCMean[intersect(p4d1, goodRowsHighAlpha)], pch=".", col="green")
points(swc$SWS_30_minute$startDateTime[intersect(p5d1, goodRowsHighAlpha)], swc$SWS_30_minute$VSWCMean[intersect(p5d1, goodRowsHighAlpha)], pch=".", col="blue")
legend("topleft", legend=c("Plot 1", "Plot 2", "Plot 3", "Plot 4", "Plot 5"), lty=1, col=c("black", "red", "orange", "green", "blue"), bty="n")
# dev.off()


