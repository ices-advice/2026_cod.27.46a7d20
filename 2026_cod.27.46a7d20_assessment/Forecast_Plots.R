.libPaths(c(
  "C:/Users/RH28/Documents/R/win-library/4.3",
  "C:/Users/RH28/AppData/Local/Programs/R/R-4.3.2/library"
))

library(stockassessment)
library(multiStockassessment)
library(icesTAF)

TAF::mkdir("report/forecasts")

load("model/model.Rdata")
load("model/forecasts/Forecast_0.7FScaler_1000SIM_Final.Rdata")

fix_fit <- fit 
fix_mf <- FC[[13]]$result # was 15 common F reduction now 13 2026


# #all comparing error forecast to forecast which we don't have this year 
# #SSB_D_O <- ssbtable(old_fit, returnList=TRUE)
# #SSB_D_F <- ssbtable(fix_fit, returnList=TRUE)
# 
# #SSB_F_F <- lapply(lapply(fix_mf,attr,"tab"),function(x) x[1:4,7:9])
# #SSB_F_O <- lapply(lapply(old_mf,attr,"tab"),function(x) x[1:4,7:9])
# 
# 
# 
# png("report/forecasts/update_forecast_ssb.png",1500,1500)
# par(mfrow=c(3,2), mar=c(0.1,0.1,0.1,0.1), oma=c(6,6,6,6))
# for(s in 1:3){
#   yy <- sort(unique(as.numeric(c(rownames(SSB_D_F[[s]]),rownames(SSB_F_F[[s]])))))
#   plot(as.numeric(rownames(SSB_D_O[[s]])),SSB_D_O[[s]][,1], xlim=range(yy),ylim=range(SSB_D_F[[s]],SSB_F_F[[s]],SSB_F_O[[s]]), type ="l",lwd=3,xlab=NA,ylab=NA,axes=FALSE)
#   ## Add the other version
#   lines(as.numeric(rownames(SSB_F_F[[s]])),SSB_F_F[[s]][,1],lty=1,col=caMisc::dtucols(1),lwd=3)
#   # ## Add deterministic, bio process projection
#   # lines(YR_DF_F_BP[[s]],SSB_DF_F_BP[[s]],lty=2,col=caMisc::dtucols(2),lwd=3)
#   # ## Add deterministic, average projection
#   # lines(YR_DF_F_AV[[s]],SSB_DF_F_AV[[s]],lty=3,col=caMisc::dtucols(3),lwd=3)    
#   lines(as.numeric(rownames(SSB_D_O[[s]])),SSB_D_O[[s]][,1],lwd=3,lty=1)
#   lines(as.numeric(rownames(SSB_D_O[[s]])),SSB_D_O[[s]][,2],lwd=3,lty=2)
#   lines(as.numeric(rownames(SSB_D_O[[s]])),SSB_D_O[[s]][,3],lwd=3,lty=2)
#   suppressWarnings(arrows(as.numeric(rownames(SSB_F_O[[s]])),
#                           SSB_F_O[[s]][,2],
#                           as.numeric(rownames(SSB_F_O[[s]])),
#                           SSB_F_O[[s]][,3], lwd = 3, col = "lightgrey", 
#                           angle = 90, code = 3, length = 0.1))
#   points(as.numeric(rownames(SSB_F_O[[s]])),SSB_F_O[[s]][,1],pch=16,cex=1.5)
#   mtext(names(old_mf)[s],3,-3)
#   box()
#   axis(2)
#   if(s==1)
#     axis(3)
#   if(s==3)
#     axis(1)    
#   plot(as.numeric(rownames(SSB_D_F[[s]])),SSB_D_F[[s]][,1], xlim=range(yy),ylim=range(SSB_D_F[[s]],SSB_F_F[[s]],SSB_F_O[[s]]), type ="l",lwd=3,xlab=NA,ylab=NA,axes=FALSE)
#   lines(as.numeric(rownames(SSB_F_O[[s]])),SSB_F_O[[s]][,1],lty=1,col="darkgrey",lwd=3)
#   ## Add the other version
#   lines(as.numeric(rownames(SSB_F_O[[s]])),SSB_F_O[[s]][,1],lty=1,col=caMisc::dtucols(1),lwd=3)
#   # ## Add deterministic, bio process projection
#   # lines(YR_DF_F_BP[[s]],SSB_DF_F_BP[[s]],lty=2,col=caMisc::dtucols(2),lwd=3)
#   # ## Add deterministic, average projection
#   # lines(YR_DF_F_AV[[s]],SSB_DF_F_AV[[s]],lty=3,col=caMisc::dtucols(3),lwd=3)    
#   lines(as.numeric(rownames(SSB_D_F[[s]])),SSB_D_F[[s]][,1],lwd=3,lty=1)   
#   lines(as.numeric(rownames(SSB_D_F[[s]])),SSB_D_F[[s]][,2],lwd=3,lty=2)
#   lines(as.numeric(rownames(SSB_D_F[[s]])),SSB_D_F[[s]][,3],lwd=3,lty=2)
#   suppressWarnings(arrows(as.numeric(rownames(SSB_F_F[[s]])),
#                           SSB_F_F[[s]][,2],
#                           as.numeric(rownames(SSB_F_F[[s]])),
#                           SSB_F_F[[s]][,3], lwd = 3, col = "lightgrey", 
#                           angle = 90, code = 3, length = 0.1))
#   points(as.numeric(rownames(SSB_F_F[[s]])),SSB_F_F[[s]][,1],pch=16,cex=1.5)
#   mtext(names(old_mf)[s],3,-3)
#   box()
#   axis(4)
#   if(s==1)
#     axis(3)
#   if(s==3)
#     axis(1)
#   mtext("SSB",2,line=3,outer=TRUE)
#   mtext("SSB",4,line=3,outer=TRUE)
# }
# mtext("Previous",3,line=3,outer=TRUE,at=0.25,font=2)
# mtext("Updated",3,line=3,outer=TRUE,at=0.75,font=2)
# dev.off()
# 
# 
# 
# 
# C_D_O <- catchtable(old_fit, returnList=TRUE)
# C_D_F <- catchtable(fix_fit, returnList=TRUE)
# 
# C_F_F <- lapply(lapply(fix_mf,attr,"tab"),function(x) x[1:3,10:12])
# C_F_O <- lapply(lapply(old_mf,attr,"tab"),function(x) x[1:3,10:12])
# 
# 
# png("report/forecasts/update_forecast_catch.png",1500,1500)
# par(mfrow=c(3,2), mar=c(0.1,0.1,0.1,0.1), oma=c(6,6,6,6))
# for(s in 1:3){
#   yy <- sort(unique(as.numeric(c(rownames(C_D_F[[s]]),rownames(C_F_F[[s]])))))
#   plot(as.numeric(rownames(C_D_O[[s]])),C_D_O[[s]][,1], xlim=range(yy),ylim=range(C_D_F[[s]],C_F_F[[s]],C_F_O[[s]]), type ="l",lwd=3,xlab=NA,ylab=NA,axes=FALSE)
#   ## Add the other version
#   lines(as.numeric(rownames(C_F_F[[s]])),C_F_F[[s]][,1],lty=1,col=caMisc::dtucols(1),lwd=3)
#   # ## Add deterministic, bio process projection
#   # lines(YR_DF_F_BP[[s]],C_DF_F_BP[[s]],lty=2,col=caMisc::dtucols(2),lwd=3)
#   # ## Add deterministic, average projection
#   # lines(YR_DF_F_AV[[s]],C_DF_F_AV[[s]],lty=3,col=caMisc::dtucols(3),lwd=3)    
#   lines(as.numeric(rownames(C_D_O[[s]])),C_D_O[[s]][,1],lwd=3,lty=2)
#   lines(as.numeric(rownames(C_D_O[[s]])),C_D_O[[s]][,2],lwd=3,lty=2)
#   lines(as.numeric(rownames(C_D_O[[s]])),C_D_O[[s]][,3],lwd=3,lty=2)
#   suppressWarnings(arrows(as.numeric(rownames(C_F_O[[s]])),
#                           C_F_O[[s]][,2],
#                           as.numeric(rownames(C_F_O[[s]])),
#                           C_F_O[[s]][,3], lwd = 3, col = "lightgrey", 
#                           angle = 90, code = 3, length = 0.1))
#   points(as.numeric(rownames(C_F_O[[s]])),C_F_O[[s]][,1],pch=16,cex=1.5)
#   mtext(names(old_mf)[s],3,-3)
#   box()
#   axis(2)
#   if(s==1)
#     axis(3)
#   if(s==3)
#     axis(1)    
#   plot(as.numeric(rownames(C_D_F[[s]])),C_D_F[[s]][,1], xlim=range(yy),ylim=range(C_D_F[[s]],C_F_F[[s]],C_F_O[[s]]), type ="l",lwd=3,xlab=NA,ylab=NA,axes=FALSE)
#   lines(as.numeric(rownames(C_D_F[[s]])),C_D_F[[s]][,2],lwd=3,lty=2)
#   lines(as.numeric(rownames(C_F_O[[s]])),C_F_O[[s]][,1],lty=1,col="darkgrey",lwd=3)
#   ## Add the other version
#   lines(as.numeric(rownames(C_F_O[[s]])),C_F_O[[s]][,1],lty=1,col=caMisc::dtucols(1),lwd=3)
#   # ## Add deterministic, bio process projection
#   # lines(YR_DF_F_BP[[s]],C_DF_F_BP[[s]],lty=2,col=caMisc::dtucols(2),lwd=3)
#   # ## Add deterministic, average projection
#   # lines(YR_DF_F_AV[[s]],C_DF_F_AV[[s]],lty=3,col=caMisc::dtucols(3),lwd=3)    
#   lines(as.numeric(rownames(C_D_F[[s]])),C_D_F[[s]][,1],lwd=3,lty=2)   
#   lines(as.numeric(rownames(C_D_F[[s]])),C_D_F[[s]][,2],lwd=3,lty=2)
#   lines(as.numeric(rownames(C_D_F[[s]])),C_D_F[[s]][,3],lwd=3,lty=2)
#   suppressWarnings(arrows(as.numeric(rownames(C_F_F[[s]])),
#                           C_F_F[[s]][,2],
#                           as.numeric(rownames(C_F_F[[s]])),
#                           C_F_F[[s]][,3], lwd = 3, col = "lightgrey", 
#                           angle = 90, code = 3, length = 0.1))
#   points(as.numeric(rownames(C_F_F[[s]])),C_F_F[[s]][,1],pch=16,cex=1.5)
#   mtext(names(old_mf)[s],3,-3)
#   box()
#   axis(4)
#   if(s==1)
#     axis(3)
#   if(s==3)
#     axis(1)
#   mtext("Catch",2,line=3,outer=TRUE)
#   mtext("Catch",4,line=3,outer=TRUE)
# }
# mtext("Previous",3,line=3,outer=TRUE,at=0.25,font=2)
# mtext("Updated",3,line=3,outer=TRUE,at=0.75,font=2)
# dev.off()

# Plots showing what GMRF is doing - produce these plots 

## natMor
png("report/forecasts/update_sim_M_7ages.png", 1500, 1500)
par(mfrow=c(3,4),mar=c(0.1,0.1,0.1,0.1),oma=c(6,6,6,6), cex.axis = 1.4, cex = 1.4)
yy <- attr(fix_fit,"m_data")$sam[[1]]$years
yx <- max(yy) + 1:10
for(s in 1:3){
  for(a in 1:4){
    #logPredO <- lapply(1:length(old_mf[[1]]),function(i) log(old_mf[[s]][[i]]$bio_natMor[a,]))
    logPredF <- lapply(1:length(fix_mf[[1]]),function(i) log(fix_mf[[s]][[i]]$bio_natMor[a,]))
    
    logNM <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$logNM)[[s]]
    logNMsd <-multiStockassessment:::splitParameter(attr(fix_fit,"m_plsd")$logNM)[[s]]
    logNM_CIL <- logNM - 2 * logNMsd
    logNM_CIH <- logNM + 2 * logNMsd
    logMean <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$meanLogNM)[[s]][fix_fit[[s]]$conf$keyMortalityMean+1][a]
    plot(c(yy,yx),logNM[,a],type="l",lty=1,lwd=3,axes=FALSE, ylim = range(logNM[,a],logNM_CIL[,a],logNM_CIH[,a],logPredF),xlab=NA,ylab=NA)
    abline(v=2026,col="grey", h = logMean,lwd=3)
    lines(c(yy,yx),logNM[,a],type="l",lty=1,lwd=3)
    lines(c(yy,yx),logNM_CIL[,a],type="l",lty=2,lwd=3)
    lines(c(yy,yx),logNM_CIH[,a],type="l",lty=2,lwd=3)
    for(i in 1:length(fix_mf[[1]]))
      points(jitter(rep(fix_mf[[s]][[i]]$year,length(fix_mf[[s]][[i]]$bio_natMor[a,])),0.01),logPredF[[i]],pch=16,col=caMisc::addTrans(caMisc::dtucols(1),0.1))
    points(sapply(fix_mf[[s]],function(xx)xx$year),sapply(logPredF,mean), pch=4,cex=4, lwd=2)
    mtext(sprintf("%s age %d",names(fix_mf)[s],fix_fit[[s]]$conf$minAge+a-1),1,-3, cex = 1.2)
    box()
    if(a == 1)
      axis(2)
    if(a == 4)
      axis(4)
    if(s==1)
      axis(3)
    if(s==3)
      axis(1)    
  }
}
mtext("log-M", 2,line=3, outer = TRUE, cex = 1.5)
mtext("log-M", 4,line=3, outer = TRUE, cex = 1.5)
dev.off()

#------------------------------------------------------------------------------------------------------------------------
## Stock weights
#-------------------------------------------------------------------------------------------------------------------------

png("report/forecasts/update_sim_SW.png", 1500, 1500)
par(mfrow=c(3,7),mar=c(0.1,0.1,0.1,0.1),oma=c(6,6,6,6), cex.axis = 1.4, cex = 1.4)
yy <- attr(fix_fit,"m_data")$sam[[1]]$years
yx <- max(yy) + 1:10
for(s in 1:3){
  for(a in 1:7){
    #logPredO <- lapply(1:length(old_mf[[1]]),function(i) log(old_mf[[s]][[i]]$bio_stockMeanWeight[a,]))
    logPredF <- lapply(1:length(fix_mf[[1]]),function(i) log(fix_mf[[s]][[i]]$bio_stockMeanWeight[a,]))
    
    logSW <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$logSW)[[s]]
    logSWsd <-multiStockassessment:::splitParameter(attr(fix_fit,"m_plsd")$logSW)[[s]]
    logSW_CIL <- logSW - 2 * logSWsd
    logSW_CIH <- logSW + 2 * logSWsd
    logMean <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$meanLogSW)[[s]][fix_fit[[s]]$conf$keyStockWeightMean+1][a]
    plot(c(yy,yx),logSW[,a],type="l",lty=1,lwd=3,axes=FALSE, ylim = range(logSW[,a],logSW_CIL[,a],logSW_CIH[,a],logPredF),xlab=NA,ylab=NA)
    abline(v=2025.5,col="grey", h = logMean,lwd=3)
    lines(c(yy,yx),logSW[,a],type="l",lty=1,lwd=3)
    lines(c(yy,yx),logSW_CIL[,a],type="l",lty=2,lwd=3)
    lines(c(yy,yx),logSW_CIH[,a],type="l",lty=2,lwd=3)
    for(i in 1:length(fix_mf[[1]]))
      points(jitter(rep(fix_mf[[s]][[i]]$year,length(fix_mf[[s]][[i]]$bio_stockMeanWeight[a,])),0.01),logPredF[[i]],pch=16,col=caMisc::addTrans(caMisc::dtucols(1),0.1))
    points(sapply(fix_mf[[s]],function(xx)xx$year),sapply(logPredF,mean), pch=4,cex=4, lwd=2)
    mtext(sprintf("%s age %d",names(fix_mf)[s],fix_fit[[s]]$conf$minAge+a-1),1,-3, cex = 1.2)
    box()
    if(a == 1)
      axis(2)
    if(a == 4)
      axis(4)
    if(s==1)
      axis(3)
    if(s==3)
      axis(1)    
  }
}
mtext("log-stockMeanWeight", 2,line=3, outer = TRUE, cex = 1.5)
mtext("log-stockMeanWeight", 4,line=3, outer = TRUE, cex = 1.5)
dev.off()



#-------------------------------------------------------------------------------------------------------------------------
## Maturity

png("report/forecasts/update_sim_MO.png", 1500, 1500)
par(mfrow=c(3,7),mar=c(0.1,0.1,0.1,0.1),oma=c(6,6,6,6), cex.axis = 1.4, cex = 1.4)
yy <- attr(fix_fit,"m_data")$sam[[1]]$years
yx <- max(yy) + 1:10
for(s in 1:3){
  for(a in 1:7){
    logit <- function(x) log(x/(1-x))
    #logPredO <- lapply(1:length(old_mf[[1]]),function(i) logit(old_mf[[s]][[i]]$bio_propMat[a,]))
    logPredF <- lapply(1:length(fix_mf[[1]]),function(i) logit(fix_mf[[s]][[i]]$bio_propMat[a,]))
    logMO <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$logitMO)[[s]]
    logMOsd <-multiStockassessment:::splitParameter(attr(fix_fit,"m_plsd")$logitMO)[[s]]
    logMO_CIL <- logMO - 2 * logMOsd
    logMO_CIH <- logMO + 2 * logMOsd
    logMean <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$meanLogitMO)[[s]][fix_fit[[s]]$conf$keyMatureMean+1][a]
    plot(c(yy,yx),logMO[,a],type="l",lty=1,lwd=3,axes=FALSE, ylim = range(logMO[,a],logMO_CIL[,a],logMO_CIH[,a],logPredF),xlab=NA,ylab=NA)
    abline(v=2025.5,col="grey", h = logMean,lwd=3)
    lines(c(yy,yx),logMO[,a],type="l",lty=1,lwd=3)
    lines(c(yy,yx),logMO_CIL[,a],type="l",lty=2,lwd=3)
    lines(c(yy,yx),logMO_CIH[,a],type="l",lty=2,lwd=3)
    for(i in 1:length(fix_mf[[1]]))
      points(jitter(rep(fix_mf[[s]][[i]]$year,length(fix_mf[[s]][[i]]$bio_propMat[a,])),0.01),logPredF[[i]],pch=16,col=caMisc::addTrans(caMisc::dtucols(1),0.1))
    points(sapply(fix_mf[[s]],function(xx)xx$year),sapply(logPredF,mean), pch=4,cex=4, lwd=2)
    mtext(sprintf("%s age %d",names(fix_mf)[s],fix_fit[[s]]$conf$minAge+a-1),1,-3, cex = 1.2)
    box()
    if(a == 1)
      axis(2)
    if(a == 4)
      axis(4)
    if(s==1)
      axis(3)
    if(s==3)
      axis(1)    
  }
}
mtext("logit-propMat", 2,line=3, outer = TRUE, cex = 1.5)
mtext("logit-propMat", 4,line=3, outer = TRUE, cex = 1.5)
dev.off()

##########################################################################################

# Natural Mortality natural scale - these are plotted in the foreacst addition plot section - 
# next year adjust the axis so all the same for each age and substock

###########################################################################################

## natMor
png("report/forecasts/update_sim_M_naturalScale_0406.png", 3000, 1500)
par(mfrow=c(3,7),mar=c(3,3,3,3),oma=c(0,4,0,4), cex.axis = 1.2, cex = 1.2)#mar=c(0.1,0.1,0.1,0.1),oma=c(6,6,6,6))
yy <- attr(fix_fit,"m_data")$sam[[1]]$years
yx <- max(yy) + 1:10
for(s in 1:3){
  for(a in 1:7){
    #logPredO <- lapply(1:length(old_mf[[1]]),function(i) log(old_mf[[s]][[i]]$bio_natMor[a,]))
    logPredF <- lapply(1:length(fix_mf[[1]]),function(i) log(fix_mf[[s]][[i]]$bio_natMor[a,]))
    logNM <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$logNM)[[s]]
    logNMsd <-multiStockassessment:::splitParameter(attr(fix_fit,"m_plsd")$logNM)[[s]]
    logNM_CIL <- logNM - 2 * logNMsd
    logNM_CIH <- logNM + 2 * logNMsd
    logMean <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$meanLogNM)[[s]][fix_fit[[s]]$conf$keyMortalityMean+1][a]
    plot(c(yy,yx),exp(logNM[,a]),type="l",lty=1,lwd=3,axes=FALSE, ylim = exp(range(logNM[,a],logNM_CIL[,a],logNM_CIH[,a],logPredF)),xlab=NA,ylab=NA)
    abline(v=2025,col="grey", h = exp(logMean),lwd=3)
    lines(c(yy,yx),exp(logNM[,a]),type="l",lty=1,lwd=3)
    lines(c(yy,yx),exp(logNM_CIL[,a]),type="l",lty=2,lwd=3)
    lines(c(yy,yx),exp(logNM_CIH[,a]),type="l",lty=2,lwd=3)
    for(i in 1:length(fix_mf[[1]]))
      points(jitter(rep(fix_mf[[s]][[i]]$year,length(fix_mf[[s]][[i]]$bio_natMor[a,])),0.01),exp(logPredF[[i]]),pch=16,col=caMisc::addTrans(caMisc::dtucols(1),0.1))
    points(sapply(fix_mf[[s]],function(xx)xx$year),exp(sapply(logPredF,mean)), pch=4,cex=4, lwd=2)
    mtext(sprintf("%s age %d",names(fix_mf)[s],fix_fit[[s]]$conf$minAge+a-1),1,-3, cex = 1.8)
    box()
    ## if(a == 1)
    ##     axis(2)
    ## if(a == 4)
    ##     axis(4)
    ## if(s==1)
    ##     axis(3)
    ## if(s==3)
    ##     axis(1)
    axis(1);axis(2)
  }
}
mtext("M", 2,line=1, outer = TRUE, cex = 2.5)
mtext("M", 4,line=1, outer = TRUE, cex = 2.5)
dev.off()


## Stock weights -------------------------------------------------------------------------------------------------------

png("report/forecasts/update_sim_SW_naturalScale_0406.png", 3000, 1500)
par(mfrow=c(3,7),mar=c(3,3,3,3),oma=c(0,4,0,4), cex.axis = 1.2, cex = 1.2)#mar=c(0.1,0.1,0.1,0.1),oma=c(6,6,6,6))
yy <- attr(old_fit,"m_data")$sam[[1]]$years
yx <- max(yy) + 1:10
for(s in 1:3){
  for(a in 1:7){
    #logPredO <- lapply(1:length(old_mf[[1]]),function(i) log(old_mf[[s]][[i]]$bio_stockMeanWeight[a,]))
    logPredF <- lapply(1:length(fix_mf[[1]]),function(i) log(fix_mf[[s]][[i]]$bio_stockMeanWeight[a,]))
    logSW <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$logSW)[[s]]
    logSWsd <-multiStockassessment:::splitParameter(attr(fix_fit,"m_plsd")$logSW)[[s]]
    logSW_CIL <- logSW - 2 * logSWsd
    logSW_CIH <- logSW + 2 * logSWsd
    logMean <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$meanLogSW)[[s]][fix_fit[[s]]$conf$keyStockWeightMean+1][a]
    plot(c(yy,yx),exp(logSW[,a]),type="l",lty=1,lwd=3,axes=FALSE, ylim = exp(range(logSW[,a],logSW_CIL[,a],logSW_CIH[,a],logPredF)),xlab=NA,ylab=NA)
    abline(v=2024.5,col="grey", h = exp(logMean),lwd=3)
    lines(c(yy,yx),exp(logSW[,a]),type="l",lty=1,lwd=3)
    lines(c(yy,yx),exp(logSW_CIL[,a]),type="l",lty=2,lwd=3)
    lines(c(yy,yx),exp(logSW_CIH[,a]),type="l",lty=2,lwd=3)
    for(i in 1:length(fix_mf[[1]]))
      points(jitter(rep(fix_mf[[s]][[i]]$year,length(fix_mf[[s]][[i]]$bio_stockMeanWeight[a,])),0.01),exp(logPredF[[i]]),pch=16,col=caMisc::addTrans(caMisc::dtucols(1),0.1))
    points(sapply(fix_mf[[s]],function(xx)xx$year),exp(sapply(logPredF,mean)), pch=4,cex=4, lwd=2)
    mtext(sprintf("%s age %d",names(fix_mf)[s],fix_fit[[s]]$conf$minAge+a-1),1,-3, cex = 1.8)
    box()
    ## if(a == 1)
    ##     axis(2)
    ## if(a == 4)
    ##     axis(4)
    ## if(s==1)
    ##     axis(3)
    ## if(s==3)
    ##     axis(1)
    axis(1); axis(2)
  }
}
mtext("stockMeanWeight", 2,line=3, outer = TRUE, cex = 2.5)
mtext("stockMeanWeight", 4,line=3, outer = TRUE, cex = 2.5)
dev.off()



## Maturity

png("report/forecasts/update_sim_MO_naturalScale_0406.png", 3000, 1500)
par(mfrow=c(3,7),mar=c(3,3,3,3),oma=c(0,4,0,4), cex.axis = 1.2, cex = 1.2)
yy <- attr(fix_fit,"m_data")$sam[[1]]$years
yx <- max(yy) + 1:10
for(s in 1:3){
  for(a in 1:7){
    logit <- function(x) log(x/(1-x))
    invlogit <- function(x) 1 / (1 + exp(-x))
    #logPredO <- lapply(1:length(old_mf[[1]]),function(i) logit(old_mf[[s]][[i]]$bio_propMat[a,]))
    logPredF <- lapply(1:length(fix_mf[[1]]),function(i) logit(fix_mf[[s]][[i]]$bio_propMat[a,]))
    logMO <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$logitMO)[[s]]
    logMOsd <-multiStockassessment:::splitParameter(attr(fix_fit,"m_plsd")$logitMO)[[s]]
    logMO_CIL <- logMO - 2 * logMOsd
    logMO_CIH <- logMO + 2 * logMOsd
    logMean <- multiStockassessment:::splitParameter(attr(fix_fit,"m_pl")$meanLogitMO)[[s]][fix_fit[[s]]$conf$keyMatureMean+1][a]
    plot(c(yy,yx),invlogit(logMO[,a]),type="l",lty=1,lwd=3,axes=FALSE, ylim = invlogit(range(logMO[,a],logMO_CIL[,a],logMO_CIH[,a],logPredF)),xlab=NA,ylab=NA)
    abline(v=2024.5,col="grey", h = invlogit(logMean),lwd=3)
    lines(c(yy,yx),invlogit(logMO[,a]),type="l",lty=1,lwd=3)
    lines(c(yy,yx),invlogit(logMO_CIL[,a]),type="l",lty=2,lwd=3)
    lines(c(yy,yx),invlogit(logMO_CIH[,a]),type="l",lty=2,lwd=3)
    for(i in 1:length(fix_mf[[1]]))
      points(jitter(rep(fix_mf[[s]][[i]]$year,length(fix_mf[[s]][[i]]$bio_propMat[a,])),0.01),invlogit(logPredF[[i]]),pch=16,col=caMisc::addTrans(caMisc::dtucols(1),0.1))
    points(sapply(fix_mf[[s]],function(xx)xx$year),invlogit(sapply(logPredF,mean)), pch=4,cex=4, lwd=2)
    mtext(sprintf("%s age %d",names(fix_mf)[s],fix_fit[[s]]$conf$minAge+a-1),1,-3, cex = 1.8)
    box()
    axis(1);axis(2)
    ## if(a == 1)
    ##     axis(2)
    ## if(a == 4)
    ##     axis(4)
    ## if(s==1)
    ##     axis(3)
    ## if(s==3)
    ##     axis(1)    
  }
}
mtext("propMat", 2,line=3, outer = TRUE, cex = 2.5)
mtext("propMat", 4,line=3, outer = TRUE, cex = 2.5)
dev.off()

#--------------------------------------------------------------------------------------------------------------------------------
# Ale requested proportions plots
#---------------------------------------------------------------------------------------------------------------------------------
library(RColorBrewer) 
naa <- ntable(fit, returnList=TRUE)
sw <- multiStockassessment:::splitParameter(exp(attr(fit,"m_pl")$logSW))
mo <- multiStockassessment:::splitParameter(plogis(attr(fit,"m_pl")$logitMO))

png("report/forecasts/ssb_at_age.png", 1500, 1500)
par(mfrow=c(3,2),mar=c(3,3,3,3),oma=c(0,4,0,4))
for (s in 1:3){
  
  # From the forecasts
  ssb_fc <- do.call(rbind, lapply(FC[[13]]$result[[s]], function(x) apply(exp(x$sim[,1:7])*t(x$bio_stockMeanWeight)*t(x$bio_propMat), 2, median)))[2:4,]
  rownames(ssb_fc) <- rownames(attr(FC[[13]]$result[[s]],"tab"))[2:4]
  
  # And from the assessment
  ssb_ass <- naa[[s]][-nrow(naa[[s]]),] * sw[[s]][1:(nrow(sw[[s]])-11),] * mo[[s]][1:(nrow(mo[[s]])-11),]
  
  ssb <- rbind(ssb_ass, ssb_fc)
  
  barplot(t(ssb), border = NA, space = c(0), col = brewer.pal(n=ncol(ssb), name = "Spectral"))
  abline(v=nrow(ssb)-3, lty=2)
  mtext(names(fit)[s],3,-3)
  box()
  axis(2)
  if(s==1)
    axis(3)
  if(s==3)
    axis(1) 
  barplot(t(ssb/rowSums(ssb)), border = NA, space = c(0), col = brewer.pal(n=ncol(ssb), name = "Spectral"))
  text(1, cumsum(t(ssb/rowSums(ssb))[, 1]) - 0.5 * t(ssb/rowSums(ssb))[, 1], label = as.character(1:ncol(ssb)),
       adj = c(0, 0.2))
  abline(v=nrow(ssb)-3, lty=2)
  mtext(names(fix_mf)[s],3,-3)
  box()
  axis(4)
  if(s==1)
    axis(3)
  if(s==3)
    axis(1)
  mtext("SSB",2,line=3,outer=TRUE)
  mtext("Proportion of SSB",4,line=3,outer=TRUE)
}
mtext("Absolute SSB",3,line=3,outer=TRUE,at=0.25,font=2)
mtext("Proportion of SSB",3,line=3,outer=TRUE,at=0.75,font=2)
dev.off()


rp <- attr(fit, "m_rep")
caa <- list()
for(i in 1:length(fit)){
  caa[[i]] <- as.data.frame(t(apply(exp(rp$logCatchByFleetAge[[i]][,,]),1:2,sum)))
  caa[[i]] <- caa[[i]][-nrow(caa[[i]]),]
  colnames(caa[[i]]) <- c(as.character(1:7))
  rownames(caa[[i]]) <- 1983:(1983+nrow(caa[[i]])-1)
}
cw <- data.frame(fit[[1]]$data$catchMeanWeight)

png("report/forecasts/catch_at_age.png", 1500, 1500)
par(mfrow=c(3,2),mar=c(3,3,3,3),oma=c(0,4,0,4))
for (s in 1:3){
  
  # From the forecasts
  ave <- as.vector(colMeans(cw[(nrow(cw)-2):nrow(cw),]))
  
  catch_fc <- do.call(rbind, lapply(FC[[13]]$result[[s]], function(x) apply(x$catchatage, 1, median)))[1:3,]
  rownames(catch_fc) <- rownames(attr(FC[[13]]$result[[s]],"tab"))[1:3]
  
  # And from the assessment
  catch_ass <- caa[[s]][-nrow(caa[[s]]),] * cw[-nrow(cw),]
  
  catch <- rbind(catch_ass, catch_fc)
  
  barplot(t(catch), border = NA, space = c(0), col = brewer.pal(n=ncol(catch), name = "Spectral"))
  abline(v=nrow(catch)-3, lty=2)
  mtext(names(fit)[s],3,-3)
  box()
  axis(2)
  if(s==1)
    axis(3)
  if(s==3)
    axis(1) 
  barplot(t(catch/rowSums(catch)), border = NA, space = c(0), col = brewer.pal(n=ncol(ssb), name = "Spectral"))
  text(1, cumsum(t(catch/rowSums(catch))[, 1]) - 0.5 * t(catch/rowSums(catch))[, 1], label = as.character(1:ncol(catch)),
       adj = c(0, 0.2))
  abline(v=nrow(catch)-3, lty=2)
  mtext(names(fix_mf)[s],3,-3)
  box()
  axis(4)
  if(s==1)
    axis(3)
  if(s==3)
    axis(1)
  mtext("Catch",2,line=3,outer=TRUE)
  mtext("Proportion of Catch",4,line=3,outer=TRUE)
}
mtext("Absolute Catch",3,line=3,outer=TRUE,at=0.25,font=2)
mtext("Proportion of Catch",3,line=3,outer=TRUE,at=0.75,font=2)
dev.off()

#---------------------------------------------------------------------------------------------------------------------------
# TSB version of the plot above - Ale request 2026
#---------------------------------------------------------------------------------------------------------------------------

# no maturity in tsb like ssb: exp(x$sim[,1:7]) * t(x$bio_stockMeanWeight) * t(x$bio_propMat)
# So using nos. at age and stock mean weight: exp(x$sim[,1:7]) * t(x$bio_stockMeanWeight)

png("report/forecasts/tsb_at_age.png", 1500, 1500)
par(mfrow=c(3,2), mar=c(3,3,3,3), oma=c(0,4,0,4))
for (s in 1:3){
  
  # From the forecasts
  tsb_fc <- do.call(rbind,lapply(FC[[13]]$result[[s]],function(x)apply(exp(x$sim[,1:7]) * t(x$bio_stockMeanWeight),2,median
        )))[2:4,]
  rownames(tsb_fc) <- rownames(attr(FC[[13]]$result[[s]], "tab"))[2:4]
  
  # And from the assessment
  tsb_ass <- naa[[s]][-nrow(naa[[s]]),] * sw[[s]][1:(nrow(sw[[s]])-11),]
  
  # Combine assessment and forecast
  tsb <- rbind(tsb_ass, tsb_fc)
  
  # Absolute TSB at age
  barplot(t(tsb),border=NA,space=0,col=brewer.pal(n=ncol(tsb), name="Spectral"))
  
  abline(v=nrow(tsb)-3, lty=2)
  
  mtext(names(fit)[s], 3, -3)
  box()
  axis(2)
  
  if(s==1)
    axis(3)
  
  if(s==3)
    axis(1)
  
  # Proportion of TSB by age
  barplot(t(tsb/rowSums(tsb)),border=NA,space=0,col=brewer.pal(n=ncol(tsb), name="Spectral"))
  
  text(1,cumsum(t(tsb/rowSums(tsb))[,1]) - 0.5 * t(tsb/rowSums(tsb))[,1],label=as.character(1:ncol(tsb)),adj=c(0,0.2))
  abline(v=nrow(tsb)-3, lty=2)
  
  mtext(names(fit)[s], 3, -3)
  box()
  axis(4)
  
  if(s==1)
    axis(3)
  
  if(s==3)
    axis(1)
  
  mtext("TSB", 2, line=3, outer=TRUE)
  mtext("Proportion of TSB", 4, line=3, outer=TRUE)
}

mtext("Absolute TSB", 3, line=3, outer=TRUE, at=0.25, font=2)
mtext("Proportion of TSB", 3, line=3, outer=TRUE, at=0.75, font=2)

dev.off()

