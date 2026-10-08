## Reference points - Northwestern

rm(list = ls())

.libPaths(c(
  "C:/Users/RH28/Documents/R/win-library/4.3",
  "C:/Users/RH28/AppData/Local/Programs/R/R-4.3.2/library"
))

library(icesTAF)
library(ggplot2)
library(reshape2)
library(stockassessment)
library(multiStockassessment)
library(msy)
library(FLCore)
library(remotes)

## 1 Assessment
load('model/model.Rdata')
load('model/residuals.Rdata')
load('model/retro.Rdata')
#load('model/forecast.Rdata') - # haven't run forecasts yet 

load("output/cod46a7d20_FLstockObjects_assessment_2026.Rdata")

## Extract sd of log(SSB)

TAF::mkdir("model/refpts")
TAF::mkdir("model/refpts/northwestern")

sigmas <- lapply(ssbtable(fit, addTotal = TRUE, returnList = TRUE),function(x) tail((log(x[,"High"]) - log(x[,"Low"]))/4,1))

#saveRDS(sigmas,"model/refpts/sigmas.rds")

#------------------------------------------------------------------------------------------------------------------------------------------
# Northwestern 
#------------------------------------------------------------------------------------------------------------------------------------------

sigma <- sigmas$Northwest #0.179 if less than 0.2 it means the assessment has less uncertainty, but we use 0.2 to be more precautionary 
sigma <- ifelse(sigmas[[1]]<0.2, 0.2, sigmas[[1]]) # More precautionary 

## extract subcomponents ----
northwestern <- stks[[1]]

## rename stocks ----
northwestern@name <- "Northwestern"

## Fbar ranges ----
range(northwestern, c("minfbar", "maxfbar")) <- c(2, 4) 

## Landings obligation adjustments ----
# Use wanted catch for ages 1-2: Fish below MCRS cannot be marketed
# Use all catch for ages 3+: Landing obligation
northwestern@landings.n[3:7,,,,]<-northwestern@landings.n[3:7,,,,]+northwestern@discards.n[3:7,,,,]
northwestern@landings.wt[3:7,,,,]<-northwestern@catch.wt[3:7,,,,]
northwestern@discards.n[3:7,,,,]<-0

northwestern@landings <- apply(northwestern@landings.n *  northwestern@landings.wt,2,sum)
northwestern@discards <- apply(northwestern@discards.n *  northwestern@discards.wt,2,sum)

#-------------------------------------------------------------------------------------------------------------------------------------------
# Stock-Recruitment Plots
#-------------------------------------------------------------------------------------------------------------------------------------------

# 1 . SR - remember lag
SSB <- data.frame(ssbtable(fit))
REC <- data.frame(rectable(fit))

SSB_NW <- data.frame(Year = as.numeric(rownames(SSB)),SSB = SSB$Northwest..Estimate)
REC_NW <- data.frame(Year = as.numeric(rownames(REC)),REC = REC$Northwest..Estimate)

# Recruitment comes from previous spawning year
REC_NW$spawn_year <- REC_NW$Year - 1

# Match SSB_t with Recruitment_t+1
SR <- merge(
  SSB_NW[, c("Year", "SSB")],
  REC_NW[, c("spawn_year", "REC")],
  by.x = "Year",
  by.y = "spawn_year"
)

SR <- SR[order(SR$Year), ]

png("model/refpts/northwestern/SR_NW.png", width = 2000, height = 1600, res = 300)
cols <- ifelse(SR$Year >= 1997, "red", "black")

plot(SR$SSB,
     SR$REC,
     type = "n",
     xlab = "SSB",
     ylab = "Recruitment",
     xlim = c(0, max(SR$SSB, na.rm = TRUE)),
     ylim = c(0, max(SR$REC, na.rm = TRUE) * 1.15))

lines(SR$SSB, SR$REC, col = "black", lwd = 1)

points(SR$SSB, SR$REC,pch = 16,col = cols)

text(SR$SSB,SR$REC,labels = SR$Year,col = cols,cex = 0.7,pos = 3)
dev.off()

# 2. SR - R/SSB vs SSB
SR$R_SSB <- SR$REC / SR$SSB

# Match SSB_t with Recruitment_t+1
SR <- merge(
  SSB_NW[, c("Year", "SSB")],
  REC_NW[, c("spawn_year", "REC")],
  by.x = "Year",
  by.y = "spawn_year"
)

SR <- SR[order(SR$Year), ]

# Recruitment per unit SSB
SR$R_SSB <- SR$REC / SR$SSB

png("model/refpts/northwestern/R_SSB_NW.png", width = 2000, height = 1600, res = 300)
cols <- ifelse(SR$Year >= 1997, "red", "black")

plot(SR$SSB,
     SR$R_SSB,
     type = "n",
     xlab = "SSB",
     ylab = "R/SSB",
     xlim = c(0, max(SR$SSB, na.rm = TRUE)),
     ylim = c(0, max(SR$R_SSB, na.rm = TRUE) * 1.15))

lines(SR$SSB, SR$R_SSB,
      col = "black",
      lwd = 1)

points(SR$SSB, SR$R_SSB,
       pch = 16,
       col = cols)

text(SR$SSB,
     SR$R_SSB,
     labels = SR$Year,
     col = cols,
     cex = 0.7,
     pos = 3)
dev.off()

#-------------------------------------------------------------------------------------------------------------------------------------------
# Reference Points
#-------------------------------------------------------------------------------------------------------------------------------------------

Fcv <- 0.212
Fphi <- 0.423

# Northwestern Decisions at TT x WGNSSK 04/09/26
# Truncation 1998 for all other reference points apart from Blim
# Type 5 / Type 6 - doesn't matter as the breakpoint is the same as Bloss - calculation for FMSY Only
# Blim Type 2: 	- Just Segreg for the full time-series 

## SR fit 1997 -----------------------------------------------------------------
# Truncate time-series 
stk_1997 <- window(northwestern, start = 1997, end = 2025)

set.seed(5)

## Type 5: stocks with no evidence that recruitment has been impaired or with no clear relation between stock & recruitment 
## (no apparent S-R relationship) 

# according to plot first point from the left above the line is 2006
Bloss <- SSB_NW[SSB_NW$Year == 2006, "SSB"]

#-------------------------------------------------------------------------------------------------------------------------------
## Type 5 / Type 6 (Breakpoint for either is Bloss for first equation in EQsim) 
#-------------------------------------------------------------------------------------------------------------------------------
B = Bloss
SegregFix <- function (ab, ssb) {
  log(ifelse (ssb>=B, ab$a*B, ab$a*ssb))
}

set.seed(5)
NWFit1_1997 <- eqsr_fit(stk_1997, nsamp = 2000, models = "SegregFix")
eqsr_plot(NWFit1_1997, n=2e4)

SIM_1997_NW_FMSY <-
  eqsim_run(
    NWFit1_1997, 
    bio.years = c(2016, 2025), #last 10 years
    bio.const = FALSE,
    sel.years = c(2023, 2025), #last 3 years 
    sel.const = FALSE,
    Fcv = Fcv,
    Fphi = Fphi,
    Btrigger = 0,
    Blim = B,
    Fscan = seq(0, 1.2, len = 61),
    verbose = FALSE
  )

eqsim_plot(SIM_1997_NW_FMSY, catch = FALSE) 

jpeg("model/refpts/northwestern/eqsim_Fmsy_BPBLoss_truncTS.jpeg", width = 1200, height = 900, res = 150)
eqsim_plot_range(SIM_1997_NW_FMSY, type="median")
dev.off()

eqsim_plot_range(SIM_1997_NW_FMSY, type='ssb')
SIM_1997_NW_FMSY$Refs2
Plot_Values_NW<-SIM_1997_NW_FMSY$rbp # to get value for B0, pull out value at p50 that corresponds to Ftarget = 0

# B0 and BMST calcs for sense checks 
B0_NW <- Plot_Values_NW[Plot_Values_NW$Ftarget == 0 & Plot_Values_NW$variable == "Spawning stock biomass", "p50"]
B0_NW_20 <- 0.20 * B0_NW
B0_NW_10 <- 0.10 * B0_NW
B0_NW_5 <- 0.05 * B0_NW

#BMSY
#Extraction and plot of median 
FMSY_med <- SIM_1997_NW_FMSY$Refs2["lanF", "medianMSY"] 
iF <- which.min(abs(Plot_Values_NW$Ftarget - FMSY_med))

BMSY_med <- Plot_Values_NW[
  Plot_Values_NW$variable == "Spawning stock biomass" &
    Plot_Values_NW$Ftarget == Plot_Values_NW$Ftarget[iF],
  "p50"
]

FMSY_plot <- Plot_Values_NW$Ftarget[iF]
FMSY_med #0.1897898
BMSY_med #73500 B that corresponds to FMSY

# the value of 0.1933 is between two Fscan values 0.18 and 0.20 so assuming linear line the value is:
F1 <- 0.18
F2 <- 0.20

B1 <- Plot_Values_NW[Plot_Values_NW$Ftarget == 0.18 & Plot_Values_NW$variable == "Spawning stock biomass", "p50"]
B2 <- Plot_Values_NW[Plot_Values_NW$Ftarget == 0.20 & Plot_Values_NW$variable == "Spawning stock biomass", "p50"]

FMSY <- FMSY_med
FMSY_plot<-FMSY

BMSY <- B1 + (FMSY - F1) * (B2 - B1) / (F2 - F1) #66801

save(SIM_1997_NW_FMSY, file = "model/refpts/northwestern/NW_FMSY.RData")
#load("model/refpts/northwestern/NW_FMSY.RData")

#Plot for BMSY check 
load('model/refpts/northwestern/NW_FMSY.RData')
eqsim_plot_range(SIM_1997_NW_FMSY, type='ssb')

abline(v = FMSY_plot, col = "blue", lty = 2, lwd = 1.5)

points(FMSY_plot, BMSY,
       col = "blue", pch = 19, cex = 1.3)

text(FMSY_plot, BMSY,
     labels = paste0("BMSY = ", round(BMSY, 0)),
     pos = 4, col = "blue")

# The median of SSB estimates at FMSY - Needed for the report
FMSY_med <- SIM_1997_NW_FMSY$Refs2["lanF", "medianMSY"]

ssb_FMSY <- subset(
  Plot_Values_NW,
  variable == "Spawning stock biomass"
)

SSB_med <- approx(
  x = ssb_FMSY$Ftarget,
  y = ssb_FMSY$p50,
  xout = FMSY_med
)$y

FMSY_med
SSB_med #70673

#-----------------------------------------------------------------
# Blim - FULL Time-Series
#-----------------------------------------------------------------

northwestern_1983 <- window(northwestern, start = 1983, end = 2025)

set.seed(5)

## Type 2: estimate breakpoint of segemented regression internally in EqSim ----
FIT_1983_nw <- eqsr_fit(northwestern_1983, nsamp = 2000, models = "Segreg")
save(FIT_1983_nw, file = "model/refpts/northwestern/FIT_1983_nw_type2_Blim.RData")
eqsr_plot(FIT_1983_nw, n=2e4)


## Variance params ----
Fcv <- 0.212
Fphi <- 0.423

## Blim ----

Blim <- FIT_1983_nw$sr.det$b # correct b?? #23422 vs TT fit Blim 24582
#Checks need to be done with BMSY
# suggestion from Christoffer: Blim = min(Blim calculated, Bmsy)

## Bpa and Btrigger ----
Bpa <- Blim * exp(1.645 * sigma) #when sigma= 0.2 you get a slightly higher Bpa more precautionary 
Btrig <- Bpa #32548

## FP0.5 ----
set.seed(7)

SIM_1983_nw_type2_Fp.05 <- #Fpa
  eqsim_run(
    NWFit1_1997, #Use FMSY object for truncated TS here 
    bio.years = c(2016, 2025), #10 years
    bio.const = FALSE,
    sel.years = c(2023, 2025), # 3 years 
    sel.const = FALSE,
    Fcv = Fcv,
    Fphi = Fphi,
    Btrigger = Btrig,
    Blim = Blim,
    Fscan = seq(0, 1.2, len = 61),
    verbose = FALSE
  )
eqsim_plot(SIM_1983_nw_type2_Fp.05, catch = "FALSE")

taf.png("model/refpts/northwestern/eqsim_Fpa.png")
eqsim_plot_range(SIM_1983_nw_type2_Fp.05, type="median")
dev.off()

save(SIM_1983_nw_type2_Fp.05, file = "model/refpts/northwestern/SIM_1983_nw_type2_Fp.05.RData")

## Final refpoints ----
NW_refPoints <- list(MSYbtrigger = Btrig,
                                Fmsy = SIM_1997_NW_FMSY$Refs2["lanF", "medianMSY"],
                                Fmsyupper = SIM_1997_NW_FMSY$Refs2["lanF", "Medupper"],
                                Fmsylower = SIM_1997_NW_FMSY$Refs2["lanF", "Medlower"],
                                Blim = Blim,
                                Bpa = Bpa,
                              #  Flim = Flim,
                                Fp.05 = SIM_1983_nw_type2_Fp.05$Refs2["catF", "F05"],
                                Fpa = SIM_1983_nw_type2_Fp.05$Refs2["catF", "F05"],
                                biol.years = dim(SIM_1997_NW_FMSY$ibya$weca)[2],
                                sel.years = dim(SIM_1997_NW_FMSY$ibya$sel)[2],
                                B0 = B0_NW,
                                BMSY = BMSY)

NW_refPoints <- as.data.frame(NW_refPoints)

save(NW_refPoints, file = "model/refpts/northwestern/NW_refPoints.RData")

load("model/refpts/northwestern/NW_refPoints.RData")

