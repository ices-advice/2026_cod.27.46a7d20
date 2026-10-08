## Reference points - Viking

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
TAF::mkdir("model/refpts/viking")

sigmas <- lapply(ssbtable(fit, addTotal = TRUE, returnList = TRUE),function(x) tail((log(x[,"High"]) - log(x[,"Low"]))/4,1))

#saveRDS(sigmas,"model/refpts/sigmas.rds")

#------------------------------------------------------------------------------------------------------------------------------------------
# Viking
#------------------------------------------------------------------------------------------------------------------------------------------

sigma <- sigmas$Viking #0.193 if less than 0.2 it means the assessment has less uncertainty, but we use 0.2 to be more precautionary 
sigma <- ifelse(sigmas[[3]]<0.2, 0.2, sigmas[[3]]) # More precautionary 

## extract subcomponents ----
Viking <- stks[[3]]

## rename stocks ----
Viking@name <- "Viking"

## Fbar ranges ----
range(Viking, c("minfbar", "maxfbar")) <- c(2, 4) 

## Landings obligation adjustments ----
# Use wanted catch for ages 1-2: Fish below MCRS cannot be marketed
# Use all catch for ages 3+: Landing obligation
Viking@landings.n[3:7,,,,]<-Viking@landings.n[3:7,,,,]+Viking@discards.n[3:7,,,,]
Viking@landings.wt[3:7,,,,]<-Viking@catch.wt[3:7,,,,]
Viking@discards.n[3:7,,,,]<-0

Viking@landings <- apply(Viking@landings.n *  Viking@landings.wt,2,sum)
Viking@discards <- apply(Viking@discards.n *  Viking@discards.wt,2,sum)

#-------------------------------------------------------------------------------------------------------------------------------------------
# Stock-Recruitment Plots
#-------------------------------------------------------------------------------------------------------------------------------------------

# 1 . SR - remember lag
SSB <- data.frame(ssbtable(fit))
REC <- data.frame(rectable(fit))

SSB_V <- data.frame(Year = as.numeric(rownames(SSB)),SSB = SSB$Viking..Estimate)
REC_V <- data.frame(Year = as.numeric(rownames(REC)),REC = REC$Viking..Estimate)

# Recruitment comes from previous spawning year
REC_V$spawn_year <- REC_V$Year - 1

# Match SSB_t with Recruitment_t+1
SR <- merge(
  SSB_V[, c("Year", "SSB")],
  REC_V[, c("spawn_year", "REC")],
  by.x = "Year",
  by.y = "spawn_year"
)

SR <- SR[order(SR$Year), ]

png("model/refpts/viking/SR_V.png", width = 2000, height = 1600, res = 300)
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
  SSB_V[, c("Year", "SSB")],
  REC_V[, c("spawn_year", "REC")],
  by.x = "Year",
  by.y = "spawn_year"
)

SR <- SR[order(SR$Year), ]

# Recruitment per unit SSB
SR$R_SSB <- SR$REC / SR$SSB

png("model/refpts/viking/R_SSB_V.png", width = 2000, height = 1600, res = 300)
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

# Viking Decisions at TT x WGNSSK 04/09/26
# Truncation 1998 for all other reference points apart from Blim
# Type 5 / Type 6 - doesn't matter as the breakpoint is the same as Bloss - calculation for FMSY Only
# Blim Type 2: 	- Just Segreg for the full time-series 

## SR fit 1997 -----------------------------------------------------------------
# Truncate time-series 
stk_1997 <- window(Viking, start = 1997, end = 2025)

set.seed(5)

## Type 5: stocks with no evidence that recruitment has been impaired or with no clear relation between stock & recruitment 
## (no apparent S-R relationship) 

# according to plot first point from the left above the line is 2006
Bloss <- SSB_V[SSB_V$Year == 2006, "SSB"]

#-------------------------------------------------------------------------------------------------------------------------------
## Type 5 / Type 6 (Breakpoint for either is Bloss for first equation in EQsim) 
#-------------------------------------------------------------------------------------------------------------------------------
B = Bloss
SegregFix <- function (ab, ssb) {
  log(ifelse (ssb>=B, ab$a*B, ab$a*ssb))
}

set.seed(5)
VFit1_1997 <- eqsr_fit(stk_1997, nsamp = 2000, models = "SegregFix")
eqsr_plot(VFit1_1997, n=2e4)

SIM_1997_V_FMSY <-
  eqsim_run(
    VFit1_1997, 
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

eqsim_plot(SIM_1997_V_FMSY, catch = FALSE) 

jpeg("model/refpts/viking/eqsim_Fmsy_BPBLoss_truncTS.jpeg", width = 1200, height = 900, res = 150)
eqsim_plot_range(SIM_1997_V_FMSY, type="median")
dev.off()

eqsim_plot_range(SIM_1997_V_FMSY, type='ssb')
SIM_1997_V_FMSY$Refs2
Plot_Values_V<-SIM_1997_V_FMSY$rbp # to get value for B0, pull out value at p50 that corresponds to Ftarget = 0

# B0 and BMST calcs for sense checks 
B0_V <- Plot_Values_V[Plot_Values_V$Ftarget == 0 & Plot_Values_V$variable == "Spawning stock biomass", "p50"]
B0_V_20 <- 0.20 * B0_V
B0_V_10 <- 0.10 * B0_V
B0_V_5 <- 0.05 * B0_V

#BMSY
#Extraction and plot of median 
FMSY_med <- SIM_1997_V_FMSY$Refs2["lanF", "medianMSY"] 
iF <- which.min(abs(Plot_Values_V$Ftarget - FMSY_med))

BMSY_med <- Plot_Values_V[
  Plot_Values_V$variable == "Spawning stock biomass" &
    Plot_Values_V$Ftarget == Plot_Values_V$Ftarget[iF],
  "p50"
]

FMSY_plot <- Plot_Values_V$Ftarget[iF]
FMSY_med #0.198
BMSY_med #50161

# the value of 0.1933 is between two Fscan values 0.18 and 0.20 so assuming linear line the value is:
F1 <- 0.18
F2 <- 0.20

B1 <- Plot_Values_V[Plot_Values_V$Ftarget == 0.18 & Plot_Values_V$variable == "Spawning stock biomass", "p50"]
B2 <- Plot_Values_V[Plot_Values_V$Ftarget == 0.20 & Plot_Values_V$variable == "Spawning stock biomass", "p50"]

FMSY <- FMSY_med
FMSY_plot<-FMSY

BMSY <- B1 + (FMSY - F1) * (B2 - B1) / (F2 - F1) #52588

save(SIM_1997_V_FMSY, file = "model/refpts/viking/V_FMSY.RData")
load("model/refpts/viking/V_FMSY.RData")

#Plot for BMSY check TT script 
load("model/refpts/viking/V_FMSY.RData")
eqsim_plot_range(SIM_1997_V_FMSY, type='ssb')

abline(v = FMSY_plot, col = "blue", lty = 2, lwd = 1.5)

points(FMSY_plot, BMSY,
       col = "blue", pch = 19, cex = 1.3)

text(FMSY_plot, BMSY,
     labels = paste0("BMSY = ", round(BMSY, 0)),
     pos = 4, col = "blue")

# The median of SSB estimates at FMSY - Needed for the report
FMSY_med <- SIM_1997_V_FMSY$Refs2["lanF", "medianMSY"]

ssb_FMSY <- subset(
  Plot_Values_V,
  variable == "Spawning stock biomass"
)

SSB_med <- approx(
  x = ssb_FMSY$Ftarget,
  y = ssb_FMSY$p50,
  xout = FMSY_med
)$y

FMSY_med
SSB_med #50672


#-----------------------------------------------------------------
# Blim - FULL Time-Series
#-----------------------------------------------------------------

Viking_1983 <- window(Viking, start = 1983, end = 2025)

set.seed(5)

## Type 2: estimate breakpoint of segemented regression internally in EqSim ----
FIT_1983_V <- eqsr_fit(Viking_1983, nsamp = 2000, models = "Segreg")
save(FIT_1983_V, file = "model/refpts/viking/FIT_1983_V_type2_Blim.RData")
eqsr_plot(FIT_1983_V, n=2e4)


## Variance params ----
Fcv <- 0.212
Fphi <- 0.423

## Blim ----

Blim <- FIT_1983_V$sr.det$b  #14027 
#Checks need to be done with BMSY
# suggestion from Christoffer: Blim = min(Blim calculated, Bmsy)

## Bpa and Btrigger ----
Bpa <- Blim * exp(1.645 * sigma) #when sigma= 0.2 you get a slightly higher Bpa more precautionary 
Btrig <- Bpa #19492

## FP0.5 ----
set.seed(7)

SIM_1983_V_Fp.05 <- #Fpa
  eqsim_run(
    VFit1_1997, #Use FMSY object for truncated TS here 
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
eqsim_plot(SIM_1983_V_Fp.05, catch = "FALSE")

taf.png("model/refpts/viking/eqsim_Fpa.png")
eqsim_plot_range(SIM_1983_V_Fp.05, type="median")
dev.off()

save(SIM_1983_V_Fp.05, file = "model/refpts/viking/SIM_1983_V_Fp.05.RData")
load("model/refpts/viking/SIM_1983_V_Fp.05.RData")

## Final refpoints ----
V_refPoints <- list(MSYbtrigger = Btrig,
                     Fmsy = SIM_1997_V_FMSY$Refs2["lanF", "medianMSY"],
                     Fmsyupper = SIM_1997_V_FMSY$Refs2["lanF", "Medupper"],
                     Fmsylower = SIM_1997_V_FMSY$Refs2["lanF", "Medlower"],
                     Blim = Blim,
                     Bpa = Bpa,
                     #  Flim = Flim,
                     Fp.05 = SIM_1983_V_Fp.05$Refs2["catF", "F05"],
                     Fpa = SIM_1983_V_Fp.05$Refs2["catF", "F05"],
                     biol.years = dim(SIM_1997_V_FMSY$ibya$weca)[2],
                     sel.years = dim(SIM_1997_V_FMSY$ibya$sel)[2],
                     B0 = B0_V,
                     BMSY = BMSY)

V_refPoints <- as.data.frame(V_refPoints)

save(V_refPoints, file = "model/refpts/viking/V_refPoints.RData")



