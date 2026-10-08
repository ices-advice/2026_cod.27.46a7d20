## 4 Reference points - Northwestern

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
TAF::mkdir("model/refpts/southern")

sigmas <- lapply(ssbtable(fit, addTotal = TRUE, returnList = TRUE),function(x) tail((log(x[,"High"]) - log(x[,"Low"]))/4,1))

#saveRDS(sigmas,"model/refpts/sigmas.rds")

#------------------------------------------------------------------------------------------------------------------------------------------
# Southern 
#------------------------------------------------------------------------------------------------------------------------------------------

sigma <- sigmas$South #0.244
sigma <- ifelse(sigmas[[2]]<0.2, 0.2, sigmas[[2]]) # More precautionary 

## extract subcomponents ----
South <- stks[[2]]

## rename stocks ----
South@name <- "South"

## Fbar ranges ----
range(South, c("minfbar", "maxfbar")) <- c(2, 4) 

## Landings obligation adjustments ----
# Use wanted catch for ages 1-2: Fish below MCRS cannot be marketed
# Use all catch for ages 3+: Landing obligation
South@landings.n[3:7,,,,]<-South@landings.n[3:7,,,,]+South@discards.n[3:7,,,,]
South@landings.wt[3:7,,,,]<-South@catch.wt[3:7,,,,]
South@discards.n[3:7,,,,]<-0

South@landings <- apply(South@landings.n *  South@landings.wt,2,sum)
South@discards <- apply(South@discards.n *  South@discards.wt,2,sum)

#-------------------------------------------------------------------------------------------------------------------------------------------
# Stock-Recruitment Plots
#-------------------------------------------------------------------------------------------------------------------------------------------

# 1 . SR - remember lag
SSB <- data.frame(ssbtable(fit))
REC <- data.frame(rectable(fit))

SSB_S <- data.frame(Year = as.numeric(rownames(SSB)),SSB = SSB$South..Estimate)
REC_S <- data.frame(Year = as.numeric(rownames(REC)),REC = REC$South..Estimate)

# Recruitment comes from previous spawning year
REC_S$spawn_year <- REC_S$Year - 1

# Match SSB_t with Recruitment_t+1
SR <- merge(
  SSB_S[, c("Year", "SSB")],
  REC_S[, c("spawn_year", "REC")],
  by.x = "Year",
  by.y = "spawn_year"
)

SR <- SR[order(SR$Year), ]

png("model/refpts/southern/SR_S.png", width = 2000, height = 1600, res = 300)
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

text(SR$SSB,
     SR$REC,
     labels = SR$Year,
     col = cols,
     cex = 0.7,
     pos = 3)
dev.off()

# 2. SR - R/SSB vs SSB
SR$R_SSB <- SR$REC / SR$SSB

# Match SSB_t with Recruitment_t+1
SR <- merge(
  SSB_S[, c("Year", "SSB")],
  REC_S[, c("spawn_year", "REC")],
  by.x = "Year",
  by.y = "spawn_year"
)

SR <- SR[order(SR$Year), ]

# Recruitment per unit SSB
SR$R_SSB <- SR$REC / SR$SSB

png("model/refpts/southern/R_SSB_S.png", width = 2000, height = 1600, res = 300)
cols <- ifelse(SR$Year >= 1997, "red", "black")

plot(SR$SSB,
     SR$R_SSB,
     type = "n",
     xlab = "SSB",
     ylab = "R/SSB",
     xlim = c(0, max(SR$SSB, na.rm = TRUE)),
     ylim = c(0, max(SR$R_SSB, na.rm = TRUE) * 1.15))

lines(SR$SSB, SR$R_SSB,col = "black",lwd = 1)

points(SR$SSB, SR$R_SSB,pch = 16,col = cols)

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
# Decisions made at TT x WGNSSK Meeting 
#Do everything with the truncated time-series for the southern stock & Type 2

## SR fit 1997 Type 2 ----------------------------------------------------------

set.seed(1)

southern_1997 <- window(South, start = 1997, end = 2025)
                        
FIT_1997_s_type2 <- eqsr_fit(southern_1997, nsamp = 2000, models = "Segreg")
eqsr_plot(FIT_1997_s_type2, n=2e4)
save(FIT_1997_s_type2, file = "model/refpts/southern/FIT_1997_s_type2.RData")

## EqSim 1997 Type 2 5yr -------------------------------------------------------

## Variance params ----
Fcv <- 0.212
Fphi <- 0.423

## Blim and Bpa ----
Blim <- FIT_1997_s_type2$sr.det$b #14433
Bpa <- Blim * exp(1.645 * sigma)

### FMSY ----
set.seed(5)

SIM_1997_s_type2_FMSY <-
  eqsim_run(
    FIT_1997_s_type2,
    bio.years = c(2016, 2025), # 10 year biols
    bio.const = FALSE,
    sel.years = c(2023, 2025), # 3 year selects
    sel.const = FALSE,
    Fcv = Fcv,
    Fphi = Fphi,
    Btrigger = 0,
    Blim = Blim,
    Fscan = seq(0, 1.2, len = 61),
    verbose = FALSE
  )

eqsim_plot(SIM_1997_s_type2_FMSY, catch = "FALSE")

taf.png("model/refpts/southern/eqsim_Fmsy.png")
eqsim_plot_range(SIM_1997_s_type2_FMSY, type="median")
dev.off()

eqsim_plot_range(SIM_1997_s_type2_FMSY, type='ssb')
SIM_1997_s_type2_FMSY$Refs2
Plot_Values_S<-SIM_1997_s_type2_FMSY$rbp # to get value for B0, pull out value at p50 that corresponds to Ftarget = 0

# B0 and BMST calcs for sense checks 
B0_S <- Plot_Values_S[Plot_Values_S$Ftarget == 0 & Plot_Values_S$variable == "Spawning stock biomass", "p50"]
B0_S_20 <- 0.20 * B0_S
B0_S_10 <- 0.10 * B0_S
B0_S_5 <- 0.05 * B0_S

#BMSY
#Extraction and plot of median 
FMSY_med <- SIM_1997_s_type2_FMSY$Refs2["lanF", "medianMSY"] 
iF <- which.min(abs(Plot_Values_S$Ftarget - FMSY_med))

BMSY_med <- Plot_Values_S[
  Plot_Values_S$variable == "Spawning stock biomass" &
    Plot_Values_S$Ftarget == Plot_Values_S$Ftarget[iF],
  "p50"
]

FMSY_plot <- Plot_Values_S$Ftarget[iF]
FMSY_med #0.243
BMSY_med #36925 B that corresponds to FMSY

# the value of 0.1933 is between two Fscan values 0.18 and 0.20 so assuming linear line the value is:
F1 <- 0.24
F2 <- 0.26

B1 <- Plot_Values_S[Plot_Values_S$Ftarget == 0.24 & Plot_Values_S$variable == "Spawning stock biomass", "p50"]
B2 <- Plot_Values_S[Plot_Values_S$Ftarget == 0.26 & Plot_Values_S$variable == "Spawning stock biomass", "p50"]

FMSY <- FMSY_med
FMSY_plot<-FMSY

BMSY <- B1 + (FMSY - F1) * (B2 - B1) / (F2 - F1) #36004
Bmsy_C <- SIM_1997_s_type2_FMSY$Refs2["lanB","medianMSY"] #Christoffer's code 
#check this value lines up on the plot

save(SIM_1997_s_type2_FMSY, file = "model/refpts/southern/SIM_1997_s_type2_FMSY.RData")

#Plot for BMSY check 
load("model/refpts/southern/SIM_1997_s_type2_FMSY.RData")
eqsim_plot_range(SIM_1997_s_type2_FMSY, type='ssb')

abline(v = FMSY_plot, col = "blue", lty = 2, lwd = 1.5)

points(FMSY_plot, BMSY,
       col = "blue", pch = 19, cex = 1.3)

text(FMSY_plot, BMSY,
     labels = paste0("BMSY = ", round(BMSY, 0)),
     pos = 4, col = "blue")


# The median of SSB estimates at FMSY - Needed for the report
FMSY_med <- SIM_1997_s_type2_FMSY$Refs2["lanF", "medianMSY"]

ssb_FMSY <- subset(
  Plot_Values_S,
  variable == "Spawning stock biomass"
)

SSB_med <- approx(
  x = ssb_FMSY$Ftarget,
  y = ssb_FMSY$p50,
  xout = FMSY_med
)$y

FMSY_med
SSB_med #36439



## Bpa and Btrig ----
Btrig <- Bpa #21550

### FP0.5 ----
set.seed(6)

SIM_1997_s_type2_Fp.05 <-
  eqsim_run(
    FIT_1997_s_type2, 
    bio.years = c(2016, 2025), # 5 year biols
    bio.const = FALSE,
    sel.years = c(2023, 2025), # 3 year selects
    sel.const = FALSE,
    Fcv = Fcv,
    Fphi = Fphi,
    Btrigger = Btrig,
    Blim = Blim,
    Fscan = seq(0, 1.2, len = 61),
    verbose = FALSE
  )
eqsim_plot(SIM_1997_s_type2_Fp.05, catch = "FALSE")

taf.png("model/refpts/southern/eqsim_Fpa.png")
eqsim_plot_range(SIM_1997_s_type2_Fp.05, type="median")
dev.off()

save(SIM_1997_s_type2_Fp.05, file = "model/refpts/southern/SIM_1997_s_type2_Fp.05.RData")

## Final refPoints ----
S_1997_type2_refPoints <- list(MSYbtrigger = Btrig,
                                   Fmsy = SIM_1997_s_type2_FMSY$Refs2["lanF", "medianMSY"],
                                   Fmsyupper = SIM_1997_s_type2_FMSY$Refs2["lanF", "Medupper"],
                                   Fmsylower = SIM_1997_s_type2_FMSY$Refs2["lanF", "Medlower"],
                                   Blim = Blim,
                                   Bpa = Bpa,
                                  # Flim = Flim,
                                   Fp.05 = SIM_1997_s_type2_Fp.05$Refs2["catF", "F05"],
                                   Fpa = SIM_1997_s_type2_Fp.05$Refs2["catF", "F05"],
                                   biol.years = dim(SIM_1997_s_type2_FMSY$ibya$weca)[2],
                                   sel.years = dim(SIM_1997_s_type2_FMSY$ibya$sel)[2],
                                   B0 = B0_S,
                                   BMSY = BMSY)

S_1997_type2_refPoints <- as.data.frame(S_1997_type2_refPoints)
save(S_1997_type2_refPoints, file = "model/refpts/southern/S_1997_type2_refPoints.RData")

