## Run analysis, write model results

## Before: data.Rdata, conf.Rdata
## After: model.Rdata
#clear environment check data folder has data.Rdata

dir.create("C:/Users/RH28/Documents/R/win-library/4.3",recursive = TRUE,showWarnings = FALSE)
.libPaths("C:/Users/RH28/Documents/R/win-library/4.3")

library(icesTAF)
library(stockassessment)

# #Re-installing Stock Assessment & Multi-stock Assessment Packages after CMO update on 14/09/26
# tarfile_stock <- "C:/Users/RH28/Documents/stockassessment_0.12.5.9003.tar.gz"
# download.file("https://calbertsen.r-universe.dev/src/contrib/stockassessment_0.12.5.9003.tar.gz",tarfile_stock,mode = "wb")
# install.packages(tarfile_stock,repos = NULL,type = "source")
# 
packageVersion("stockassessment") # 0.12.5.9003

# tarfile_multi <- "C:/Users/RH28/Documents/multiStockassessment_0.5.0.9004.tar.gz" #includes second software update made 22/09/26
# download.file("https://calbertsen.r-universe.dev/src/contrib/multiStockassessment_0.5.0.9004.tar.gz",tarfile_multi,mode = "wb")
# install.packages(tarfile_multi,repos = NULL,type = "source")

library(multiStockassessment) 
packageVersion("multiStockassessment") # 0.5.0.9004 Includes TT updates & two uodates to the forecast software 

#TAF::mkdir("model") 
#TAF::mkdir("config") 

source("utilities.R")

# 1 Fit multistock SAM
#taf.boot(software = FALSE)
load('boot/data/data.Rdata')
conf <- readRDS("config/conf_recal.Rds") # updated as per CMO TT script update 
#capture.output(print(conf),file = "config/conf_recal.txt")

## 1) Fit
# re-fit model as per CMO advises following update of package software before running the forecasts 

system.time(fit <- runFit(dat, conf, run = TRUE, fullDerived=FALSE, phForm = attr(conf,"phForm"),shared_keys=attr(conf,"shared_keys"),shared_selectivity=attr(conf,"shared_selectivity"),shared_oneFScalePars=attr(conf,"shared_oneFScalePars"),map=attr(conf,"map"),pls=attr(conf,"pls"), newtonsteps = 0))

AIC(fit) #AIC: -2725.478 (-2488.12 following software update)
save(fit, file="model/model.Rdata")
saveRDS(fit, "model/model.rds")

## 2 Residuals
RES <- residuals(fit)
save(RES, file="model/residuals.Rdata")
saveRDS(RES, "model/residuals.rds")

## 3 Retrospective runs 
system.time(RETRO <- retro(fit, 5, 1, initializePars = TRUE, initializeRandomEffects = TRUE))
stockassessment::mohn(RETRO, addTotal = TRUE) # values for slides on retros, report mohn's Rho for SSB, Rec includes 2026 Q1
stockassessment::mohn(RETRO, addTotal=TRUE, lag = 1) # values for slides on retros just for F #lag of 1, because we don't have fishing mortality for 2025
save(RETRO, file="model/retro.Rdata")
saveRDS(RETRO, file="model/retro.rds")

## 4 Reference points In seperate scripts for the substocks, only extracting here: 
load("model/refpts/northwestern/NW_refPoints.Rdata") # copied over from last years repository into model
load("model/refpts/southern/S_1997_type2_refPoints.RData")
load("model/refpts/viking/V_refPoints.RData")
NW <- NW_refPoints  
SO <- S_1997_type2_refPoints
VI <- V_refPoints

## Reference points
Fmsy <- c("Northwest" = as.numeric(round(NW['Fmsy'],3)), "South" = as.numeric(round(SO['Fmsy'],3)), "Viking" = as.numeric(round(VI['Fmsy'],3)))
Flower <- c("Northwest" = as.numeric(round(NW['Fmsylower'],3)), "South" = as.numeric(round(SO['Fmsylower'],3)), "Viking" = as.numeric(round(VI['Fmsylower'],3)))
Fupper <- c("Northwest" = as.numeric(round(NW['Fmsyupper'],3)), "South" = as.numeric(round(SO['Fmsyupper'],3)), "Viking" = as.numeric(round(VI['Fmsyupper'],3)))
MSYBtrigger <- c("Northwest" = as.numeric(round(NW['MSYbtrigger'])), "South" = as.numeric(round(SO['MSYbtrigger'])), "Viking" = as.numeric(round(VI['MSYbtrigger'])))
Blim <- c("Northwest" = as.numeric(round(NW['Blim'])), "South" = as.numeric(round(SO['Blim'])), "Viking" = as.numeric(round(VI['Blim'])))
Fpa <- c("Northwest" = as.numeric(round(NW['Fpa'],3)), "South" = as.numeric(round(SO['Fpa'],3)), "Viking" = as.numeric(round(VI['Fpa'],3)))

## 5 Forecasts ------------------------------------------------------------------------------------------
load("model/model.Rdata") 
#load("model/forecasts/Forecast_0.7FScaler_1000SIM_Final.Rdata") 
#load("model/forecasts/Forecast_0.7FScaler_1000SIM_Final_+ZeroAdviceRun.Rdata")

## Forecast settings are chosen to be as close to SAM forecast as possible
cores <- 1
DF <- FALSE        # deterministicF (do not set variance of F process to approximately 0)
UNLC <- FALSE      # useNonLinearityCorrection (do not do a non-linearity correction of short cut MSE F targets)
UFH <- TRUE        # useFHessian (use the hessian of F estimates to determine future variability of F)
PNF <- FALSE       # processNoiseF (do not let F variance increase with time as a random walk)
FFD <- FALSE       # fixedFDeviation (fix the simulated deviations in F per replicate)
URP <- FALSE        # useRecPool (FALSE = If it should sample from a log-normal distribution with the same median and log-variance, TRUE = only resample from the specific estimated median recruitment values in rec.years) 
FACW <- FALSE       # forceAvg_CW (force the use of an average CW in forecasts instead of the GMRF, GMRF = FALSE)
FASW <- FALSE       # forceAvg_SW (force the use of an average SW in forecasts instead of the GMRF, GMRF = FALSE)
FANM <- FALSE       # forceAvg_NM (force the use of an average NM in forecasts instead of the GMRF, GMRF = FALSE)
FAMO <- FALSE       # forceAvg_MO (force the use of an average MO in forecasts instead of the GMRF, GMRF = FALSE)
## NOTE: The arguments custom_CW, custom_SW, custom_NM, and custom_MO can be used to specify specific values to use in the forecast.
NOSIM <- 1000     # Needs changing to 1000 - changed to 100 to run without precautionary considerations
cstrIntermediateYear <- "F=1*" ## F status quo = F=1* , F=0.7* in intermediate year, change F to 0.5 etc for F scaler F status quo 

## Average biological input over last three years
aveYears <- lapply(fit,function(x) tail(head(x$data$years,-1),3))
## Use recruitment from 1998 onwards instead of log-random walk (old option)
#recYears <- lapply(fit,function(x) 1998:max(x$data$years)) # Average 1998 
recYears <- lapply(fit,function(x) 2024:max(x$data$years)) # 2024-2026 because SAM estimates a value for 2026 based on the Q1 data

AdviceYear <- 2027 
  
# # #------------------------------------------------------------------------------------------------------------------------
# # # Dummy Forecasts to provide different F status quo options for a different F scaler value #100 NoSIMS
# # #------------------------------------------------------------------------------------------------------------------------
# 
# FC <- list()
# 
# # Dummy forecast assumptions to test
# cstrValues <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
# 
# 
# # ==================================================================================================
# # LOOP THROUGH cstrIntermediateYear VALUES
# # ==================================================================================================
# 
# for (x in cstrValues) {
# 
#   # Set intermediate-year F constraint
#   cstrIntermediateYear <- paste0("F=", x, "*")
# 
# 
#   # ------------------------------------------------------------------------------------------------
#   # 1. F = FMSY
#   # ------------------------------------------------------------------------------------------------
# 
#   set.seed(12345)
# 
#   cstr <- lapply(Fmsy, function(f) {c(cstrIntermediateYear, rep(sprintf("F=%f", f), 2))})
# 
#   FC[[paste0("01_FMSY_", x)]] <- list(name = paste0("F = F~MSY~, intermediate F = ", x),
#                                       constraints = cstr,
#                                       result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,ave.years = aveYears,
#                                                                                                         rec.years = recYears,
#                                                                                                         year.base = "lastCatchYear",
#                                                                                                         useModelLastN = TRUE,
#                                                                                                         resampleFirst = TRUE,
#                                                                                                         splitLD = TRUE,
#                                                                                                         deterministicF = DF,
#                                                                                                         useNonLinearityCorrection = UNLC,
#                                                                                                         processNoiseF = PNF,
#                                                                                                         fixedFDeviation = FFD,
#                                                                                                         useFHessian = UFH,
#                                                                                                         useRecPool = URP,
#                                                                                                         forceAvg_CW = FACW,
#                                                                                                         forceAvg_SW = FASW,
#                                                                                                         forceAvg_NM = FANM,
#                                                                                                         forceAvg_MO = FAMO,
#                                                                                                         nosim = NOSIM,
#                                                                                                         ncores = cores,
#                                                                                                         progress = TRUE)
#   )
# 
# 
#   # ------------------------------------------------------------------------------------------------
#   # 2. SSB = SSB of previous year
#   #    F = F of previous year
#   # ------------------------------------------------------------------------------------------------
# 
#   set.seed(12345)
# 
#   cstr <- lapply(1:3, function(s) {
#     c(cstrIntermediateYear, "SSB=1*", cstrIntermediateYear)
#   })
# 
#   FC[[paste0("11_SSBasLastYear_", x)]] <- list(name = paste0("SSB(2027) = SSB(2026), intermediate F = ", x),
#                                           constraints = cstr,
#                                          result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,ave.years = aveYears,
#                                                                                                         rec.years = recYears,
#                                                                                                         year.base = "lastCatchYear",
#                                                                                                         useModelLastN = TRUE,
#                                                                                                         resampleFirst = TRUE,
#                                                                                                         splitLD = TRUE,
#                                                                                                         deterministicF = DF,
#                                                                                                         useNonLinearityCorrection = UNLC,
#                                                                                                         processNoiseF = PNF,
#                                                                                                         fixedFDeviation = FFD,
#                                                                                                         useFHessian = UFH,
#                                                                                                         useRecPool = URP,
#                                                                                                         forceAvg_CW = FACW,
#                                                                                                         forceAvg_SW = FASW,
#                                                                                                         forceAvg_NM = FANM,
#                                                                                                         forceAvg_MO = FAMO,
#                                                                                                         nosim = NOSIM,
#                                                                                                         ncores = cores,
#                                                                                                         progress = TRUE)
#   )
# 
# 
#   # ------------------------------------------------------------------------------------------------
#   # 3. FMTC = total catch from the corresponding FMSY forecast
#   # ------------------------------------------------------------------------------------------------
# 
#   set.seed(12345)
# 
#   # Get total advice catch from THIS x-specific FMSY forecast
#   FmsyApproachTotalAdvice <- sum(sapply(lapply(FC[[paste0("01_FMSY_", x)]]$result,attr,which = "tab"),
#       function(z) z[as.character(AdviceYear), "catch:median"]))
# 
#   cstr <- c(cstrIntermediateYear,sprintf("FMTC=%f", FmsyApproachTotalAdvice),cstrIntermediateYear)
# 
#   FC[[paste0("13_WPC_SpecialCod_", x)]] <- list(name = sprintf("C = special cod = %.0f, intermediate F = %s",FmsyApproachTotalAdvice,x),
#                                             constraints = cstr,
#                                              result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,ave.years = aveYears,
#                                                                                                         rec.years = recYears,
#                                                                                                         year.base = "lastCatchYear",
#                                                                                                         useModelLastN = TRUE,
#                                                                                                         resampleFirst = TRUE,
#                                                                                                         splitLD = TRUE,
#                                                                                                         deterministicF = DF,
#                                                                                                         useNonLinearityCorrection = UNLC,
#                                                                                                         processNoiseF = PNF,
#                                                                                                         fixedFDeviation = FFD,
#                                                                                                         useFHessian = UFH,
#                                                                                                         useRecPool = URP,
#                                                                                                         forceAvg_CW = FACW,
#                                                                                                         forceAvg_SW = FASW,
#                                                                                                         forceAvg_NM = FANM,
#                                                                                                         forceAvg_MO = FAMO,
#                                                                                                         nosim = NOSIM,
#                                                                                                         ncores = cores,
#                                                                                                         progress = TRUE)
# )}
# 
# 
# # ==========================================================================================================================
# # SAVE
# # ==========================================================================================================================
# 
#save(FC,file = "model/forecasts/Dummy_Forecast_assumptions_Updated_100SIMS.Rdata")
# 
# catch_2026 <- do.call(rbind, lapply(names(FC), function(run_name) {
#   tabs <- lapply(FC[[run_name]]$result, attr, which = "tab")
# 
#   data.frame(Run = run_name,Substock = names(tabs),
#     Catch_2026 = sapply(tabs, function(x) {
#       x["2026", "catch:median"]}),row.names = NULL)
# }))
# View(catch_2026)
# 
# library(tidyr)
# 
# catch_2026_wide <- pivot_wider(catch_2026,names_from = Substock,values_from = Catch_2026)
# 
# catch_2026_wide$Total <- rowSums(catch_2026_wide[, setdiff(names(catch_2026_wide), "Run")],na.rm = TRUE)
# write.csv(catch_2026_wide,"model/forecasts/Dummy_Forecast_Median_Catch_2026_F_Scaler_Updated.csv",row.names = FALSE)

FC <- list()
# make sure 100 and 1000 sim results are similar 

# -----------------------------------------------------------------------------------------------------------------------
# #1 F = FMSY
#------------------------------------------------------------------------------------------------------------------------

set.seed(12345) 
cstr <- lapply(Fmsy, function(f) c(cstrIntermediateYear, rep(sprintf("F=%f",f),2)))
FC[["01_FMSY"]] <- list(name = "F = F~MSY~",
                        constraints = cstr,
                        result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                         ave.years=aveYears,
                                                                                         rec.years=recYears,
                                                                                         year.base="lastCatchYear",
                                                                                         useModelLastN=TRUE,
                                                                                         resampleFirst = TRUE,
                                                                                         splitLD = TRUE,
                                                                                         deterministicF = DF,
                                                                                         useNonLinearityCorrection= UNLC,
                                                                                         processNoiseF = PNF,
                                                                                         fixedFDeviation = FFD,
                                                                                         useFHessian=UFH,
                                                                                         useRecPool=URP,
                                                                                         forceAvg_CW = FACW,
                                                                                         forceAvg_SW = FASW,
                                                                                         forceAvg_NM = FANM,
                                                                                         forceAvg_MO = FAMO,
                                                                                         nosim=NOSIM,
                                                                                         ncores = cores,
                                                                                         progress = TRUE))

# Don't use Christoffer's function it applies an FMSY to each simulation, in the advice sheet it won't match??
# Issue with the MSY harvest control rule 
# Take SSB in 2027 / BTrigger * FMSY 

# -----------------------------------------------------------------------------------------------------------------------
# #2 F = FMSYlower
# ------------------------------------------------------------------------------------------------------------------------

set.seed(12345)
cstr <- lapply(Flower, function(f) c(cstrIntermediateYear, rep(sprintf("F=%f",f),2)))
FC[["02_FMSYlower"]] <- list(name = "F = F~MSY lower~",
                             constraints = cstr,
                             result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                              ave.years=aveYears,
                                                                                              rec.years=recYears,
                                                                                              year.base="lastCatchYear",
                                                                                              useModelLastN=TRUE,
                                                                                              resampleFirst = TRUE,
                                                                                              splitLD = TRUE,
                                                                                              deterministicF = DF,
                                                                                              useNonLinearityCorrection= UNLC,
                                                                                              processNoiseF = PNF,
                                                                                              fixedFDeviation = FFD,
                                                                                              useFHessian=UFH,
                                                                                              useRecPool=URP,
                                                                                              forceAvg_CW = FACW,
                                                                                              forceAvg_SW = FASW,
                                                                                              forceAvg_NM = FANM,
                                                                                              forceAvg_MO = FAMO,
                                                                                              nosim=NOSIM,
                                                                                              ncores = cores,
                                                                                              progress = TRUE))



# -----------------------------------------------------------------------------------------------------------------------
# #3 F = FMSYupper (Not in advice sheet, don't show below Btrigger or blim check, do the same as last year, check)
#------------------------------------------------------------------------------------------------------------------------

set.seed(12345)
cstr <- lapply(Fupper, function(f) c(cstrIntermediateYear, rep(sprintf("F=%f",f),2)))
FC[["03_FMSYUpper"]] <- list(name = "F = F~MSY Upper~",
                             constraints = cstr,
                             result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                              ave.years=aveYears,
                                                                                              rec.years=recYears,
                                                                                              year.base="lastCatchYear",
                                                                                              useModelLastN=TRUE,
                                                                                              resampleFirst = TRUE,
                                                                                              splitLD = TRUE,
                                                                                              deterministicF = DF,
                                                                                              useNonLinearityCorrection= UNLC,
                                                                                              processNoiseF = PNF,
                                                                                              fixedFDeviation = FFD,
                                                                                              useFHessian=UFH,
                                                                                              useRecPool=URP,
                                                                                              forceAvg_CW = FACW,
                                                                                              forceAvg_SW = FASW,
                                                                                              forceAvg_NM = FANM,
                                                                                              forceAvg_MO = FAMO,
                                                                                              nosim=NOSIM,
                                                                                              ncores = cores,
                                                                                              progress = TRUE))

# -----------------------------------------------------------------------------------------------------------------------
# #4 F = 0
#------------------------------------------------------------------------------------------------------------------------

set.seed(12345)
cstr <- lapply(1:3, function(i) {c(cstrIntermediateYear, rep(sprintf("F=%f", 0.000001), 2))})
names(cstr) <- c("Northwest", "South", "Viking")
FC[["04_F=0"]] <- list(name = "F = 0",
                             constraints = cstr,
                       result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                        ave.years=aveYears,
                                                                                        rec.years=recYears,
                                                                                        year.base="lastCatchYear",
                                                                                        useModelLastN=TRUE,
                                                                                        resampleFirst = TRUE,
                                                                                        splitLD = TRUE,
                                                                                        deterministicF = DF,
                                                                                        useNonLinearityCorrection= UNLC,
                                                                                        processNoiseF = PNF,
                                                                                        fixedFDeviation = FFD,
                                                                                        useFHessian=UFH,
                                                                                        useRecPool=URP,
                                                                                        forceAvg_CW = FACW,
                                                                                        forceAvg_SW = FASW,
                                                                                        forceAvg_NM = FANM,
                                                                                        forceAvg_MO = FAMO,
                                                                                        nosim=NOSIM,
                                                                                        ncores = cores,
                                                                                        progress = TRUE))

# -----------------------------------------------------------------------------------------------------------------------
# #5 F = Fpa
#------------------------------------------------------------------------------------------------------------------------

set.seed(12345)
cstr <- lapply(Fpa, function(f) c(cstrIntermediateYear, rep(sprintf("F=%f",f),2)))
FC[["05_Fpa"]] <- list(name = "F = Fpa",
                             constraints = cstr,
                             result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                              ave.years=aveYears,
                                                                                              rec.years=recYears,
                                                                                              year.base="lastCatchYear",
                                                                                              useModelLastN=TRUE,
                                                                                              resampleFirst = TRUE,
                                                                                              splitLD = TRUE,
                                                                                              deterministicF = DF,
                                                                                              useNonLinearityCorrection= UNLC,
                                                                                              processNoiseF = PNF,
                                                                                              fixedFDeviation = FFD,
                                                                                              useFHessian=UFH,
                                                                                              useRecPool=URP,
                                                                                              forceAvg_CW = FACW,
                                                                                              forceAvg_SW = FASW,
                                                                                              forceAvg_NM = FANM,
                                                                                              forceAvg_MO = FAMO,
                                                                                              nosim=NOSIM,
                                                                                              ncores = cores,
                                                                                              progress = TRUE))

# -----------------------------------------------------------------------------------------------------------------------
# #6 F = F2026 -
#------------------------------------------------------------------------------------------------------------------------
set.seed(12345)

#cstr <- lapply("F=1*", function(f) c(cstrIntermediateYear, rep("F=1*",f),2)) # F status Quo
cstr <- lapply(1:3, function(i) {c(cstrIntermediateYear, rep("F=1*", 2))})
names(cstr) <- c("Northwest", "South", "Viking")
FC[["06_F=F2026"]] <- list(name = "F = F2026",
                             constraints = cstr,
                           result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                            ave.years=aveYears,
                                                                                            rec.years=recYears,
                                                                                            year.base="lastCatchYear",
                                                                                            useModelLastN=TRUE,
                                                                                            resampleFirst = TRUE,
                                                                                            splitLD = TRUE,
                                                                                            deterministicF = DF,
                                                                                            useNonLinearityCorrection= UNLC,
                                                                                            processNoiseF = PNF,
                                                                                            fixedFDeviation = FFD,
                                                                                            useFHessian=UFH,
                                                                                            useRecPool=URP,
                                                                                            forceAvg_CW = FACW,
                                                                                            forceAvg_SW = FASW,
                                                                                            forceAvg_NM = FANM,
                                                                                            forceAvg_MO = FAMO,
                                                                                            nosim=NOSIM,
                                                                                            ncores = cores,
                                                                                            progress = TRUE))

#------------------------------------------------------------------------------------------------------------------------
# 7 ICES Advice Rule FMSY - MSY approach: F~MSY~ x SSB (2027) / MSY B~trigger (TAC year) - HCR (compare 7 & 9 for the southern & NW)
#------------------------------------------------------------------------------------------------------------------------

set.seed(12345)

SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))
fval <- pmin(Fmsy*SSB/MSYBtrigger, Fmsy) # fval would be our target here - check

#cstr <- lapply("F=1*", function(f) c(cstrIntermediateYear, rep(sprintf("F=%f",fval),2)))
cstr <- lapply(fval, function(f) {c(cstrIntermediateYear,rep(sprintf("F=%f", f), 2))})
FC[["07_MSY approach: F~MSY~ x SSB (2027) / MSY B~trigger"]] <- list(name = "MSY approach: F~MSY~ x SSB (2027) / MSY B~trigger",
                             constraints = cstr,
                             result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                              ave.years=aveYears,
                                                                                              rec.years=recYears,
                                                                                              year.base="lastCatchYear",
                                                                                              useModelLastN=TRUE,
                                                                                              resampleFirst = TRUE,
                                                                                              splitLD = TRUE,
                                                                                              deterministicF = DF,
                                                                                              useNonLinearityCorrection= UNLC,
                                                                                              processNoiseF = PNF,
                                                                                              fixedFDeviation = FFD,
                                                                                              useFHessian=UFH,
                                                                                              useRecPool=URP,
                                                                                              forceAvg_CW = FACW,
                                                                                              forceAvg_SW = FASW,
                                                                                              forceAvg_NM = FANM,
                                                                                              forceAvg_MO = FAMO,
                                                                                              nosim=NOSIM,
                                                                                              ncores = cores,
                                                                                              progress = TRUE))

#------------------------------------------------------------------------------------------------------------------------
# 8 ICES Advice Rule FLower - F = F~MSY lower~ x SSB (2027) / MSY B~trigger~ (treat sub-stocks independently)
#------------------------------------------------------------------------------------------------------------------------
set.seed(12345)

SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))
fval <- pmin(Flower*SSB/MSYBtrigger, Flower)

cstr <- lapply(fval, function(f) {c(cstrIntermediateYear,rep(sprintf("F=%f", f), 2))})
FC[["08_Advice Rule: F= F~MSY lower x SSB (2027) / MSY B~trigger"]] <- list(name = "F= F~MSY lower x SSB (2027) / MSY B~trigger",
                                      constraints = cstr,
                                      result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                    ave.years=aveYears,
                                                                    rec.years=recYears,
                                                                    year.base="lastCatchYear",
                                                                    useModelLastN=TRUE,
                                                                    resampleFirst = TRUE,
                                                                    splitLD = TRUE,
                                                                    deterministicF = DF,
                                                                    useNonLinearityCorrection= UNLC,
                                                                    processNoiseF = PNF,
                                                                    fixedFDeviation = FFD,
                                                                    useFHessian=UFH,
                                                                    useRecPool=URP,
                                                                    forceAvg_CW = FACW,
                                                                    forceAvg_SW = FASW,
                                                                    forceAvg_NM = FANM,
                                                                    forceAvg_MO = FAMO,
                                                                    nosim=NOSIM,
                                                                    ncores = cores,
                                                                    progress = TRUE))
# -----------------------------------------------------------------------------------------------------------------------
# #9 SSB (2028) = Blim 
#------------------------------------------------------------------------------------------------------------------------

set.seed(12345)

SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))

cstr <- lapply(Blim, function(x) {c(cstrIntermediateYear,sprintf("SSB=%f", x),"F=1*")})

FC[["09_SSB=Blim"]] <- list(name = "SSB=Blim",
                            constraints = cstr,
                            result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                             ave.years=aveYears,
                                                                                             rec.years=recYears,
                                                                                             year.base="lastCatchYear",
                                                                                             useModelLastN=TRUE,
                                                                                             resampleFirst = TRUE,
                                                                                             splitLD = TRUE,
                                                                                             deterministicF = DF,
                                                                                             useNonLinearityCorrection= UNLC,
                                                                                             processNoiseF = PNF,
                                                                                             fixedFDeviation = FFD,
                                                                                             useFHessian=UFH,
                                                                                             useRecPool=URP,
                                                                                             forceAvg_CW = FACW,
                                                                                             forceAvg_SW = FASW,
                                                                                             forceAvg_NM = FANM,
                                                                                             forceAvg_MO = FAMO,
                                                                                             nosim=NOSIM,
                                                                                             ncores = cores,
                                                                                             progress = TRUE))

# -----------------------------------------------------------------------------------------------------------------------
# #10 SSB = MSY Btrigger -
#------------------------------------------------------------------------------------------------------------------------
set.seed(12345)

SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))

cstr <- lapply(MSYBtrigger, function(x) {c(cstrIntermediateYear,sprintf("SSB=%f", x),"F=1*")})
FC[["10_SSB=MSYBtrigger"]] <- list(name = "SSB=MSY Btrigger",
                            constraints = cstr,
                            result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                             ave.years=aveYears,
                                                                                             rec.years=recYears,
                                                                                             year.base="lastCatchYear",
                                                                                             useModelLastN=TRUE,
                                                                                             resampleFirst = TRUE,
                                                                                             splitLD = TRUE,
                                                                                             deterministicF = DF,
                                                                                             useNonLinearityCorrection= UNLC,
                                                                                             processNoiseF = PNF,
                                                                                             fixedFDeviation = FFD,
                                                                                             useFHessian=UFH,
                                                                                             useRecPool=URP,
                                                                                             forceAvg_CW = FACW,
                                                                                             forceAvg_SW = FASW,
                                                                                             forceAvg_NM = FANM,
                                                                                             forceAvg_MO = FAMO,
                                                                                             nosim=NOSIM,
                                                                                             ncores = cores,
                                                                                             progress = TRUE))


# -----------------------------------------------------------------------------------------------------------------------
# #11 SSB(2028) = SSB(2027)
#------------------------------------------------------------------------------------------------------------------------
# #If you prefer, you can change "SSB=1*" to eg "SSB=35837".

set.seed(12345)

SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))

# F same as year before, SSB same as year before, F same as year before
cstr <- lapply(1:3, function(s) c(cstrIntermediateYear, "SSB=1*","F=1*")) # first value is the intermediate year assumption = cstrIntermediateYear
FC[["11_SSBasLastYear_Corrected_100SIMS"]] <- list(name = "11_SSB(2028) = SSB(2027)",
                                 constraints = cstr,
                                 result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                                  ave.years=aveYears,
                                                                                                  rec.years=recYears,
                                                                                                  year.base="lastCatchYear",
                                                                                                  useModelLastN=TRUE,
                                                                                                  resampleFirst = TRUE,
                                                                                                  splitLD = TRUE,
                                                                                                  deterministicF = DF,
                                                                                                  useNonLinearityCorrection= UNLC,
                                                                                                  processNoiseF = PNF,
                                                                                                  fixedFDeviation = FFD,
                                                                                                  useFHessian=UFH,
                                                                                                  useRecPool=URP,
                                                                                                  forceAvg_CW = FACW,
                                                                                                  forceAvg_SW = FASW,
                                                                                                  forceAvg_NM = FANM,
                                                                                                  forceAvg_MO = FAMO,
                                                                                                  nosim=NOSIM,
                                                                                                  ncores = cores,
                                                                                                  progress = TRUE))

#-------------------------------------------------------------------------------------------------------------------------------------
# 12 Common F reduction - With Precautionary Considerations
# Specifying the SSB=blim scenario as an SSB initially to find southern value, then an F constraint to find the multiplier for NW & V
#-------------------------------------------------------------------------------------------------------------------------------------

set.seed(12345)

Fmult <- fval / attr(FC[[1]]$result[[2]], "shorttab")[1,2] # Applies the same proportional F reduction from the south to all three substocks
Ftarget <- sapply(1:3, function(s) {attr(FC[[6]]$result[[s]], "shorttab")[1,2] * Fmult})
names(Ftarget) <- c("Northwest", "South", "Viking")
cstr <- lapply(Ftarget, function(f) {c(cstrIntermediateYear, rep(sprintf("F=%f", f), 2))})
FC[["12_Common F Reduction_S"]] <- list(name = "Common F Multiplier - South",
                                 constraints = cstr,
                                 result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                                  ave.years=aveYears,
                                                                                                  rec.years=recYears,
                                                                                                  year.base="lastCatchYear",
                                                                                                  useModelLastN=TRUE,
                                                                                                  resampleFirst = TRUE,
                                                                                                  splitLD = TRUE,
                                                                                                  deterministicF = DF,
                                                                                                  useNonLinearityCorrection= UNLC,
                                                                                                  processNoiseF = PNF,
                                                                                                  fixedFDeviation = FFD,
                                                                                                  useFHessian=UFH,
                                                                                                  useRecPool=URP,
                                                                                                  forceAvg_CW = FACW,
                                                                                                  forceAvg_SW = FASW,
                                                                                                  forceAvg_NM = FANM,
                                                                                                  forceAvg_MO = FAMO,
                                                                                                  nosim=NOSIM,
                                                                                                  ncores = cores,
                                                                                                  progress = TRUE))


#-------------------------------------------------------------------------------------------------------------------------------------
# 13 Common F reduction - With Precautionary Considerations 
# Specifying the SSB=blim scenario as an SSB initially to find southern & Northwestern value, then an F constraint to find the multiplier for V
# Headline advice
# F that NW compare to intermediate year F 
#-------------------------------------------------------------------------------------------------------------------------------------
#FMSY for the south and precautionary considerations 
# By using the max reduction automatically encompassing protection for both NW & S

#NW and South both have SSB = Blim constraints, and the lower of their implied F reductions 
#determines the common multiplier applied to all three substocks

set.seed(12345)

#Fmult <- min(fval[1] / attr(FC[[1]]$result[[1]], "shorttab")[1,2],fval[2] / attr(FC[[1]]$result[[2]], "shorttab")[1,2])
fval <- pmin(Fmsy*SSB/MSYBtrigger, Fmsy) # fval would be our target here - check

Fmult_NW <- fval[1] / attr(FC[[1]]$result[[1]], "shorttab")[1,2] # should be the same for all scenarios intermediate year should be taking 2026 F's
Fmult_S  <- fval[2] / attr(FC[[1]]$result[[2]], "shorttab")[1,2] #0.29 south needs the bigger reduction 
Fmult <- min(Fmult_NW, Fmult_S) # 0.3 so need a 70% reduction 

Ftarget <- sapply(1:3, function(s) {attr(FC[["07_MSY approach: F~MSY~ x SSB (2027) / MSY B~trigger"]]$result[[s]], "shorttab")[1,2] * Fmult}) # if you needed a 90% you would multiply by 0.1 or 1-0.9
#*Fmult for NW & Viking 
names(Ftarget) <- c("Northwest", "South", "Viking")
cstr <- lapply(Ftarget, function(f) {c(cstrIntermediateYear, rep(sprintf("F=%f", f), 2))})


FC[["13_Common F Reduction_S&NW"]] <- list(name = "13_WPC_Common F Multiplier - min S,NW",
                                         constraints = cstr,
                                         result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                                          ave.years=aveYears,
                                                                                                          rec.years=recYears,
                                                                                                          year.base="lastCatchYear",
                                                                                                          useModelLastN=TRUE,
                                                                                                          resampleFirst = TRUE,
                                                                                                          splitLD = TRUE,
                                                                                                          deterministicF = DF,
                                                                                                          useNonLinearityCorrection= UNLC,
                                                                                                          processNoiseF = PNF,
                                                                                                          fixedFDeviation = FFD,
                                                                                                          useFHessian=UFH,
                                                                                                          useRecPool=URP,
                                                                                                          forceAvg_CW = FACW,
                                                                                                          forceAvg_SW = FASW,
                                                                                                          forceAvg_NM = FANM,
                                                                                                          forceAvg_MO = FAMO,
                                                                                                          nosim=NOSIM,
                                                                                                          ncores = cores,
                                                                                                          progress = TRUE))


#--------------------------------------------------------------------------------------------------------------------------
# 14 Without Precautionary Considerations  
# F multiplier that equates to the cumulative substock advice 
# if you apply the same mortality reduction for everything, what F gives you the advice for the cumulative substock 
# Takes scenario 7 scales everything up, what F gives me that total 
# Maybe needed to get the 67% Risk below Blim? 
# This would follow the guidelines 
# If they change to zero advice then the below would change to that option not 07 but 04 (In the FMSYApproachTotalAdvice)
#-------------------------------------------------------------------------------------------------------------------------

# should it be WPC for the NW & Southern 
# and MSY approach for the southern 
# We ran this but the MSY approach is the independent MSY approach for each substock 

set.seed(12345) 

## Using FMTC= constraint (*F* *M*ultiplier *T*otal *C*atch)
FmsyApproachTotalAdvice <- sum(sapply(lapply(FC[["07_MSY approach: F~MSY~ x SSB (2027) / MSY B~trigger"]]$result,attr,which="tab"),function(x) x[as.character(AdviceYear),"catch:median"]))
# Unless group override with zero FmsyApproachTotalAdvice be 0 south + No7 catch 2027 for NW & V
# Unless group override with zero FmsyApproachTotalAdvice be 0 south & NW + No7 catch 2027 V

cstr <- c(cstrIntermediateYear,sprintf("FMTC=%f",FmsyApproachTotalAdvice),"F=1*") # first value is the intermediate assumption = cstrIntermediateYear defined above as 0.7
FC[["14_WPC_SpecialCod"]] <- list(name = sprintf("14_WO_PC = special cod",FmsyApproachTotalAdvice),
                           constraints = cstr,
                           result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                            ave.years=aveYears,
                                                                                            rec.years=recYears,
                                                                                            year.base="lastCatchYear",
                                                                                            useModelLastN=TRUE,
                                                                                            resampleFirst = TRUE,
                                                                                            splitLD = TRUE,
                                                                                            deterministicF = DF,
                                                                                            useNonLinearityCorrection= UNLC,
                                                                                            processNoiseF = PNF,
                                                                                            fixedFDeviation = FFD,
                                                                                            useFHessian=UFH,
                                                                                            useRecPool=URP,
                                                                                            forceAvg_CW = FACW,
                                                                                            forceAvg_SW = FASW,
                                                                                            forceAvg_NM = FANM,
                                                                                            forceAvg_MO = FAMO,
                                                                                            nosim=NOSIM,
                                                                                            ncores = cores,
                                                                                            progress = TRUE))

#--------------------------------------------------------------------------------------------------------------------------
# 15 Without Precautionary Considerations  
# F multiplier that equates to the cumulative substock advice 
# if you apply the same mortality reduction for everything, what F gives you the advice for the cumulative substock 
# Takes scenario 7 scales everything up, what F gives me that total 
# Maybe needed to get the 67% Risk below Blim? 
# This would follow the guidelines 
# If they change to zero advice then the below would change to that option not 07 but 04 (In the FMSYApproachTotalAdvice)
#-------------------------------------------------------------------------------------------------------------------------

# should it be WPC for the NW & Southern 
# and MSY approach for the southern 
# We ran this but the MSY approach is the independent MSY approach for each substock 

# This is zero advice 

set.seed(12345) 

## Using FMTC= constraint (*F* *M*ultiplier *T*otal *C*atch)
FmsyApproachTotalAdvice <- sum(sapply(lapply(FC[["04_F=0"]]$result,attr,which="tab"),function(x) x[as.character(AdviceYear),"catch:median"]))
# Unless group override with zero FmsyApproachTotalAdvice be 0 south + No7 catch 2027 for NW & V
# Unless group override with zero FmsyApproachTotalAdvice be 0 south & NW + No7 catch 2027 V

cstr <- c(cstrIntermediateYear,sprintf("FMTC=%f",FmsyApproachTotalAdvice),"F=1*") #First value intermediate year assumption
FC[["15_WPC_SpecialCod_Zero"]] <- list(name = sprintf("15_WO_PC_SpecialCod_Zero",FmsyApproachTotalAdvice),
                                  constraints = cstr,
                                  result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                                   ave.years=aveYears,
                                                                                                   rec.years=recYears,
                                                                                                   year.base="lastCatchYear",
                                                                                                   useModelLastN=TRUE,
                                                                                                   resampleFirst = TRUE,
                                                                                                   splitLD = TRUE,
                                                                                                   deterministicF = DF,
                                                                                                   useNonLinearityCorrection= UNLC,
                                                                                                   processNoiseF = PNF,
                                                                                                   fixedFDeviation = FFD,
                                                                                                   useFHessian=UFH,
                                                                                                   useRecPool=URP,
                                                                                                   forceAvg_CW = FACW,
                                                                                                   forceAvg_SW = FASW,
                                                                                                   forceAvg_NM = FANM,
                                                                                                   forceAvg_MO = FAMO,
                                                                                                   nosim=NOSIM,
                                                                                                   ncores = cores,
                                                                                                   progress = TRUE))


#--------------------------------------------------------------------------------------------------------------------------
# 16 P(SSB < Blim) = 0.05 scenario   
#-------------------------------------------------------------------------------------------------------------------------

set.seed(12345) 
cstr <- lapply(Blim, function(f) c(cstrIntermediateYear, sprintf("quantile(SSB,0.05)=%f",f),"F=1*"))
FC[["16_5%Blim"]] <- list(name = "16_PSSB(5%Blim)",
                          constraints = cstr,
                          result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                           ave.years=aveYears,
                                                                                           rec.years=recYears,
                                                                                           year.base="lastCatchYear",
                                                                                           useModelLastN=TRUE,
                                                                                           resampleFirst = TRUE,
                                                                                           splitLD = TRUE,
                                                                                           deterministicF = DF,
                                                                                           useNonLinearityCorrection= UNLC,
                                                                                           processNoiseF = PNF,
                                                                                           fixedFDeviation = FFD,
                                                                                           useFHessian=UFH,
                                                                                           useRecPool=URP,
                                                                                           forceAvg_CW = FACW,
                                                                                           forceAvg_SW = FASW,
                                                                                           forceAvg_NM = FANM,
                                                                                           forceAvg_MO = FAMO,
                                                                                           SSBreference = list(Northwest = c(Blim=Blim[[1]]),
                                                                                                               South=c(Blim=Blim[[2]]),
                                                                                                               Viking=c(Blim=Blim[[3]])),
                                                                                           nosim=NOSIM,
                                                                                           ncores = cores,
                                                                                           progress = TRUE))


#--------------------------------------------------------------------------------------------------------------------------
# 17 Without Precautionary Considerations  - ZERO ADVICE SHEET SECOND OPTION
# F multiplier that equates to the cumulative substock advice 
# if you apply the same mortality reduction for everything, what F gives you the advice for the cumulative substock for NW, & V but zero for southern 
# Takes scenario 7 scales everything up, what F gives me that total 
# Maybe needed to get the 67% Risk below Blim? 
# This would follow the guidelines 
# If they change to zero advice then the below would change to that option not 07 but 04 (In the FMSYApproachTotalAdvice)
#-------------------------------------------------------------------------------------------------------------------------

# should it be WPC for the NW & Southern 
# and MSY approach for the southern 
# We ran this but the MSY approach is the independent MSY approach for each substock 

set.seed(12345) 

## Using FMTC= constraint (*F* *M*ultiplier *T*otal *C*atch)
#FmsyApproachTotalAdvice <- sum(sapply(lapply(FC[["07_MSY approach: F~MSY~ x SSB (2027) / MSY B~trigger"]]$result,attr,which="tab"),function(x) x[as.character(AdviceYear),"catch:median"]))
FmsyApproachTotalAdvice <- sum(sapply(FC[["07_MSY approach: F~MSY~ x SSB (2027) / MSY B~trigger"]]$result[c(1, 3)],
    function(x) {tab <- attr(x, "tab")
      tab[as.character(AdviceYear), "catch:median"]
    }
  )
)

# Unless group override with zero FmsyApproachTotalAdvice be 0 south + No7 catch 2027 for NW & V
# Unless group override with zero FmsyApproachTotalAdvice be 0 south & NW + No7 catch 2027 V

cstr <- c(cstrIntermediateYear,sprintf("FMTC=%f",FmsyApproachTotalAdvice),"F=1*") # first value is the intermediate assumption = cstrIntermediateYear defined above as 0.7
FC[["17_WPC_NW_V_FMSY_0South_Zero_Advice"]] <- list(name = sprintf("17_WO_PC_Zero_Advice",FmsyApproachTotalAdvice),
                                  constraints = cstr,
                                  result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
                                                                                                   ave.years=aveYears,
                                                                                                   rec.years=recYears,
                                                                                                   year.base="lastCatchYear",
                                                                                                   useModelLastN=TRUE,
                                                                                                   resampleFirst = TRUE,
                                                                                                   splitLD = TRUE,
                                                                                                   deterministicF = DF,
                                                                                                   useNonLinearityCorrection= UNLC,
                                                                                                   processNoiseF = PNF,
                                                                                                   fixedFDeviation = FFD,
                                                                                                   useFHessian=UFH,
                                                                                                   useRecPool=URP,
                                                                                                   forceAvg_CW = FACW,
                                                                                                   forceAvg_SW = FASW,
                                                                                                   forceAvg_NM = FANM,
                                                                                                   forceAvg_MO = FAMO,
                                                                                                   nosim=NOSIM,
                                                                                                   ncores = cores,
                                                                                                   progress = TRUE))



save(FC, file="model/forecasts/Forecast_FStQuo_ADG_BackUp_300926.Rdata")

# 
# #--------------------------------------------------------------------------------------------------------------------------
# # Request to test 3-year average for SW, MO, & NM - testing for the headline advice
# #--------------------------------------------------------------------------------------------------------------------------
# 
# FC <- list()
# 
# # -----------------------------------------------------------------------------------------------------------------------
# # #1 F = FMSY
# #------------------------------------------------------------------------------------------------------------------------
# 
# set.seed(12345) 
# cstr <- lapply(Fmsy, function(f) c(cstrIntermediateYear, rep(sprintf("F=%f",f),2)))
# FC[["01_FMSY"]] <- list(name = "F = F~MSY~",
#                         constraints = cstr,
#                         result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
#                                                                                          ave.years=aveYears,
#                                                                                          rec.years=recYears,
#                                                                                          year.base="lastCatchYear",
#                                                                                          useModelLastN=TRUE,
#                                                                                          resampleFirst = TRUE,
#                                                                                          splitLD = TRUE,
#                                                                                          deterministicF = DF,
#                                                                                          useNonLinearityCorrection= UNLC,
#                                                                                          processNoiseF = PNF,
#                                                                                          fixedFDeviation = FFD,
#                                                                                          useFHessian=UFH,
#                                                                                          useRecPool=URP,
#                                                                                          forceAvg_CW = FACW,
#                                                                                          forceAvg_SW = FASW,
#                                                                                          forceAvg_NM = FANM,
#                                                                                          forceAvg_MO = FAMO,
#                                                                                          nosim=NOSIM,
#                                                                                          ncores = cores,
#                                                                                          progress = TRUE))
# 
# 
# 
# #------------------------------------------------------------------------------------------------------------------------
# # 7 ICES Advice Rule FMSY - MSY approach: F~MSY~ x SSB (2027) / MSY B~trigger (TAC year) - HCR (compare 7 & 9 for the southern & NW)
# #------------------------------------------------------------------------------------------------------------------------
# 
# set.seed(12345)
# 
# SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))
# fval <- pmin(Fmsy*SSB/MSYBtrigger, Fmsy) # fval would be our target here - check
# 
# #cstr <- lapply("F=1*", function(f) c(cstrIntermediateYear, rep(sprintf("F=%f",fval),2)))
# cstr <- lapply(fval, function(f) {c(cstrIntermediateYear,rep(sprintf("F=%f", f), 2))})
# FC[["07_MSY approach: F~MSY~ x SSB (2027) / MSY B~trigger"]] <- list(name = "MSY approach: F~MSY~ x SSB (2027) / MSY B~trigger",
#                                                                      constraints = cstr,
#                                                                      result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
#                                                                                                                                       ave.years=aveYears,
#                                                                                                                                       rec.years=recYears,
#                                                                                                                                       year.base="lastCatchYear",
#                                                                                                                                       useModelLastN=TRUE,
#                                                                                                                                       fixedFDeviation = FFD,
#                                                                                                                                       useFHessian=UFH,
#                                                                                                                                       useRecPool=URP,
#                                                                                                                                       forceAvg_CW = FACW,
#                                                                                                                                       forceAvg_SW = FASW,
#                                                                                                                                       forceAvg_NM = FANM,
#                                                                                                                                       forceAvg_MO = FAMO,
#                                                                                                                                       nosim=NOSIM,
#                                                                                                                                       ncores = cores,
#                                                                                                                                       progress = TRUE))
# 
# #-------------------------------------------------------------------------------------------------------------------------------------
# # 13 Common F reduction - With Precautionary Considerations 
# # Specifying the SSB=blim scenario as an SSB initially to find southern & Northwestern value, then an F constraint to find the multiplier for V
# # Headline advice
# # F that NW compare to intermediate year F 
# #-------------------------------------------------------------------------------------------------------------------------------------
# #FMSY for the south and precautionary considerations 
# # By using the max reduction automatically encompassing protection for both NW & S
# 
# #NW and South both have SSB = Blim constraints, and the lower of their implied F reductions 
# #determines the common multiplier applied to all three substocks
# 
# set.seed(12345)
# 
# #Fmult <- min(fval[1] / attr(FC[[1]]$result[[1]], "shorttab")[1,2],fval[2] / attr(FC[[1]]$result[[2]], "shorttab")[1,2])
# fval <- pmin(Fmsy*SSB/MSYBtrigger, Fmsy) # fval would be our target here - check
# 
# Fmult_NW <- fval[1] / attr(FC[[1]]$result[[1]], "shorttab")[1,2] # should be the same for all scenarios intermediate year should be taking 2026 F's
# Fmult_S  <- fval[2] / attr(FC[[1]]$result[[2]], "shorttab")[1,2] #0.29 south needs the bigger reduction 
# Fmult <- min(Fmult_NW, Fmult_S) # 0.3 so need a 70% reduction 
# 
# Ftarget <- sapply(1:3, function(s) {attr(FC[["07_MSY approach: F~MSY~ x SSB (2027) / MSY B~trigger"]]$result[[s]], "shorttab")[1,2] * Fmult}) # if you needed a 90% you would multiply by 0.1 or 1-0.9
# #*Fmult for NW & Viking 
# names(Ftarget) <- c("Northwest", "South", "Viking")
# cstr <- lapply(Ftarget, function(f) {c(cstrIntermediateYear, rep(sprintf("F=%f", f), 2))})
# 
# 
# FC[["13_Common F Reduction_S&NW"]] <- list(name = "13_WPC_Common F Multiplier - min S,NW",
#                                            constraints = cstr,
#                                            result = multiStockassessment:::backcorrected_modelforecast.msam(fit, cstr,
#                                                                                                             ave.years=aveYears,
#                                                                                                             rec.years=recYears,
#                                                                                                             year.base="lastCatchYear",
#                                                                                                             useModelLastN=TRUE,
#                                                                                                             resampleFirst = TRUE,
#                                                                                                             splitLD = TRUE,
#                                                                                                             deterministicF = DF,
#                                                                                                             useNonLinearityCorrection= UNLC,
#                                                                                                             processNoiseF = PNF,
#                                                                                                             fixedFDeviation = FFD,
#                                                                                                             useFHessian=UFH,
#                                                                                                             useRecPool=URP,
#                                                                                                             forceAvg_CW = FACW,
#                                                                                                             forceAvg_SW = FASW,
#                                                                                                             forceAvg_NM = FANM,
#                                                                                                             forceAvg_MO = FAMO,
#                                                                                                             nosim=NOSIM,
#                                                                                                             ncores = cores,
#                                                                                                             progress = TRUE))
# 
# 
# 
# save(FC, file="model/forecasts/Forecast_0.7FScaler_1000SIM_Ave_3_Years.Rdata")






# Extracting Mohn's rho for TSB - report plots
# Full fit + retrospective peels
TSB_all <- c(list(attr(RETRO, "fit")),RETRO)

# Extract TSB for each fit
TSB_retro <- lapply(TSB_all, function(x) {
  
  tab <- tsbtable(x, returnList = TRUE)
  
  data.frame(
    Year = as.numeric(rownames(tab[[1]])),
    Northwest = tab[[1]][, 1],
    South = tab[[2]][, 1],
    Viking = tab[[3]][, 1]
  )
})

# Full assessment
TSB_full <- TSB_retro[[1]]

# Calculate Mohn's rho for each substock
mohn_TSB <- sapply(c("Northwest", "South", "Viking"), function(stock) {
  
  rho <- sapply(2:length(TSB_retro), function(i) {
    
    retro <- TSB_retro[[i]]
    
    # Terminal year of this retrospective
    yr <- max(retro$Year)
    
    # TSB from retrospective and full assessment
    retro_TSB <- retro[retro$Year == yr, stock]
    full_TSB  <- TSB_full[TSB_full$Year == yr, stock]
    
    # Relative difference
    (retro_TSB - full_TSB) / full_TSB
  })
  
  mean(rho, na.rm = TRUE)
})

mohn_TSB








#FC <- FC[order(names(FC))]

#names(FC)[14] <- "14_WPC_SpecialCod" - changing name in the list
#FC[[14]]$name <- "14_WPC_SpecialCod" - changing the name within the FC function itself so it displays the correct name in the plot

# e <- new.env()
# load("model/forecasts/Forecast_0.7FScaler_Updated_URP_100SIM.Rdata",envir = e)
# FC_100 <- e$FC
# 
# FC_combined <- FC_100
# FC_combined[names(FC)] <- FC
# 
# save(FC_combined,file = "model/forecasts/Forecast_Combined_1000and100SIM.Rdata")
