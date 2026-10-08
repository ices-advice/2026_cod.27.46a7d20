dir.create("C:/Users/RH28/Documents/R/win-library/4.3",recursive = TRUE,showWarnings = FALSE)
.libPaths("C:/Users/RH28/Documents/R/win-library/4.3")

library(surveyIndex)
library(mapplots)
#library(XLConnect)
library(openxlsx)

load("boot/initial/data/codData210926.Rdata") 
ICESarea <- read.csv("boot/initial/data/ICESrect.csv") 
#wb=XLConnect::loadWorkbook("boot/data/Area_NSCOD3_table_6a_inshore_offshore.xlsx")
wb <- read.xlsx("boot/initial/data/Area_NSCOD3_table_6a_inshore_offshore.xlsx") # added in initial, changed over to open xlsx as java dependency wouldn't work for XL Connect

dQ34 <- c(dQ3,dQ4)

dQ1 = NULL
dQ3 = NULL
dQ4 = NULL

dQ34 = subset(dQ34, !is.na(lon))

years=1990:2025 # updated years, ws 2023 last year
dQ34 = subset(dQ34, Year %in% years)

TAF::mkdir("data/plots/Q34")
for (yr in years){
  png(paste0("data/plots/Q34/",yr,".png"), width = 15, height = 15, units = "cm", res=600)
  plot(subset(dQ34, Year==yr), col=subset(dQ34, Year==yr)$Survey)
  dev.off()
}

# Check surveys and years
xtabs(~Year+Survey,data=dQ34[[2]])

# # Fill in missing StatRecs
# dQ34$StatRec <- as.character(dQ34$StatRec)
# sel <- which(is.na(dQ34$StatRec)) 
# dQ34$StatRec[sel] <- ices.rect2(dQ34$lon[sel], dQ34$lat[sel])
# dQ34$StatRec <- factor(dQ34$StatRec)

# # Add ICES areas
# ICESarea$ICESNAME <- ices.rect2(ices.rect(ICESarea$ICESNAME)$lon, ices.rect(ICESarea$ICESNAME)$lat)
# dQ34$StatRec <- ices.rect2(ices.rect(dQ34$StatRec)$lon, ices.rect(dQ34$StatRec)$lat)
# dQ34$ICESarea <- ICESarea$Area_27[match(dQ34$StatRec,ICESarea$ICESNAME)]

#Guard against exponent failed above after new computer and package reinstall and update 
ICESarea <- read.csv("boot/initial/data/ICESrect.csv",colClasses = "character")

# make coordinate columns numeric
ICESarea$stat_x <- as.numeric(ICESarea$stat_x)
ICESarea$stat_y <- as.numeric(ICESarea$stat_y)

# rebuild ICESNAME from coordinates, not from corrupted ICESNAME text
ICESarea$ICESNAME <- ices.rect2(ICESarea$stat_x, ICESarea$stat_y)

# fill missing StatRec from coordinates
sel <- which(is.na(dQ34$StatRec) & !is.na(dQ34$lon) & !is.na(dQ34$lat))
dQ34$StatRec[sel] <- ices.rect2(dQ34$lon[sel], dQ34$lat[sel])

# only standardise StatRec values that already look like valid ICES rectangles
valid <- grepl("^[0-9]{2}[A-Z][0-9]$", dQ34$StatRec)

dQ34$StatRec[valid] <- ices.rect2(ices.rect(dQ34$StatRec[valid])$lon,ices.rect(dQ34$StatRec[valid])$lat)

# now match to area
dQ34$ICESarea <- ICESarea$Area_27[match(dQ34$StatRec, ICESarea$ICESNAME)]

# Subset to stock definition
dQ34 <- subset(dQ34, ICESarea %in% c("4.a","4.b","4.c","6.a","3.a.20"))
plot(dQ34, col=dQ34$Survey)

# Add cod subareas
#areadef=XLConnect::readWorksheet(wb,sheet=1)
areadef <- read.xlsx("boot/initial/data/Area_NSCOD3_table_6a_inshore_offshore.xlsx", sheet = 1) # updated for open xlsx
areadef$SubArea <- NA
areadef$SubArea[areadef$areaPopulation=="Viking"] <- 1
areadef$SubArea[is.element(areadef$areaPopulation, c("Northwestern offshore"))] <- 2
areadef$SubArea[is.element(areadef$areaPopulation, c("Northwestern inshore"))] <- 3
areadef$SubArea[areadef$areaPopulation=="Southern"] <- 4
#areadef$SubArea[is.element(areadef$Statistical.Rectangle, c("40E4","40E5","39E4","39E5"))] <- "C"

dQ34$subArea <- areadef$SubArea[match(dQ34$StatRec, areadef$Statistical.Rectangle)]
plot(dQ34, col=dQ34$subArea)
points(subset(dQ34, is.na(subArea)), col="red")


# Take care of Scottish vessels - make it Ship+Survey
unique(subset(dQ34, Country=="GB-SCT")[[2]]$Ship)
xtabs(~Year+Ship, data=subset(dQ34, Ship %in% c("749S","748S"))[[2]])

dQ34$Ship = as.character(dQ34$Ship)
sel = c(grep("749S",dQ34$Ship), grep("748S",dQ34$Ship))
dQ34$Ship[sel] = paste(dQ34$Ship[sel],as.numeric(dQ34$Survey[sel]),sep=":")
dQ34$Ship = factor(dQ34$Ship)
xtabs(~Ship+Survey,dQ34[[2]])


# Check gears
xtabs(~Year+Gear, dQ34[[2]]) # Mix of gears in 1991 (Also 92-97)
dQ34 = subset(dQ34, !Year %in% 1990:1991) 

#Removal of new gears JTDS JTFS --Very Important thses are removed 
#dQ34[[2]] <- subset(dQ34[[2]], !Gear %in% c("JTDS", "JTFS")) # Removing JTDS & JTFS 
dQ34 <- subset(dQ34, Gear %in% c("GOV", "ABD")) # Removing JTDS & JTFS 

# Double Checking it is all removed and we should only hae GOV and ABD
table(dQ34$Gear)
table(dQ34$Year, dQ34$Gear) 
table(dQ34$Gear)
levels(dQ34$Gear)

# Check for enough age data
xtabs(NoAtALK~Year+Age,data=dQ34[[1]])
xtabs(NoAtALK~Gear+Age,data=dQ34[[1]])

## impute missing depths
dmodelQ34=gam(log(Depth) ~ s(lon,lat,k=200),data=dQ34[[2]])
selQ34=subset(dQ34,is.na(Depth))
selQ34$Depth=0; ## Guard against NA-error
dQ34$Depth[is.na(dQ34$Depth)]=exp(predict(dmodelQ34,newdata=selQ34[[2]]))

save(dQ34,file="data/Q34preprocessed.RData")

# load("data/Q34preprocessed.RData") # 2026 Assessment
# dQ34_2026<-dQ34
# load("data/Q34preprocessed_2025_Assessment.RData")
# dQ34_2025<-dQ34
# 
# # checking no of samples at age for the last 10 years from both datasets:
# library(dplyr)
# 
# years <- 2015:2025
# 
# samples_2026 <- dQ34_2026[[1]] %>%
#   mutate(
#     Year = as.numeric(as.character(Year)),
#     Age = as.numeric(as.character(Age))
#   ) %>%
#   filter(Year %in% years) %>%
#   group_by(Year, Age) %>%
#   summarise(
#     n_samples = n(),
#     .groups = "drop"
#   )
# 
# samples_2025 <- dQ34_2025[[1]] %>%
#   mutate(
#     Year = as.numeric(as.character(Year)),
#     Age = as.numeric(as.character(Age))
#   ) %>%
#   filter(Year %in% years) %>%
#   group_by(Year, Age) %>%
#   summarise(
#     n_samples = n(),
#     .groups = "drop"
#   )
# 
# samples_compare <- full_join(
#   samples_2025 %>% rename(n_samples_2025 = n_samples),
#   samples_2026 %>% rename(n_samples_2026 = n_samples),
#   by = c("Year", "Age")
# ) %>%
#   arrange(Year, Age)
# 
# View(samples_compare)
# 
# 
# library(dplyr)
# library(tidyr)
# 
# samples_2026_wide <- samples_2026 %>%
#   pivot_wider(
#     names_from = Age,
#     values_from = n_samples,
#     values_fill = 0
#   ) %>%
#   arrange(Year)
# 
# samples_2025_wide <- samples_2025 %>%
#   pivot_wider(
#     names_from = Age,
#     values_from = n_samples,
#     values_fill = 0
#   ) %>%
#   arrange(Year)
# View(samples_2026_wide)
# 
# samples_both_wide <- full_join(
#   samples_2025 %>%
#     rename(n_2025 = n_samples),
#   samples_2026 %>%
#     rename(n_2026 = n_samples),
#   by = c("Year", "Age")
# ) %>%
#   arrange(Year, Age)
# 
# samples_both_wide <- samples_both_wide %>%
#   pivot_wider(
#     names_from = Age,
#     values_from = c(n_2025, n_2026),
#     values_fill = 0
#   ) %>%
#   arrange(Year)
