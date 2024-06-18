library(ggplot2)
library(cowplot)
library(dplyr)
library(ggpubr)
library(lubridate)
library(openxlsx)

WD <- "~/TU_Dresden/Masterarbeit/Daten/Daten_MA/" # Working Directory

# Data----

Pts <- read.csv(paste0(WD, "Eff_Precipitation_SnowModel.csv"), sep=";", skip=1) # Precipitation time series with semicolon as separator
SMts <- read.csv(paste0(WD, "Soil_moisture_2_EZG.csv"), sep=";", nrows = 2869) # Soil moisture time series # Data structure: 1st column Date, 2nd column SM data
SMts <- SMts[, c(2, 4)]

Pts$Date <- as.POSIXct(Pts$Date, format = "%d.%m.%Y")
SMts$Date <- as.POSIXct(SMts$Date, format = "%d.%m.%Y")

# Data preparation----
mindate <- min(SMts$Date) 
maxdate <- max(SMts$Date)

# Setting Pts dataset to the same time period as SMts
Pts <- subset(Pts, Date >= mindate & Date <= maxdate)             

# Model application to the soil moisture time series----
B = 1 # seq(0.1, 1, by = 0.1) # shape parameter [-]
Ks = 4.2 # Hydraulic conductivity [mm/d] # This Ks value must be adjusted in 3 other places in the file

SM_min_G <- min(SMts$SM_Prozent, na.rm = TRUE) # SM min value 
SM_max_G <- max(SMts$SM_Prozent, na.rm = TRUE) # SM max value

SMts$FluSim <- NA # Recharge sim in column X ("FluSim")

mylist <- list(B, Ks) # To create a list

for (i_B in 1:length(B)) {
  
  for (i in 1:nrow(SMts)) {
    SMts[i,3] <- Ks * ((SMts[i,2] - SM_min_G) / (SM_max_G - SM_min_G))^((2 + 3 * B[i_B]) / B[i_B]) # FluSM equation
    SMts2 <- SMts # [,c(2,3,4)]
  }
  # name <- paste(B[i_B], Ks[j_ks], sep=',')
  name <- paste(i_B, Ks, sep=',')
  mylist[[name]] <- (SMts2)
  
}

mylist <- mylist[-c(1,2)]
DF <- as.data.frame(mylist)
DF <- select(DF, X1.4.2.Date, X1.4.2.SM_Prozent, X1.4.2.FluSim) # ks must be adjusted to the defined Ks value

DF$Recharge_mean <- DF[,3]

names(DF)[names(DF)=="X1.4.2.Date"] <- "Date" # ks must be adjusted to the defined Ks value
names(DF)[names(DF)=="X1.4.2.SM_Prozent"] <- "SoMo.ml_daily_data" # ks must be adjusted to the defined Ks value

# Recharge and Recharge rate 
DF$Rtot = DF$Recharge_mean # Do not divide by 24 here, since daily data is already used

# Data daily because P data daily, to compute the recharge rate in % 
Recharge_daily <- DF[, c(1,4,5)]
Recharge_daily$Date <- as.Date(Recharge_daily$Date, format="%Y-%m-%d") 

names(Recharge_daily )[names(Recharge_daily) =="Date"] <- "date"
Pts$date <-  as.Date(Pts$Date, format="%Y-%m-%d")
Recharge <- merge(Recharge_daily, Pts, by = "date")
Recharge <- subset(Recharge, select = -date)
Recharge$RRate <- (Recharge$Rtot / Recharge$daily_eff_prec..mm.) * 100
Recharge$RRate[Recharge$RRate == 'Inf'] <- '0'
Recharge$RRate <- as.numeric(as.character(Recharge$RRate))

# Recharge Rate per Year 
Recharge_Year <- data.frame(Year=character(0), "Recharge_mm/a"=numeric(0), "Precipitation_mm/a"=numeric(0), RRate=numeric(0))

Recharge_Year[1,1] <- "2010-2011"
sub <- subset(Recharge, Date > ymd("2010-10-01") & Date < ymd("2011-09-30"))
Recharge_Year[1,2] <- sum(sub$Rtot, na.rm = TRUE)
Recharge_Year[1,3] <- sum(sub$daily_eff_prec..mm., na.rm = TRUE)

Recharge_Year[2,1] <- "2011-2012"
sub <- subset(Recharge, Date > ymd("2011-10-01") & Date < ymd("2012-09-30"))
Recharge_Year[2,2] <- sum(sub$Rtot, na.rm = T)
Recharge_Year[2,3] <- sum(sub$daily_eff_prec..mm., na.rm = T)

Recharge_Year[3,1] <- "2012-2013"
sub <- subset(Recharge, Date > ymd("2012-10-01") & Date < ymd("2013-09-30"))
Recharge_Year[3,2] <- sum(sub$Rtot, na.rm = T)
Recharge_Year[3,3] <- sum(sub$daily_eff_prec..mm., na.rm = T)

Recharge_Year[4,1] <- "2013-2014"
sub <- subset(Recharge, Date > ymd("2013-10-01") & Date < ymd("2014-09-30"))
Recharge_Year[4,2] <- sum(sub$Rtot, na.rm = T)
Recharge_Year[4,3] <- sum(sub$daily_eff_prec..mm., na.rm = T)

Recharge_Year[5,1] <- "2014-2015"
sub <- subset(Recharge, Date > ymd("2014-10-01") & Date < ymd("2015-09-30"))
Recharge_Year[5,2] <- sum(sub$Rtot, na.rm = T)
Recharge_Year[5,3] <- sum(sub$daily_eff_prec..mm., na.rm = T)

Recharge_Year[6,1] <- "2015-2016"
sub <- subset(Recharge, Date > ymd("2015-10-01") & Date < ymd("2016-09-30"))
Recharge_Year[6,2] <- sum(sub$Rtot, na.rm = T)
Recharge_Year[6,3] <- sum(sub$daily_eff_prec..mm., na.rm = T)

Recharge_Year[7,1] <- "2016-2017"
sub <- subset(Recharge, Date > ymd("2016-10-01") & Date < ymd("2017-09-30"))
Recharge_Year[7,2] <- sum(sub$Rtot, na.rm = T)
Recharge_Year[7,3] <- sum(sub$daily_eff_prec..mm., na.rm = T)
rm(sub)
Recharge_Year$RRate <- (Recharge_Year$Recharge / Recharge_Year$Precipitation) * 100

selected_columns <- DF[, c(1, 2, 4)]

write.xlsx(selected_columns, paste0(WD, "final/GWR_SoMo_ED_1_4.2.xlsx"))
