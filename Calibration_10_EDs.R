library(ggplot2)
library(dplyr)
library(reshape)
library(tidyverse)
library(officer)
library(rvg)
library(ggnewscale)
library(metR)

####### DATA SoMo.ml
WD <- "~/TU_Dresden/Masterarbeit/Daten/Daten_MA/"


### TS (Time series) is the SoMo.ml time series ##

# Loading the time series, soil moisture data, and measured GWN (runoff) for calibration
# SoMo.ml data
SoMo_ml_GWN_TS <- read.csv(paste0(WD, "Soil_moisture_2_EZG.csv"), sep = ";", nrows = 3286) # Here SOMo.ml Data
SoMo_ml_GWN_TS <- SoMo_ml_GWN_TS[, c(1, 2, 4, 7)] # 6 -> 7 if delay in the GWR of runoff #1,2,4,5,6 are X,Date,SM%,Runoff m³/s, Runoff mm/d

##### DATA BETHELIN
## Read input data for SM and GWR from Lauter Runoff
# WD <- "~/TU_Dresden/Masterarbeit/Daten/Daten_MA/GWR_Statistik/"

## TS is the SoMo.ml time series ##
# SoMo_ml_GWN_TS <- read.csv(paste0(WD, "Berthelin_Daten_Kalibrierung.csv"), sep = ";", nrows = 2870)
# SoMo_ml_GWN_TS <- SoMo_ml_GWN_TS[, c(1, 2, 4, 6)]


# Function to put the graphs in pptx
create_pptx <- function(plt = last_plot(), path = file.choose()){
  if(!file.exists(path)) {
    out <- read_pptx()
  } else {
    out <- read_pptx(path)
  }
  
  out %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = dml(ggobj = plt), location = ph_location_fullsize()) %>%
    print(target = path)
}


# Function to simulate recharge and calibrate Ks and B using Euclidean Distance (ED) as performance criteria
funFluSim <- function(TITRE, ts) {
  SM_min_G <- min(ts[, 3], na.rm = TRUE)
  SM_max_G <- max(ts[, 3], na.rm = TRUE)
  
  B <- seq(0.01, 1, by = 0.01) # Currently 100 x 400   # shape parameter [-]
  Ks <- seq(0.1, 40, by = 0.1) # saturated hydraulic conductivity [mm/d]
  
  ts$FluSim <- NA
  r <- matrix(NA, nrow = length(B), ncol = length(Ks))
  b <- matrix(NA, nrow = length(B), ncol = length(Ks))
  a <- matrix(NA, nrow = length(B), ncol = length(Ks))
  
  for (i_B in 1:length(B)) {
    for (j_ks in 1:length(Ks)) {
      B_val <- B[i_B]
      Ks_val <- Ks[j_ks]
      
      for (i in 1:nrow(ts)) {
        ts[i, 5] <- Ks_val * ((ts[i, 3] - SM_min_G) / (SM_max_G - SM_min_G))^((2 + 3 * B_val) / B_val)
      }
      
      r[i_B, j_ks] <- cor(ts[, 5], ts[, 4], use = "complete.obs")
      a[i_B, j_ks] <- mean(ts[, 5], na.rm = TRUE) / mean(ts[, 4], na.rm = TRUE)
      b[i_B, j_ks] <- sd(ts[, 5], na.rm = TRUE) / sd(ts[, 4], na.rm = TRUE)
    }
  }
  
  # Calculation of Euclidean Distance (ED) with weighting (x*(a-1²))
  ED <- sqrt((1*(r - 1))^2 + (2*(a - 1))^2 + (0*(b - 1))^2) # r = Pearson correlation coefficient (r²); a = Bias ratio; b = Variability similarity
  colnames(ED) <- Ks
  rownames(ED) <- B
  
  best <- min(ED)
  coordinates <- which(ED == best, arr.ind = TRUE)
  bestKs <- Ks[coordinates[2]]
  bestB <- B[coordinates[1]]
  best <- round(best, digits = 5)
  
  ts$GraphfluObs <- ts$Abfluss_mm_tag
  ts$GraphfluSim <- bestKs * ((ts[, 3] - SM_min_G) / (SM_max_G - SM_min_G))^((2 + 3 * bestB) / bestB) # [,3] is SM
  
  graphED <- melt(ED)
  
  min_ed <- min(ED)
  max_ed <- max(ED)
  
  q <- quantile(ED, 0.1)
  ED10 <- ED
  ED10[ED10 > q] <- NA
  ED10 <- melt(ED10)
  ED10 <- na.omit(ED10)
  
  plotED <- ggplot(data = graphED, aes(x = X2, y = X1)) +
    geom_raster(aes(fill = value), interpolate = TRUE) +
    scale_fill_distiller(palette = "GnBu", direction = -1, limits = c(0, 20)) +
    stat_subset(aes(subset = value < q), geom = "point", size = 0.3, color = "pink") +
    annotate("point", x = bestKs, y = bestB, colour = "red", size = 6) +
    annotate("text", x = 40, y = 0.8, label = paste("ED =", best, "Ks =", bestKs, "B =", bestB), hjust = 0) +
    labs(fill = "Euclidean Distance") +
    scale_x_continuous("Ks (mm/d)", limits = c(0, 40)) +
    scale_y_continuous("B (-)", limits = c(0, 1)) +
    ggtitle(TITRE) +
    theme_bw() +
    theme(axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          title = element_text(size = 15),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 12))
  
  create_pptx(plt = plotED, path = paste0("~/TU_Dresden/Masterarbeit/Grafiken/Kalibrierung/SoMo_Kalibrierung_ED.pptx"))
}


TITRE <- "GWR SoMo.ml with performance measure ED without b and weighting a = 5"
ts <- SoMo_ml_GWN_TS
funFluSim(TITRE, ts)


### EXTENSION FOR MIN, MAX, MEAN DETERMINATION BASED ON RECHARGE SUM

# Function to calculate groundwater recharge for each combination of B and Ks in ED10
funFluSimExtended <- function(ED10, ts) {
  # Initialize the matrix to store the total sums
  GWN_sum <- matrix(NA, nrow = nrow(ED10), ncol = 3)
  colnames(GWN_sum) <- c("B", "Ks", "GWN_Sum")
  
  # Loop over each row in ED10
  for (i in 1:nrow(ED10)) {
    B_val <- ED10[i, "X1"]
    Ks_val <- ED10[i, "X2"]
    
    # Calculate the groundwater recharge for the current combination of B and Ks
    ts$FluSim <- Ks_val * ((ts[, 3] - SM_min_G) / (SM_max_G - SM_min_G))^((2 + 3 * B_val) / B_val)
    
    # Store the total sum of groundwater recharge in the matrix
    GWN_sum[i, "B"] <- B_val
    GWN_sum[i, "Ks"] <- Ks_val
    GWN_sum[i, "GWN_Sum"] <- sum(ts$FluSim, na.rm = TRUE)
  }
  
  # Determine the lowest, highest, and mean sum
  min_sum <- min(GWN_sum[, "GWN_Sum"])
  max_sum <- max(GWN_sum[, "GWN_Sum"])
  mean_sum <- mean(GWN_sum[, "GWN_Sum"])
  
  # Assign the B and Ks values to the sums
  min_B_Ks <- GWN_sum[GWN_sum[, "GWN_Sum"] == min_sum, c("B", "Ks")]
  max_B_Ks <- GWN_sum[GWN_sum[, "GWN_Sum"] == max_sum, c("B", "Ks")]
  # Find the B and Ks values that are closest to the mean sum
  closest_index <- which.min(abs(GWN_sum[, "GWN_Sum"] - mean_sum))
  mean_B_Ks <- GWN_sum[closest_index, c("B", "Ks")]
  
  list(GWN_sum = GWN_sum, min_sum = min_sum, max_sum = max_sum, mean_sum = mean_sum,
       min_B_Ks = min_B_Ks, max_B_Ks = max_B_Ks, mean_B_Ks = mean_B_Ks)
}

# Application of the extended function
result <- funFluSimExtended(ED10, ts)
