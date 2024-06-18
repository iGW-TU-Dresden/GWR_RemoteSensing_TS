## The four R scripts are part of the master's thesis by Jonas Hube on the topic:
"Abschätzung der Grundwasserneubildung in dem Gebiet der Schwäbischen Alb unter Verwendung von Fernerkundungsdaten"
(Estimation of Groundwater Recharge in the Region of the Swabian Alb Using Remote Sensing Data)

A more detailed explanation of the background can be found in this thesis. Below is a brief overview of the scripts:

## SoMo.ml_data_prep.R:
This script is used to convert the .nc format of the remote sensing data into a .csv format. 
The global raster data is clipped to the catchment area, which is imported as a shapefile.

## SM_Comparison.R:
This script compares two soil moisture time series and evaluates them statistically.

## GWR_Model.R:
Here, the soil moisture time series are converted into groundwater recharge time series using the FluSM function.

## Calibration_10_EDs.R:
This script calibrates the GWR_Model using the calibration parameters Ks and B. 
For this, observed groundwater recharge data and a performance metric are required to assess the adjustment of the groundwater recharge. 
In this work, the weighted Euclidean Distance was used.