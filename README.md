# Spatiotemporal-Cod 🐟

This repository contains the R code used to implement the analysis and develop the figures included in the manuscript: 

> "Gonzalez, G.M., Wiff, R., Marshall C.T., Cornulier, T. 2021. Estimating spatio-temporal distribution of fish and gear selectivity functions from pooled scientific survey and commercial fishing data"  

As a guide: 
* Files starting with "M", includes scripts related with the Model implementation.
* Files starting with "F", includes scripts containing the development of the Figures incldued in the manuscript.
* Files starting with "S" includes scripts containing the work included in the Supplementary materials.

Several of the scripts are still under development and will be included shortly.

## Complementary figures

### Animated pairwise plots of gear spatio-temporal overlaps

Calculated spatio-temporal volumes around the data for four pairs of gears, including the IBTS survey (“GOV”). Hypervolumes are materialized by random points within each hypervolume (raw data not shown). 

\\

The 3 axes are: 
* scaled latitude (“ShootLat.sc”); 
* scaled longitude (“ShootLon.sc”) 
* scaled time of year (“J.day.sc”).

#### MTH vs NTR
![MTH vs NTR](/GIFplots/GearVolumes_NTR_vs_MTH.gif)

#### MTN vs NTR
![MTN vs NTR](/GIFplots/GearVolumes_NTR_vs_MTN.gif)

#### MTH vs MTN
![MTH vs MTN](/GIFplots/GearVolumes_MTN_vs_MTH.gif)

#### NTR vs GOV (IBTS)
![NTR vs GOV](/GIFplots/GearVolumes_GOV_vs_NTR.gif)

#### MTR vs SEN
![MTR vs SEN](/GIFplots/GearVolumes_MTR_vs_SEN.gif)

