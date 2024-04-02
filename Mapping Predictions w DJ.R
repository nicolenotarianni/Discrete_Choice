Toprug_focal_stats<-raster("C:/Users/User/Desktop/dc2/TopRug30m_p95.tif")
p90_focal_stats<-raster("C:/Users/User/Desktop/dc2/p90.tif")
percfirst_focal_stats<-raster("C:/Users/User/Desktop/dc2/Perc_First_5m_to_1m.tif")

library(sf)
library(tidyverse)
library(nngeo)
library(raster)
library(terra)
crs(Toprug_focal_stats)
crs(p90_focal_stats)
crs(percfirst_focal_stats)

ncell(Toprug_focal_stats)
ncell(p90_focal_stats)
ncell(percfirst_focal_stats)


trvals = values(Toprug_focal_stats)
pvals = values(p90_focal_stats)
fvals = values(percfirst_focal_stats)
