library(sf); library(tidyverse); library(nngeo); library(raster); library(terra)

# read in lidar rasters
Toprug_focal_stats<-raster("C:/Users/User/Desktop/dc2/TopRug30m_p95.tif")
p90_focal_stats<-raster("C:/Users/User/Desktop/dc2/p90.tif")
percfirst_focal_stats<-raster("C:/Users/User/Desktop/dc2/Perc_First_5m_to_1m.tif")

# confirm that CRSs are all the same
crs(Toprug_focal_stats)
crs(p90_focal_stats)
crs(percfirst_focal_stats)

# read in shapefile of PA
pa <- shapefile("./shapefiles/pa_counties.shp")
studyarea <- subset(pa, NAME == "Centre" | NAME == "Clinton")
plot(studyarea)
studyarea2 <- spTransform(studyarea, crs(Toprug_focal_stats))

# crop and mask lidar rasters
TR_crop <- raster::crop(x = Toprug_focal_stats, y = studyarea2) # crop for top rugosity
plot(TR_crop); plot(studyarea2, add = TRUE, border = "white") # confirm it worked

ncell(TR_crop) #89046360 cells #check number of cells to confirm theyre the same
ncell(P_crop) #89046360  cells #check number of cells to confirm theyre the same

P_crop <- raster::crop(x = p90_focal_stats, y = studyarea2) #crop for p90
First_crop <- raster::crop(x = percfirst_focal_stats, y = studyarea2) #crop for percfirst

P_cropvals = values(P_crop) 
First_cropvals = values(First_crop)
TR_cropvals = values(TR_crop)
# NOW REPEAT FOR OTHER TWO RASTERS ALSO!

#### Also run the stuff below here!

# confirm that ncells are all the same
ncell(Toprug_focal_stats)
ncell(p90_focal_stats)
ncell(percfirst_focal_stats)

trvals = values(Toprug_focal_stats)
pvals = values(p90_focal_stats)
fvals = values(percfirst_focal_stats)
