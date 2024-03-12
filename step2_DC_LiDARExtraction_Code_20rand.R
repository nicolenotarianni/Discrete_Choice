#this is the LiDAR extraction code for 20 randoms
library(raster)

# read in fake EWPW data
ewpw1 <- read.csv("./FakeEWPWPtsForDCw20rand.csv")
plot(ewpw1$Long, ewpw1$Lat)



# convert fake EWPW Locations to spatial data
Locs <- SpatialPoints(coords = data.frame("x" = ewpw1$Long, "y" = ewpw1$Lat)) # create spatial object
crs(Locs) <- CRS("+init=epsg:4269") # define CRS as NAD83
Locs <- spTransform(Locs, crs(iqr)) # reproject to match LiDAR

plot(Locs, add = TRUE)

# read in LiDAR at 100m and extract at all 39,500 points
iqr_100m <- extract(x = raster("C:/Users/User/Desktop/dc2/IQR.tif"), y = Locs)
p75_100m <- extract(raster("C:/Users/User/Desktop/dc2/p75.tif"), y = Locs)
p90_100m <- extract(raster("C:/Users/User/Desktop/dc2/p90.tif"), y = Locs)
perc5to1_100m <- extract(raster("C:/Users/User/Desktop/dc2/Perc_5m_to_1m.tif"), y = Locs)
percfirst5to1_100m <- extract(raster("C:/Users/User/Desktop/dc2/Perc_First_5m_to_1m.tif"), y = Locs)
TopRug30m_p95_100m <- extract(raster("C:/Users/User/Desktop/dc2/TopRug30m_p95.tif"), y = Locs)

# add extracted LiDAR values to Nicole's fake dataset
ewpw1$iqr_100 <- iqr_100m
ewpw1$p75_100 <- p75_100m
ewpw1$p90_100 <- p90_100m
ewpw1$perc5to1_100 <- perc5to1_100m
ewpw1$percfirst5to1_100 <- percfirst5to1_100m
ewpw1$rugosity_100 <- TopRug30m_p95_100m

# export
write.csv(ewpw1, "./FakeEWPWPtsForDCwCovs20rand.csv", row.names = FALSE)
