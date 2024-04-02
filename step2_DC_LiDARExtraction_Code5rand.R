#this is the LiDAR extraction code for 5 randoms

library(raster)

# read in fake EWPW data
ewpw1 <- read.csv("./FakeEWPWPtsForDC.csv")
plot(ewpw1$Long, ewpw1$Lat)

# read in one LiDAR raster just to make sure it works
iqr <- raster("F:/LiDAR_21aug2022/FullLiDAR_maps/masked_&_focal/iqrfocalstats100.tif")
plot(iqr)

# convert fake EWPW Locations to spatial data
Locs <- SpatialPoints(coords = data.frame("x" = ewpw1$Long, "y" = ewpw1$Lat)) # create spatial object
crs(Locs) <- CRS("+init=epsg:4269") # define CRS as NAD83
Locs <- spTransform(Locs, crs(iqr)) # reproject to match LiDAR

plot(Locs, add = TRUE)

# read in LiDAR at 100m and extract at all 39,500 points
iqr_100m <- extract(x = raster("F:/LiDAR_21aug2022/FullLiDAR_maps/masked_&_focal/iqrfocalstats100.tif"), y = Locs)
p75_100m <- extract(raster("F:/LiDAR_21aug2022/FullLiDAR_maps/masked_&_focal/p75focalstats100.tif"), y = Locs)
p90_100m <- extract(raster("F:/LiDAR_21aug2022/FullLiDAR_maps/masked_&_focal/p90focalstats100.tif"), y = Locs)
perc5to1_100m <- extract(raster("F:/LiDAR_21aug2022/FullLiDAR_maps/masked_&_focal/perc5to1focalstats100.tif"), y = Locs)
percfirst5to1_100m <- extract(raster("F:/LiDAR_21aug2022/FullLiDAR_maps/masked_&_focal/percfirst5to1focalstats100.tif"), y = Locs)
TopRug30m_p95_100m <- extract(raster("F:/LiDAR_21aug2022/FullLiDAR_maps/masked_&_focal/TopRug30m_p95focalstats100.tif"), y = Locs)

# add extracted LiDAR values to Nicole's fake dataset
ewpw1$iqr_100 <- iqr_100m
ewpw1$p75_100 <- p75_100m
ewpw1$p90_100 <- p90_100m
ewpw1$perc5to1_100 <- perc5to1_100m
ewpw1$percfirst5to1_100 <- percfirst5to1_100m
ewpw1$rugosity_100 <- TopRug30m_p95_100m

# export
write.csv(ewpw1, "./FakeEWPWPtsForDCwCovs.csv", row.names = FALSE)
