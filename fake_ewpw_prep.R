library(raster); library(sf); library(ggplot2)
install.packages("raster")
install.packages("sf")
install.packages("ggplot2")
ewpw <- read.csv("./EWPW_DCM_CID.csv")
head(ewpw)

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
BigDF <- data.frame("SID" = 0, "CID" = 0, "Alts" = 0, "Use" = 0, "Lat" = 0, "Long" = 0)

for(i in 1:length(unique(ewpw$Bird.ID))){
  ewpw_i <- subset(ewpw, Bird.ID == unique(ewpw$Bird.ID)[i])
  for(j in 1:nrow(ewpw_i)){
    ewpw_i_CID_j <- ewpw_i[j,]
    ewpw_i_CID_j_sp <- SpatialPoints(coords = data.frame("x" = ewpw_i_CID_j$Longitude, "y" = ewpw_i_CID_j$Latitude)) # create spatial object
    crs(ewpw_i_CID_j_sp) <- CRS("+init=epsg:4326")
    plot(ewpw_i_CID_j_sp, axes = TRUE)
    
    spTransform(ewpw_i_CID_j_sp, CRS("+init=epsg:4326"))
    buff1 <- buffer(ewpw_i_CID_j_sp, width=200)
    
    plot(buff1, axes = TRUE); plot(ewpw_i_CID_j_sp, add = TRUE)
    buff2 <- st_as_sf(buff1)
    points1 = st_sample(buff2, size = 19)
    points1 <- as_Spatial(points1)
    plot(buff1, lwd = 2); plot(points1, add = TRUE, col = "red", cex = 5, pch = "."); plot(ewpw_i_CID_j_sp, add = TRUE, col = "blue", cex = 10, pch = ".")
    points2 <- spTransform(points1, CRS("+init=epsg:4326"))
    randomcoords <- data.frame(points2@coords)
    names(randomcoords) <- c("long", "lat")
    choice_j_data <- data.frame("SID" = rep(i, 20),
                                "CID" = rep(j, 20),
                                "Alts" = c(1:20),
                                "Use" = c(rep(0,19),1),
                                "Lat" = c(randomcoords$long, ewpw_i_CID_j$Longitude),
                                "Long" = c(randomcoords$lat, ewpw_i_CID_j$Latitude))
    print(paste0("bird ", i, " choice ", j, " done!"))
    BigDF <- rbind(BigDF, choice_j_data)
  }
}
