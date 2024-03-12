#this is the prep code for 5 randoms

library(raster); library(rgeos); library(sf)

ewpw <- read.csv("./EWPW_DCM_CID.csv")
head(ewpw)
# Bird.ID is the individual bird
# CID is the individual bird
# We also need "alts" which will be 1-20

# SID  CID  Alts  Use (SID = individual animal, CID = choice ID, Alts = alternate #, use = 1/0)
# 1    1    1     0
# 1    1    2     0
# .    .    .     .
# 1    1    19    0
# 1    1    20    1 # this concludes first choice (CID) for animal #1
# 1    2    1     0
# 1    2    2     0
# .    .    .     .
# 1    2    19    0
# 1    2    20    1 # this concludes second choice (CID) for animal #2
# Etc., for all choices and all birds

# define Albers Equal Area Conic projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# blank data.frame to hold all data
BigDF <- data.frame("SID" = 0, "CID" = 0, "Alts" = 0, "Use" = 0, "Lat" = 0, "Long" = 0)

for(i in 1:length(unique(ewpw$Bird.ID))){ # For each individual bird...
  # i = 2 #(e.g., bird # 10 = "51615")
  ewpw_i <- subset(ewpw, Bird.ID == unique(ewpw$Bird.ID)[i]) # subset the data to isolate bird "i"
  
  for(j in 1:nrow(ewpw_i)){ # For each GPS point/choice ID...
    # j = 1 #(e.g., bird i, choice #5)
    ewpw_i_CID_j <- ewpw_i[j,] # subset ewpw_i for the jth choice
    
    # Plot and buffer coordinate location EWPW_i_CID_j
    ewpw_i_CID_j_sp <- SpatialPoints(coords = data.frame("x" = ewpw_i_CID_j$Longitude, "y" = ewpw_i_CID_j$Latitude)) # create spatial object
    crs(ewpw_i_CID_j_sp) <- CRS("+init=epsg:4326") # define CRS as WGS84
    #plot(ewpw_i_CID_j_sp, axes = TRUE)
    ewpw_i_CID_j_sp <- spTransform(ewpw_i_CID_j_sp, CRS(albers)) # reproject to Albers (b/c gBuffer requires planar CRS)
    buff1 <- rgeos::gBuffer(ewpw_i_CID_j_sp, width = 200)
    #plot(buff1, axes = TRUE); plot(ewpw_i_CID_j_sp, add = TRUE)
    
    # generate random points (requires st_sample() which can only be done using sf)
    buff2 <- st_as_sf(buff1) # convert to sf
    points1 = st_sample(buff2, size = 4) # generate 2 random points for each "used" point using st_sample()
    points1 <- as_Spatial(points1) # convert back to sp
    #plot(buff1, lwd = 2); plot(points1, add = TRUE, col = "red", cex = 5, pch = "."); plot(ewpw_i_CID_j_sp, add = TRUE, col = "blue", cex = 10, pch = ".") # plot to confir pch = 3m it worked
    points2 <- spTransform(points1, CRS("+init=epsg:4326")) # convert to lat/long because this is what original data uses
    randomcoords <- data.frame(points2@coords) # extract lat/long from the points and convert to data.frame
    names(randomcoords) <- c("long", "lat") # rename headers
    
    # assemble the data.frame chunk for bird i, choice j
    choice_j_data <- data.frame("SID" = rep(i, 5),
                                "CID" = rep(max(BigDF$CID)+1, 5),
                                "Alts" = c(1:5),
                                "Use" = c(rep(0,4),1),
                                "Lat" = c(randomcoords$lat, ewpw_i_CID_j$Latitude),
                                "Long" = c(randomcoords$long, ewpw_i_CID_j$Longitude))
    #print(paste0("bird ", i, " choice ", j, " done!"))
    BigDF <- rbind(BigDF, choice_j_data) # add new chunk to big data.frame
    
    compl <- round(i/length(unique(ewpw$Bird.ID))*30, 0)
    cat(paste0("\r [", strrep("🌞", compl), strrep("🌚", 30-compl), "] ", round(100*i/length(unique(ewpw$Bird.ID)),0)), "% done")
  }
}
nrow(BigDF)
BigDF <- BigDF[2:nrow(BigDF),]
write.csv(BigDF, "./FakeEWPWPtsForDC.csv", row.names = FALSE)
