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

for(i in 1:length(unique(ewpw$Bird.ID))){ # For each individual bird...
  # i = 10 #(e.g., bird # 10 = "51615")
  ewpw_i <- subset(ewpw, Bird.ID == unique(ewpw$Bird.ID)[i]) # subset the data to isolate bird "i"
  df_i <- data.frame() # blank dataframe to hold bird i's new data (used + 19 randoms per choice per bird)
  
  for(j in 1:nrow(ewpw_i)){ # For each GPS point/choice ID...
    # j = 5 #(e.g., bird i, choice #5)
    ewpw_i_CID_j <- ewpw_i[j,] # subset ewpw_i for the jth choice
    
    # ** Plot and buffer coordinate location EWPW_i_CID_j **
    ewpw_i_CID_j_sp <- SpatialPoints(coords = data.frame("x" = ewpw_i_CID_j$Longitude, "y" = ewpw_i_CID_j$Latitude)) # create spatial object
    crs(ewpw_i_CID_j_sp) <- CRS("+init=epsg:4326") # define CRS as WGS84
    plot(ewpw_i_CID_j_sp, axes = TRUE)
    ewpw_i_CID_j_sp <- spTransform(ewpw_i_CID_j_sp, CRS(albers)) # reproject to Albers (b/c gBuffer requires planar CRS)
    buff1 <- rgeos::gBuffer(ewpw_i_CID_j_sp, width = 200)
    plot(buff1, axes = TRUE); plot(ewpw_i_CID_j_sp, add = TRUE)
    
    # generate random points (requires st_sample() which can only be done using sf)
    buff2 <- st_as_sf(buff1) # convert to sf
    points1 = st_sample(buff2, size = 19) # generate 2 random points for each "used" point using st_sample()
    points1 <- as_Spatial(points1) # convert back to sp
    plot(buff1, lwd = 2); plot(points1, add = TRUE, col = "red", cex = 5, pch = "."); plot(ewpw_i_CID_j_sp, add = TRUE, col = "blue", cex = 10, pch = ".") # plot to confir pch = 3m it worked
    points2 <- spTransform(points1, CRS("+init=epsg:4326")) # convert to lat/long because this is what original data uses
    randomcoords <- data.frame(points2@coords) # extract lat/long from the points and convert to data.frame
    
    
    
    
    # buff_i_j <- ... save buffer as object
  }
  
}


###### for i in 1:unique(birdID) # For each individual bird...
###### ewpw_i <- subset( ewpw, birdid == unique [[i]])
###### df <- data.frame()
# 
###### for(j in 1:neow(ewpw_i)) # For each GPS Point...
###### ewpw_i_CID_j <- subset(ewpw_i, CID == j)
# ** Plot and buffer coordinate location EWPW_i_CID_j **
# buff_i_j <- ... save buffer as object
# df_i_j <- data.frame()
# for(k in 1:19) # For each random
# generate random coord in buffer
# add those coordinates to df_i_j
# }
# df1 <- rbind(df_i, df_i_j)
# }
# df <- rbind()
# }

