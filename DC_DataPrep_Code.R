ewpw <- read.csv("./EWPW_DCM_CID.csv")
head(ewpw)
# Bird.ID is the individual bird
# CID is the individual bird
# We also need "alts" which will be 1-20

for(i in 1:length(unique(ewpw$Bird.ID))){ # For each individual bird...
  # i = 10
  ewpw_i <- subset(ewpw, Bird.ID == unique(ewpw$Bird.ID)[i])
  df_i <- data.frame() # blank dataframe to hold bird i's data
  
  for(j in 1:nrow(ewpw_i)){ # For each GPS point/choice ID...
    # j = 1
    ewpw_i_CID_j <- ewpw_i[j,] # subset ewpw_i for the jth choice
    # ** Plot and buffer coordinate location EWPW_i_CID_j **
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

