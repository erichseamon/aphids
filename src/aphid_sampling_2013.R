library(plyr)

finalmatrix <- data.frame()
file <- c(1,2,11,12,13,14,15,16,17,22,23,24,25,31,32,33,34,35,36,38,39,51,53,54,55,56,57,58,59,60,61,62,63,64,65,66) #--number of aphid observations for 2011
for (l in file) {

  l <- paste("2013_", l, sep="")
  


#--sets the workspace
#setwd("/nethome/erichs/Aphid 2013 csv results")

  
  #--reads in the csv file as a data frame
  x <- read.delim2("data/sweepnet_2013_final_WW_feb2021.txt", sep=",", header=TRUE)
  
  x$ToBufDist <- as.numeric(as.character(x$ToBufDist))
  #x <- x[-which(x$Identifier== " "), ]
  #x <- x[-which(x$ToBufDist== 0), ]
  #x <- x[-which(x$Identifier== ""), ]
  #x <- x[-which(x$GRIDCODE== "0"), ]
  
  #subset based on observation number
  x <- x[which(x$Identifier==l),]


obs <- unique(x[6])
ring <- as.data.frame(unique(x[13]))
ring <- as.numeric(as.character(ring$ToBufDist))
ring <- t(ring)
ring <- t(ring)
croptype <- data.matrix(unique(x[17]))
colnames(ring) <- 'ring'

#--creates a variable for the total number of observations, ring types, and croptypes

obs_total <- nrow(obs)
ring_total <- nrow(ring)
croptype_total <- nrow(croptype)

#--creates a NA matrix for prepopulation in a loop

obs_matrix <- matrix(data=NA,nrow=ring_total, ncol=croptype_total)
colnames(obs_matrix) <- croptype

croptypespan <- c(1:croptype_total)
ringspan <- c(1:ring_total)

#--loops that looks at each observation, and then iterates 
#--thru ringbycroptype (387 different combinations of croptype and ring distance.  
#--Then take that all those vectors and append
#--as columns

x$ToBufDist <- as.numeric(as.character(x$ToBufDist))
x$F_AREA <- as.numeric(as.character(x$F_AREA))

 for (j in ring) {
  for (k in croptype) {
        L <- subset(x, ToBufDist == j & GRIDCODE == k )
           agg <- matrix(c(sum(L$F_AREA), k, j))
           jj <- which(croptype[,1] == k, arr.ind=TRUE)
           kk <- which(ring[,1] == j, arr.ind=TRUE)
           ringer <- subset(x, ToBufDist == j)
           ringareatotal <- sum(ringer$F_AREA)
           obs_matrix[kk,jj] <- agg[1,1] / ringareatotal 
           #obs_matrix <- cbind(ring, obs_matrix)
    }
  }
idlist <- c(l,l,l,l,l,l,l,l)
idlist <- t(idlist)
idlist <- t(idlist)
colnames(idlist) <- "SiteID"

assign(paste("obs_matrix", "_", l, sep = ""), cbind(idlist, obs_matrix))
}


#obs_matrixlist <- list()
#for (i in file) {

#  data.frame(get(paste("obs_matrix", "_", i, sep = "")))
  
#  obs_matrix_final <- data.frame(get(paste("obs_matrix", "_", i, sep = "")))
#  finalmatrix <- rbind.fill(finalmatrix, obs_matrix_final)
#  tmp <- list(obs_matrix_final)
#  name <- paste('site',i,sep='')
#  obs_matrixlist[[name]] <- tmp
  
#}

#Fixing duplicate geometry that got merged earlier.  
# 
# obs_matrix_2013_3 <- obs_matrix_2013_2
# obs_matrix_2013_3[,1] = "2013_3"
# 
# obs_matrix_2013_4 <- obs_matrix_2013_2
# obs_matrix_2013_4[,1] = "2013_4"
# obs_matrix_2013_5 <- obs_matrix_2013_2
# obs_matrix_2013_5[,1] = "2013_5"
# obs_matrix_2013_6 <- obs_matrix_2013_2
# obs_matrix_2013_6[,1] = "2013_6"
# 
# obs_matrix_2013_7 <- obs_matrix_2013_2
# obs_matrix_2013_7[,1] = "2013_7"
# obs_matrix_2013_8 <- obs_matrix_2013_2
# obs_matrix_2013_8[,1] = "2013_8"
# obs_matrix_2013_9 <- obs_matrix_2013_2
# obs_matrix_2013_9[,1] = "2013_9"
# obs_matrix_2013_10 <- obs_matrix_2013_2
# obs_matrix_2013_10[,1] = "2013_10"
# 
# obs_matrix_2013_19 <- obs_matrix_2013_18
# obs_matrix_2013_19[,1] = "2013_19"
# 
# obs_matrix_2013_21 <- obs_matrix_2013_20
# obs_matrix_2013_21[,1] = "2013_21"
# 
# obs_matrix_2013_26 <- obs_matrix_2013_25
# obs_matrix_2013_26[,1] = "2013_26"
# obs_matrix_2013_27 <- obs_matrix_2013_25
# obs_matrix_2013_27[,1] = "2013_27"
# obs_matrix_2013_28 <- obs_matrix_2013_25
# obs_matrix_2013_28[,1] = "2013_28"
# obs_matrix_2013_29 <- obs_matrix_2013_25
# obs_matrix_2013_29[,1] = "2013_29"
# obs_matrix_2013_30 <- obs_matrix_2013_25
# obs_matrix_2013_30[,1] ="2013_30"
# 
# obs_matrix_2013_37 <- obs_matrix_2013_36
# obs_matrix_2013_37[,1] = "2013_37"
# 
# obs_matrix_2013_40 <- obs_matrix_2013_39
# obs_matrix_2013_40[,1] = "2013_40"
# obs_matrix_2013_41 <- obs_matrix_2013_39
# obs_matrix_2013_41[,1] = "2013_41"
# obs_matrix_2013_42 <- obs_matrix_2013_39
# obs_matrix_2013_42[,1] = "2013_42"
# obs_matrix_2013_43 <- obs_matrix_2013_39
# obs_matrix_2013_43[,1] = "2013_43"
# obs_matrix_2013_44 <- obs_matrix_2013_39
# obs_matrix_2013_44[,1] = "2013_44"
# obs_matrix_2013_45 <- obs_matrix_2013_39
# obs_matrix_2013_45[,1] = "2013_45"
# obs_matrix_2013_46 <- obs_matrix_2013_39
# obs_matrix_2013_46[,1] = "2013_46"
# obs_matrix_2013_47 <- obs_matrix_2013_39
# obs_matrix_2013_47[,1] = "2013_47"
# obs_matrix_2013_48 <- obs_matrix_2013_39
# obs_matrix_2013_48[,1] = "2013_48"
# obs_matrix_2013_49 <- obs_matrix_2013_39
# obs_matrix_2013_49[,1] = "2013_49"
# obs_matrix_2013_50 <- obs_matrix_2013_39
# obs_matrix_2013_50[,1] = "2013_50"
# 
# obs_matrix_2013_52 <- obs_matrix_2013_51
# obs_matrix_2013_52[,1] = "2013_52"
# 
# obs_matrix_2013_65 <- obs_matrix_2013_64
# obs_matrix_2013_65[,1] = "2013_65"



final <- rbind.fill.matrix(obs_matrix_2013_1, obs_matrix_2013_2,obs_matrix_2013_11,obs_matrix_2013_12, obs_matrix_2013_13,obs_matrix_2013_14,
                           obs_matrix_2013_15, obs_matrix_2013_16,obs_matrix_2013_17,obs_matrix_2013_22,obs_matrix_2013_23,obs_matrix_2013_24, 
                           obs_matrix_2013_25,obs_matrix_2013_31,obs_matrix_2013_32,obs_matrix_2013_33, obs_matrix_2013_34,
                           obs_matrix_2013_35,obs_matrix_2013_36,obs_matrix_2013_38,obs_matrix_2013_39, 
                           obs_matrix_2013_51, obs_matrix_2013_53,obs_matrix_2013_54, 
                           obs_matrix_2013_55,obs_matrix_2013_56,obs_matrix_2013_57, obs_matrix_2013_58,obs_matrix_2013_59,
                           obs_matrix_2013_60, obs_matrix_2013_61,obs_matrix_2013_62,obs_matrix_2013_63, obs_matrix_2013_64,
                           obs_matrix_2013_65,obs_matrix_2013_66, fill=TRUE)

final <- final[1:280,]

ringfinal <- ring

#filefor <- c(1:66)
filefor <- c(1,2,11,12,13,14,15,16,17,22,23,24,25,31,32,33,34,35,36,38,39,51,53,54,55,56,57,58,59,60,61,62,63,64,65,66) #--number of aphid observations for 2011
obsnumber <- length(filefor)
#for (i in filefor) {

#ringfinal <- rbind(ring, ringfinal)  
ringfinal <- rep(ring, obsnumber)

#}

#ringfinal <- as.matrix(ringfinal)
final <- cbind(ringfinal, final)
#write(final, file="2011aphidfinalresult.txt")

final2 <- data.frame(final)
colnames(final2) <- colnames(final)

final <- final2
#names(pre.use.survey) <- new.cols$new.colname


#colnames(final) <- c("ring", "SiteID", "Spring Wheat", "Dry Beans", "Barley", "Grassland Herbaceous", 
#                     "Developed/Low Intensity", "Developed Open Space", "Shrubland", "Winter Wheat", 
#                     "Chick Peas", "Camelina", "Fallow/Idle Cropland", "Developed High Intensity", 
#                     "Other Hay/Non-Alfalfa", "Alfalfa", "Canola", "Evergreen Forest", "SugarBeets", "Sweet Corn", 
#                     "Pumpkins", "Apples", "Corn", "Grapes", 
#                     "Onions", "Potatoes", "Developed/Med Intensity", "Lentils", "Woody Wetlands", 
#                     "Sod/Grass Seed", "Sunflower", "Developed/High Intensity", "Herbs", "Cherries", "Carrots", 
#                     "Herbacous Wetlands", "Triticale", "Oats", "Rape Seed", "Deciduous Forest", "Mixed Forest", "Open Water", 
#                     "Safflower", "Mustard")

#--fix below to write xls file

write.csv(final, file = "/mnt/lfs2/erichs/git/aphids/data/sweepnet_2013_final_WW_rev2.csv")

#write.xlsx(final, "2013aphidfinalresult.xls", sheetName="Sheet1",
#           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)


