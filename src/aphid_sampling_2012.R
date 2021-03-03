library(plyr)

finalmatrix <- data.frame()
file <- c(1,2,8,9,10,11,13,14,15,16,17,18,19,20,21,22,29,30,31,32,33,34,35,36,48,52,53,54,55,56,57,58,59,60,61,63,64,65,66) #--number of aphid observations for 2011
for (l in file) {
  
  l <- paste("2012_", l, sep="")




#--sets the workspace
#setwd("/nethome/erichs/Aphid 2012 csv results")
  
  #--reads in the csv file as a data frame
  x <- read.delim2("data/sweepnet_2012_buffer_WW.txt", sep=",", header=TRUE)
  
  x$ToBufDist <- as.numeric(as.character(x$ToBufDist))
  #x <- x[-which(x$Identifier== " "), ]
  x <- x[-which(x$ToBufDist== 0), ]
  #x <- x[-which(x$Identifier== ""), ]
  x <- x[-which(x$GRIDCODE== "0"), ]
  
  #subset based on observation number
  x <- x[which(x$Identifier==l),]


obs <- unique(x[11])
ring <- as.data.frame(unique(x[9]))
ring <- as.numeric(as.character(ring$ToBufDist))
ring <- sort(ring, decreasing=TRUE)
ring <- t(ring)
ring <- t(ring)
croptype <- data.matrix(unique(x[4]))
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
# obs_matrix_2012_3 <- obs_matrix_2012_2
# obs_matrix_2012_3[,1] = "2012_3"
# 
# obs_matrix_2012_4 <- obs_matrix_2012_2
# obs_matrix_2012_4[,1] = "2012_4"
# obs_matrix_2012_5 <- obs_matrix_2012_2
# obs_matrix_2012_5[,1] = "2012_5"
# obs_matrix_2012_6 <- obs_matrix_2012_2
# obs_matrix_2012_6[,1] = "2012_6"
# 
# obs_matrix_2012_7 <- obs_matrix_2012_6
# obs_matrix_2012_7[,1] = "2012_7"
# 
# obs_matrix_2012_12 <- obs_matrix_2012_11
# obs_matrix_2012_12[,1] = "2012_12"
# 
# obs_matrix_2012_23 <- obs_matrix_2012_22
# obs_matrix_2012_23[,1] = "2012_23"
# 
# obs_matrix_2012_24 <- obs_matrix_2012_22
# obs_matrix_24[,1] = "2012_24"
# 
# obs_matrix_2012_25 <- obs_matrix_2012_22
# obs_matrix_2012_25[,1] = "2012_25"
# 
# obs_matrix_2012_26 <- obs_matrix_2012_22
# obs_matrix_2012_26[,1] = "2012_26"
# 
# obs_matrix_2012_27 <- obs_matrix_2012_22
# obs_matrix_27[,1] = "2012_27"
# 
# obs_matrix_2012_28 <- obs_matrix_2012_22
# obs_matrix_2012_28[,1] = "2012_28"
# 
# 
#  
# obs_matrix_2012_37 <- obs_matrix_2012_36
# obs_matrix_2012_37[,1] = "2012_37"
# 
# obs_matrix_2012_38 <- obs_matrix_2012_36
# obs_matrix_2012_38[,1] = "2012_38"
# 
# obs_matrix_2012_39 <- obs_matrix_2012_36
# obs_matrix_2012_39[,1] = "2012_39"
# 
# obs_matrix_2012_40 <- obs_matrix_2012_36
# obs_matrix_2012_40[,1] = "2012_40"
# 
# obs_matrix_2012_41 <- obs_matrix_2012_36
# obs_matrix_2012_41[,1] = "2012_41"
# 
# obs_matrix_2012_42 <- obs_matrix_2012_36
# obs_matrix_2012_42[,1] = "2012_42"
# 
# obs_matrix_2012_43 <- obs_matrix_2012_36
# obs_matrix_2012_43[,1] = "2012_43"
# 
# obs_matrix_2012_44 <- obs_matrix_2012_36
# obs_matrix_2012_44[,1] = "2012_44"
# 
# obs_matrix_2012_45 <- obs_matrix_2012_36
# obs_matrix_2012_45[,1] = "2012_45"
# 
# obs_matrix_2012_46 <- obs_matrix_2012_36
# obs_matrix_2012_46[,1] = "2012_46"
# 
# obs_matrix_2012_47 <- obs_matrix_2012_36
# obs_matrix_2012_47[,1] = "2012_47"
# 
# 
# obs_matrix_2012_49 <- obs_matrix_2012_48
# obs_matrix_2012_49[,1] = "2012_49"
# 
# obs_matrix_2012_50 <- obs_matrix_2012_48
# obs_matrix_2012_50[,1] = "2012_50"
# 
# obs_matrix_2012_51 <- obs_matrix_2012_48
# obs_matrix_2012_51[,1] = "2012_51"
# 
# 
# obs_matrix_2012_63 <- obs_matrix_2012_62
# obs_matrix_2012_63[,1] = "2012_63"


final <- rbind.fill.matrix(obs_matrix_2012_1, obs_matrix_2012_2,obs_matrix_2012_8,obs_matrix_2012_9,obs_matrix_2012_10, 
                           obs_matrix_2012_11,obs_matrix_2012_13,obs_matrix_2012_14,
                           obs_matrix_2012_15, obs_matrix_2012_16,obs_matrix_2012_17,obs_matrix_2012_18, obs_matrix_2012_19,
                           obs_matrix_2012_20,obs_matrix_2012_21, obs_matrix_2012_22,obs_matrix_2012_29,
                           obs_matrix_2012_30, obs_matrix_2012_31,obs_matrix_2012_32,obs_matrix_2012_33, obs_matrix_2012_34,
                           obs_matrix_2012_35,obs_matrix_2012_36, obs_matrix_2012_48, obs_matrix_2012_52,obs_matrix_2012_53,obs_matrix_2012_54, 
                           obs_matrix_2012_55,obs_matrix_2012_56,obs_matrix_2012_57, obs_matrix_2012_58,obs_matrix_2012_59,
                           obs_matrix_2012_60, obs_matrix_2012_61,obs_matrix_2012_63, obs_matrix_2012_64,
                           obs_matrix_2012_65,obs_matrix_2012_66, fill=TRUE)

final <- final[1:312,]

ringfinal <- ring

#filefor <- c(1:66)
filefor <- c(1,2,8,9,10,11,13,14,15,16,17,18,19,20,21,22,29,30,31,32,33,34,35,36,48,52,53,54,55,56,57,58,59,60,61,63,64,65,66) #--number of aphid observations for 2011
obsnumber <- length(filefor)
#for (i in filefor) {
  
#ringfinal <- rbind(ring, ringfinal)  
ringfinal <- rep(ring, obsnumber)

#}

#ringfinal <- as.matrix(ringfinal)
final <- cbind(ringfinal, final)

final2 <- data.frame(final)
colnames(final2) <- colnames(final)

final <- final2
#write(final, file="2011aphidfinalresult.txt")

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

write.csv(final, file = "/mnt/lfs2/erichs/git/aphids/data/sweepnet_2012_final_WW.csv")

#write.xlsx(final, "2012aphidfinalresult.xls", sheetName="Sheet1",
#           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)


