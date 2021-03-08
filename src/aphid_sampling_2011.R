library(plyr)

finalmatrix <- data.frame()
file <- c(2,3,7,9,10,12,14,16,18,19,20,21,22,23,36,38,39,41,42,43,44,52,56,57,58,59,60,62,63,65,66,68,70,72) #--number of aphid observations for 2011
for (l in file) {

  l <- paste("2011_", l, sep="")
  
#--sets the workspace
#setwd("/nethome/erichs/Aphid 2011 csv results")
#setwd("../../owncloud/Aphids_sweepnet/")

  
#--reads in the csv file as a data frame
x <- read.delim2("data/sweepnet_2011_final_WW_feb2021.txt", header=TRUE, sep=",")

x$ToBufDist <- as.numeric(as.character(x$ToBufDist))
#x <- x[-which(x$Identifier== " "), ]
#x <- x[-which(x$ToBufDist== 0), ]
#x <- x[-which(x$Identifier== ""), ]
#x <- x[-which(x$GRIDCODE== "0"), ]

#subset based on observation number
x <- x[which(x$Identifier==l),]


obs <- unique(x[6])
ring <- data.matrix(unique(x[13]))
ring <- sort(ring, decreasing=TRUE)
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
# obs_matrix_2011_2 <- obs_matrix_2011_1
# obs_matrix_2011_2[,1] = "2011_2"
# 
# obs_matrix_2011_4 <- obs_matrix_2011_3
# obs_matrix_2011_4[,1] = "2011_4"
# 
# obs_matrix_2011_5 <- obs_matrix_2011_3
# obs_matrix_2011_5[,1] = "2011_5"
# 
# obs_matrix_2011_6 <- obs_matrix_2011_3
# obs_matrix_2011_6[,1] = "2011_6"
# 
# obs_matrix_2011_9 <- obs_matrix_2011_8
# obs_matrix_2011_9[,1] = "2011_9"
# 
# obs_matrix_2011_12 <- obs_matrix_2011_11
# obs_matrix_2011_12[,1] = "2011_12"
# 
# obs_matrix_2011_16 <- obs_matrix_2011_15
# obs_matrix_2011_16[,1] = "2011_16"
# 
# obs_matrix_2011_24 <- obs_matrix_2011_23
# obs_matrix_2011_24[,1] = "2011_24"
# 
# 
# obs_matrix_2011_25 <- obs_matrix_2011_23
# obs_matrix_2011_25[,1] = "2011_25"
# 
# 
# obs_matrix_2011_26 <- obs_matrix_2011_23
# obs_matrix_2011_26[,1] = "2011_26"
# 
# 
# obs_matrix_2011_27 <- obs_matrix_2011_23
# obs_matrix_2011_27[,1] = "2011_27"
# 
# 
# obs_matrix_2011_28 <- obs_matrix_2011_23
# obs_matrix_2011_28[,1] = "2011_28"
# 
# 
# obs_matrix_2011_29 <- obs_matrix_2011_23
# obs_matrix_2011_29[,1] = "2011_29"
# 
# 
# obs_matrix_2011_30 <- obs_matrix_2011_23
# obs_matrix_2011_30[,1] = "2011_30"
# 
# 
# obs_matrix_2011_31 <- obs_matrix_2011_23
# obs_matrix_2011_31[,1] = "2011_31"
# 
# obs_matrix_2011_32 <- obs_matrix_2011_23
# obs_matrix_2011_32[,1] = "2011_32"
# 
# obs_matrix_2011_33 <- obs_matrix_2011_23
# obs_matrix_2011_33[,1] = "2011_33"
# 
# obs_matrix_2011_34 <- obs_matrix_2011_23
# obs_matrix_2011_34[,1] = "2011_34"
# 
# obs_matrix_2011_45 <- obs_matrix_2011_44
# obs_matrix_2011_45[,1] = "2011_45"
# 
# obs_matrix_2011_46 <- obs_matrix_2011_44
# obs_matrix_2011_46[,1] = "2011_46"
# 
# obs_matrix_2011_47 <- obs_matrix_2011_44
# obs_matrix_2011_47[,1] = "2011_47"
# 
# obs_matrix_2011_48 <- obs_matrix_2011_44
# obs_matrix_2011_48[,1] = "2011_48"
# 
# obs_matrix_2011_49 <- obs_matrix_2011_44
# obs_matrix_2011_49[,1] = "2011_49"
# 
# obs_matrix_2011_50 <- obs_matrix_2011_44
# obs_matrix_2011_50[,1] = "2011_50"
# 
# obs_matrix_2011_51 <- obs_matrix_2011_45
# obs_matrix_2011_51[,1] = "2011_51"
# 
# obs_matrix_2011_53 <- obs_matrix_2011_52
# obs_matrix_2011_53[,1] = "2011_53"
# 
# obs_matrix_2011_54 <- obs_matrix_2011_52
# obs_matrix_2011_54[,1] = "2011_54"
# 
# obs_matrix_2011_55 <- obs_matrix_2011_52
# obs_matrix_2011_55[,1] = "2011_55"
# 
# obs_matrix_2011_62 <- obs_matrix_2011_61
# obs_matrix_2011_62[,1] = "2011_62"
# 
# obs_matrix_2011_65 <- obs_matrix_2011_64
# obs_matrix_2011_65[,1] = "2011_65"
# 
# obs_matrix_2011_70 <- obs_matrix_2011_69
# obs_matrix_2011_70[,1] = "2011_70"
# 
# obs_matrix_2011_72 <- obs_matrix_2011_71
# obs_matrix_2011_72[,1] = "2011_72"



final <- rbind.fill.matrix(obs_matrix_2011_2,obs_matrix_2011_3,obs_matrix_2011_7, obs_matrix_2011_9,obs_matrix_2011_10, 
                           obs_matrix_2011_12, obs_matrix_2011_14,
                           obs_matrix_2011_16,obs_matrix_2011_18, obs_matrix_2011_19,
                           obs_matrix_2011_20,obs_matrix_2011_21, obs_matrix_2011_22,obs_matrix_2011_23,obs_matrix_2011_36, 
                           obs_matrix_2011_38,obs_matrix_2011_39, 
                           obs_matrix_2011_41,obs_matrix_2011_42, obs_matrix_2011_43,obs_matrix_2011_44,
                           obs_matrix_2011_52,obs_matrix_2011_56,obs_matrix_2011_57, obs_matrix_2011_58,obs_matrix_2011_59,
                           obs_matrix_2011_60, obs_matrix_2011_62,obs_matrix_2011_63,
                           obs_matrix_2011_65,obs_matrix_2011_66,obs_matrix_2011_68,
                           obs_matrix_2011_70,obs_matrix_2011_72, fill=TRUE)

final <- final[1:272,]

ringfinal <- ring

#filefor <- c(1:72)
filefor <- c(2,3,7,9,10,12,14,16,18,19,20,21,22,23,36,38,39,41,42,43,44,52,56,57,58,59,60,62,63,65,66,68,70,72)
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
#colnames(final) <- c("ring", "SiteID", "Spring Wheat", "Dry Beans", "Barley", "Grassland Herbaceous", 
#                     "Developed/Low Intensity", "Developed Open Space", "Shrubland", "Winter Wheat", 
#                     "Chick Peas", "Camelina", "Fallow/Idle Cropland", "Developed High Intensity", 
#                     "Other Hay/Non-Alfalfa", "Alfalfa", "Canola", "Evergreen Forest", "SugarBeets", "Sweet Corn", 
#                     "Pumpkins", "Apples", "Corn", "Grapes", 
#                     "Onions", "Potatoes", "Developed/Med Intensity", "Lentils", "Woody Wetlands", 
#                     "Sod/Grass Seed", "Sunflower", "Developed/High Intensity", "Herbs", "Cherries", "Carrots", 
#                     "Herbacous Wetlands", "Triticale", "Oats", "Rape Seed", "Deciduous Forest", "Mixed Forest", "Open Water", 
#                     "Safflower", "Mustard")

#finaltest <- final
#finaltest$SiteID <- as.factor(finaltest$SiteID)
#tot <- finaltest %>% 
#  group_by(SiteID) %>%
#  summarise(no_rows = length(SiteID))

#tot/length(unique(final$SiteID))

#--fix below to write xls file
write.csv(final, file = "/mnt/lfs2/erichs/git/aphids/data/sweepnet_2011_final_WW_rev2.csv")

#write.xlsx(final, "2014aphidfinalresult.xls", sheetName="Sheet1",
#           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)


