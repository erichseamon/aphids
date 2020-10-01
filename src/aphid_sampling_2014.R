library(plyr)

finalmatrix <- data.frame()
file <- c(1,4,6,8,9,13,15,16,19,28,32,33,34,35,39,41,43,44,45,47,48,49,51,53,55,56,57,59,60,62,63,66,71) #--number of aphid observations for 2011
for (l in file) {

  l <- paste("2014_", l, sep="")
  


#--sets the workspace
#setwd("/nethome/erichs/Aphid 2014 csv results")

#--reads in the csv file as a data frame
xxx <- paste("data/sweepnet_2014_buffer.txt", sep = "")

x <- as.data.frame(read.delim2(xxx, header=TRUE, sep=","))
x$ToBufDist <- as.numeric(as.character(x$ToBufDist))
x <- x[-which(x$Identifier== " "), ]

x <- x[-which(x$GRIDCODE== "0"), ]

#subset based on observation number
x <- x[which(x$Identifier==l),]


obs <- unique(x[11])
ring <- data.matrix(unique(x[9]))
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
obs_matrix_2014_2 <- obs_matrix_2014_1
obs_matrix_2014_2[,1] = "2014_2"
obs_matrix_2014_3 <- obs_matrix_2014_1
obs_matrix_2014_3[,1] = "2014_3"


obs_matrix_2014_5 <- obs_matrix_2014_4
obs_matrix_2014_5[,1] = "2014_5"

obs_matrix_2014_7 <- obs_matrix_2014_6
obs_matrix_2014_7[,1] = "2014_7"

obs_matrix_2014_10 <- obs_matrix_2014_9
obs_matrix_2014_10[,1] = "2014_10"

obs_matrix_2014_11 <- obs_matrix_2014_9
obs_matrix_2014_11[,1] = "2014_11"

obs_matrix_2014_12 <- obs_matrix_2014_9
obs_matrix_2014_12[,1] = "2014_12"

obs_matrix_2014_14 <- obs_matrix_2014_13
obs_matrix_2014_14[,1] = "2014_14"


obs_matrix_2014_17 <- obs_matrix_2014_16
obs_matrix_2014_17[,1] = "2014_17"

obs_matrix_2014_18 <- obs_matrix_2014_16
obs_matrix_2014_18[,1] = "2014_18"

obs_matrix_2014_20 <- obs_matrix_2014_19
obs_matrix_20[,1] = "2014_20"

obs_matrix_2014_21 <- obs_matrix_2014_19
obs_matrix_2014_21[,1] = "2014_21"

obs_matrix_2014_22 <- obs_matrix_2014_19
obs_matrix_2014_22[,1] = "2014_22"

obs_matrix_2014_23 <- obs_matrix_2014_19
obs_matrix_2014_23[,1] = "2014_23"

obs_matrix_2014_24 <- obs_matrix_2014_19
obs_matrix_2014_24[,1] = "2014_24"

obs_matrix_2014_25 <- obs_matrix_2014_19
obs_matrix_2014_25[,1] = "2014_25"

obs_matrix_2014_26 <- obs_matrix_2014_19
obs_matrix_2014_26[,1] = "2014_26"

obs_matrix_2014_27 <- obs_matrix_2014_19
obs_matrix_2014_27[,1] = "2014_27"

obs_matrix_2014_29 <- obs_matrix_2014_28
obs_matrix_2014_29[,1] = "2014_29"

obs_matrix_2014_30 <- obs_matrix_2014_28
obs_matrix_2014_30[,1] = "2014_30"

obs_matrix_2014_31 <- obs_matrix_2014_28
obs_matrix_2014_31[,1] = "2014_31"
 
obs_matrix_2014_36 <- obs_matrix_2014_35
obs_matrix_2014_36[,1] = "2014_36"

obs_matrix_2014_37 <- obs_matrix_2014_35
obs_matrix_2014_37[,1] = "2014_37"

obs_matrix_2014_38 <- obs_matrix_2014_35
obs_matrix_2014_38[,1] = "2014_38"

obs_matrix_2014_40 <- obs_matrix_2014_39
obs_matrix_2014_40[,1] = "2014_40"

obs_matrix_2014_42 <- obs_matrix_2014_41
obs_matrix_2014_42[,1] = "2014_42"


obs_matrix_2014_46 <- obs_matrix_2014_45
obs_matrix_2014_46[,1] = "2014_46"


obs_matrix_2014_50 <- obs_matrix_2014_49
obs_matrix_2014_50[,1] = "2014_50"

obs_matrix_2014_52 <- obs_matrix_2014_51
obs_matrix_52[,1] = "2014_52"

obs_matrix_2014_54 <- obs_matrix_2014_53
obs_matrix_2014_54[,1] = "2014_54"

obs_matrix_2014_58 <- obs_matrix_2014_57
obs_matrix_2014_58[,1] = "2014_58"


obs_matrix_2014_61 <- obs_matrix_2014_60
obs_matrix_2014_61[,1] = "2014_61"

obs_matrix_2014_64 <- obs_matrix_2014_63
obs_matrix_2014_64[,1] = "2014_64"

obs_matrix_2014_65 <- obs_matrix_2014_63
obs_matrix_2014_65[,1] = "2014_65"


obs_matrix_2014_67 <- obs_matrix_2014_66
obs_matrix_2014_67[,1] = "2014_67"

obs_matrix_2014_68 <- obs_matrix_2014_66
obs_matrix_2014_68[,1] = "2014_68"


obs_matrix_2014_69 <- obs_matrix_2014_66
obs_matrix_2014_69[,1] = "2014_69"

obs_matrix_2014_70 <- obs_matrix_2014_66
obs_matrix_2014_70[,1] = "2014_70"

obs_matrix_2014_72 <- obs_matrix_2014_71
obs_matrix_2014_72[,1] = "2014_72"

obs_matrix_2014_73 <- obs_matrix_2014_71
obs_matrix_2014_73[,1] = "2014_73"


















final <- rbind.fill.matrix(obs_matrix_2014_1, obs_matrix_2014_2,obs_matrix_2014_3,obs_matrix_2014_4, obs_matrix_2014_5,
                           obs_matrix_2014_6,obs_matrix_2014_7, obs_matrix_2014_8,obs_matrix_2014_9,obs_matrix_2014_10, 
                           obs_matrix_2014_11,obs_matrix_2014_3,obs_matrix_2014_12, obs_matrix_2014_13,obs_matrix_2014_14,
                           obs_matrix_2014_15, obs_matrix_2014_16,obs_matrix_2014_17,obs_matrix_2014_18, obs_matrix_2014_19,
                           obs_matrix_2014_20,obs_matrix_2014_21, obs_matrix_2014_22,obs_matrix_2014_23,obs_matrix_2014_24, 
                           obs_matrix_2014_25,obs_matrix_2014_26,obs_matrix_2014_27, obs_matrix_2014_28,obs_matrix_2014_29,
                           obs_matrix_2014_30, obs_matrix_2014_31,obs_matrix_2014_32,obs_matrix_2014_33, obs_matrix_2014_34,
                           obs_matrix_2014_35,obs_matrix_2014_36, obs_matrix_2014_37,obs_matrix_2014_38,obs_matrix_2014_39, 
                           obs_matrix_2014_40,obs_matrix_2014_41,obs_matrix_2014_42, obs_matrix_2014_43,obs_matrix_2014_44,
                           obs_matrix_2014_45, obs_matrix_2014_46,obs_matrix_2014_47,obs_matrix_2014_48, obs_matrix_2014_49,
                           obs_matrix_2014_50,obs_matrix_2014_51, obs_matrix_2014_52,obs_matrix_2014_53,obs_matrix_2014_54, 
                           obs_matrix_2014_55,obs_matrix_2014_56,obs_matrix_2014_57, obs_matrix_2014_58,obs_matrix_2014_59,
                           obs_matrix_2014_60, obs_matrix_2014_61,obs_matrix_2014_62,obs_matrix_2014_63, obs_matrix_2014_64,
                           obs_matrix_2014_65,obs_matrix_2014_66,obs_matrix_2014_67,obs_matrix_2014_68,obs_matrix_2014_69,
                           obs_matrix_2014_70,obs_matrix_2014_71,obs_matrix_2014_72,obs_matrix_2014_73, fill=TRUE)

final <- final[1:592,]

ringfinal <- ring

filefor <- c(1:73)

for (i in filefor) {
  
ringfinal <- rbind(ring, ringfinal)  

}

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

#--fix below to write xls file

write.csv(final, file = "data/sweepnet_2014_final.csv")


write.xlsx(final, "2014aphidfinalresult.xls", sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)


