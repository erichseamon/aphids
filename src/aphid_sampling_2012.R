library(plyr)
library(xlsx)

finalmatrix <- data.frame()
file <- c(1,2,8,9,10,11,22,29,30,31,32,33,34,35,36,48,52,53,54,55,56,57,58,59,60,61,62,64,65,66) #--number of aphid observations for 2011
for (l in file) {




#--sets the workspace
setwd("/nethome/erichs/Aphid 2012 csv results")

#--reads in the csv file as a data frame
xxx <- paste("Sweep12outcsv", ".csv", sep = "")

x <- as.data.frame(read.csv(xxx))
x <- x[-which(x$Identifi_1== "0"), ]
x <- x[-which(x$GRIDCODE== "0"), ]

#subset based on observation number
x <- x[which(x$Identifi_1==l),]


obs <- unique(x[12])
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

Fixing duplicate geometry that got merged earlier.  

obs_matrix_3 <- obs_matrix_2
obs_matrix_3[,1] = 3

obs_matrix_4 <- obs_matrix_2
obs_matrix_4[,1] = 4
obs_matrix_5 <- obs_matrix_2
obs_matrix_5[,1] = 5
obs_matrix_6 <- obs_matrix_2
obs_matrix_6[,1] = 6

obs_matrix_7 <- obs_matrix_6
obs_matrix_7[,1] = 7

obs_matrix_12 <- obs_matrix_11
obs_matrix_12[,1] = 12

obs_matrix_23 <- obs_matrix_22
obs_matrix_23[,1] = 23

obs_matrix_24 <- obs_matrix_22
obs_matrix_24[,1] = 24

obs_matrix_25 <- obs_matrix_22
obs_matrix_25[,1] = 25

obs_matrix_26 <- obs_matrix_22
obs_matrix_26[,1] = 26

obs_matrix_27 <- obs_matrix_22
obs_matrix_27[,1] = 27

obs_matrix_28 <- obs_matrix_22
obs_matrix_28[,1] = 28


 
obs_matrix_37 <- obs_matrix_36
obs_matrix_37[,1] = 37

obs_matrix_38 <- obs_matrix_36
obs_matrix_38[,1] = 38

obs_matrix_39 <- obs_matrix_36
obs_matrix_39[,1] = 39

obs_matrix_40 <- obs_matrix_36
obs_matrix_40[,1] = 40

obs_matrix_41 <- obs_matrix_36
obs_matrix_41[,1] = 41

obs_matrix_42 <- obs_matrix_36
obs_matrix_42[,1] = 42

obs_matrix_43 <- obs_matrix_36
obs_matrix_43[,1] = 43

obs_matrix_44 <- obs_matrix_36
obs_matrix_44[,1] = 44

obs_matrix_45 <- obs_matrix_36
obs_matrix_45[,1] = 45

obs_matrix_46 <- obs_matrix_36
obs_matrix_46[,1] = 6

obs_matrix_47 <- obs_matrix_36
obs_matrix_47[,1] = 47


obs_matrix_49 <- obs_matrix_48
obs_matrix_49[,1] = 49

obs_matrix_50 <- obs_matrix_48
obs_matrix_50[,1] = 50

obs_matrix_51 <- obs_matrix_48
obs_matrix_51[,1] = 51


obs_matrix_63 <- obs_matrix_62
obs_matrix_63[,1] = 63
























final <- rbind.fill.matrix(obs_matrix_1, obs_matrix_2,obs_matrix_3,obs_matrix_4, obs_matrix_5,
                           obs_matrix_6,obs_matrix_7, obs_matrix_8,obs_matrix_9,obs_matrix_10, 
                           obs_matrix_11,obs_matrix_3,obs_matrix_12, obs_matrix_13,obs_matrix_14,
                           obs_matrix_15, obs_matrix_16,obs_matrix_17,obs_matrix_18, obs_matrix_19,
                           obs_matrix_20,obs_matrix_21, obs_matrix_22,obs_matrix_23,obs_matrix_24, 
                           obs_matrix_25,obs_matrix_26,obs_matrix_27, obs_matrix_28,obs_matrix_29,
                           obs_matrix_30, obs_matrix_31,obs_matrix_32,obs_matrix_33, obs_matrix_34,
                           obs_matrix_35,obs_matrix_36, obs_matrix_37,obs_matrix_38,obs_matrix_39, 
                           obs_matrix_40,obs_matrix_41,obs_matrix_42, obs_matrix_43,obs_matrix_44,
                           obs_matrix_45, obs_matrix_46,obs_matrix_47,obs_matrix_48, obs_matrix_49,
                           obs_matrix_50,obs_matrix_51, obs_matrix_52,obs_matrix_53,obs_matrix_54, 
                           obs_matrix_55,obs_matrix_56,obs_matrix_57, obs_matrix_58,obs_matrix_59,
                           obs_matrix_60, obs_matrix_61,obs_matrix_62,obs_matrix_63, obs_matrix_64,
                           obs_matrix_65,obs_matrix_66, fill=TRUE)

final <- final[1:536,]

ringfinal <- ring

filefor <- c(1:66)

for (i in filefor) {
  
ringfinal <- rbind(ring, ringfinal)  

}

#ringfinal <- as.matrix(ringfinal)
final <- cbind(ringfinal, final)
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

write.xlsx(final, "2012aphidfinalresult.xls", sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)


