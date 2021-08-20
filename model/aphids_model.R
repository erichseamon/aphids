#aphid_modeling.R

#i options:

#TotAph
#Mfc
#TotAphNoMfc
#RelAbundMfc
#TotDn
#Totsg
#TotRp
#TotSa
#TotMd
#TotRm
#TotMfc
#TotSe
#TotAm
#TotRi

aphid_model <- function(i) {

library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)
library(caret)
library(doParallel)
library(plyr)
library(gridExtra)


library(cluster)
library(factoextra)

data <- read.csv("/mnt/ceph/erichs/git/aphids/modeldata/Aphid_data_Wide_RAW.csv")

data2 <- read.csv("/mnt/ceph/erichs/git/aphids/modeldata/Aphid_wide_Allrings_5.9.21.csv")

data3 <- read.csv("/mnt/ceph/erichs/git/aphids/finaldata/aphid_data_long.csv")

data4 <- read.csv("/mnt/ceph/erichs/git/aphids/finaldata/aphid_data_wide.csv")


data2 <- data2[,-c(3,6,8,9)]


set.seed(103)

fits<-list()
fits_summary <- NULL

#dependent <- colnames(data2[c(32,22,20,23)])
fmla <- as.formula(paste(colnames(data2[i]), " ~ ", paste(colnames(data2[c(8,9,10,11,12,3,4)]), collapse= "+")))

inTraining <- createDataPartition(eval(parse(text=paste("data2$", colnames(data2[i]), sep=""))), p = .7, list = FALSE)
training <- data2[ inTraining,]
testing  <- data2[-inTraining,]

train_control<- trainControl(method="repeatedcv", savePredictions = "final", returnResamp='all')

fit_rf <- caret::train(fmla, data=training, method="rf", trControl = train_control)
# fit_glm <- caret::train(fmla, data=training, method="glm", trControl = train_control)
# fit_knn <- caret::train(fmla, data=training, method="knn",trControl = train_control)
# fit_svm <- caret::train(fmla, data=training, method="svmRadial", trControl = train_control)
# fit_treebag <- caret::train(fmla, data=training, method="treebag", trControl = train_control)
# fit_gbm <- caret::train(fmla, data=training, method="gbm", trControl = train_control)


# 
#data.fit1 <- randomForest(TotMfc ~ Fallow + AllCereal + AllGrasslands + AllLegumes + CDD + LULCRich + CP + ringfinal, data = training, importance = TRUE, na.action = na.omit, mtry = 3)
# data.fit2 <- randomForest(TotAphNoMfc ~ Fallow + AllCereal + AllGrasslands + AllLegumes + CDD + LULCRich + CP + ringfinal, data = training, importance = TRUE, na.action = na.omit, mtry = 3)
# data.fit3 <- randomForest(TotAph ~ Fallow + AllCereal + AllGrasslands + AllLegumes  + CDD + LULCRich + CP + ringfinal, data = training, importance = TRUE, na.action = na.omit, mtry = 3)
# data.fit4 <- randomForest(RelAbundMfc ~ Fallow + AllCereal + AllGrasslands + AllLegumes  + CDD + LULCRich + CP + ringfinal, data = training, importance = TRUE, na.action = na.omit, mtry = 3)

#---Learning Curve Analysis


tree_num <- which(fit_rf$finalModel$err.rate[, 1] == min(fit_rf$finalModel$err.rate[, 1]))

lda_data <- learning_curve_dat(dat = fit_rf$trainingData,
                               outcome = ".outcome",
                               ## `train` arguments:
                               metric = "RMSE",
                               trControl = train_control,
                               method = "rf")

pts <- pretty(lda_data$RMSE)
#pts <- c(0,0.1, 0.2, 0.3, 0.4)

lda_data$Data[lda_data$Data == "Resampling"] <- c("Validation")
p1 <- ggplot(lda_data, aes(x = Training_Size, y = RMSE, color = Data)) +
  
  geom_smooth(method = loess, span = .8) +
  theme_bw()  + ggtitle("Learning Curve Analysis") + theme(axis.title.y = element_text(family = "Serif", size=18), axis.title.x = element_text(family = "Serif", size = 18), axis.text.x = element_text(size=rel(1.9), angle = 90, hjust = 1, family = "Serif"), axis.text.y = element_text(size=rel(1.9), hjust = 1, family = "Serif")) + theme(plot.title = element_text(family = "Serif", vjust = 2))  + theme(legend.text=element_text(family = "Serif", size=14)) + theme(legend.title=element_text(family = "Serif", size=16, face = "bold")) + theme(plot.title = element_text(size=24, face = "bold")) + scale_y_continuous(labels = pts, breaks = pts ) + xlab("Training Size") + ylab("RMSE") + theme(legend.position="bottom") + scale_fill_discrete(name = "Legend")


print(fit_rf$finalModel)

p2 <- ggplot(varImp(fit_rf))

grid.arrange(p1,p2, ncol = 2)

}



