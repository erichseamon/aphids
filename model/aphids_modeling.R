#aphid_modeling.R


library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)
library(caret)
library(doParallel)
library(plyr)


library("cluster")
library("factoextra")

data <- read.csv("/mnt/ceph/erichs/git/aphids/modeldata/Aphid_data_Wide_RAW.csv")

data2 <- read.csv("/mnt/ceph/erichs/git/aphids/modeldata/Aphid_wide_Allrings_5.9.21.csv")

data3 <- read.csv("/mnt/ceph/erichs/git/aphids/finaldata/aphid_data_long.csv")

data4 <- read.csv("/mnt/ceph/erichs/git/aphids/finaldata/aphid_data_wide.csv")


data2 <- data2[,-c(3,6,8,9)]


set.seed(103)

fits<-list()

for (i in c(1:4)) {
  
dependent <- colnames(data2[c(32,22,20,23)])
fmla <- as.formula(paste(dependent[1], " ~ ", paste(colnames(data2[c(8,9,10,11,12,3,4)]), collapse= "+")))

inTraining <- createDataPartition(eval(parse(text=paste("data2$", dependent[i], sep=""))), p = .7, list = FALSE)
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
# data.fit1 <- randomForest(TotMfc ~ Fallow + AllCereal + AllGrasslands + AllLegumes + CDD + LULCRich + CP + ringfinal, data = training, importance = TRUE, na.action = na.omit, mtry = 3)
# data.fit2 <- randomForest(TotAphNoMfc ~ Fallow + AllCereal + AllGrasslands + AllLegumes + CDD + LULCRich + CP + ringfinal, data = training, importance = TRUE, na.action = na.omit, mtry = 3)
# data.fit3 <- randomForest(TotAph ~ Fallow + AllCereal + AllGrasslands + AllLegumes  + CDD + LULCRich + CP + ringfinal, data = training, importance = TRUE, na.action = na.omit, mtry = 3)
# data.fit4 <- randomForest(RelAbundMfc ~ Fallow + AllCereal + AllGrasslands + AllLegumes  + CDD + LULCRich + CP + ringfinal, data = training, importance = TRUE, na.action = na.omit, mtry = 3)

fit <- varImp(fit_rf)
fit$importance$type <- i

fit$importance$name <- rownames(fit$importance)



fits[[i]] <- fit$importance
}


fits2 <- bind_rows(fits)

ggplot(fits2, aes(x=name, y=Overall, group = type, fill = type)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle = 90))

