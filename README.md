# Sandipan_Ensemble
Code for Ensemble modeling

library(plyr)
library(readr)
library(dplyr)
library(caret)
library(caretEnsemble)
library(ROSE)
library(caTools)
library(relaimpo)
library(CSTools)
library(readxl)
#==================================================================================

data  <-read_xlsx("D:/2. PDF/2. PDF_Work/2023_2024/WORK/7. Climate_Change/3. Scomberomorus_Commerson (Narrow_Barred_Spanish_Mackerel)_53201/1. CPUE_Prediction/2014_2019_53201.xlsx")
data1  <-read_xlsx("D:/2. PDF/2. PDF_Work/2023_2024/WORK/7. Climate_Change/3. Scomberomorus_Commerson (Narrow_Barred_Spanish_Mackerel)_53201/1. CPUE_Prediction/RCP_8.5_2095-2100.xlsx")

data$S.CPUE <- data$S.CPUE

model_gam  <-  gam(S.CPUE      ~ s(SSH) + s(SSC) + s(SSS) + s(SST)           , data = data)
model_brt  <-  gbm::gbm(S.CPUE ~ (SSH)  + (SSC)  + (SSS)  + (SST)            , data = data)
model_rf   <-  randomForest(S.CPUE ~ (SSH)  + (SSC)  + (SSS)  + (SST)        , data = data)

model_cart <-  rpart(S.CPUE    ~ (SSH)  + (SSC)  + (SSS)  + (SST)            , data = data)
model_glm  <-  glm(CPUE      ~ (SST)  + (SSC)  + (MLD)  + (SSH)  + (SSS)     , data = data)



predict_gam  <- (predict.gam(model_gam, data = data ,type="response",se.fit=T)[[1]])
predict_brt  <- stats::predict(object = model_brt,  data = data)
predict_rf   <- stats::predict(object = model_rf,  data = data)

predict_cart <- stats::predict(object = model_cart, data = data)
predict_glm  <- (predict.glm(model_glm, data=data ,type="response",se.fit=T)[[1]])


algorithmList <- c('gam', 'rf', 'gbm')
model <- caretList(S.CPUE ~ (SSH) + (SSC) + (SSS) + (SST), data = data, methodList=algorithmList)


brt_stack <- caretStack(model, method="gbm")
preds <- data.frame(predict(brt_stack, newdata = data1))

desk<-"D:/2. PDF/2. PDF_Work/2023_2024/WORK/7. Climate_Change/3. Scomberomorus_Commerson (Narrow_Barred_Spanish_Mackerel)_53201/1. CPUE_Prediction/"
write.csv(preds,paste(desk,"Prediction.csv"),row.names=F)



train_control1 <- trainControl(method = "LOOCV")
train_control2 <- trainControl(method = "cv", number = 10)
train_control3 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

model_rf  <- train(S.CPUE ~ SST + SSS + SSH + SSC, data = data, method = "rf", trControl = train_control2)


