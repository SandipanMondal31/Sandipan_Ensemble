#R-Script for Climate-Driven Shifts in Narrow-Barred Spanish Mackerel (Scomberomorus commerson) Distribution in the Taiwan Strait: Insights from Remote Sensing Oceanography

data <- read_xlsx("D:/2. PDF/2. PDF_Work/2023_2024/WORK/3. Climatic_Oscillations/Data.xlsx")

data$CPUE <- data$CPUE
data$logCPUE<-log(data$CPUE)
#=====================================================================================================================

#Impact of Climatic Oscillation on Catch Rate (CPUE)

##GAM analysis

library (mgcv)

GAM  <- gam(logCPUE ~ s(Climatic Oscillation), data = data)
AIC(GAM)
summary(GAM) #For GCV value
plot(GAM)


##Wavelet analysis

library(biwavelet)
library(readxl)

attach(data)
t1 = cbind(Year, CPUE)
t2 = cbind(Year, Climatic_Oscillation)
nrands = 100
wtc.AB = wtc(t1, t2)
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Phase", xlab = "Year", 
     plot.cb = TRUE, main = "IOD(-1)", cex.axis=1.5,cex.lab=1.5)
#======================================================================================================================

#N.CPUE Standardization

library (mgcv)

model ~ gam(logCPUE ~ s(Year) + s(Month) + s(Latitude) + s(Longitude) + s(Vessel_Tonnage) + s(Interactions) + s(Climatic_Oscillation), data = data)

data <- subset(data,data$Month >= 1 & data$Month <= 12);
data$S.CPUE <-exp(predict.gam(model,data=data,type="response",se.fit=T)[[1]])
desk<-"D:/Desktop/"
write.csv(data,paste(desk,"Standardized_CPUE(S.CPUE).csv"),row.names=F)

#data1 <- Standardized_CPUE(S.CPUE)
#======================================================================================================================

#Preferred Oceanographic Conditions

##Importance of oceanographic parameters

library (mgcv)
library (randomForest)
library (gbm)
library (rpart)


data1$CPUE <- data1$CPUE
data1$logCPUE<-log(data1$CPUE)

GAM   <- gam(logCPUE ~ s(Oceanographic Condition), data = data1)
GLM   <- glm(logCPUE ~ Oceanographic Condition, data = data1)
GBM   <- gbm::gbm(logCPUE ~ Oceanographic Condition, data=data1)
CART  <- gbm::gbm(logCPUE ~ Oceanographic Condition, data=data1)
RF    <- randomForest(logCPUE ~ Oceanographic Condition, data=data1)

predict_GLM   <- (predict.glm(GLM, data = data1 ,type="response",se.fit=T)[[1]])
predict_GAM   <- (predict.gam(GAM, data = data1 ,type="response",se.fit=T)[[1]])
predict_GBM   <- stats::predict(object = GBM, data = data1)
predict_CART  <- stats::predict(object = CTA, data = data1)
predict_RF    <- stats::predict(object = RF, data = data1)


data.frame(RMSE = RMSE(predict_GAM, data1$CPUE),
          MAE  = MAE (predict_GAM, data1$CPUE))

data.frame(RMSE = RMSE(predict_GLM, data1$CPUE),
           MAE  = MAE (predict_GLM, data1$CPUE))

caret::RMSE(pred = predict_GBM, data1$CPUE)
caret::MAE (pred = predict_GBM, data1$CPUE)

caret::RMSE(pred = predict_CART, data1$CPUE)
caret::MAE (pred = predict_CART, data1$CPUE)

caret::RMSE(pred = predict_RF , data1$CPUE)
caret::MAE (pred = predict_RF , data1$CPUE)
#=====================================================================================================================

#Distribution Modeling

##Collinearity assessment

library(car)

model <- lm(S.CPUE ~ Oceanographic_Condition_1 + Oceanographic_Condition_2 + ... + Oceanographic_Condition_X, data = data1)

vif_values <- vif(model)
barplot(vif_values, main = "a1",col = 'blue',
        ylim = c(0.0,6.0),ylab = "VIF Value",sub = "Oceanographic Conditions",
        cex.lab = 1.5, cex.axis = 1.5,cex.sub = 2) 
bad_vif <- 5.0
abline(h = bad_vif, lwd = 3, lty = 2,col = 'red')


##Single-algorithm model construction

GAM   <- gam(logCPUE ~ s(Oceanographic Condition_1) + s(Oceanographic Condition_2) + ... + s(Oceanographic Condition_x), data = data1)
GLM   <- glm(logCPUE ~ Oceanographic_Condition_1 + Oceanographic_Condition_2 + ... + Oceanographic_Condition_X, data = data1)
GBM   <- gbm::gbm(logCPUE ~ Oceanographic_Condition_1 + Oceanographic_Condition_2 + ... + Oceanographic_Condition_X, data=data1)
CART  <- gbm::gbm(logCPUE ~ Oceanographic_Condition_1 + Oceanographic_Condition_2 + ... + Oceanographic_Condition_X, data=data1)
RF    <- randomForest(logCPUE ~ Oceanographic_Condition_1 + Oceanographic_Condition_2 + ... + Oceanographic_Condition_X, data=data1)


##Analysis of the efficiency and accuracy of single-algorithm models

predict_GLM   <- (predict.glm(GLM, data = data1 ,type="response",se.fit=T)[[1]])
predict_GAM   <- (predict.gam(GAM, data = data1 ,type="response",se.fit=T)[[1]])
predict_GBM   <- stats::predict(object = GBM, data = data1)
predict_CART  <- stats::predict(object = CTA, data = data1)
predict_RF    <- stats::predict(object = RF, data = data1)


data.frame(RMSE = RMSE(predict_GAM, data1$CPUE),
           MAE  = MAE (predict_GAM, data1$CPUE))

data.frame(RMSE = RMSE(predict_GLM, data1$CPUE),
           MAE  = MAE (predict_GLM, data1$CPUE))

caret::RMSE(pred = predict_GBM, data1$CPUE)
caret::MAE (pred = predict_GBM, data1$CPUE)

caret::RMSE(pred = predict_CART, data1$CPUE)
caret::MAE (pred = predict_CART, data1$CPUE)

caret::RMSE(pred = predict_RF , data1$CPUE)
caret::MAE (pred = predict_RF , data1$CPUE)


##Ensemble model construction

library(plyr)
library(readr)
library(dplyr)
library(caret)
library(caretEnsemble)
library(ROSE)
library(caTools)
library(relaimpo)
library(CSTools)

algorithmList <- c('selected single algorithms')
model <- caretList(S.CPUE ~ Oceanographic_Condition_1 + Oceanographic_Condition_2 + ... + Oceanographic_Condition_X, data = data1, methodList=algorithmList)


brt_stack <- caretStack(model, method="gbm")
preds <- data.frame(predict(brt_stack, newdata = data1))

desk<-"D:/2. PDF/2. PDF_Work/2023_2024/WORK/7. Climate_Change/3. Scomberomorus_Commerson (Narrow_Barred_Spanish_Mackerel)_53201/1. CPUE_Prediction/"
write.csv(preds,paste(desk,"Prediction.csv"),row.names=F)


##Validation

train_control1 <- trainControl(method = "LOOCV")
train_control2 <- trainControl(method = "cv", number = 10)
train_control3 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

model  <- train(S.CPUE ~ Oceanographic_Condition_1 + Oceanographic_Condition_2 + ... + Oceanographic_Condition_X, data = data1, method = "ensemble", trControl = train_control1)
