##  R Code File Name:  F2 - Predictive Modeling - Sid.r

############################################################################################################
# Build Predictive models to predict the Overall Star Rating for Providers.
############################################################################################################

setwd("C:/Users/Siddartha Sathyanara/Dropbox/Team Project/CMS Data Files/RData Files")
load("overall.rating.df.Rdata")

temp.df <- overall.rating.df

dim(temp.df) # 107471  3430

# Also, the measures in percentages are on a 0-100 scale, bringing them down to 0-1 scale inline with the other variables
temp.df$HO.HHT.began.care.in.timely.manner.18 <-  temp.df$HO.HHT.began.care.in.timely.manner.18/100
temp.df$HO.HHT.taught.about.their.drugs.20 <-  temp.df$HO.HHT.taught.about.their.drugs.20/100
temp.df$HO.HHT.checked.for.risk.of.falling.22 <-  temp.df$HO.HHT.checked.for.risk.of.falling.22/100
temp.df$HO.HHT.checked.for.depression.24 <-  temp.df$HO.HHT.checked.for.depression.24/100
temp.df$HO.HHT.ensured.received.flu.shot.26 <-  temp.df$HO.HHT.ensured.received.flu.shot.26/100
temp.df$HO.HHT.made.received.pneumonia.shot.28 <-  temp.df$HO.HHT.made.received.pneumonia.shot.28/100
temp.df$HO.HHT.taught.gave.foot.care.30 <-  temp.df$HO.HHT.taught.gave.foot.care.30/100
temp.df$HO.HHT.checked.for.pain.32 <-  temp.df$HO.HHT.checked.for.pain.32/100
temp.df$HO.HHT.treated.for.pain.34 <-  temp.df$HO.HHT.treated.for.pain.34/100
temp.df$HO.HHT.treated.heart.failure.weakening.of.the.heart.36 <-  temp.df$HO.HHT.treated.heart.failure.weakening.of.the.heart.36/100
temp.df$HO.HHT.took.action.to.prevent.pressure.bed.sores.38 <-  temp.df$HO.HHT.took.action.to.prevent.pressure.bed.sores.38/100
temp.df$HO.HHT.included.treatm.to.prevent.pressure.bed.sores.40 <-  temp.df$HO.HHT.included.treatm.to.prevent.pressure.bed.sores.40/100
temp.df$HO.HHT.checked.for.risk.of.pressure.bed.sores.42 <-  temp.df$HO.HHT.checked.for.risk.of.pressure.bed.sores.42/100
temp.df$HO.PAT.got.better.at.moving.around.44 <-  temp.df$HO.PAT.got.better.at.moving.around.44/100
temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 <-  temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46/100
temp.df$HO.PAT..got.better.at.bathing.48 <-  temp.df$HO.PAT..got.better.at.bathing.48/100
temp.df$HO.PAT.had.less.pain.when.moving.around.50 <-  temp.df$HO.PAT.had.less.pain.when.moving.around.50/100
temp.df$HO.PAT.breathing.improved.52 <- temp.df$HO.PAT.breathing.improved.52/100
temp.df$HO.PAT.wounds.improved.healed.54 <-  temp.df$HO.PAT.wounds.improved.healed.54/100
temp.df$HO.PAT.got.better.at.taking.drugs.correctly.56 <-  temp.df$HO.PAT.got.better.at.taking.drugs.correctly.56/100
temp.df$HO.PAT.needed.urgent.unplanned.ER.wout.admission.58 <-  temp.df$HO.PAT.needed.urgent.unplanned.ER.wout.admission.58/100
temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 <- temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60/100


# So, setting up smaller data frame for Modeling 
myvars <- c( "HO.HHT.began.care.in.timely.manner.18"
                , "HO.HHT.taught.about.their.drugs.20"
                , "HO.HHT.ensured.received.flu.shot.26"
                , "HO.PAT.got.better.at.moving.around.44"
                , "HO.PAT.got.better.at.getting.in.and.out.of.bed.46"
                , "HO.PAT..got.better.at.bathing.48"
                , "HO.PAT.had.less.pain.when.moving.around.50"
                , "HO.PAT.breathing.improved.52"
                , "HO.PAT.had.to.be.admitted.to.the.hospital.60",
                "Cityalhambra",
                "Citymiami",
                "Citymiami.lakes",
                "Citynorthridge",
                "Citypalmdale",
                "Citysaint.clair.shores",
                "Citysouthfield",
                "HO.HHT.checked.for.depression.24",
                "HO.HHT.checked.for.pain.32",
                "HO.HHT.checked.for.risk.of.falling.22",
                "HO.HHT.checked.for.risk.of.pressure.bed.sores.42",
                "HO.HHT.included.treatm.to.prevent.pressure.bed.sores.40",
                "HO.HHT.made.received.pneumonia.shot.28",
                "HO.HHT.taught.gave.foot.care.30",
                "HO.HHT.took.action.to.prevent.pressure.bed.sores.38",
                "HO.HHT.treated.for.pain.34",
                "HO.HHT.treated.heart.failure.weakening.of.the.heart.36",
                "HO.PAT.got.better.at.taking.drugs.correctly.56",
                "HO.PAT.needed.urgent.unplanned.ER.wout.admission.58",
                "HO.PAT.wounds.improved.healed.54",
                "Offers.Medical.Social.ServicesN",
                "Offers.Medical.Social.ServicesY",
                "StateAR",
                "StateCA",
                "StateFL",
                "StateIL",
                "StateLA",
                "StateMI",
                "StateOH",
                "StateTX",
                "Type.of.Ownershipproprietary",
                "year.quarter201301",
                "year.quarter201401",
                "year.quarter201402",
                "year.quarter201403",
                "year.quarter201404",
                "year.quarter201501",
                "year.quarter201502",
                "overall.hhc.rating"
)

# Create a sub data set with only the variables chosen
predmodel.df <- temp.df[,myvars]

dim(predmodel.df)  # 107471  48

# library(leaps)
# leaps( x=predmodel.df[,1:48], y=predmodel.df[,49], names=names(predmodel.df)[1:48], method="Cp") ## Fails beacuse leaps does not allow more than 31 variables; use regsubsets()

########################################################################################
# Create random TRAIN and TEST data sets.

# Identify the indexes of a random sample.  
set.seed(123)
pred.df.train.index <- sample(1:nrow(predmodel.df),(0.75*nrow(predmodel.df)))
class(pred.df.train.index)
pred.df.train.index.df <- as.data.frame((pred.df.train.index))
dim(pred.df.train.index.df) # 80603   1

# create the TRAIN data set 
sid.train.df <- subset(predmodel.df[pred.df.train.index,])
dim(sid.train.df) # 80603    49

# create the TEST data set 
sid.test.df <- subset(predmodel.df[-pred.df.train.index,])
dim(sid.test.df) # 26868    49

# Save the above data set for future quick upload
setwd("C:/Users/Siddartha Sathyanara/Dropbox/Team Project/CMS Data Files/RData Files")
save(list = c('sid.train.df'
              ,'sid.test.df'), file = 'pred.model.train.test.data.RData')

load("pred.model.train.test.data.RData")
dim(sid.train.df)  #  80603    48
dim(sid.test.df)  # 26868    48

########################################################################################
#####RANDOM FOREST MODEL#####

require(randomForest)

rf.hhc = randomForest(as.factor(sid.train.df$overall.hhc.rating)~.,
                      data=sid.train.df,mtry=8,
                      importance =TRUE,type="classification")

rf.hhc

yhat.rf.hhc <- predict(rf.hhc,newdata=sid.train.df, type = "class")

require(caret)

confusionMatrix(yhat.rf.hhc, sid.train.df$overall.hhc.rating) ##99.99% training accuracy

yhat.rf.test.hhc <- predict(rf.hhc,newdata=sid.test.df, type = "class")

confusionMatrix(yhat.rf.test.hhc, sid.test.df$overall.hhc.rating) ##85.4 % testing accuracy

varImpPlot(rf.hhc,
           main="Variable Importance Plot for Random Forest Model",
           pch=19)

require(pROC)

ROC1 <- roc(yhat.rf.hhc, sid.train.df$overall.hhc.rating, auc = TRUE) # AUC 0.9989

plot(ROC1, col = "blue")

ROC2 <- roc(yhat.rf.test.hhc, sid.test.df$overall.hhc.rating, auc = TRUE) # AUC 0.9686

plot(ROC2, col = "green")

# Get all the Important Variables and save them into a .csv file.
model.rf.hhc.imp <- importance(rf.hhc)
setwd("C:/Users/Siddartha Sathyanara/Dropbox/Team Project/CMS Data Files/RData Files")
write.csv(model.rf.hhc.imp,"model.rf.hhc.imp.csv")

####SUPPORT VECTOR MACHINE####
require(e1071)

set.seed(454)

svm.out1 <- svm(as.factor(overall.hhc.rating)~.,
               data=sid.train.df, kernel="linear",
               cost=10)

svm.out2 <- svm(as.factor(overall.hhc.rating)~.,
                data=sid.train.df, kernel="radial",
                cost=10) 

svm.pred1 <- predict(svm.out1, sid.train.df)

confusionMatrix(svm.pred1, sid.train.df$overall.hhc.rating) ## 66.57% Accuracy

ROC3 <- roc(svm.pred1, sid.train.df$overall.hhc.rating, auc = TRUE) # AUC 0.968

plot(ROC3, col = "red")

svm.pred2 <- predict(svm.out1, sid.test.df)

confusionMatrix(svm.pred2, sid.test.df$overall.hhc.rating) ## 65.99% test error

ROC4 <- roc(svm.pred2, sid.test.df$overall.hhc.rating, auc = TRUE) # AUC 0.8456

plot(ROC4, col = "orange")

###Radial Kernel
set.seed(454)

svm.out2

svm.pred3 <- predict(svm.out2, sid.train.df)

require(caret)

confusionMatrix(svm.pred3, sid.train.df$overall.hhc.rating) ## 90.62% training error

ROC5 <- roc(svm.pred3, sid.train.df$overall.hhc.rating, auc = TRUE) # AUC 0.9899

plot(ROC5, col = "purple")

svm.pred4 <- predict(svm.out2, sid.test.df)

confusionMatrix(svm.pred4, sid.test.df$overall.hhc.rating) ## 81.62% test error

ROC6 <- roc(svm.pred4, sid.test.df$overall.hhc.rating, auc = TRUE) # AUC 0.9216

plot(ROC6, col = "yellow")

# Logic to plot all lines together
# Training ROC Curves
plot(ROC1, col = "blue")
par(new = TRUE)
plot(ROC3, col = "red", xaxt = "n", yaxt = "n")
par(new = TRUE)
plot(ROC5, col = "purple", xaxt = "n", yaxt = "n")
par(new = TRUE)
legend("right", legend = c("RF Train", "SVM Linear Train", "SVM Radial Train"), col = c("blue", "red", "purple"), lty = 1)


# Testing ROC Curves
plot(ROC2, col = "green", xaxt = "n", yaxt = "n")
par(new = TRUE)
plot(ROC4, col = "orange", xaxt = "n", yaxt = "n")
par(new = TRUE)
plot(ROC6, col = "yellow", xaxt = "n", yaxt = "n")
legend("right", legend = c("RF Test", "SVM Linear Test", "SVM Radial Test"), col = c("green", "orange", "yellow"), lty = 1)
