##  R Code File Name:  F1 - Linear Regression - Sandra.r

###############################################################################
###############################################################################
##    This R Code does the following:
##      1.  Creates a TRAIN and TEST data set based on the munged and scored data
##      2.  Fits a Linear Regression model with the Important Variables identified
##            by the RF technique in E2 - Identification of Important Variables with Random Forest - Sandra.r.
##            and it uses the overall.rating.df data set as the starting data set.
##      3.  Makes Predictions.
##      4.  Evaluates Linear Regression model via several measures and graphs.
##
##  Author:   Sandra Duenas
##  Date:     2/16/2016
###############################################################################
###############################################################################


setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
load("overall.rating.df.RData")
dim(overall.rating.df)  # 107471   3430
df <- overall.rating.df
df.names.df <- as.data.frame(names(df))
View(df.names.df)
rm(overall.rating.df)

# Keep the 38 Important Variables identified via the RF technique, plus the 9 measures
#  plus the CMS ID.
keep.vars <- c( "CMS.Cer.Number.CCN"
                , "HO.HHT.began.care.in.timely.manner.18"
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

# Create a sub data set with only the variables for the lm
lm.df <- df[,keep.vars]
dim(lm.df) # 107471     49
lm.df.names.df <- as.data.frame(names(lm.df))
View(lm.df.names.df)
rm(df)


########################################################################################
# Create random TRAIN and TEST data sets.

# Identify the indexes of a random sample.  Only use 15% of the data due to R memory limitations.
set.seed(123)
lm.df.train.index <- sample(1:nrow(lm.df),(0.75*nrow(lm.df)))
class(lm.df.train.index)
lm.df.train.index.df <- as.data.frame((lm.df.train.index))
dim(lm.df.train.index.df) # 80603   1

# create the TRAIN data set 
train.df <- subset(lm.df[lm.df.train.index,])
dim(train.df) # 80603    49
overall.hhc.rating.train.observed <- train.df$overall.hhc.rating


# create the TEST data set 
test.df <- subset(lm.df[-lm.df.train.index,])
dim(test.df) # 26868    49
overall.hhc.rating.test.observed <- test.df$overall.hhc.rating


# Save the above data set for future quick upload
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('train.df'
              ,'test.df'
              ,'overall.hhc.rating.train.observed'
              ,'overall.hhc.rating.test.observed'), file = 'linear.regression.train.test.data.RData')
load("linear.regression.train.test.data.RData")
dim(train.df)  #  80603    49
dim(test.df)  # 26868    49


###############################################################################
### Fit Linear Regression Model using the TRAIN data set
###############################################################################
# Fit Model
lm.1 <- lm(overall.hhc.rating ~ ., data=train.df)

par(mfrow=c(2,2))
plot(lm.1)  ##  GOOD!

# Make Predictions with the TRAIN data set:
lm.1.predictions.train <- predict(lm.1, newdata = train.df)
# Root Mean of the Square Error (RMSE: the measuare of the width of the data cloud around the line of perfect prediction)
#   the smaller the RMSE, the better
mean.sqr.error.lm.1.train <- sqrt(mean((lm.1.predictions.train - overall.hhc.rating.train.observed)^2))
mean.sqr.error.lm.1.train  # 0.4315546


# Make Predictions for the TEST data set:
# Root Mean of the Square Error (RMSE: the measuare of the width of the data cloud around the line of perfect prediction)
#   the smaller the RMSE, the better
lm.1.predictions.test <- predict(lm.1, newdata = test.df)
mean.sqr.error.lm.1.test <- sqrt(mean((lm.1.predictions.test - overall.hhc.rating.test.observed)^2))
mean.sqr.error.lm.1.test  # 0.4336423


# Plotting actual y or Total Paid Amount as a function of the prediction  GOOD!
# the model is good IF the data points are arranged near the straight blue line.
library("ggplot2")
ggplot(data=test.df, aes(x=lm.1.predictions.test, y=overall.hhc.rating)) +
  scale_x_continuous(limits=c(0,6)) +
  xlab("PREDICTION for overall.hhc.rating") +
  scale_y_continuous(limits=c(0,6)) +
  ylab("ACTUAL of overall.hhc.rating") +
  labs(title = "Actual overall.hhc.rating vs. Predicted overall.hhc.rating\nred line is perfect prediction\nblue line is modeled prediction") +
  labs(colour = "Lines") +
  geom_point(alpha=0.2, color="black") +
  geom_line(aes(x=overall.hhc.rating,  # perfect prediction
                y=overall.hhc.rating,
                color="red")) +
  geom_smooth((aes(x=lm.1.predictions.test, # model prediction
                   y=overall.hhc.rating)))


# Plotting the residuals Total Paid Amount as a function of Predicted Total Paid Amount
#   The of perfect prediction is the line y=0.
library("ggplot2")
ggplot(data=test.df, aes(x=lm.1.predictions.test,
                         y=lm.1.predictions.test - overall.hhc.rating)) +
  #scale_x_continuous(limits=c(-2,2)) +
  xlab("PREDICTION for overall.hhc.rating") +  
 # scale_y_continuous(limits=c(-1,6)) +
  ylab("Residuals for ACTUAL - PREDICTED overall.hhc.rating") +  
  labs(title = "Residuals of Actual overall.hhc.rating vs. Predicted overall.hhc.rating") +  
  geom_point(alpha=0.2, color="black")  +
  geom_smooth((aes(x=lm.1.predictions.test,
                   y=lm.1.predictions.test - overall.hhc.rating,
                   color="black")))


