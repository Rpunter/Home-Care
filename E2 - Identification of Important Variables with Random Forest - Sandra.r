##  R Code File Name:  E2 - Identification of Important Variables with Random Forest - Sandra.r


###############################################################################
###############################################################################
##    This R Code does the following:
##      1.  Creates a TRAIN and TEST data set based on the munged and scored data
##      2.  Fits a Random Forest model to identify Important Variables.
##
##  Author:   Sandra Duenas
##  Date:     2/13/2016
###############################################################################
###############################################################################


setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
load("overall.rating.df.RData")
dim(overall.rating.df)  # 107471   3430
df <- overall.rating.df
rm(overall.rating.df)

all.vars <- names(df)

# remove variables not need for Random Forest because these variables were used to 
#  create the Target variable.  Also, remove the Agency identifier.
remove.vars <- c(  "IDR.SCORE.18",
                   "IDR.SCORE.20",
                   "IDR.SCORE.26",
                   "IDR.SCORE.44",
                   "IDR.SCORE.46",
                   "IDR.SCORE.48",
                   "IDR.SCORE.50",
                   "IDR.SCORE.52",
                   "IDR.SCORE.60",
                   "count.providers",
                   "pvalue.18",
                   "pvalue.20",
                   "pvalue.26",
                   "pvalue.44",
                   "pvalue.46",
                   "pvalue.48",
                   "pvalue.50",
                   "pvalue.52",
                   "pvalue.60",
                   "avg.decile"
                   , "HO.HHT.began.care.in.timely.manner.18"
                   , "HO.HHT.taught.about.their.drugs.20"
                   , "HO.HHT.ensured.received.flu.shot.26"
                   , "HO.PAT.got.better.at.moving.around.44"
                   , "HO.PAT.got.better.at.getting.in.and.out.of.bed.46"
                   , "HO.PAT..got.better.at.bathing.48"
                   , "HO.PAT.had.less.pain.when.moving.around.50"
                   , "HO.PAT.breathing.improved.52"
                   , "HO.PAT.had.to.be.admitted.to.the.hospital.60"
                   ,"CMS.Cer.Number.CCN")

keep.vars <- setdiff(all.vars,remove.vars)

rf.df <- df[,keep.vars]
dim(rf.df) # 107471   3400
rf.df.names <- as.data.frame(names(rf.df))
View(rf.df.names)


########################################################################################
# Create random TRAIN and TEST data sets.

# Identify the indexes of a random sample.  Only use 15% of the data due to R memory limitations.
set.seed(123)
rf.df.train.index <- sample(1:nrow(rf.df),(0.15*nrow(rf.df)))
class(rf.df.train.index)
rf.df.train.index.df <- as.data.frame((rf.df.train.index))
dim(rf.df.train.index.df) # 16120     1

# create the TRAIN data set 
train.df <- subset(rf.df[rf.df.train.index,])
dim(train.df) # 16120  3400

# Save the above data set for future quick upload
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('train.df'), file = 'overall.rating.train.RData')
load("overall.rating.train.RData")
dim(train.df)  #  80603  3430


###############################################################################
#  Fit a Random Forest model to identify Important Variables
###############################################################################
install.packages("randomForest")
library("randomForest")

set.seed(123)
model.rf.1 <- randomForest(overall.hhc.rating ~ ., data=train.df
                            , mtry = 1123
                            , ntree = 100
                            , nodesize = 10
                            , nPerm = 1
                            , importance = TRUE
                            , do.trace = TRUE)

setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('model.rf.1'), file = 'model.rf.1.RData')
load("model.rf.1.RData")

# Create the plot for the top 30 Important Variables
varImpPlot(model.rf.1, sort=TRUE, n.var=min(30,nrow(model.rf.1$importance)), type=NULL, class = NULL, scale = TRUE
           , main="\nTop 30 Important Variables\nWith High Predictive Influence on Agency Ranking")

# Get all the Important Variables and save them into a .csv file.
model.rf.1.imp <- importance(model.rf.1)
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
write.csv(model.rf.1.imp,"model.rf.1.imp.csv")


