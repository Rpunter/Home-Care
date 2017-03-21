##  R Code File Name:  F3 - Lasso - Sandra.r

###############################################################################
###############################################################################
##    This R Code does the following:
##      1.  Loads the dichotomized scoring data set overall.rating.df and the 
##            non-dichotomized imputed data set providers.df.all.munged.2
##      2.  The overall score from the overall.rating.df is put into the 
##            providers.df.all.munged.2 data set by matching the CMS.Cer.Number.CCN
##            variable.
##      3.  The resulting data set has the all of the original non-dichotomized but
##            imputed data with the overall scoring.  The resulting data set is called
##                providers.df.all.munged.2.rated
##      4.  Evaluates Lasso model via several measures and graphs.
##
##  Author:   Sandra Duenas
##  Date:     2/20/2016
###############################################################################
###############################################################################

#####  The overall.rating.df data set contains the overall scoring for each CMS
####    This data set only has dichotomized fields.
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
load("overall.rating.df.RData")
dim(overall.rating.df)  # 107471   3430
df <- overall.rating.df
df.names.df <- as.data.frame(names(df))
View(df.names.df)
rm(overall.rating.df)


#########################################################
#######   This providers.df.all.munged.2 data set contains both the categorical 
##          variables with standardized names
#######     and values and the numerical variables with IMPUTED missing values.

# load munged imputed data without dichotomized categorical variables.
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
load("providers.df.all.munged.2.RData")
dim(providers.df.all.munged.2) # 135464     40
View(as.data.frame(names(providers.df.all.munged.2)))

get.provider.score <- function(CMS.number) {
  cms.score.index <- which(rating.df$CMS.Cer.Number.CCN == CMS.number)
  unique.cms.score.df <- unique(rating.df[cms.score.index,])
  provider.score <- unique.cms.score.df$overall.hhc.rating
  provider.score <- mean(provider.score)
  return (provider.score)
}

dim(providers.df.all.munged.2.rated) #135464     41
providers.df.all.munged.2.rated$overall.hhc.rating <- 0
unique(providers.df.all.munged.2.rated$overall.hhc.rating)
View(providers.df.all.munged.2.rated)

unique.CMS.Cer.Number.CCN <- unique(rating.df$CMS.Cer.Number.CCN)
unique.CMS.Cer.Number.CCN.df <- as.data.frame(unique.CMS.Cer.Number.CCN)
View(unique.CMS.Cer.Number.CCN.df)

for (n in 1:nrow(unique.CMS.Cer.Number.CCN.df)) {
  print(paste(c('n =', n)))
     CMS.Cer.Number.CCN <- unique.CMS.Cer.Number.CCN.df[n,"unique.CMS.Cer.Number.CCN"]
     CMS.Score <- get.provider.score(CMS.Cer.Number.CCN)
     cms.index <- which(providers.df.all.munged.2.rated$CMS.Cer.Number.CCN == CMS.Cer.Number.CCN)
     providers.df.all.munged.2.rated[cms.index,"overall.hhc.rating"] <- CMS.Score
}

histogram(providers.df.all.munged.2.rated$overall.hhc.rating)

setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save('providers.df.all.munged.2.rated', file = 'providers.df.all.munged.2.rated.RData')
load("providers.df.all.munged.2.rated.RData")
View(as.data.frame(names(providers.df.all.munged.2.rated)))
dim(providers.df.all.munged.2.rated)  # 135464     41



###########  PREPARE FINAL DATA USING EXCEL DUE TO COMPLICATED DATA MANIPULATION
###########   TASKS THAT WERE TAKING TOO LONG TO DO IN R.
#  Used Excel to modify providers.df.all.munged.2.rated data set to remove 
#   the year.quarter, the star rating for only 201503, and some other
#   other fields that are not needed for final analysis and preidcitve model, including
#   the two categorical measures that are new as of 201503.
#   Because the providers.df.all.munged.2.rated data set still has observations with 
#   measures having values greater than 100%, those values were changed to 0.  Then a 
#   sum of the 22 measures was created and any rows with a 0 in this sum were removed
#   from the data set, only leaving observations with measure that were originally 
#   between 0 and 100.
#   The resulting data set is stored in the Excel file: "FINAL Providers Distinct CSV v02.csv"
#   which was read below and subsequently stored in the 
#   RData file:"FINAL.Providers.Distinct.df.RData
# 
#   This is the FINAL data set containing the mean overall historical Star Rating for
#   CMS Number.  The data set has the original categorical variables that can be used
#   for Visualization in Tableau.
#
#   This FINAL data set will also be used with the Lasso technique below.
#
#########################################################
# Read the FINAL data set with the distinct CMS Numbers and historical Star Rating that
#   was prepared in Excel

# Path location of CMS data files:
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")

missing.types <- c("NA", '',"","<NA>","N/A")

FINAL.Providers.Distinct.df <- read.csv("FINAL Providers Distinct CSV v02.csv",
                                 header=TRUE,
                                 na=missing.types,
                                 sep=",",
                                 stringsAsFactors = FALSE
)

dim(FINAL.Providers.Distinct.df)  # 18172    35 - 1st attempt which was wrong.
dim(FINAL.Providers.Distinct.df)  # 19371    35 - v02 which was corrected in Excel.

any(is.na(FINAL.Providers.Distinct.df))  #  FALSE

View(as.data.frame(names(FINAL.Providers.Distinct.df)))

setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save('FINAL.Providers.Distinct.df', file = 'FINAL.Providers.Distinct.df.RData')
load("FINAL.Providers.Distinct.df.RData")

library("lattice")
histogram(FINAL.Providers.Distinct.df$overall.hhc.rating)

unique(FINAL.Providers.Distinct.df$CMS.Cer.Number.CCN)  #11,623
unique(providers.df.all.munged.2.rated$CMS.Cer.Number.CCN) # 13,362
# dropped 2,562 unique CMS Numbers by removing observations with measures having
#   values greater than 100%.


##########################################################################################
##########################################################################################
# Install the 'useful' package that has the function build.x
#   The build.x function creates a predictor Matrix for the Lasso technique that does
#     not remove the Reference Category from the dichotomized Categorical Variables.
#   Also, this function excludes the Intercept from the predictor Matrix.
##########################################################################################

library("glmnet")
library("useful")
library("ggplot2")


View(as.data.frame(names(FINAL.Providers.Distinct.df)))

# Remove these two variables prior to creating the matrix for the Lasso.
#  The variable Offers.Nursing.Care.Services only has one category and the build.x function
#   expects all categorical variables to have 2 or more categories before it dichotomizes them.
remove.vars <- c('CMS.Cer.Number.CCN'
                 , 'Offers.Nursing.Care.Services')

all.vars <- names(FINAL.Providers.Distinct.df)
keep.vars <- setdiff(all.vars,remove.vars)
dim(FINAL.Providers.Distinct.df)  # 19371    35

FINAL.Providers.Distinct.df.small <- FINAL.Providers.Distinct.df[,keep.vars]
dim(FINAL.Providers.Distinct.df.small) # 19371    33

# save a copy of the above data set for quick load if needed.
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save('FINAL.Providers.Distinct.df.small', file = 'FINAL.Providers.Distinct.df.small.RData')
load("FINAL.Providers.Distinct.df.small.RData")
View(as.data.frame(names(FINAL.Providers.Distinct.df.small)))
dim(FINAL.Providers.Distinct.df.small)  # 19371    33



##################    PREPARE THE DATA FOR THE LASSO WITH THE COMPLETE DATA SET
### Build the Predictors x matrix for the glmnet (Elastic Net) model, which is matrix
x.all <- build.x(overall.hhc.rating ~ . -1, FINAL.Providers.Distinct.df.small, contrasts = FALSE)
dim(x.all)  # 19371 rows; 14,714 variables

### Build the Response y matrix for the glmnet (Elastic Net) model, which is a numeric vector
y.all <- build.y(overall.hhc.rating ~ . -1, FINAL.Providers.Distinct.df.small)
dim(as.data.frame(y.all))  # 19371     1

### Create the TRAIN and TEST random sample indexes and the Response Vector.
# at .75 gives 14,528 rows; 
set.seed(123)

train.index <- sample(1:nrow(x.all),(0.75*nrow(x.all)))  
dim(as.data.frame(train.index))  #  14528     1

test.index <- -train.index
dim(as.data.frame(test.index))  # 14528     1

y.test <- y.all[test.index]  # from the response vector y.all, create a numeric vector with the indexes
                             # of the test.index so that prediction can be done against these observed outcomes.
dim(as.data.frame(y.test))  # 4843    1


grid <- 10^seq(10,-2,length=100)

setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save( 'x.all'
     ,'y.all'
     ,'train.index'
     ,'test.index'
     ,'y.test'
     ,'grid'
     , file = 'lasso.75.sample.prior.to.glmnet.RData')
library("useful")
library("ggplot2")
library("glmnet")
load("lasso.75.sample.prior.to.glmnet.RData")

###  Fit the Lasso model
lasso.mod <- glmnet(x.all[train.index,],y.all[train.index],alpha=1,lambda = grid)
plot(lasso.mod)

save( 'lasso.mod', file = 'lasso.mod.75.sample.all.vars.RData')
library("glmnet")
library("useful")
library("ggplot2")
load("lasso.mod.75.sample.all.vars.RData")


# Perform cross-validation on the train set
set.seed(123)
cv.out <- cv.glmnet(x.all[train.index,],y.all[train.index],alpha=1)
warnings() 
plot(cv.out)

save( 'cv.out', file = 'cv.out.75.sample.all.vars.RData')
library("glmnet")
library("useful")
library("ggplot2")
load("cv.out.75.sample.all.vars.RData")


#  The cross-validation find the lambda.min to have 2743 predictors
bestlam <- cv.out$lambda.min
bestlam  # 0.004714645 with .75 sample;

log(0.004714645)  # -5.357082 with .75 sample


#  The cross-validation find the lambda.min to have 2196 predictors
lambda.1se <- cv.out$lambda.1se
lambda.1se  # 0.004939136 with .75 sample

log(0.004939136)  # -5.310565 with .75 sample


cv.out$glmnet.fit


lasso.pred <- predict(lasso.mod, s=bestlam, newx=x.all[test.index,])
save( 'lasso.pred', file = 'lasso.pred.75.sample.all.vars.RData')
library("glmnet")
library("useful")
library("ggplot2")
load("lasso.pred.75.sample.all.vars.RData")

#### Compare the descriptive statistics of the observed outcome and the predicted outcome
# predicted outcome:
summary(lasso.pred)
# observed outcome
summary(FINAL.Providers.Distinct.df.small$overall.hhc.rating)

#### Compute the residual mean or the error mean for the predictions of the Lasso model
# the closer to zero this value is the more accurate the model is because it indicates
#   that the errors or differences between the predicted outcome and the observed outcome
#   is close to zero, which is good.
mean.sqr.root.error <- mean((lasso.pred - y.test)^2)
mean.sqr.root.error  # 0.198832 at .25 random TEST sample with all vars

# this error represents the variance around the perfect prediction, like the variance
root.mean.sqr.root.error <- sqrt(mean((lasso.pred - y.test)^2))
root.mean.sqr.root.error # 0.4459058 at .25 random TEST sample with all vars

# Fit a Lasso model with all the observations using the best
out <- glmnet(x.all, y.all, alpha = 1, lambda = grid)
out$nulldev # 9660.843 
out$nobs  # 19371
save( 'out', file = 'out.all.vars.RData')
library("glmnet")
library("useful")
library("ggplot2")
load("out.all.vars.RData")

# Computes the coefficients of the requested values for the s.
lasso.coef <- predict(out, type="coefficients", s=bestlam)[1:14715,]
lasso.coef.ne.zero <- lasso.coef[lasso.coef != 0]
write.csv(lasso.coef.ne.zero,file="lasso.coef.ne.zero.75.sample.all.vars.csv")

# this returns a list of the indices of the nonzero coefficients for each value of s.
lasso.nonzero.df <- predict(out, type="nonzero", s=bestlam)

# gets the variable name of the index from the predict with type=nonzero
names(as.data.frame.matrix(x.all))[1291]


