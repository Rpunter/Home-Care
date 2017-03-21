## R Code File Name:  C1 - EDA for Munged Data - Sandra.r


#######################################################################################
##  This R Code creates the R plots (Histogram, Density Plot, Box Plot, and QQ Plot)
##    for the 9 Patient Care Quality Measures
#######################################################################################


###  Load data set completely prepared to Visualize the 9 Patient Care Quality Measures
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
load("flag.1.df.RData")

# Code to put all 3,409 variable names into a data frame and be able to 
#  easily view the variable names.
names.flag.1.df <- names(flag.1.df)
names.flag.1.df <- as.data.frame(names.flag.1.df) 
View(names.flag.1.df)
dim(names.flag.1.df)  # 3409    1

names.9.vars <- c("HO.HHT.began.care.in.timely.manner.18"
                  , "HO.HHT.taught.about.their.drugs.20"
                  , "HO.HHT.ensured.received.flu.shot.26"
                  , "HO.PAT.got.better.at.moving.around.44"
                  , "HO.PAT.got.better.at.getting.in.and.out.of.bed.46"
                  , "HO.PAT..got.better.at.bathing.48"
                  , "HO.PAT.had.less.pain.when.moving.around.50"
                  , "HO.PAT.breathing.improved.52"
                  , "HO.PAT.had.to.be.admitted.to.the.hospital.60")
names.9.vars

library("dplyr")
flag.1.9.meas.df <- flag.1.df[names.9.vars]
class(flag.1.9.meas.df)
str(flag.1.9.meas.df)
dim(flag.1.9.meas.df)  #  107471      9
View(flag.1.9.meas.df)

flag.1.9.meas.summary <- my.summary(flag.1.9.meas.df)
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
write.csv(flag.1.9.meas.summary,"flag.1.9.meas.summary.csv")


############# (1) EDA VISUALIZATION FOR HO.HHT.began.care.in.timely.manner.18
library("lattice")
histogram(flag.1.df$HO.HHT.began.care.in.timely.manner.18
          , main="HO.HHT.began.care.in.timely.manner.18")
boxplot(flag.1.df$HO.HHT.began.care.in.timely.manner.18
        , main="HO.HHT.began.care.in.timely.manner.18")
densityplot(flag.1.df$HO.HHT.began.care.in.timely.manner.18
            , main="HO.HHT.began.care.in.timely.manner.18")
qqnorm(flag.1.df$HO.HHT.began.care.in.timely.manner.18
            , main="HO.HHT.began.care.in.timely.manner.18")
qqline(flag.1.df$HO.HHT.began.care.in.timely.manner.18, col = 2)



############# (2) EDA VISUALIZATION FOR HO.HHT.taught.about.their.drugs.20
histogram(flag.1.df$HO.HHT.taught.about.their.drugs.20
          , main="HO.HHT.taught.about.their.drugs.20")
boxplot(flag.1.df$HO.HHT.taught.about.their.drugs.20
          , main="HO.HHT.taught.about.their.drugs.20")
densityplot(flag.1.df$HO.HHT.taught.about.their.drugs.20
            , main="HO.HHT.taught.about.their.drugs.20")
qqnorm(flag.1.df$HO.HHT.taught.about.their.drugs.20
       , main="HO.HHT.taught.about.their.drugs.20")
qqline(flag.1.df$HO.HHT.taught.about.their.drugs.20, col = 2)



############# (3) EDA VISUALIZATION FOR HO.HHT.ensured.received.flu.shot.26
histogram(flag.1.df$HO.HHT.ensured.received.flu.shot.26
          , main="HO.HHT.ensured.received.flu.shot.26")
boxplot(flag.1.df$HO.HHT.ensured.received.flu.shot.26
        , main="HO.HHT.ensured.received.flu.shot.26")
densityplot(flag.1.df$HO.HHT.ensured.received.flu.shot.26
            , main="HO.HHT.ensured.received.flu.shot.26")
qqnorm(flag.1.df$HO.HHT.ensured.received.flu.shot.26
       , main="HO.HHT.ensured.received.flu.shot.26")
qqline(flag.1.df$HO.HHT.ensured.received.flu.shot.26, col = 2)



############# (4) EDA VISUALIZATION FOR HO.PAT.got.better.at.moving.around.44
histogram(flag.1.df$HO.PAT.got.better.at.moving.around.44
          , main="HO.PAT.got.better.at.moving.around.44")
boxplot(flag.1.df$HO.PAT.got.better.at.moving.around.44
        , main="HO.PAT.got.better.at.moving.around.44")
densityplot(flag.1.df$HO.PAT.got.better.at.moving.around.44
            , main="HO.PAT.got.better.at.moving.around.44")
qqnorm(flag.1.df$HO.PAT.got.better.at.moving.around.44
       , main="HO.PAT.got.better.at.moving.around.44")
qqline(flag.1.df$HO.PAT.got.better.at.moving.around.44, col = 2)



############# (5) EDA VISUALIZATION FOR HO.PAT.got.better.at.getting.in.and.out.of.bed.46
histogram(flag.1.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46
          , main="HO.PAT.got.better.at.getting.in.and.out.of.bed.46")
boxplot(flag.1.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46
        , main="HO.PAT.got.better.at.getting.in.and.out.of.bed.46")
densityplot(flag.1.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46
            , main="HO.PAT.got.better.at.getting.in.and.out.of.bed.46")
qqnorm(flag.1.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46
       , main="HO.PAT.got.better.at.getting.in.and.out.of.bed.46")
qqline(flag.1.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46, col = 2)



############# (6) EDA VISUALIZATION FOR HO.PAT..got.better.at.bathing.48
histogram(flag.1.df$HO.PAT..got.better.at.bathing.48
          , main="HO.PAT..got.better.at.bathing.48")
boxplot(flag.1.df$HO.PAT..got.better.at.bathing.48
          , main="HO.PAT..got.better.at.bathing.48")
densityplot(flag.1.df$HO.PAT..got.better.at.bathing.48
          , main="HO.PAT..got.better.at.bathing.48")
qqnorm(flag.1.df$HO.PAT..got.better.at.bathing.48
       , main="HO.PAT..got.better.at.bathing.48")
qqline(flag.1.df$HO.PAT..got.better.at.bathing.48, col = 2)



############# (7) EDA VISUALIZATION FOR HO.PAT.had.less.pain.when.moving.around.50
histogram(flag.1.df$HO.PAT.had.less.pain.when.moving.around.50
          , main="HO.PAT.had.less.pain.when.moving.around.50")
boxplot(flag.1.df$HO.PAT.had.less.pain.when.moving.around.50
        , main="HO.PAT.had.less.pain.when.moving.around.50")
densityplot(flag.1.df$HO.PAT.had.less.pain.when.moving.around.50
            , main="HO.PAT.had.less.pain.when.moving.around.50")
qqnorm(flag.1.df$HO.PAT.had.less.pain.when.moving.around.50
       , main="HO.PAT.had.less.pain.when.moving.around.50")
qqline(flag.1.df$HO.PAT.had.less.pain.when.moving.around.50, col = 2)



############# (8) EDA VISUALIZATION FOR HO.PAT.breathing.improved.52
histogram(flag.1.df$HO.PAT.breathing.improved.52
          , main="HO.PAT.breathing.improved.52")
boxplot(flag.1.df$HO.PAT.breathing.improved.52
          , main="HO.PAT.breathing.improved.52")
densityplot(flag.1.df$HO.PAT.breathing.improved.52
          , main="HO.PAT.breathing.improved.52")
qqnorm(flag.1.df$HO.PAT.breathing.improved.52
       , main="HO.PAT.breathing.improved.52")
qqline(flag.1.df$HO.PAT.breathing.improved.52, col = 2)



############# (9) EDA VISUALIZATION FOR HO.PAT.had.to.be.admitted.to.the.hospital.60
histogram(flag.1.df$HO.PAT.had.to.be.admitted.to.the.hospital.60
          , main="HO.PAT.had.to.be.admitted.to.the.hospital.60")
boxplot(flag.1.df$HO.PAT.had.to.be.admitted.to.the.hospital.60
        , main="HO.PAT.had.to.be.admitted.to.the.hospital.60")
densityplot(flag.1.df$HO.PAT.had.to.be.admitted.to.the.hospital.60
            , main="HO.PAT.had.to.be.admitted.to.the.hospital.60")
qqnorm(flag.1.df$HO.PAT.had.to.be.admitted.to.the.hospital.60
       , main="HO.PAT.had.to.be.admitted.to.the.hospital.60")
qqline(flag.1.df$HO.PAT.had.to.be.admitted.to.the.hospital.60, col = 2)


