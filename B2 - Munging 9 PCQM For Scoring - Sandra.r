## R Code File Name;  B2 - Munging 9 PCQM For Scoring - Sandra


#############################################################################
#  REMOVE OBSERVATIONS THAT HAVE MEASURE VALUES > 100 IN THE 9 PQCMs
#   THAT ARE USED TO SCORE THE AGENCY WITH A STAR SCORE
#
#  REMOVE OBSERVATIONS WITH TOTAL SUM OF ZERO FOR THE 8 MEASURES THAT ARE
#  THE BASIS FOR THE SCORING.
#############################################################################

# Load the previously saved dichotomized and imputed data set from above
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
load("hhc.final.df.RData")
dim(hhc.final.df)  # 135464   3409

# Check all variable names
names.hhc.final.df <- names(hhc.final.df)
names.hhc.final.df <- as.data.frame(names.hhc.final.df)
View(names.hhc.final.df)

# Place the loaded data set into to temporary data for manipulation
temp.df <- hhc.final.df
rm(hhc.final.df)

############# (1) REMOVE BAD OBSERVATIONS FOR HO.HHT.began.care.in.timely.manner.18
# provide the indexes of the rows that have the measure value greater than 100 and these
# are the rows that need to be removed from the final data set

# EDA Visualization of the variable distribution with bad data
library("lattice")
histogram(temp.df$HO.HHT.began.care.in.timely.manner.18)
boxplot(temp.df$HO.HHT.began.care.in.timely.manner.18)
densityplot(temp.df$HO.HHT.began.care.in.timely.manner.18)

remove.18.indexes <- which(temp.df$HO.HHT.began.care.in.timely.manner.18 > 100)
nrow(temp.df) # 135,464 rows

# remove the bad observations by placing the good observations in a new data set
temp.df.good <- temp.df[-remove.18.indexes,]
nrow(temp.df.good) # 128,059 rows

# replace temp.df with the good observations
rm(temp.df)
temp.df <- temp.df.good
rm(temp.df.good)
rm(remove.18.indexes)

# EDA Visualization of the variable distribution withOUT bad data
histogram(temp.df$HO.HHT.began.care.in.timely.manner.18)
boxplot(temp.df$HO.HHT.began.care.in.timely.manner.18)
densityplot(temp.df$HO.HHT.began.care.in.timely.manner.18, kernel = c("gaussian"))


############# (2) REMOVE BAD OBSERVATIONS FOR HO.HHT.taught.about.their.drugs.20
# provide the indexes of the rows that have the measure value greater than 100 and these
# are the rows that need to be removed from the final data set

# EDA Visualization of the variable distribution with bad data
library("lattice")
histogram(temp.df$HO.HHT.taught.about.their.drugs.20)
boxplot(temp.df$HO.HHT.taught.about.their.drugs.20)
densityplot(temp.df$HO.HHT.taught.about.their.drugs.20)

remove.20.indexes <- which(temp.df$HO.HHT.taught.about.their.drugs.20 > 100)
nrow(temp.df) # 128,059 rows

# remove the bad observations by placing the good observations in a new data set
temp.df.good <- temp.df[-remove.20.indexes,]
nrow(temp.df.good) # 124,166 rows

# replace temp.df with the good observations
rm(temp.df)
temp.df <- temp.df.good
rm(temp.df.good)
rm(remove.20.indexes)

# EDA Visualization of the variable distribution withOUT bad data
library("lattice")
histogram(temp.df$HO.HHT.taught.about.their.drugs.20)
boxplot(temp.df$HO.HHT.taught.about.their.drugs.20)
densityplot(temp.df$HO.HHT.taught.about.their.drugs.20)



############# (3) REMOVE BAD OBSERVATIONS FOR HO.HHT.ensured.received.flu.shot.26
# provide the indexes of the rows that have the measure value greater than 100 and these
# are the rows that need to be removed from the final data set

# EDA Visualization of the variable distribution with bad data
library("lattice")
histogram(temp.df$HO.HHT.ensured.received.flu.shot.26)
boxplot(temp.df$HO.HHT.ensured.received.flu.shot.26)
densityplot(temp.df$HO.HHT.ensured.received.flu.shot.26)

remove.26.indexes <- which(temp.df$HO.HHT.ensured.received.flu.shot.26 > 100)
nrow(temp.df) # 124,166 rows

# remove the bad observations by placing the good observations in a new data set
temp.df.good <- temp.df[-remove.26.indexes,]
nrow(temp.df.good) # 123,494 rows

# replace temp.df with the good observations
rm(temp.df)
temp.df <- temp.df.good
rm(temp.df.good)
rm(remove.26.indexes)

# EDA Visualization of the variable distribution withOUT bad data
library("lattice")
histogram(temp.df$HO.HHT.ensured.received.flu.shot.26)
boxplot(temp.df$HO.HHT.ensured.received.flu.shot.26)
densityplot(temp.df$HO.HHT.ensured.received.flu.shot.26)



############# (4) REMOVE BAD OBSERVATIONS FOR HO.PAT.got.better.at.moving.around.44
# provide the indexes of the rows that have the measure value greater than 100 and these
# are the rows that need to be removed from the final data set

# EDA Visualization of the variable distribution with bad data
library("lattice")
histogram(temp.df$HO.PAT.got.better.at.moving.around.44)
boxplot(temp.df$HO.PAT.got.better.at.moving.around.44)
densityplot(temp.df$HO.PAT.got.better.at.moving.around.44)

remove.44.indexes <- which(temp.df$HO.PAT.got.better.at.moving.around.44 > 100)
nrow(temp.df) # 123,494 rows

# remove the bad observations by placing the good observations in a new data set
temp.df.good <- temp.df[-remove.44.indexes,]
nrow(temp.df.good) # 122,620 rows

# replace temp.df with the good observations
rm(temp.df)
temp.df <- temp.df.good
rm(temp.df.good)
rm(remove.44.indexes)

# EDA Visualization of the variable distribution withOUT bad data
library("lattice")
histogram(temp.df$HO.PAT.got.better.at.moving.around.44)
boxplot(temp.df$HO.PAT.got.better.at.moving.around.44)
densityplot(temp.df$HO.PAT.got.better.at.moving.around.44)



############# (5) REMOVE BAD OBSERVATIONS FOR HO.PAT.got.better.at.getting.in.and.out.of.bed.46
# provide the indexes of the rows that have the measure value greater than 100 and these
# are the rows that need to be removed from the final data set

# EDA Visualization of the variable distribution with bad data
library("lattice")
histogram(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46)
boxplot(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46)
densityplot(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46)

remove.46.indexes <- which(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 > 100)
nrow(temp.df) # 122,620 rows

# remove the bad observations by placing the good observations in a new data set
temp.df.good <- temp.df[-remove.46.indexes,]
nrow(temp.df.good) # 122,029 rows

# replace temp.df with the good observations
rm(temp.df)
temp.df <- temp.df.good
rm(temp.df.good)
rm(remove.46.indexes)

# EDA Visualization of the variable distribution withOUT bad data
library("lattice")
histogram(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46)
boxplot(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46)
densityplot(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46)



############# (6) REMOVE BAD OBSERVATIONS FOR HO.PAT..got.better.at.bathing.48
# provide the indexes of the rows that have the measure value greater than 100 and these
# are the rows that need to be removed from the final data set

# EDA Visualization of the variable distribution with bad data
library("lattice")
histogram(temp.df$HO.PAT..got.better.at.bathing.48)
boxplot(temp.df$HO.PAT..got.better.at.bathing.48)
densityplot(temp.df$HO.PAT..got.better.at.bathing.48)

remove.48.indexes <- which(temp.df$HO.PAT..got.better.at.bathing.48 > 100)
nrow(temp.df) # 122,029 rows

# remove the bad observations by placing the good observations in a new data set
temp.df.good <- temp.df[-remove.48.indexes,]
nrow(temp.df.good) # 122,024 rows

# replace temp.df with the good observations
rm(temp.df)
temp.df <- temp.df.good
rm(temp.df.good)
rm(remove.48.indexes)

# EDA Visualization of the variable distribution withOUT bad data
library("lattice")
histogram(temp.df$HO.PAT..got.better.at.bathing.48)
boxplot(temp.df$HO.PAT..got.better.at.bathing.48)
densityplot(temp.df$HO.PAT..got.better.at.bathing.48)



############# (7) REMOVE BAD OBSERVATIONS FOR HO.PAT.had.less.pain.when.moving.around.50
# provide the indexes of the rows that have the measure value greater than 100 and these
# are the rows that need to be removed from the final data set

# EDA Visualization of the variable distribution with bad data
library("lattice")
histogram(temp.df$HO.PAT.had.less.pain.when.moving.around.50)
boxplot(temp.df$HO.PAT.had.less.pain.when.moving.around.50)
densityplot(temp.df$HO.PAT.had.less.pain.when.moving.around.50)

remove.50.indexes <- which(temp.df$HO.PAT.had.less.pain.when.moving.around.50 > 100)
nrow(temp.df) # 122,024 rows

# remove the bad observations by placing the good observations in a new data set
temp.df.good <- temp.df[-remove.50.indexes,]
nrow(temp.df.good) # 121,817 rows

# replace temp.df with the good observations
rm(temp.df)
temp.df <- temp.df.good
rm(temp.df.good)
rm(remove.50.indexes)

# EDA Visualization of the variable distribution withOUT bad data
library("lattice")
histogram(temp.df$HO.PAT.had.less.pain.when.moving.around.50)
boxplot(temp.df$HO.PAT.had.less.pain.when.moving.around.50)
densityplot(temp.df$HO.PAT.had.less.pain.when.moving.around.50)



############# (8) REMOVE BAD OBSERVATIONS FOR HO.PAT.breathing.improved.52
# provide the indexes of the rows that have the measure value greater than 100 and these
# are the rows that need to be removed from the final data set

# EDA Visualization of the variable distribution with bad data
library("lattice")
histogram(temp.df$HO.PAT.breathing.improved.52)
boxplot(temp.df$HO.PAT.breathing.improved.52)
densityplot(temp.df$HO.PAT.breathing.improved.52)

remove.52.indexes <- which(temp.df$HO.PAT.breathing.improved.52 > 100)
nrow(temp.df) # 121,817 rows

# remove the bad observations by placing the good observations in a new data set
temp.df.good <- temp.df[-remove.52.indexes,]
nrow(temp.df.good) # 121,217 rows

# replace temp.df with the good observations
rm(temp.df)
temp.df <- temp.df.good
rm(temp.df.good)
rm(remove.52.indexes)


############# (9) REMOVE BAD OBSERVATIONS FOR HO.PAT.had.to.be.admitted.to.the.hospital.60
# provide the indexes of the rows that have the measure value greater than 100 and these
# are the rows that need to be removed from the final data set

# EDA Visualization of the variable distribution with bad data
library("lattice")
histogram(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60)
boxplot(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60)
densityplot(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60)

remove.60.indexes <- which(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 > 100)
nrow(temp.df) # 121,217 rows

# remove the bad observations by placing the good observations in a new data set
temp.df.good <- temp.df[-remove.60.indexes,]
nrow(temp.df.good) # 120,226 rows

# replace temp.df with the good observations
rm(temp.df)
temp.df <- temp.df.good
rm(temp.df.good)
rm(remove.60.indexes)

# EDA Visualization of the variable distribution withOUT bad data
library("lattice")
histogram(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60)
boxplot(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60)
densityplot(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60)


##  QA Good data set
dim(temp.df)  # number of rows = 120,226; number of variables = 3,409

# Rename data set
rm(hhc.final.for.scoring.df)
hhc.final.for.scoring.df <- temp.df
dim(hhc.final.for.scoring.df)
rm(temp.df)


###  SAVE THE FINALIZED DATA SET WITH THE DICHOTOMIZED CATEGORICAL VARIABLES AND IMPUTED NUMERIC VARIABLES
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('hhc.final.for.scoring.df'), file = 'hhc.final.for.scoring.df.RData')
load("hhc.final.for.scoring.df.RData")
nrow(hhc.final.for.scoring.df)

hhc.final.for.scoring.df.names <- names(hhc.final.for.scoring.df)
hhc.final.for.scoring.df.names.df <- as.data.frame(hhc.final.for.scoring.df.names)
View(hhc.final.for.scoring.df.names.df)


# from 135,464 rows down to 120,226 rows = 15,238 or a 11.24% decrease



########################################################################
# CREATE INDICATOR FOR WHETHER AN OBSERVATION HAS SUM OF ZERO FOR THE 8
# MEASURE TO BE USED FOR SCORING AN AGENCY
# REMOVE OBSERVATIONS WITH SUM OF 0 FOR THE 8 MEASURES.
########################################################################

# Create indicator by summing the 8 measures that are basis for the Scoring
hhc.final.for.scoring.df$eight.measures.sum <- I(hhc.final.for.scoring.df$HO.HHT.began.care.in.timely.manner.18
                       + hhc.final.for.scoring.df$HO.HHT.taught.about.their.drugs.20
                       + hhc.final.for.scoring.df$HO.HHT.ensured.received.flu.shot.26
                       + hhc.final.for.scoring.df$HO.PAT.got.better.at.moving.around.44
                       + hhc.final.for.scoring.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46
                       + hhc.final.for.scoring.df$HO.PAT..got.better.at.bathing.48
                       + hhc.final.for.scoring.df$HO.PAT.had.less.pain.when.moving.around.50
                       + hhc.final.for.scoring.df$HO.PAT.breathing.improved.52
                       + hhc.final.for.scoring.df$HO.PAT.had.to.be.admitted.to.the.hospital.60)

#  Flag an observations with a 0 or 1 to indicate whether the sum of the 8 measures is 0 or greater than 0.
hhc.final.for.scoring.df$eight.measures.flag <- ifelse(hhc.final.for.scoring.df$eight.measures.sum > 0, 1, 0)
summary(hhc.final.for.scoring.df$eight.measures.flag)

# Create a data set with a shorter name for easiness of coding below.
scoring.df <- hhc.final.for.scoring.df
rm(hhc.final.for.scoring.df)
dim(scoring.df)
names(scoring.df)

#  QA of the data to identify why some measures with a Flag of 1 still have zero in one or more
#    of the 8 measures.  The finding is that these observations are ok to keep.
library("dplyr")
x <- scoring.df %>%
  select (  eight.measures.flag
          , HO.HHT.began.care.in.timely.manner.18, HO.HHT.taught.about.their.drugs.20
          , HO.HHT.ensured.received.flu.shot.26, HO.PAT.got.better.at.moving.around.44
          , HO.PAT.got.better.at.getting.in.and.out.of.bed.46, HO.PAT..got.better.at.bathing.48
          , HO.PAT.had.less.pain.when.moving.around.50, HO.PAT.breathing.improved.52
          , year.quarter201301, year.quarter201302, year.quarter201303, year.quarter201304                                     
          , year.quarter201401, year.quarter201402, year.quarter201403, year.quarter201404                                     
          , year.quarter201501, year.quarter201502, year.quarter201503) %>%
  filter(eight.measures.flag == 1 & HO.HHT.began.care.in.timely.manner.18 == 0)

View(x)

#  Create a data set with only the observations that have been flat as one.  These observations
#   have at least one of the 8 measures populated and thus can be used to create a scoring.
flag.1.df <- subset(scoring.df, scoring.df$eight.measures.flag == 1)
dim(flag.1.df)  # 107471   3411

# Removing the two variables used to identify observations with at least one of the 9 measures 
#   having a value > 0.
library("dplyr")
flag.1.df <- select(flag.1.df, -eight.measures.sum, -eight.measures.flag)
dim(flag.1.df)  # 107471   3409

flag.1.df.names <- names(flag.1.df)
flag.1.df.names.df <- as.data.frame(flag.1.df.names)
View(flag.1.df.names.df)
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files")
write.csv(flag.1.df.names.df,"flag.1.df.names.df.csv")

###  SAVE THE FINALIZED DATA SET WITHOUT OUTLIERS AND OBSERVATIONS THAT HAVE AT LEAST ONE OF THE
###  EIGHT MEASURES WITH A VALUE > 0.
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('flag.1.df'), file = 'flag.1.df.RData')
load("flag.1.df.RData")


