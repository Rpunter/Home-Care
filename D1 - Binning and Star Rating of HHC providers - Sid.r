##   R Code File Name:  D1 - Binning and Star Rating of HHC providers - Sid.r


############################################################################################################
# Create Quality Star Rating of Home Health Care Providers based on 9 measures obtained from Survey results
############################################################################################################


# Load THE DATA SET WITH THE DICHOTOMIZED CATEGORICAL VARIABLES AND IMPUTED NUMERIC VARIABLES WITH VALUES GREATER THAN 100
setwd("C:/Users/Siddartha Sathyanara/Dropbox/Team Project/CMS Data Files/RData Files")

load("flag.1.df.RData")
temp.df <- flag.1.df

#check all variable names
names.temp.df <- names(temp.df)
names.temp.df <- as.data.frame(names.temp.df)
View(names.temp.df)

#Create Initial Decile Rating for HO.HHT.began.care.in.timely.manner.18
temp.df$IDR.SCORE.18 <- ifelse(temp.df$HO.HHT.began.care.in.timely.manner.18 <= 100 & temp.df$HO.HHT.began.care.in.timely.manner.18 >= 98.7, 5.0,
                          ifelse(temp.df$HO.HHT.began.care.in.timely.manner.18 < 98.7 & temp.df$HO.HHT.began.care.in.timely.manner.18 >= 97.3, 4.5,
                                 ifelse(temp.df$HO.HHT.began.care.in.timely.manner.18 < 97.3 & temp.df$HO.HHT.began.care.in.timely.manner.18 >= 96.0, 4.0,
                                        ifelse(temp.df$HO.HHT.began.care.in.timely.manner.18 < 96.0 & temp.df$HO.HHT.began.care.in.timely.manner.18 >= 94.5, 3.5,
                                               ifelse(temp.df$HO.HHT.began.care.in.timely.manner.18 < 94.5 & temp.df$HO.HHT.began.care.in.timely.manner.18 >= 92.9, 3.0,
                                                      ifelse(temp.df$HO.HHT.began.care.in.timely.manner.18 < 92.9 & temp.df$HO.HHT.began.care.in.timely.manner.18 >= 91.1, 2.5,
                                                             ifelse(temp.df$HO.HHT.began.care.in.timely.manner.18 < 91.1 & temp.df$HO.HHT.began.care.in.timely.manner.18 >= 88.9, 2.0,
                                                                    ifelse(temp.df$HO.HHT.began.care.in.timely.manner.18 < 88.9 & temp.df$HO.HHT.began.care.in.timely.manner.18 >= 85.5, 1.5,
                                                                           ifelse(temp.df$HO.HHT.began.care.in.timely.manner.18 < 85.5 & temp.df$HO.HHT.began.care.in.timely.manner.18 >= 79.7, 1.0,
                                                                                  ifelse(temp.df$HO.HHT.began.care.in.timely.manner.18 < 79.7, 0.5,0))))))))))
summary(temp.df$IDR.SCORE.18)

#Create Initial Decile Rating for HO.HHT.taught.about.their.drugs.20
temp.df$IDR.SCORE.20 <- ifelse(temp.df$HO.HHT.taught.about.their.drugs.20 == 100.0, 5.0,
                               ifelse(temp.df$HO.HHT.taught.about.their.drugs.20 < 100.0 & temp.df$HO.HHT.taught.about.their.drugs.20 >= 99.9, 4.5,
                                      ifelse(temp.df$HO.HHT.taught.about.their.drugs.20 < 99.9 & temp.df$HO.HHT.taught.about.their.drugs.20 >= 98.9, 4.0,
                                             ifelse(temp.df$HO.HHT.taught.about.their.drugs.20 < 98.9 & temp.df$HO.HHT.taught.about.their.drugs.20 >= 97.1, 3.5,
                                                    ifelse(temp.df$HO.HHT.taught.about.their.drugs.20 < 97.1 & temp.df$HO.HHT.taught.about.their.drugs.20 >= 95.8, 3.0,
                                                           ifelse(temp.df$HO.HHT.taught.about.their.drugs.20 < 95.8 & temp.df$HO.HHT.taught.about.their.drugs.20 >= 94.1, 2.5,
                                                                  ifelse(temp.df$HO.HHT.taught.about.their.drugs.20 < 94.1 & temp.df$HO.HHT.taught.about.their.drugs.20 >= 91.7, 2.0,
                                                                         ifelse(temp.df$HO.HHT.taught.about.their.drugs.20 < 91.7 & temp.df$HO.HHT.taught.about.their.drugs.20 >= 87.8, 1.5,
                                                                                ifelse(temp.df$HO.HHT.taught.about.their.drugs.20 < 87.8 & temp.df$HO.HHT.taught.about.their.drugs.20 >= 79.7, 1.0,
                                                                                       ifelse(temp.df$HO.HHT.taught.about.their.drugs.20 < 79.7, 0.5,0))))))))))
summary(temp.df$IDR.SCORE.20)

#Create Initial Decile Rating for HO.HHT.ensured.received.flu.shot.26
temp.df$IDR.SCORE.26 <- ifelse(temp.df$HO.HHT.ensured.received.flu.shot.26 <= 100.0 & temp.df$HO.HHT.ensured.received.flu.shot.26 >= 92.3, 5.0,
                               ifelse(temp.df$HO.HHT.ensured.received.flu.shot.26 < 92.3 & temp.df$HO.HHT.ensured.received.flu.shot.26 >= 86.6, 4.5,
                                      ifelse(temp.df$HO.HHT.ensured.received.flu.shot.26 < 86.6 & temp.df$HO.HHT.ensured.received.flu.shot.26 >= 82.5, 4.0,
                                             ifelse(temp.df$HO.HHT.ensured.received.flu.shot.26 < 82.5 & temp.df$HO.HHT.ensured.received.flu.shot.26 >= 79.1, 3.5,
                                                    ifelse(temp.df$HO.HHT.ensured.received.flu.shot.26 < 79.1 & temp.df$HO.HHT.ensured.received.flu.shot.26 >= 75.5, 3.0,
                                                           ifelse(temp.df$HO.HHT.ensured.received.flu.shot.26 < 75.5 & temp.df$HO.HHT.ensured.received.flu.shot.26 >= 71.4, 2.5,
                                                                  ifelse(temp.df$HO.HHT.ensured.received.flu.shot.26 < 71.4 & temp.df$HO.HHT.ensured.received.flu.shot.26 >= 66.2, 2.0,
                                                                         ifelse(temp.df$HO.HHT.ensured.received.flu.shot.26 < 66.2 & temp.df$HO.HHT.ensured.received.flu.shot.26 >= 58.2, 1.5,
                                                                                ifelse(temp.df$HO.HHT.ensured.received.flu.shot.26 < 58.2 & temp.df$HO.HHT.ensured.received.flu.shot.26 >= 44.1, 1.0,
                                                                                       ifelse(temp.df$HO.HHT.ensured.received.flu.shot.26 < 44.1, 0.5,0))))))))))
summary(temp.df$IDR.SCORE.26)

#Create Initial Decile Rating for HO.PAT.got.better.at.moving.around.44
temp.df$IDR.SCORE.44 <- ifelse(temp.df$HO.PAT.got.better.at.moving.around.44 <= 100.0 & temp.df$HO.PAT.got.better.at.moving.around.44 >= 74.1, 5.0,
                               ifelse(temp.df$HO.PAT.got.better.at.moving.around.44 < 74.1 & temp.df$HO.PAT.got.better.at.moving.around.44 >= 68.8, 4.5,
                                      ifelse(temp.df$HO.PAT.got.better.at.moving.around.44 < 68.8 & temp.df$HO.PAT.got.better.at.moving.around.44 >= 65.6, 4.0,
                                             ifelse(temp.df$HO.PAT.got.better.at.moving.around.44 < 65.6 & temp.df$HO.PAT.got.better.at.moving.around.44 >= 62.8, 3.5,
                                                    ifelse(temp.df$HO.PAT.got.better.at.moving.around.44 < 62.8 & temp.df$HO.PAT.got.better.at.moving.around.44 >= 60.5, 3.0,
                                                           ifelse(temp.df$HO.PAT.got.better.at.moving.around.44 < 75.5 & temp.df$HO.PAT.got.better.at.moving.around.44 >= 57.9, 2.5,
                                                                  ifelse(temp.df$HO.PAT.got.better.at.moving.around.44 < 71.4 & temp.df$HO.PAT.got.better.at.moving.around.44 >= 54.7, 2.0,
                                                                         ifelse(temp.df$HO.PAT.got.better.at.moving.around.44 < 66.2 & temp.df$HO.PAT.got.better.at.moving.around.44 >= 50.1, 1.5,
                                                                                ifelse(temp.df$HO.PAT.got.better.at.moving.around.44 < 58.2 & temp.df$HO.PAT.got.better.at.moving.around.44 >= 43.4, 1.0,
                                                                                       ifelse(temp.df$HO.PAT.got.better.at.moving.around.44 < 43.4, 0.5,0))))))))))
summary(temp.df$IDR.SCORE.44)

#Create Initial Decile Rating for HO.PAT.got.better.at.getting.in.and.out.of.bed.46
temp.df$IDR.SCORE.46 <- ifelse(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 <= 100.0 & temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 >= 71.0, 5.0,
                               ifelse(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 < 71.0 & temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 >= 65.3, 4.5,
                                      ifelse(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 < 65.3 & temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 >= 61.7, 4.0,
                                             ifelse(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 < 61.7 & temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 >= 58.6, 3.5,
                                                    ifelse(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 < 58.6 & temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 >= 55.5, 3.0,
                                                           ifelse(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 < 55.5 & temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 >= 52.4, 2.5,
                                                                  ifelse(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 < 52.4 & temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 >= 48.2, 2.0,
                                                                         ifelse(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 < 48.2 & temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 >= 42.9, 1.5,
                                                                                ifelse(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 < 42.9 & temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 >= 35.0, 1.0,
                                                                                       ifelse(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 < 35.0, 0.5,0))))))))))
summary(temp.df$IDR.SCORE.46)

#Create Initial Decile Rating for HO.PAT..got.better.at.bathing.48
temp.df$IDR.SCORE.48 <- ifelse(temp.df$HO.PAT..got.better.at.bathing.48 <= 100.0 & temp.df$HO.PAT..got.better.at.bathing.48 >= 80.8, 5.0,
                               ifelse(temp.df$HO.PAT..got.better.at.bathing.48 < 80.8 & temp.df$HO.PAT..got.better.at.bathing.48 >= 75.4, 4.5,
                                      ifelse(temp.df$HO.PAT..got.better.at.bathing.48 < 75.4 & temp.df$HO.PAT..got.better.at.bathing.48 >= 72.0, 4.0,
                                             ifelse(temp.df$HO.PAT..got.better.at.bathing.48 < 72.0 & temp.df$HO.PAT..got.better.at.bathing.48 >= 69.1, 3.5,
                                                    ifelse(temp.df$HO.PAT..got.better.at.bathing.48 < 69.1 & temp.df$HO.PAT..got.better.at.bathing.48 >= 66.4, 3.0,
                                                           ifelse(temp.df$HO.PAT..got.better.at.bathing.48 < 66.4 & temp.df$HO.PAT..got.better.at.bathing.48 >= 63.3, 2.5,
                                                                  ifelse(temp.df$HO.PAT..got.better.at.bathing.48 < 63.3 & temp.df$HO.PAT..got.better.at.bathing.48 >= 59.6, 2.0,
                                                                         ifelse(temp.df$HO.PAT..got.better.at.bathing.48 < 59.6 & temp.df$HO.PAT..got.better.at.bathing.48 >= 54.6, 1.5,
                                                                                ifelse(temp.df$HO.PAT..got.better.at.bathing.48 < 54.6 & temp.df$HO.PAT..got.better.at.bathing.48 >= 45.8, 1.0,
                                                                                       ifelse(temp.df$HO.PAT..got.better.at.bathing.48 < 45.8, 0.5,0))))))))))

summary(temp.df$IDR.SCORE.48)

#Create Initial Decile Rating for HO.PAT.had.less.pain.when.moving.around.50
temp.df$IDR.SCORE.50 <- ifelse(temp.df$HO.PAT.had.less.pain.when.moving.around.50 <= 100.0 & temp.df$HO.PAT.had.less.pain.when.moving.around.50 >= 86.7, 5.0,
                               ifelse(temp.df$HO.PAT.had.less.pain.when.moving.around.50 < 86.7 & temp.df$HO.PAT.had.less.pain.when.moving.around.50 >= 78.8, 4.5,
                                      ifelse(temp.df$HO.PAT.had.less.pain.when.moving.around.50 < 78.8 & temp.df$HO.PAT.had.less.pain.when.moving.around.50 >= 73.8, 4.0,
                                             ifelse(temp.df$HO.PAT.had.less.pain.when.moving.around.50 < 73.8 & temp.df$HO.PAT.had.less.pain.when.moving.around.50 >= 69.9, 3.5,
                                                    ifelse(temp.df$HO.PAT.had.less.pain.when.moving.around.50 < 69.9 & temp.df$HO.PAT.had.less.pain.when.moving.around.50 >= 66.5, 3.0,
                                                           ifelse(temp.df$HO.PAT.had.less.pain.when.moving.around.50 < 66.5 & temp.df$HO.PAT.had.less.pain.when.moving.around.50 >= 63.1, 2.5,
                                                                  ifelse(temp.df$HO.PAT.had.less.pain.when.moving.around.50 < 63.1 & temp.df$HO.PAT.had.less.pain.when.moving.around.50 >= 59.3, 2.0,
                                                                         ifelse(temp.df$HO.PAT.had.less.pain.when.moving.around.50 < 59.3 & temp.df$HO.PAT.had.less.pain.when.moving.around.50 >= 53.8, 1.5,
                                                                                ifelse(temp.df$HO.PAT.had.less.pain.when.moving.around.50 < 53.8 & temp.df$HO.PAT.had.less.pain.when.moving.around.50 >= 43.8, 1.0,
                                                                                       ifelse(temp.df$HO.PAT.had.less.pain.when.moving.around.50 < 43.8, 0.5,0))))))))))

summary(temp.df$IDR.SCORE.50)

#Create Initial Decile Rating for HO.PAT.breathing.improved.52
temp.df$IDR.SCORE.52 <- ifelse(temp.df$HO.PAT.breathing.improved.52 <= 100.0 & temp.df$HO.PAT.breathing.improved.52 >= 80.4, 5.0,
                               ifelse(temp.df$HO.PAT.breathing.improved.52 < 80.4 & temp.df$HO.PAT.breathing.improved.52 >= 75.1, 4.5,
                                      ifelse(temp.df$HO.PAT.breathing.improved.52 < 75.1 & temp.df$HO.PAT.breathing.improved.52 >= 71.2, 4.0,
                                             ifelse(temp.df$HO.PAT.breathing.improved.52 < 71.2 & temp.df$HO.PAT.breathing.improved.52 >= 67.8, 3.5,
                                                    ifelse(temp.df$HO.PAT.breathing.improved.52 < 67.8 & temp.df$HO.PAT.breathing.improved.52 >= 64.2, 3.0,
                                                           ifelse(temp.df$HO.PAT.breathing.improved.52 < 64.2 & temp.df$HO.PAT.breathing.improved.52 >= 60.0, 2.5,
                                                                  ifelse(temp.df$HO.PAT.breathing.improved.52 < 60.0 & temp.df$HO.PAT.breathing.improved.52 >= 54.4, 2.0,
                                                                         ifelse(temp.df$HO.PAT.breathing.improved.52 < 54.4 & temp.df$HO.PAT.breathing.improved.52 >= 46.2, 1.5,
                                                                                ifelse(temp.df$HO.PAT.breathing.improved.52 < 46.2 & temp.df$HO.PAT.breathing.improved.52 >= 33.1, 1.0,
                                                                                       ifelse(temp.df$HO.PAT.breathing.improved.52 < 33.1, 0.5,0))))))))))

summary(temp.df$IDR.SCORE.52)

#Create Initial Decile Rating for HO.PAT.had.to.be.admitted.to.the.hospital.60
temp.df$IDR.SCORE.60 <- ifelse(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 <= 100.0 & temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 >= 20.1, 0.5,
                               ifelse(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 < 20.1 & temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 >= 18.3, 1.0,
                                      ifelse(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 < 18.3 & temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 >= 17.1, 1.5,
                                             ifelse(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 < 17.1 & temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 >= 16.2, 2.0,
                                                    ifelse(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 < 16.2 & temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 >= 15.3, 2.5,
                                                           ifelse(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 < 15.3 & temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 >= 14.4, 3.0,
                                                                  ifelse(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 < 14.4 & temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 >= 13.4, 3.5,
                                                                         ifelse(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 < 13.4 & temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 >= 11.9, 4.0,
                                                                                ifelse(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 < 11.9 & temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 >= 10.0, 4.5,
                                                                                       ifelse(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 < 10.0, 5.0,0))))))))))
summary(temp.df$IDR.SCORE.60)

Ini.Decile.Rating.df <- temp.df

#Recheck all variable names
names.Ini.Decile.Rating.df <- names(Ini.Decile.Rating.df)
names.temp.df <- as.data.frame(names.Ini.Decile.Rating.df)
View(names.temp.df)


###  SAVE THE DATASET WITH INITIAL DECILE RATING FOR EACH MEASURE TO BE USED FOR QUALITY STAR RATING
setwd("C:/Users/Siddartha Sathyanara/Dropbox/Team Project/CMS Data Files/RData Files")
save(list = c('Ini.Decile.Rating.df'), file = 'Ini.Decile.Rating.df.RData')

library(plyr)

providers.count.temp.df <- temp.df

# Count cases per Provider
temp.df$count.providers <- ave(temp.df$CMS.Cer.Number.CCN, temp.df$CMS.Cer.Number.CCN,  FUN = length)
summary(temp.df$count.providers)

View(temp.df$count.providers)

require(plyr)

count(temp.df, 'temp.df$CMS.Cer.Number.CCN')

# Get Mean and Median values for all 9 measures used in the Quality Star Rating
summary(temp.df$HO.HHT.began.care.in.timely.manner.18) # Median: 93.0, Mean: 89.93
sd(temp.df$HO.HHT.began.care.in.timely.manner.18, na.rm =TRUE) # Std Dev: 12.32098

summary(temp.df$HO.HHT.taught.about.their.drugs.20) # Median: 95.0, Mean: 89.99
sd(temp.df$temp.df$HO.HHT.taught.about.their.drugs.20, na.rm = FALSE) # Std Dev: 12.0

summary(temp.df$HO.HHT.ensured.received.flu.shot.26) # Median: 74.0, Mean: 67.99
sd(temp.df$HO.HHT.ensured.received.flu.shot.26, na.rm = TRUE) # Std Dev: 23.30454

summary(temp.df$HO.PAT.got.better.at.moving.around.44) # Median: 58.0, Mean: 54.50
sd(temp.df$HO.PAT.got.better.at.moving.around.44) # Std Dev: 19.02297

summary(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46) # Median: 53.0, Mean: 49.37
sd(temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46) # Std Dev: 19.93656

summary(temp.df$HO.PAT..got.better.at.bathing.48) # Median: 65.0, Mean: 60.35
sd(temp.df$HO.PAT..got.better.at.bathing.48) #Std Dev: 20.77074

summary(temp.df$HO.PAT.had.less.pain.when.moving.around.50) # Median: 65.0, Mean: 60.88
sd(temp.df$HO.PAT.had.less.pain.when.moving.around.50) # Std Dev: 23.30148

summary(temp.df$HO.PAT.breathing.improved.52) # Median: 62.0, Mean: 55.09
sd(temp.df$HO.PAT.breathing.improved.52) # Std Dev: 24.32136

summary(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60) # Median: 15.0, Mean: 14.31
sd(temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60) # Std Dev: 5.930435

# Perform z test on our data set and get p values

# For HO.HHT.began.care.in.timely.manner.18

a <- temp.df$HO.HHT.began.care.in.timely.manner.18
s <- 12.32098
xbar <- 89.93
n <- temp.df$count.providers

z <- (xbar-a)/(s/sqrt(n))

temp.df$pvalue.18 <- 2*pnorm(-abs(z))

# For HO.HHT.taught.about.their.drugs.20

a <- temp.df$HO.HHT.taught.about.their.drugs.20
s <- 12.0
xbar <- 89.99
n <- temp.df$count.providers
z <- (xbar-a)/(s/sqrt(n))
temp.df$pvalue.20 <- 2*pnorm(-abs(z))
View(temp.df$pvalue.20)

# For HO.HHT.ensured.received.flu.shot.26
a <- temp.df$HO.HHT.ensured.received.flu.shot.26
s <- 23.30454
xbar <- 69.99
n <- temp.df$count.providers
z <- (xbar-a)/(s/sqrt(n))
temp.df$pvalue.26 <- 2*pnorm(-abs(z))
View(temp.df$pvalue.26)


# For HO.PAT.got.better.at.moving.around.44
a <- temp.df$HO.PAT.got.better.at.moving.around.44
s <- 19.02297
xbar <- 54.50
n <- temp.df$count.providers
z <- (xbar-a)/(s/sqrt(n))
temp.df$pvalue.44 <- 2*pnorm(-abs(z))
View(temp.df$pvalue.44)

# For HO.PAT.got.better.at.getting.in.and.out.of.bed.46
a <- temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46
s <- 19.93656
xbar <- 49.37
n <- temp.df$count.providers
z <- (xbar-a)/(s/sqrt(n))
temp.df$pvalue.46 <- 2*pnorm(-abs(z))
View(temp.df$pvalue.46)

# For HO.PAT..got.better.at.bathing.48
a <- temp.df$HO.PAT..got.better.at.bathing.48
s <- 20.77074
xbar <- 60.35
n <- temp.df$count.providers
z <- (xbar-a)/(s/sqrt(n))
temp.df$pvalue.48 <- 2*pnorm(-abs(z))
View(temp.df$pvalue.48)

# For HO.PAT.had.less.pain.when.moving.around.50
a <- temp.df$HO.PAT.had.less.pain.when.moving.around.50
s <- 23.30148
xbar <- 60.88
n <- temp.df$count.providers
z <- (xbar-a)/(s/sqrt(n))
temp.df$pvalue.50 <- 2*pnorm(-abs(z))
View(temp.df$pvalue.50)

# For HO.PAT.breathing.improved.52
a <- temp.df$HO.PAT.breathing.improved.52
s <- 24.32136
xbar <- 55.09
n <- temp.df$count.providers
z <- (xbar-a)/(s/sqrt(n))
temp.df$pvalue.52 <- 2*pnorm(-abs(z))
View(temp.df$pvalue.52)

# For HO.PAT.had.to.be.admitted.to.the.hospital.60
a <- temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60
s <- 5.930435
xbar <- 14.31
n <- temp.df$count.providers
z <- (xbar-a)/(s/sqrt(n))
temp.df$pvalue.60 <- 2*pnorm(-abs(z))
View(temp.df$pvalue.60)


# Is the initial decile rating 2.5 or 3 && Is p < 0.05? Adjust Decile Rating
# For HO.HHT.began.care.in.timely.manner.18
new <- temp.df$IDR.SCORE.18 + 0.5
same <- temp.df$IDR.SCORE.18
temp.df$adjusted.decile.18 <- ifelse((temp.df$IDR.SCORE.18 != (3.0) | temp.df$IDR.SCORE.18 != (2.5)) & temp.df$IDR.SCORE.18 != (5.0) & temp.df$pvalue.18 <= 0.05, new, same) 
summary(temp.df$adjusted.decile.18)

# For HO.HHT.taught.about.their.drugs.20
new <- temp.df$IDR.SCORE.20 + 0.5
same <- temp.df$IDR.SCORE.20
temp.df$adjusted.decile.20 <- ifelse((temp.df$IDR.SCORE.20 != (3.0) | temp.df$IDR.SCORE.20 != (2.5)) & temp.df$IDR.SCORE.20 != (5.0) & temp.df$pvalue.20 <= 0.05, new, same) 
summary(temp.df$adjusted.decile.20)

# For HO.HHT.ensured.received.flu.shot.26
new <- temp.df$IDR.SCORE.26 + 0.5
same <- temp.df$IDR.SCORE.26
temp.df$adjusted.decile.26 <- ifelse((temp.df$IDR.SCORE.26 != (3.0) | temp.df$IDR.SCORE.26 != (2.5)) & temp.df$IDR.SCORE.26 != (5.0) & temp.df$pvalue.26 <= 0.05, new, same) 
summary(temp.df$adjusted.decile.26)

# For HO.PAT.got.better.at.moving.around.44
new <- temp.df$IDR.SCORE.44 + 0.5
same <- temp.df$IDR.SCORE.44
temp.df$adjusted.decile.44 <- ifelse((temp.df$IDR.SCORE.44 != (3.0) | temp.df$IDR.SCORE.44 != (2.5)) & temp.df$IDR.SCORE.44 != (5.0) & temp.df$pvalue.44 <= 0.05, new, same) 
summary(temp.df$adjusted.decile.44)

# For HO.PAT.got.better.at.getting.in.and.out.of.bed.46
new <- temp.df$IDR.SCORE.46 + 0.5
same <- temp.df$IDR.SCORE.46
temp.df$adjusted.decile.46 <- ifelse((temp.df$IDR.SCORE.46 != (3.0) | temp.df$IDR.SCORE.46 != (2.5)) & temp.df$IDR.SCORE.46 != (5.0) & temp.df$pvalue.46 <= 0.05, new, same) 
summary(temp.df$adjusted.decile.46)

# For HO.PAT..got.better.at.bathing.48
new <- temp.df$IDR.SCORE.48 + 0.5
same <- temp.df$IDR.SCORE.48
temp.df$adjusted.decile.48 <- ifelse((temp.df$IDR.SCORE.48 != (3.0) | temp.df$IDR.SCORE.48 != (2.5)) & temp.df$IDR.SCORE.48 != (5.0) & temp.df$pvalue.48 <= 0.05, new, same) 
summary(temp.df$adjusted.decile.48)

# For HO.PAT.had.less.pain.when.moving.around.50
new <- temp.df$IDR.SCORE.50 + 0.5
same <- temp.df$IDR.SCORE.50
temp.df$adjusted.decile.50 <- ifelse((temp.df$IDR.SCORE.50 != (3.0) | temp.df$IDR.SCORE.50 != (2.5)) & temp.df$IDR.SCORE.50 != (5.0) & temp.df$pvalue.50 <= 0.05, new, same) 
summary(temp.df$adjusted.decile.50)

# For HO.PAT.breathing.improved.52
new <- temp.df$IDR.SCORE.52 + 0.5
same <- temp.df$IDR.SCORE.52
temp.df$adjusted.decile.52 <- ifelse((temp.df$IDR.SCORE.52 != (3.0) | temp.df$IDR.SCORE.52 != (2.5)) & temp.df$IDR.SCORE.52 != (5.0) & temp.df$pvalue.52 <= 0.05, new, same) 
summary(temp.df$adjusted.decile.52)

# For HO.PAT.had.to.be.admitted.to.the.hospital.60
new <- temp.df$IDR.SCORE.60 + 0.5
same <- temp.df$IDR.SCORE.60
temp.df$adjusted.decile.60 <- ifelse((temp.df$IDR.SCORE.60 != (3.0) | temp.df$IDR.SCORE.60 != (2.5)) & temp.df$IDR.SCORE.60 != (5.0) & temp.df$pvalue.60 <= 0.05, new, same) 
summary(temp.df$adjusted.decile.60)

# Average Decile rating for provider
temp.df$avg.decile <- I(temp.df$adjusted.decile.18 +
                          temp.df$adjusted.decile.20 + 
                          temp.df$adjusted.decile.26 + 
                          temp.df$adjusted.decile.44 + 
                          temp.df$adjusted.decile.46 +
                          temp.df$adjusted.decile.48 +
                          temp.df$adjusted.decile.50 + 
                          temp.df$adjusted.decile.52 + 
                          temp.df$adjusted.decile.60)/9

summary(temp.df$avg.decile)

# Assign Quality Star Rating for each provider
temp.df$overall.hhc.rating <- ifelse(temp.df$avg.decile <= 0.5, 1.0, 
                                     ifelse(temp.df$avg.decile <= 1.0, 1.5, 
                                            ifelse(temp.df$avg.decile <= 1.5, 2.0,
                                                   ifelse(temp.df$avg.decile <= 2.0, 2.5, 
                                                          ifelse(temp.df$avg.decile <= 2.5, 3.0, 
                                                                 ifelse(temp.df$avg.decile <= 3.0, 3.5, 
                                                                        ifelse(temp.df$avg.decile <= 3.5, 4.0, 
                                                                               ifelse(temp.df$avg.decile <= 4.0, 4.5,
                                                                                      ifelse(temp.df$avg.decile > 4.0, 5.0,0)))))))))

# Removing fields used for intermediate calculations
temp.df$adjusted.decile.18 <- NULL
temp.df$adjusted.decile.20 <- NULL 
temp.df$adjusted.decile.26 <- NULL 
temp.df$adjusted.decile.44 <- NULL 
temp.df$adjusted.decile.46 <- NULL
temp.df$adjusted.decile.48 <- NULL
temp.df$adjusted.decile.50 <- NULL 
temp.df$adjusted.decile.52 <- NULL 
temp.df$adjusted.decile.60 <- NULL

dim(temp.df)
overall.rating.df <- temp.df

###  SAVE THE DATASET WITH OVERALL STAR RATING FOR EACH PROVIDER
setwd("C:/Users/Siddartha Sathyanara/Dropbox/Team Project/CMS Data Files/RData Files")
save(list = c('overall.rating.df'), file = 'overall.rating.df.RData')
