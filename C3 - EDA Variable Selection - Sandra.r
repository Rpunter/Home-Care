##  R Code File Name:  C3 - EDA Variable Selection - Sandra.r

############################

#########################################################
# Environment glObal variables setting

getOption("max.print")
options(max.print = 300000)


# load Sid's RData for the 4 quarters of 2014
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")

load("hhc.final.for.scoring.df.RData")
dim(providers.df.all.munged.2)
df <- providers.df.all.munged.2
rm(providers.df.all.munged.2)

# --- get a subset to develop the Ranking
set.seed(123)
smp_size.10 <- floor(0.10 * nrow(df))
subset_ind <- sample(seq_len(nrow(df)), size = smp_size.10)
subset.df <- df[subset_ind]  
nrow(subset.df)  # 13,546 rows


# ---- train_test_mat ----
# get the index for training/testing data
set.seed(123)
nrow(df)  # 135,464 rows
smp_size <- floor(0.75 * nrow(df))
smp_size
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
# making a "tiny" data set so I can quickly test r markdown and graphical paramters
# this will be removed in the submitted version
tiny_ind <- sample(seq_len(nrow(df)), size = floor(0.01 * nrow(df)))
# split the data
train.df <- df[train_ind, ]
test.df <- df[-train_ind, ]
rm(train.df,test.df)
tiny.df <- df[tiny_ind, ]
nrow(tiny.df)  #  1,354 rows
dim(tiny.df)

############# DESCRIPTIVE STATISTICS OF 9 MEASURES
View(df)
names(df)
str(df)

library(dplyr)
df <- select(df, -Zip, -year.quarter.1)
df$zip <- as.character(df$zip)
df$year.quarter <- as.character(df$year.quarter)

vars <- colnames(df)  
vars
# get the variables names for ONLY the numeric variables
numeric.vars <- vars[sapply(df[,vars], class) %in% c('numeric','integer','logical')]
numeric.vars

num.df <- df[numeric.vars]
View(num.df)
num.df.after.impute.summary <- my.summary(num.df)
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
write.csv(num.df.after.impute.summary,"num.df.after.impute.summary.csv")

library("lattice")
histogram(num.df$HO.HHT.began.care.in.timely.manner.18)
boxplot(num.df$HO.HHT.began.care.in.timely.manner.18)
densityplot(num.df$HO.HHT.began.care.in.timely.manner.18)
mean(num.df$HO.HHT.began.care.in.timely.manner.18)

rank(num.df)


