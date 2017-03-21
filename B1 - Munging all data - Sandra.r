## R Code File Name:  B1 - Munging all data - Sandra.r

library("stringr", lib.loc="~/R/win-library/3.2")

#########################################################
# Environment global variables setting

getOption("max.print")
options(max.print = 300000)


# load Sid's RData for the 4 quarters of 2014
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")

load("providers.df.2013.2014.RData")
dim(providers.df.2013.2014)
df.2013.2014 <- providers.df.2013.2014

load("providers.df.2015.1.RData")
dim(providers.df.2015.1)
df.2015.q1.q2 <- providers.df.2015.1

load("providers.df.2015.2.RData")
dim(providers.df.2015.2)
df.2015.q3 <- providers.df.2015.2


providers.df.all <- rbind(df.2013.2014,df.2015.q1.q2,df.2015.q3)
dim(providers.df.all) # 135464     38


# Save the .RData 
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('providers.df.all'), file = 'providers.df.all.RData')
load("providers.df.all.RData")
dim(providers.df.all)
str(providers.df.all)


#----------- get numeric/character only------------------------------------------------

nums <- sapply(providers.df.all, is.numeric)  # get numeric vars using list-apply function
num.df <- providers.df.all[nums]                   # subset numeric columns
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('num.df'), file = 'num.df.RData')
load("num.df.RData")

View(num.df)


# Create Descriptive Statistics for the Numerical Variables
num.df.summary <- my.summary(num.df)
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
write.csv(num.df.summary,"num.df.summary.csv")

chars <- !sapply(providers.df.all, is.numeric) # get character vars using list-apply function
char.df <- providers.df.all[chars]       # subset character columns
char.df$year.quarter <- providers.df.all$year.quarter
char.df$zip <- providers.df.all$Zip
View(char.df)

names(char.df)

# CHECK FOR MISSING  DATA
any(is.na(providers.df.all))


######################
#  FIX CHARACTER DATA TO HAVE STANDARDIZED VALUES

#df_dummies <- data.frame(model.matrix(~.-1, data = char.df))  # errors out due to lack of memory

# After visually inspecting the character data, there were special characters found in the text
#   which need to be replaced with text data so that the categories can be used to create the
#   variable name of the dichotomized fields:
# Clean character variables to replace special characters with text strings, such as '&' with 'AND'

char.df$Provider.Name <- gsub("&"," AND ",char.df$Provider.Name)
char.df$Provider.Name <- gsub("/"," OR ",char.df$Provider.Name)
char.df$Provider.Name <- gsub(",","",char.df$Provider.Name)
char.df$Provider.Name <- gsub("  "," ",char.df$Provider.Name)
char.df$Provider.Name <- gsub("-"," DASH ",char.df$Provider.Name)

char.df$Type.of.Ownership <- gsub("&"," AND ",char.df$Type.of.Ownership)
char.df$Type.of.Ownership <- gsub("/"," OR ",char.df$Type.of.Ownership)
char.df$Type.of.Ownership <- gsub(",","",char.df$Type.of.Ownership)
char.df$Type.of.Ownership <- gsub("  "," ",char.df$Type.of.Ownership)


# Replace Y with yes and N with no to standardize the data from 2013 to that of 2014 and 2015
char.df$Offers.Nursing.Care.Services <- gsub("Yes","Y",char.df$Offers.Nursing.Care.Services)
char.df$Offers.Nursing.Care.Services <- gsub("No","N",char.df$Offers.Nursing.Care.Services)

char.df$Offers.Physical.Therapy.Services <- gsub("Yes","Y",char.df$Offers.Physical.Therapy.Services)
char.df$Offers.Physical.Therapy.Services <- gsub("No","N",char.df$Offers.Physical.Therapy.Services)

char.df$Offers.Occupational.Therapy.Services <- gsub("Yes","Y",char.df$Offers.Occupational.Therapy.Services)
char.df$Offers.Occupational.Therapy.Services <- gsub("No","N",char.df$Offers.Occupational.Therapy.Services)

char.df$Offers.Speech.Pathology.Services <- gsub("Yes","Y",char.df$Offers.Speech.Pathology.Services)
char.df$Offers.Speech.Pathology.Services <- gsub("No","N",char.df$Offers.Speech.Pathology.Services)

char.df$Offers.Medical.Social.Services <- gsub("Yes","Y",char.df$Offers.Medical.Social.Services)
char.df$Offers.Medical.Social.Services <- gsub("No","N",char.df$Offers.Medical.Social.Services)

char.df$Offers.Home.Health.Aide.Services <- gsub("Yes","Y",char.df$Offers.Home.Health.Aide.Services)
char.df$Offers.Home.Health.Aide.Services <- gsub("No","N",char.df$Offers.Home.Health.Aide.Services)


#  Change text of character variables to lower case
library("stringr", lib.loc="~/R/win-library/3.2")
char.df$State <- str_to_lower(char.df$State)
char.df$Provider.Name <- str_to_lower(char.df$Provider.Name)
char.df$City <- str_to_lower(char.df$City)
char.df$Type.of.Ownership <- str_to_lower(char.df$Type.of.Ownership)
char.df$Offers.Nursing.Care.Services <- str_to_lower(char.df$Offers.Nursing.Care.Services)
char.df$Offers.Physical.Therapy.Services <- str_to_lower(char.df$Offers.Physical.Therapy.Services)
char.df$Offers.Occupational.Therapy.Services <- str_to_lower(char.df$Offers.Occupational.Therapy.Services)
char.df$Offers.Speech.Pathology.Services <- str_to_lower(char.df$Offers.Speech.Pathology.Services)
char.df$Offers.Medical.Social.Services <- str_to_lower(char.df$Offers.Medical.Social.Services)
char.df$Offers.Home.Health.Aide.Services <- str_to_lower(char.df$Offers.Home.Health.Aide.Services)
char.df$HO.PAT.was.re.admitted.to.hospital.62 <- str_to_lower(char.df$HO.PAT.was.re.admitted.to.hospital.62)
char.df$HO.PAT.received.ER.care.wout.re.admission.64 <- str_to_lower(char.df$HO.PAT.received.ER.care.wout.re.admission.64)
char.df$Quality.of.Patient.Care.Star.Rating <- str_to_lower(char.df$Quality.of.Patient.Care.Star.Rating)

char.df$Type.of.Ownership <- gsub("government","govt",char.df$Type.of.Ownership)
char.df$Type.of.Ownership <- gsub("govt voluntary","govt and voluntary",char.df$Type.of.Ownership)
char.df$Type.of.Ownership <- gsub(" - "," ",char.df$Type.of.Ownership)

str(char.df)

# save the munged character data into a file for quick load if needed rather than having to 
# re-run all the steps above
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('char.df'), file = 'char.df.RData')
load("char.df.RData")


#########################################################
#######   This munged.1 data set contains both the categorical variables with standardized names
#######     and values and the numerical variables with still missing values.
providers.df.all.munged.1 <- data.frame(char.df,num.df)

# save the munged character data into a file for quick load if needed rather than having to 
# re-run all the steps above
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('providers.df.all.munged.1'), file = 'providers.df.all.munged.1.RData')
load("providers.df.all.munged.1.RData")

write.csv(providers.df.all.munged.1,"providers.df.all.munged.1.csv")


#########################################################
#   IMPUTE NUMERICAL VARIABLES WITH A 0

# visual EDA of the missing numeric variables
library("Amelia") # for missmap to visually evaluate missing numeric data
missmap(num.df, main="Quality Data - Missings (yellow) Data Map", 
        col=c("yellow", "black"), legend=FALSE, rank.order=TRUE,
        y.cex = 0.8, x.cex = 0.8)


# impute missing numeric values to zero (0) as those observations would not count
for (i in names(num.df)) { 
  num.df[which(is.na(num.df[[i]])),i] <- 0
}

# QA imputation for missing values in the numeric variables
any(is.na(num.df)) # quick check that there no missing numeric data

# The function "my.summary" needs to be created first
# output descriptive statiscs AFTER imputing missing values
my.summary.out.num.vars.after.impute <- my.summary(in.df=numeric.df)
write.csv(my.summary.out.num.vars.after.impute,"my.summary.out.num.vars.after.impute.csv")

#########################################################
#######   This munged.1 data set contains both the categorical variables with standardized names
#######     and values and the numerical variables with IMPUTED missing values.
providers.df.all.munged.2 <- data.frame(char.df,num.df)

# save the munged character data into a file for quick load if needed rather than having to 
# re-run all the steps above
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('providers.df.all.munged.2'), file = 'providers.df.all.munged.2.RData')
load("providers.df.all.munged.2.RData")

write.csv(providers.df.all.munged.2,"providers.df.all.munged.2.csv")

any(is.na(providers.df.all.munged.2))
View(providers.df.all.munged.2)



#########################################################
# DICHOTOMIZYING CATEGORICAL VARIABLES
# Convert character variables to factors in order to dichotomize
# create a separate data frame with the original character variables to ensure we preserve
#  the already munged data intact

chars <- !sapply(providers.df.all.munged.2, is.numeric) # get character vars using list-apply function
char.df <- providers.df.all.munged.2[chars]       # subset character columns
char.df$year.quarter <- as.character(providers.df.all.munged.2$year.quarter)
char.df$Zip <- as.character(providers.df.all.munged.2$Zip)

char.dicho.df <- char.df
View(char.dicho.df)
rm(providers.df.all.munged.2)

# convert the character variables to factor to get the levels name and create the dummy variables
char.dicho.df$State <- as.factor(char.df$State)
char.dicho.df$Provider.Name <- as.factor(char.df$Provider.Name)
char.dicho.df$City <- as.factor(char.df$City)
char.dicho.df$Type.of.Ownership <- as.factor(char.df$Type.of.Ownership)
char.dicho.df$Offers.Nursing.Care.Services <- as.factor(char.df$Offers.Nursing.Care.Services)
char.dicho.df$Offers.Physical.Therapy.Services <- as.factor(char.df$Offers.Physical.Therapy.Services)
char.dicho.df$Offers.Occupational.Therapy.Services <- as.factor(char.df$Offers.Occupational.Therapy.Services)
char.dicho.df$Offers.Speech.Pathology.Services <- as.factor(char.df$Offers.Speech.Pathology.Services)
char.dicho.df$Offers.Medical.Social.Services <- as.factor(char.df$Offers.Medical.Social.Services)
char.dicho.df$Offers.Home.Health.Aide.Services <- as.factor(char.df$Offers.Home.Health.Aide.Services)
char.dicho.df$HO.PAT.was.re.admitted.to.hospital.62 <- as.factor(char.df$HO.PAT.was.re.admitted.to.hospital.62)
char.dicho.df$HO.PAT.received.ER.care.wout.re.admission.64 <- as.factor(char.df$HO.PAT.received.ER.care.wout.re.admission.64)
char.dicho.df$Quality.of.Patient.Care.Star.Rating <- as.factor(char.dicho.df$Quality.of.Patient.Care.Star.Rating)
char.dicho.df$year.quarter <- as.factor(char.dicho.df$year.quarter)
char.dicho.df$zip <- as.factor(char.dicho.df$Zip)

rm(char.df)

# save the munged character data into a file for quick load if needed rather than having to 
# re-run all the steps above
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Home Health Care Data/R Code/RData")
save(list = c('char.dicho.df'), file = 'char.dicho.df.RData')
load("char.dicho.df.RData")

# Create the dummy variables for each Categorical factor variable based on the Levels
#   for that Categorical variable.  
# The dummy variables will be numeric and initialize to 0.
# The naming convention uses the categorical variable name as a prefix, followed by an 
#   underscore, followed by the category string after spaces are replaced with dashes.
#   For example, the dummy variable name for the category 24 HOUR CARE LLC" in the 
#     Provider.Name categorical field is named as Provider.Name_24-HOUR-CARE-LLC

library("stringr", lib.loc="~/R/win-library/3.2")


#########   1.  Type.of.Ownership DICHOTOMIZATION
Type.of.Ownership <- char.dicho.df$Type.of.Ownership
Type.of.Ownership.df <- as.data.frame(Type.of.Ownership)
Type.of.Ownership.dicho.df <- data.frame(model.matrix(~.-1, data = Type.of.Ownership.df))  
names(Type.of.Ownership.dicho.df)
View(Type.of.Ownership.dicho.df)
rm(Type.of.Ownership,Type.of.Ownership.df)


#########   2.  State DICHOTOMIZATION
State <-  str_to_upper(char.dicho.df$State)
State.df <- as.data.frame(State)
State.dicho.df <- data.frame(model.matrix(~.-1, data = State.df))  
names(State.dicho.df)
View(State.dicho.df)
rm(State,State.df)

#########   3.  Provider Name DICHOTOMIZATION    ###  ERROR WITH MEMORY ALLOCATION
#Provider.Name <-  char.dicho.df$Provider.Name
#Provider.Name.df <- as.data.frame(Provider.Name)
#Provider.Name.dicho.df <- data.frame(model.matrix(~.-1, data = Provider.Name.df))  
#names(Provider.Name.dicho.df)
#rm(Provider.Name) #,Provider.Name.df)


#########   4.  City DICHOTOMIZATION  ###  ERROR WITH MEMORY ALLOCATION
City <-  char.dicho.df$City
City.df <- as.data.frame(City)
City.dicho.df <- data.frame(model.matrix(~.-1, data = City.df))  
names(City.dicho.df)
rm(City,City.df)


#########   5.  Offers.Physical.Therapy.Services DICHOTOMIZATION 
Offers.Physical.Therapy.Services <-  str_to_upper(char.dicho.df$Offers.Physical.Therapy.Services)
Offers.Physical.Therapy.Services.df <- as.data.frame(Offers.Physical.Therapy.Services)
Offers.Physical.Therapy.Services.dicho.df <- data.frame(model.matrix(~.-1, data = Offers.Physical.Therapy.Services.df))  
names(Offers.Physical.Therapy.Services.dicho.df)
rm(Offers.Physical.Therapy.Services,Offers.Physical.Therapy.Services.df)


#########   6.  Offers.Occupational.Therapy.Services DICHOTOMIZATION 
Offers.Occupational.Therapy.Services <-  str_to_upper(char.dicho.df$Offers.Occupational.Therapy.Services)
Offers.Occupational.Therapy.Services.df <- as.data.frame(Offers.Occupational.Therapy.Services)
Offers.Occupational.Therapy.Services.dicho.df <- data.frame(model.matrix(~.-1, data = Offers.Occupational.Therapy.Services.df))  
names(Offers.Occupational.Therapy.Services.dicho.df)
rm(Offers.Occupational.Therapy.Services,Offers.Occupational.Therapy.Services.df)


#########   7.  Offers.Speech.Pathology.Services DICHOTOMIZATION  
Offers.Speech.Pathology.Services <-  str_to_upper(char.dicho.df$Offers.Speech.Pathology.Services)
Offers.Speech.Pathology.Services.df <- as.data.frame(Offers.Speech.Pathology.Services)
Offers.Speech.Pathology.Services.dicho.df <- data.frame(model.matrix(~.-1, data = Offers.Speech.Pathology.Services.df))  
names(Offers.Speech.Pathology.Services.dicho.df)
rm(Offers.Speech.Pathology.Services,Offers.Speech.Pathology.Services.df)


#########   8.  Offers.Medical.Social.Services DICHOTOMIZATION  
Offers.Medical.Social.Services <-  str_to_upper(char.dicho.df$Offers.Medical.Social.Services)
Offers.Medical.Social.Services.df <- as.data.frame(Offers.Medical.Social.Services)
Offers.Medical.Social.Services.dicho.df <- data.frame(model.matrix(~.-1, data = Offers.Medical.Social.Services.df))  
names(Offers.Medical.Social.Services.dicho.df)
rm(Offers.Medical.Social.Services,Offers.Medical.Social.Services.df)


#########   9.  Offers.Home.Health.Aide.Services DICHOTOMIZATION    ###  ERROR WITH MEMORY ALLOCATION
Offers.Home.Health.Aide.Services <-  str_to_upper(char.dicho.df$Offers.Home.Health.Aide.Services)
Offers.Home.Health.Aide.Services.df <- as.data.frame(Offers.Home.Health.Aide.Services)
Offers.Home.Health.Aide.Services.dicho.df <- data.frame(model.matrix(~.-1, data = Offers.Home.Health.Aide.Services.df))  
names(Offers.Home.Health.Aide.Services.dicho.df)
rm(Offers.Home.Health.Aide.Services,Offers.Home.Health.Aide.Services.df)


#########   10. year.quarter DICHOTOMIZATION 
year.quarter <-  str_to_upper(char.dicho.df$year.quarter)
year.quarter.df <- as.data.frame(year.quarter)
year.quarter.dicho.df <- data.frame(model.matrix(~.-1, data = year.quarter.df))  
names(year.quarter.dicho.df)
rm(year.quarter,year.quarter.df)


#########   11. Zip DICHOTOMIZATION    ###  ERROR WITH MEMORY ALLOCATION
Zip <-  str_to_upper(char.dicho.df$Zip)
Zip.df <- as.data.frame(Zip)
Zip.dicho.df <- data.frame(model.matrix(~.-1, data = Zip.df))  
names(Zip.dicho.df)
rm(Zip,Zip.df)


#########   CREATE A FINAL DICHOTOMIZED DATA FRAME

final.dicho.df <- data.frame(Type.of.Ownership.dicho.df, 
                             State.dicho.df,
                             #Provider.Name.dicho.df,
                             City.dicho.df,
                             Offers.Physical.Therapy.Services.dicho.df,
                             Offers.Occupational.Therapy.Services.dicho.df,
                             Offers.Speech.Pathology.Services.dicho.df,
                             Offers.Medical.Social.Services.dicho.df,
                             Offers.Home.Health.Aide.Services.dicho.df,
                             year.quarter.dicho.df
                             #Zip.dicho.df
)

dim(final.dicho.df)
str(final.dicho.df)
View(final.dicho.df)

# save the dichotomized character data into a file for quick load if needed rather than having to 
# re-run all the steps above
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('final.dicho.df'), file = 'final.dicho.df.RData')
load("final.dicho.df.RData")

#final.dicho.df.summary <- my.summary(final.dicho.df) # MEMORY ERROR

###  To get the numeric imputed data, load the providers.df.all.munged.2.RData IF NECESSARY
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
load("providers.df.all.munged.2.RData")
str(providers.df.all.munged.2)


######################################
#  COMBINE THE DICHOTOMIZED CHARACTER DATA SET WITH THE NUMERICAL IMPUTED DATA SET:

## FIRST GET A DATA FRAME CONTAINING ONLY THE NUMERICAL VARIABLES
# get all the variables from the complete data set:
vars <- colnames(providers.df.all.munged.2)  
vars
# get the variables names for ONLY the numeric variables
numeric.vars <- vars[sapply(providers.df.all.munged.2[,vars], class) %in% c('numeric','integer','logical')]
numeric.vars
# remove any unwanted numerica variable names
remove.num.vars <- c("year.quarter","zip","Zip","year.quarter.1")
numeric.vars <- setdiff(numeric.vars,remove.num.vars)
numeric.vars
# create the data set with only the numeric imputed variables
nums.df <- providers.df.all.munged.2[numeric.vars]     
any(is.na(nums.df))  # this should be FALSE
View(nums.df)

##  SECOND, COMBINE THE DATA THAT HAS THE NUMERIC IMPUTED VARIABLES WITH THE
##    DATA SET THAT HAS THE DICHOTOMIZED FACTOR VARIABLES

hhc.final.df <- data.frame(final.dicho.df,nums.df)
dim(hhc.final.df)  # 135464   3408
str(hhc.final.df)
View(hhc.final.df)


###  SAVE THE FINALIZED DATA SET WITH THE DICHOTOMIZED CATEGORICAL VARIABLES AND IMPUTED NUMERIC VARIABLES
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('hhc.final.df'), file = 'hhc.final.df.RData')
load("hhc.final.df.RData")

