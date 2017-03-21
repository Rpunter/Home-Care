## R Code File Name:  A1 - Reading multiple Files - 2013 - Sandra.r

#########################################################
# SET VALUES FOR MISSING DATA
# Numeric missing values are represented by NA
# Character missing values are represented by <NA>

missing.types <- c("NA", '',"","<NA>","N/A")

#########################################################
# Create shorter variable names
col.names <- c(
  "CMS.Cer.Number.CCN",
  "Provider.Name",
  "Address",
  "City",
  "State",  
  "Zip",
  "Phone",
  "Type.of.Ownership",
  "Date.Certified", 
  "Offers.Nursing.Care.Services",
  "Offers.Physical.Therapy.Services",
  "Offers.Occupational.Therapy.Services",
  "Offers.Speech.Pathology.Services",
  "Offers.Medical.Social.Services",
  "Offers.Home.Health.Aide.Services",
  "HO.HHT.began.care.in.timely.manner.18",
  "HO.HHT.taught.about.their.drugs.20",
  "HO.HHT.checked.for.risk.of.falling.22",
  "HO.HHT.checked.for.depression.24",
  "HO.HHT.ensured.received.flu.shot.26",
  "HO.HHT.made.received.pneumonia.shot.28",
  "HO.HHT.taught.gave.foot.care.30",
  "HO.HHT.checked.for.pain.32",
  "HO.HHT.treated.for.pain.34",
  "HO.HHT.treated.heart.failure.weakening.of.the.heart.36",
  "HO.HHT.took.action.to.prevent.pressure.bed.sores.38",
  "HO.HHT.included.treatm.to.prevent.pressure.bed.sores.40",
  "HO.HHT.checked.for.risk.of.pressure.bed.sores.42",
  "HO.PAT.got.better.at.moving.around.44",
  "HO.PAT.got.better.at.getting.in.and.out.of.bed.46",
  "HO.PAT..got.better.at.bathing.48",
  "HO.PAT.had.less.pain.when.moving.around.50",
  "HO.PAT.breathing.improved.52",
  "HO.PAT.wounds.improved.healed.54",
  "HO.PAT.got.better.at.taking.drugs.correctly.56",
  "HO.PAT.needed.urgent.unplanned.ER.wout.admission.58",
  "HO.PAT.had.to.be.admitted.to.the.hospital.60"
)

####  these variables are in the 2014 and 2015 files but not in the 2013 files
#"Quality.of.Patient.Care.Star.Rating",
#"Footnote.Quality.of.Patient.Care.Star.Rating",
#"Footnote.HO.HHT.began.care.in.timely.manner.19",  
#"Footnote.HO.HHT.taught.about.their.drugs.21",  
#"Footnote.HO.HHT.checked.for.risk.of.falling.23",  
#"Footnote.HO.HHT.checked.for.depression.25",  
#"Footnote.HO.HHT.ensured.received.flu.shot.27",  
#"Footnote.HO.HHT.made.received.pneumonia.shot.29",  
#"Footnote.HO.HHT.taught.gave.foot.care.31",  
#"Footnote.HO.HHT.checked.for.pain.33",  
#"Footnote.HO.HHT.treated.for.pain.35",  
#"Footnote.HO.HHT.treated.heart.failure.weakening.of.the.heart.37",  
#"Footnote.HO.HHT.took.action.to.prevent.pressure.bed.sores.39",  
#"FootnoteHO.HHT.included.treatm.to.prevent.pressure.bed.sores.41",  
#"Footnote.HO.HHT.checked.for.risk.of.pressure.bed.sores.43",  
#"Footnote.HO.PAT.got.better.at.moving.around.45",  
#"Footnote.HO.PAT.got.better.at.getting.in.and.out.of.bed.47",
#"Footnote.HO.PAT..got.better.at.bathing.49",  
#"Footnote.HO.PAT.had.less.pain.when.moving.around.51",  
#"Footnote.HO.PAT.breathing.improved.53",
#"Footnote.HO.PAT.wounds.improved.healed.55",
#"Footnote.HO.PAT.got.better.at.taking.drugs.correctly.57",  
#"Footnote.HO.PAT.needed.urgent.unplanned.ER.wout.admission.59",  
#"Footnote.HO.PAT.had.to.be.admitted.to.the.hospital.61",  
#"Footnote.HO.PAT.was.re.admitted.to.hospital.63",  
#"Footnote.HO.PAT.received.ER.care.wout.re.admission.65",
#"HO.PAT.was.re.admitted.to.hospital.62",
#"HO.PAT.received.ER.care.wout.re.admission.64"
#"Footnote.HO.PAT.was.re.admitted.to.hospital.63",  
#"Footnote.HO.PAT.received.ER.care.wout.re.admission.65",
#"Footnote"


#########################################################
# SET DATA FILE DIRECTORY LOCATION

# Path location of CMS data files:
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/Archive Data/Ready Data Files")


hhc.prvdr.csv.201301 <- read.csv("HHC_PRVDR_201301.csv",
                          header=TRUE,
                          na=missing.types,
                          sep=",",
                          stringsAsFactors = FALSE
)


hhc.prvdr.csv.201302 <- read.csv("HHC_PRVDR_201304.csv",
                          header=TRUE,
                          na=missing.types,
                          sep=",",
                          stringsAsFactors = FALSE                          
)


hhc.prvdr.csv.201303 <- read.csv("HHC_PRVDR_201307.csv",
                                 header=TRUE,
                                 na=missing.types,
                                 sep=",",
                                 stringsAsFactors = FALSE                          
)


hhc.prvdr.csv.201304 <- read.csv("HHC_PRVDR_201310.csv",
                                 header=TRUE,
                                 na=missing.types,
                                 sep=",",
                                 stringsAsFactors = FALSE                          
)


# RENAMES ORIGINAL COLUMN NAMES WITH SHORTER NAMES
colnames(hhc.prvdr.csv.201301) <- col.names
colnames(hhc.prvdr.csv.201302) <- col.names
colnames(hhc.prvdr.csv.201303) <- col.names
colnames(hhc.prvdr.csv.201304) <- col.names


all.vars <- names(hhc.prvdr.csv.201301)
remove.vars <- c("Phone","Address","Date.Certified")
vars <- setdiff(all.vars,remove.vars)

providers.df.201301 <- hhc.prvdr.csv.201301[vars]
providers.df.201302 <- hhc.prvdr.csv.201302[vars]
providers.df.201303 <- hhc.prvdr.csv.201303[vars]
providers.df.201304 <- hhc.prvdr.csv.201304[vars]

# CREATE NEW FIELD TO STORE THE YEAR QUARTER OF THE FILE
providers.df.201301$year.quarter <- 201301
providers.df.201302$year.quarter <- 201302
providers.df.201303$year.quarter <- 201303
providers.df.201304$year.quarter <- 201304

providers.df.201301$Quality.of.Patient.Care.Star.Rating <- 0
providers.df.201302$Quality.of.Patient.Care.Star.Rating <- 0
providers.df.201303$Quality.of.Patient.Care.Star.Rating <- 0
providers.df.201304$Quality.of.Patient.Care.Star.Rating <- 0

providers.df.201301$HO.PAT.was.re.admitted.to.hospital.62 <- 0
providers.df.201302$HO.PAT.was.re.admitted.to.hospital.62 <- 0
providers.df.201303$HO.PAT.was.re.admitted.to.hospital.62 <- 0
providers.df.201304$HO.PAT.was.re.admitted.to.hospital.62 <- 0

providers.df.201301$HO.PAT.received.ER.care.wout.re.admission.64 <- 0
providers.df.201302$HO.PAT.received.ER.care.wout.re.admission.64 <- 0
providers.df.201303$HO.PAT.received.ER.care.wout.re.admission.64 <- 0
providers.df.201304$HO.PAT.received.ER.care.wout.re.admission.64 <- 0


dim(providers.df.201301)  # 12122    37
dim(providers.df.201302)  # 12181    37
dim(providers.df.201303)  # 12262    37
dim(providers.df.201304)  # 12375    37


# Union all 4 files for 2013 to create one data set for the year
providers.df.2013 <- rbind(providers.df.201301,providers.df.201302,providers.df.201303,providers.df.201304)
dim(providers.df.2013)
View(providers.df.2013)


# Save the .RData 
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('providers.df.2013'), file = 'providers.df.2013.RData')


##########################################################
# load Sid's RData for the 4 quarters of 2014
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
load("HHC_PRVDR_2014.RData")
dim(providers.df.2014)

# Combine Sid's 2014 data with 2013
providers.df.2013.2014 <- rbind(providers.df.2013, providers.df.2014)
dim(providers.df.2013.2014)

# Save the .RData 
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('providers.df.2013.2014'), file = 'providers.df.2013.2014.RData')

View(providers.df.2013.2014)
