## R Code File Name:  A2 - Reading multiple Files - 2014 - Sid.r

#########################################################
# SET VALUES FOR MISSING DATA
# Numeric missing values are represented by NA
# Character missing values are represented by <NA>

missing.types <- c("NA", '',"","<NA>","N/A")

#########################################################
# Create shorter variable names
col.names <- c(
  "State",  
  "CMS.Cer.Number.CCN",
  "Provider.Name",
  "Address",
  "City",
  "Zip",
  "Phone",
  "Type.of.Ownership",
  "Offers.Nursing.Care.Services",
  "Offers.Physical.Therapy.Services",
  "Offers.Occupational.Therapy.Services",
  "Offers.Speech.Pathology.Services",
  "Offers.Medical.Social.Services",
  "Offers.Home.Health.Aide.Services",
  "Date.Certified", 
  "HO.HHT.began.care.in.timely.manner.18",
  "Footnote.HO.HHT.began.care.in.timely.manner.19",
  "HO.HHT.taught.about.their.drugs.20",
  "Footnote.HO.HHT.taught.about.their.drugs.21",
  "HO.HHT.checked.for.risk.of.falling.22",
  "Footnote.HO.HHT.checked.for.risk.of.falling.23",
  "HO.HHT.checked.for.depression.24",
  "Footnote.HO.HHT.checked.for.depression.25",
  "HO.HHT.ensured.received.flu.shot.26",
  "Footnote.HO.HHT.ensured.received.flu.shot.27",
  "HO.HHT.made.received.pneumonia.shot.28",
  "Footnote.HO.HHT.made.received.pneumonia.shot.29",
  "HO.HHT.taught.gave.foot.care.30",
  "Footnote.HO.HHT.taught.gave.foot.care.31",
  "HO.HHT.checked.for.pain.32",
  "Footnote.HO.HHT.checked.for.pain.33",
  "HO.HHT.treated.for.pain.34",
  "Footnote.HO.HHT.treated.for.pain.35",
  "HO.HHT.treated.heart.failure.weakening.of.the.heart.36",
  "Footnote.HO.HHT.treated.heart.failure.weakening.of.the.heart.37",  
  "HO.HHT.took.action.to.prevent.pressure.bed.sores.38",
  "Footnote.HO.HHT.took.action.to.prevent.pressure.bed.sores.39",
  "HO.HHT.included.treatm.to.prevent.pressure.bed.sores.40",
  "FootnoteHO.HHT.included.treatm.to.prevent.pressure.bed.sores.41",
  "HO.HHT.checked.for.risk.of.pressure.bed.sores.42",
  "Footnote.HO.HHT.checked.for.risk.of.pressure.bed.sores.43",
  "HO.PAT.got.better.at.moving.around.44",
  "Footnote.HO.PAT.got.better.at.moving.around.45",
  "HO.PAT.got.better.at.getting.in.and.out.of.bed.46",
  "Footnote.HO.PAT.got.better.at.getting.in.and.out.of.bed.47",
  "HO.PAT..got.better.at.bathing.48",
  "Footnote.HO.PAT..got.better.at.bathing.49",
  "HO.PAT.had.less.pain.when.moving.around.50",
  "Footnote.HO.PAT.had.less.pain.when.moving.around.51",
  "HO.PAT.breathing.improved.52",
  "Footnote.HO.PAT.breathing.improved.53",
  "HO.PAT.wounds.improved.healed.54",
  "Footnote.HO.PAT.wounds.improved.healed.55",
  "HO.PAT.got.better.at.taking.drugs.correctly.56",
  "Footnote.HO.PAT.got.better.at.taking.drugs.correctly.57",
  "HO.PAT.needed.urgent.unplanned.ER.wout.admission.58",
  "Footnote.HO.PAT.needed.urgent.unplanned.ER.wout.admission.59",
  "HO.PAT.had.to.be.admitted.to.the.hospital.60",
  "Footnote.HO.PAT.had.to.be.admitted.to.the.hospital.61",
  "Footnote")




#########################################################
# SET DATA FILE DIRECTORY LOCATION

# Path location of CMS data files:
setwd("C:/Users/Sandra/Dropbox/Team Project/CMS Data Files/Archive Data/Ready Data Files")

setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/Archive Data/Ready Data Files")

hhc.prvdr.csv.201401 <- read.csv("HHC_PRVDR_201401.csv",
                          header=TRUE,
                          na=missing.types,
                          sep=",",
                          stringsAsFactors = FALSE
)


hhc.prvdr.csv.201402 <- read.csv("HHC_PRVDR_201404.csv",
                          header=TRUE,
                          na=missing.types,
                          sep=",",
                          stringsAsFactors = FALSE                          
)


hhc.prvdr.csv.201403 <- read.csv("HHC_PRVDR_201407.csv",
                                 header=TRUE,
                                 na=missing.types,
                                 sep=",",
                                 stringsAsFactors = FALSE                          
)


hhc.prvdr.csv.201404 <- read.csv("HHC_PRVDR_201410.csv",
                                 header=TRUE,
                                 na=missing.types,
                                 sep=",",
                                 stringsAsFactors = FALSE                          
)


# RENAMES ORIGINAL COLUMN NAMES WITH SHORTER NAMES
colnames(hhc.prvdr.csv.201401) <- col.names
colnames(hhc.prvdr.csv.201402) <- col.names
colnames(hhc.prvdr.csv.201403) <- col.names
colnames(hhc.prvdr.csv.201404) <- col.names


all.vars <- names(hhc.prvdr.csv.201401)
remove.vars <- c("Phone",
                 "Address","Date.Certified",
                 "Footnote.HO.HHT.began.care.in.timely.manner.19",
                 "Footnote.HO.HHT.taught.about.their.drugs.21",
                 "Footnote.HO.HHT.checked.for.risk.of.falling.23",
                 "Footnote.HO.HHT.checked.for.depression.25",
                 "Footnote.HO.HHT.ensured.received.flu.shot.27",
                 "Footnote.HO.HHT.made.received.pneumonia.shot.29",
                 "Footnote.HO.HHT.taught.gave.foot.care.31",
                 "Footnote.HO.HHT.checked.for.pain.33",
                 "Footnote.HO.HHT.treated.for.pain.35",
                 "Footnote.HO.HHT.treated.heart.failure.weakening.of.the.heart.37",  
                 "Footnote.HO.HHT.took.action.to.prevent.pressure.bed.sores.39",
                 "FootnoteHO.HHT.included.treatm.to.prevent.pressure.bed.sores.41",
                 "Footnote.HO.HHT.checked.for.risk.of.pressure.bed.sores.43",
                 "Footnote.HO.PAT.got.better.at.moving.around.45",
                 "Footnote.HO.PAT.got.better.at.getting.in.and.out.of.bed.47",
                 "Footnote.HO.PAT..got.better.at.bathing.49",
                 "Footnote.HO.PAT.had.less.pain.when.moving.around.51",
                 "Footnote.HO.PAT.breathing.improved.53",
                 "Footnote.HO.PAT.wounds.improved.healed.55",
                 "Footnote.HO.PAT.got.better.at.taking.drugs.correctly.57",
                 "Footnote.HO.PAT.needed.urgent.unplanned.ER.wout.admission.59",
                 "Footnote.HO.PAT.had.to.be.admitted.to.the.hospital.61",
                 "Footnote")

vars <- setdiff(all.vars,remove.vars)

providers.df.201401 <- hhc.prvdr.csv.201401[vars]
providers.df.201402 <- hhc.prvdr.csv.201402[vars]
providers.df.201403 <- hhc.prvdr.csv.201403[vars]
providers.df.201404 <- hhc.prvdr.csv.201404[vars]

# CREATE NEW FIELD TO STORE THE YEAR QUARTER OF THE FILE
providers.df.201401$year.quarter <- 201401
providers.df.201402$year.quarter <- 201402
providers.df.201403$year.quarter <- 201403
providers.df.201404$year.quarter <- 201404

providers.df.201401$Quality.of.Patient.Care.Star.Rating <- 0
providers.df.201402$Quality.of.Patient.Care.Star.Rating <- 0
providers.df.201403$Quality.of.Patient.Care.Star.Rating <- 0
providers.df.201404$Quality.of.Patient.Care.Star.Rating <- 0

providers.df.201401$HO.PAT.was.re.admitted.to.hospital.62 <- 0
providers.df.201402$HO.PAT.was.re.admitted.to.hospital.62 <- 0
providers.df.201403$HO.PAT.was.re.admitted.to.hospital.62 <- 0
providers.df.201404$HO.PAT.was.re.admitted.to.hospital.62 <- 0

providers.df.201401$HO.PAT.received.ER.care.wout.re.admission.64 <- 0
providers.df.201402$HO.PAT.received.ER.care.wout.re.admission.64 <- 0
providers.df.201403$HO.PAT.received.ER.care.wout.re.admission.64 <- 0
providers.df.201404$HO.PAT.received.ER.care.wout.re.admission.64 <- 0


dim(providers.df.201401)  # 12445   37
dim(providers.df.201402)  # 12446   37
dim(providers.df.201403)  # 12414   37
dim(providers.df.201404)  # 12361   37

providers.df.2014 <- rbind(providers.df.201401,providers.df.201402,providers.df.201403,providers.df.201404)
dim(providers.df.2014) # 49666 37
View(providers.df.2014)

# Save the .RData 
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
save(list = c('providers.df.2014'), file = 'providers.df.2014.RData')

colnames(providers.df.2014)

providers.df.2014[c(
  "State",
  "Provider.Name",
  "City",
  "Zip",
  "Type.of.Ownership",
  "Offers.Nursing.Care.Services",
  "Offers.Physical.Therapy.Services",
  "Offers.Occupational.Therapy.Services",
  "Offers.Speech.Pathology.Services",
  "Offers.Medical.Social.Services",
  "Offers.Home.Health.Aide.Services",
  "Quality.of.Patient.Care.Star.Rating",
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
  "HO.PAT.had.to.be.admitted.to.the.hospital.60")]


