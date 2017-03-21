##  R Code File Name:  D2 - EDA for Binning - Sid.r

###  SAVE THE FINALIZED DATA SET WITHOUT OUTLIERS AND OBSERVATIONS THAT HAVE AT LEAST ONE OF THE
###  EIGHT MEASURES WITH A VALUE > 0.
setwd("C:/Users/Sandra/Dropbox/Capstone 498 - Don Wedding/Team Project/CMS Data Files/RData Files")
load("Ini.Decile.Rating.df.RData")
dim(Ini.Decile.Rating.df)  # 107471   3406

idr.df <- Ini.Decile.Rating.df
rm(Ini.Decile.Rating.df)

idr.df.names <- names(idr.df)
idr.df.names.df <- as.data.frame(idr.df.names)
View(idr.df.names.df)

idr.measures.vars <- c("HO.HHT.began.care.in.timely.manner.18",
                       "HO.HHT.taught.about.their.drugs.20",
                       "HO.HHT.ensured.received.flu.shot.26",
                       "HO.PAT.got.better.at.moving.around.44",
                       "HO.PAT.got.better.at.getting.in.and.out.of.bed.46",
                       "HO.PAT..got.better.at.bathing.48",
                       "HO.PAT.had.less.pain.when.moving.around.50",
                       "HO.PAT.breathing.improved.52",
                       "HO.PAT.had.to.be.admitted.to.the.hospital.60",
                       "IDR.SCORE.18",
                       "IDR.SCORE.20",
                       "IDR.SCORE.26",
                       "IDR.SCORE.44",
                       "IDR.SCORE.46",
                       "IDR.SCORE.48",
                       "IDR.SCORE.50",
                       "IDR.SCORE.52",
                       "IDR.SCORE.60"
)

idr.measures.df <- idr.df[idr.measures.vars]
dim(idr.measures.df)

View(idr.measures.df)
