## R Code File Name:  C2 - EDA of Process vs Outcomes - Sid.r


########################################################################
# Perform EDA on measures that each Health Care Agency is rated on. 
##########################################################################


# Load THE DATA SET WITH THE DICHOTOMIZED CATEGORICAL VARIABLES AND IMPUTED NUMERIC VARIABLES WITH VALUES GREATER THAN 100
setwd("C:/Users/Siddartha Sathyanara/Dropbox/Team Project/CMS Data Files/RData Files")

load("flag.1.df.RData")
temp.df <- flag.1.df

#check all varaible names
names.temp.df <- names(temp.df)
names.temp.df <- as.data.frame(names.temp.df)
View(names.temp.df)

# Remove Measures that are not part of the 8 being used to calculate the Quality Star Rating
temp.df[,"HO.HHT.checked.for.risk.of.falling.22"] <- NULL
temp.df[,"HO.HHT.checked.for.depression.24"] <- NULL          
temp.df[,"HO.HHT.made.received.pneumonia.shot.28"] <- NULL               
temp.df[,"HO.HHT.taught.gave.foot.care.30"] <- NULL  
temp.df[,"HO.HHT.checked.for.pain.32"] <- NULL          
temp.df[,"HO.HHT.treated.for.pain.34"] <- NULL                     
temp.df[,"HO.HHT.treated.heart.failure.weakening.of.the.heart.36"] <- NULL
temp.df[,"HO.HHT.took.action.to.prevent.pressure.bed.sores.38"] <- NULL
temp.df[,"HO.HHT.included.treatm.to.prevent.pressure.bed.sores.40"] <- NULL
temp.df[,"HO.HHT.checked.for.risk.of.pressure.bed.sores.42"] <- NULL
temp.df[,"HO.PAT.wounds.improved.healed.54"] <- NULL
temp.df[,"HO.PAT.got.better.at.taking.drugs.correctly.56"] <- NULL
temp.df[,"HO.PAT.needed.urgent.unplanned.ER.wout.admission.58"] <- NULL
temp.df[,"HO.PAT.had.to.be.admitted.to.the.hospital.60"] <- NULL

#Recheck all variable names
names.temp.df <- names(temp.df)
names.temp.df <- as.data.frame(names.temp.df)
View(names.temp.df)

## EDA plots using base graphics
library(ggplot2)
library(lattice)
#creating density plots for each quantitative variables
pdf("HHCHistograms.pdf",width = 30, height = 13)
{
p = histogram(~HO.HHT.began.care.in.timely.manner.18, data=temp.df, plot.points= FALSE, ref=TRUE)
print(p)
p = histogram(~HO.HHT.taught.about.their.drugs.20, data=temp.df, plot.points= FALSE, ref=TRUE)
print(p)
p = histogram(~HO.HHT.ensured.received.flu.shot.26, data=temp.df, plot.points= FALSE, ref=TRUE)
print(p)
p = histogram(~HO.PAT.got.better.at.moving.around.44, data=temp.df, plot.points= FALSE, ref=TRUE)
print(p)
p = histogram(~HO.PAT.got.better.at.getting.in.and.out.of.bed.46, data=temp.df, plot.points= FALSE, ref=TRUE)
print(p)
p = histogram(~HO.PAT..got.better.at.bathing.48, data=temp.df, plot.points= FALSE, ref=TRUE)
print(p)
p = histogram(~HO.PAT.had.less.pain.when.moving.around.50, data=temp.df, plot.points= FALSE, ref=TRUE)
print(p)
p = histogram(~HO.PAT.breathing.improved.52, data=temp.df, plot.points= FALSE, ref=TRUE)
print(p)
p = histogram(~HO.PAT.was.re.admitted.to.hospital.62, data=temp.df, plot.points= FALSE, ref=TRUE)
print(p)
}
dev.off()

#EDA to understand the effects of Timely Care on Getting Better Moving based on Type of Ownership
temp.govtvol.df <- subset(temp.df, temp.df$Type.of.Ownershipcombination.govt.and.voluntary == 1)
plot(temp.govtvol.df$HO.HHT.began.care.in.timely.manner.18, temp.govtvol.df$HO.PAT.got.better.at.moving.around.44,
     xlab = "Timely Care", ylab = "Got Better Moving",
     main = "For Govt and Vol", pch = 16)

temp.Govtlocal.df <- subset(temp.df, temp.df$Type.of.Ownershipgovt.local == 1)
plot(temp.Govtlocal.df$HO.HHT.began.care.in.timely.manner.18, temp.Govtlocal.df$HO.PAT.got.better.at.moving.around.44,
     xlab = "Timely Care", ylab = "Got Better Moving",
     main = "For Govt or Local", pch = 16)

temp.GovtStateCounty.df <- subset(temp.df, temp.df$Type.of.Ownershipgovt.state.or.county == 1)
plot(temp.GovtStateCounty.df$HO.HHT.began.care.in.timely.manner.18, temp.GovtStateCounty.df$HO.PAT.got.better.at.moving.around.44,
     xlab = "Timely Care", ylab = "Got Better Moving",
     main = "For Govt or State or County", pch = 16)

temp.Local.df <- subset(temp.df, temp.df$Type.of.Ownershiplocal == 1)
plot(temp.Local.df$HO.HHT.began.care.in.timely.manner.18, temp.Local.df$HO.PAT.got.better.at.moving.around.44,
     xlab = "Timely Care", ylab = "Got Better Moving",
     main = "For Local", pch = 16)

temp.Other.df <- subset(temp.df, temp.df$Type.of.Ownershipother == 1)
plot(temp.Other.df$HO.HHT.began.care.in.timely.manner.18, temp.Other.df$HO.PAT.got.better.at.moving.around.44,
     xlab = "Timely Care", ylab = "Got Better Moving",
     main = "For Other", pch = 16)

temp.Private.df <- subset(temp.df, temp.df$Type.of.Ownershipprivate == 1)
plot(temp.Private.df$HO.HHT.began.care.in.timely.manner.18, temp.Private.df$HO.PAT.got.better.at.moving.around.44,
     xlab = "Timely Care", ylab = "Got Better Moving",
     main = "For Private", pch = 16)

temp.Proprietary.df <- subset(temp.df, temp.df$Type.of.Ownershipproprietary == 1)
plot(temp.Proprietary.df$HO.HHT.began.care.in.timely.manner.18, temp.Proprietary.df$HO.PAT.got.better.at.moving.around.44,
     xlab = "Timely Care", ylab = "Got Better Moving",
     main = "For Prorietary", pch = 16)

#EDA to understand the effects of Timely Care on Got Better in and out of Bed based on Type of Ownership
temp.govtvol.df <- subset(temp.df, temp.df$Type.of.Ownershipcombination.govt.and.voluntary == 1)
plot(temp.govtvol.df$HO.HHT.began.care.in.timely.manner.18, temp.govtvol.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46,
     xlab = "Timely Care", ylab = "Got Better at Getting and Out of Bed",
     main = "For Govt and Vol", pch = 16)

temp.Govtlocal.df <- subset(temp.df, temp.df$Type.of.Ownershipgovt.local == 1)
plot(temp.Govtlocal.df$HO.HHT.began.care.in.timely.manner.18, temp.Govtlocal.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46,
     xlab = "Timely Care", ylab = "Got Better at Getting and Out of Bed",
     main = "For Govt or Local", pch = 16)

temp.GovtStateCounty.df <- subset(temp.df, temp.df$Type.of.Ownershipgovt.state.or.county == 1)
plot(temp.GovtStateCounty.df$HO.HHT.began.care.in.timely.manner.18, temp.GovtStateCounty.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46,
     xlab = "Timely Care", ylab = "Got Better at Getting and Out of Bed",
     main = "For Govt or State or County", pch = 16)

temp.Local.df <- subset(temp.df, temp.df$Type.of.Ownershiplocal == 1)
plot(temp.Local.df$HO.HHT.began.care.in.timely.manner.18, temp.Local.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46,
     xlab = "Timely Care", ylab = "Got Better at Getting and Out of Bed",
     main = "For Local", pch = 16)

temp.Other.df <- subset(temp.df, temp.df$Type.of.Ownershipother == 1)
plot(temp.Other.df$HO.HHT.began.care.in.timely.manner.18, temp.Other.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46,
     xlab = "Timely Care", ylab = "Got Better at Getting and Out of Bed",
     main = "For Other", pch = 16)

temp.Private.df <- subset(temp.df, temp.df$Type.of.Ownershipprivate == 1)
plot(temp.Private.df$HO.HHT.began.care.in.timely.manner.18, temp.Private.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46,
     xlab = "Timely Care", ylab = "Got Better at Getting and Out of Bed",
     main = "For Private", pch = 16)

temp.Proprietary.df <- subset(temp.df, temp.df$Type.of.Ownershipproprietary == 1)
plot(temp.Proprietary.df$HO.HHT.began.care.in.timely.manner.18, temp.Proprietary.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46,
     xlab = "Timely Care", ylab = "Got Better at Getting and Out of Bed",
     main = "For Prorietary", pch = 16)

#EDA to understand the effects of Timely Care on Got Better at Bathing based on Type of Ownership
temp.govtvol.df <- subset(temp.df, temp.df$Type.of.Ownershipcombination.govt.and.voluntary == 1)
plot(temp.govtvol.df$HO.HHT.began.care.in.timely.manner.18, temp.govtvol.df$HO.PAT..got.better.at.bathing.48,
     xlab = "Timely Care", ylab = "Got better at Bathing",
     main = "For Govt and Vol", pch = 16)

temp.Govtlocal.df <- subset(temp.df, temp.df$Type.of.Ownershipgovt.local == 1)
plot(temp.Govtlocal.df$HO.HHT.began.care.in.timely.manner.18, temp.Govtlocal.df$HO.PAT..got.better.at.bathing.48,
     xlab = "Timely Care", ylab = "Got better at Bathing",
     main = "For Govt or Local", pch = 16)

temp.GovtStateCounty.df <- subset(temp.df, temp.df$Type.of.Ownershipgovt.state.or.county == 1)
plot(temp.GovtStateCounty.df$HO.HHT.began.care.in.timely.manner.18, temp.GovtStateCounty.df$HO.PAT..got.better.at.bathing.48,
     xlab = "Timely Care", ylab = "Got better at Bathing",
     main = "For Govt or State or County", pch = 16)

temp.Local.df <- subset(temp.df, temp.df$Type.of.Ownershiplocal == 1)
plot(temp.Local.df$HO.HHT.began.care.in.timely.manner.18, temp.Local.df$HO.PAT..got.better.at.bathing.48,
     xlab = "Timely Care", ylab = "Got better at Bathing",
     main = "For Local", pch = 16)

temp.Other.df <- subset(temp.df, temp.df$Type.of.Ownershipother == 1)
plot(temp.Other.df$HO.HHT.began.care.in.timely.manner.18, temp.Other.df$HO.PAT..got.better.at.bathing.48,
     xlab = "Timely Care", ylab = "Got better at Bathing",
     main = "For Other", pch = 16)

temp.Private.df <- subset(temp.df, temp.df$Type.of.Ownershipprivate == 1)
plot(temp.Private.df$HO.HHT.began.care.in.timely.manner.18, temp.Private.df$HO.PAT..got.better.at.bathing.48,
     xlab = "Timely Care", ylab = "Got better at Bathing",
     main = "For Private", pch = 16)

temp.Proprietary.df <- subset(temp.df, temp.df$Type.of.Ownershipproprietary == 1)
plot(temp.Proprietary.df$HO.HHT.began.care.in.timely.manner.18, temp.Proprietary.df$HO.PAT..got.better.at.bathing.48,
     xlab = "Timely Care", ylab = "Got better at Bathing",
     main = "For Prorietary", pch = 16)

# Rename data set
hhc.final.for.clustering.df <- temp.df

save(list = c('hhc.final.for.clustering.df'), file = 'hhc.final.for.scoring.df.RData')
load("hhc.final.for.clustering.df.RData")


