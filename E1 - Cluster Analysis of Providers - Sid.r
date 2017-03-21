##  R Code File Name:  E1 - Cluster Analysis of Providers - Sid.r


############################################################################################################
# Perform Cluster Analysis on the updated CMS data set to understand factors that distinguish
# good providers from bad.
############################################################################################################

setwd("C:/Users/Siddartha Sathyanara/Dropbox/Team Project/CMS Data Files/RData Files")
load("overall.rating.df.Rdata")

temp.df <- overall.rating.df

#check all variable names
names.temp.df <- names(overall.rating.df)
names.temp.df <- as.data.frame(names.temp.df)
View(names.temp.df)

# Explore Distribution of HHC Rating for providers
library(lattice)
histogram(temp.df$overall.hhc.rating, main = "Distribution of Overall HHC Rating", xlab="Overall HHC Rating")


# Running Clustering algorithm on complete set of 3430 variables is causing memory issues
# So, setting up smaller data frame for clustering 

summary(temp.df$HO.HHT.began.care.in.timely.manner.18)

temp.df$HO.HHT.began.care.in.timely.manner.18 <-  temp.df$HO.HHT.began.care.in.timely.manner.18/100
temp.df$HO.HHT.taught.about.their.drugs.20 <-  temp.df$HO.HHT.taught.about.their.drugs.20/100
temp.df$HO.HHT.checked.for.risk.of.falling.22 <-  temp.df$HO.HHT.checked.for.risk.of.falling.22/100
temp.df$HO.HHT.checked.for.depression.24 <-  temp.df$HO.HHT.checked.for.depression.24/100
temp.df$HO.HHT.ensured.received.flu.shot.26 <-  temp.df$HO.HHT.ensured.received.flu.shot.26/100
temp.df$HO.HHT.made.received.pneumonia.shot.28 <-  temp.df$HO.HHT.made.received.pneumonia.shot.28/100
temp.df$HO.HHT.taught.gave.foot.care.30 <-  temp.df$HO.HHT.taught.gave.foot.care.30/100
temp.df$HO.HHT.checked.for.pain.32 <-  temp.df$HO.HHT.checked.for.pain.32/100
temp.df$HO.HHT.treated.for.pain.34 <-  temp.df$HO.HHT.treated.for.pain.34/100
temp.df$HO.HHT.treated.heart.failure.weakening.of.the.heart.36 <-  temp.df$HO.HHT.treated.heart.failure.weakening.of.the.heart.36/100
temp.df$HO.HHT.took.action.to.prevent.pressure.bed.sores.38 <-  temp.df$HO.HHT.took.action.to.prevent.pressure.bed.sores.38/100
temp.df$HO.HHT.included.treatm.to.prevent.pressure.bed.sores.40 <-  temp.df$HO.HHT.included.treatm.to.prevent.pressure.bed.sores.40/100
temp.df$HO.HHT.checked.for.risk.of.pressure.bed.sores.42 <-  temp.df$HO.HHT.checked.for.risk.of.pressure.bed.sores.42/100
temp.df$HO.PAT.got.better.at.moving.around.44 <-  temp.df$HO.PAT.got.better.at.moving.around.44/100
temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46 <-  temp.df$HO.PAT.got.better.at.getting.in.and.out.of.bed.46/100
temp.df$HO.PAT..got.better.at.bathing.48 <-  temp.df$HO.PAT..got.better.at.bathing.48/100
temp.df$HO.PAT.had.less.pain.when.moving.around.50 <-  temp.df$HO.PAT.had.less.pain.when.moving.around.50/100
temp.df$HO.PAT.breathing.improved.52 <- temp.df$HO.PAT.breathing.improved.52/100
temp.df$HO.PAT.wounds.improved.healed.54 <-  temp.df$HO.PAT.wounds.improved.healed.54/100
temp.df$HO.PAT.got.better.at.taking.drugs.correctly.56 <-  temp.df$HO.PAT.got.better.at.taking.drugs.correctly.56/100
temp.df$HO.PAT.needed.urgent.unplanned.ER.wout.admission.58 <-  temp.df$HO.PAT.needed.urgent.unplanned.ER.wout.admission.58/100
temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60 <- temp.df$HO.PAT.had.to.be.admitted.to.the.hospital.60/100


myvars <- c ("HO.HHT.began.care.in.timely.manner.18",
             "HO.HHT.taught.about.their.drugs.20",
             "HO.HHT.ensured.received.flu.shot.26",
             "HO.PAT.got.better.at.moving.around.44",
             "HO.PAT.got.better.at.getting.in.and.out.of.bed.46",
             "HO.PAT..got.better.at.bathing.48",
             "HO.PAT.had.less.pain.when.moving.around.50",
             "HO.PAT.breathing.improved.52",
             "HO.PAT.had.to.be.admitted.to.the.hospital.60",
             "HO.HHT.checked.for.depression.24",
             "HO.HHT.checked.for.pain.32",
             "HO.HHT.checked.for.risk.of.falling.22",
             "HO.HHT.checked.for.risk.of.pressure.bed.sores.42",
             "HO.HHT.included.treatm.to.prevent.pressure.bed.sores.40",
             "HO.HHT.made.received.pneumonia.shot.28",
             "HO.HHT.taught.gave.foot.care.30",
             "HO.HHT.took.action.to.prevent.pressure.bed.sores.38",
             "HO.HHT.treated.for.pain.34",
             "HO.HHT.treated.heart.failure.weakening.of.the.heart.36",
             "HO.PAT.got.better.at.taking.drugs.correctly.56",
             "HO.PAT.needed.urgent.unplanned.ER.wout.admission.58",
             "HO.PAT.wounds.improved.healed.54",
             "Offers.Medical.Social.ServicesN",
             "Offers.Medical.Social.ServicesY",
             "StateCA",
             "StateFL",
             "StateTX",
             "Type.of.Ownershipproprietary")

HHCtrain <- temp.df[myvars]

dim(HHCtrain) #107471 28

# Trying out Clustering method from Jim Porzak's document

require(lattice)
require(grDevices)
require(vcd)
require(flexclust)

## set up flexclust control object
fc_cont <- new("flexclustControl")
fc_cont@tolerance <- 0.1 
fc_cont@iter.max <- 100 
my_seed <- 1
my_family <- "ejaccard"
num_clust <- 5
my_seed <- my_seed + 1
set.seed(my_seed)
cl <- kcca(HHCtrain, k = num_clust, save.data = TRUE, control = fc_cont,
           family = kccaFamily(my_family))

summary(cl)

pop_av_dist <- with(cl@clusinfo, sum(size*av_dist)/sum(size))

main_txt <- paste("kcca ", cl@family@name, " - ",num_clust, " clusters (", "k sample, seed = ", my_seed,")", sep = "")

# Neighborhood Graph on 1st principle components
csb.pca <- prcomp(HHCtrain)

plot(csb.pca, scale = TRUE, xlab = "Principal Components", main = "Principal Component Analysis")

csb.pca

plot(cl, data = as.matrix(HHCtrain), project = csb.pca,
     main = main_txt,
     sub = paste("\nAv Dist = ", format(pop_av_dist, digits = 5),
                 ", k = ", cl@k, sep = ""))


# Activity Profiles for each segment
print(barchart(cl, main = main_txt, strip.prefix = "#",
               scales = list(cex = 0.75)))