
require(dplyr)
require(spatstat)
require(reshape2)

dat <- read.csv("map.dat.all.csv")

# remove unnecessary columns
dat <- dat %>% 
  select(year=Year, plot=Plot, ID, X, Y, 
         species=SPEC, diam=D, status=CC)

# years that each plot was sampled from 1989 - present (not using 2014 data)
# plot 12: 90, 97
# plot 13: 89, 97
# plot 14: 90, 97
# plot 91: 91, 98
# plot 93: 91, 98
# plot 97: 90, 98

# remove survey years occurring before 1989 and create separate dfs for each plot
d12 <- dat[dat$plot==12,]
keep <- c(1990,1997)
d12 <- d12[d12$year %in% keep,]

d13 <- dat[dat$plot==13,]
keep <- c(1989,1997)
d13 <- d13[d13$year %in% keep,]

d14 <- dat[dat$plot==14,]
keep <- c(1990,1997)
d14 <- d14[d14$year %in% keep,]

d91 <- dat[dat$plot==91,]
keep <- c(1991,1998)
d91 <- d91[d91$year %in% keep,]

d93 <- dat[dat$plot==93,]
keep <- c(1991,1998)
d93 <- d93[d93$year %in% keep,]

d97 <- dat[dat$plot==97,]
keep <- c(1990,1998)
d97 <- d97[d97$year %in% keep,]

# X, Y coordinates should be in meters; convert where necessary
d12$X <- d12$X/10
d12$Y <- d12$Y/10

d91$X <- d91$X/10
d91$Y <- d91$Y/10

d97$X <- d97$X/10
d97$Y <- d97$Y/10

# separate each plot by year sampled, do merges, calculate growth rates, then rbind 
# plot 12: 90, 97, 14
d12.90 <- d12 %>% 
  filter(year==1990)
d12.97 <- d12 %>% 
  filter(year==1997)

d12.90 <- d12.90 %>% 
  select(-year)
d12.90 <- d12.90 %>% 
  rename(diam1=diam)
d12.97 <- d12.97 %>% 
  select(-year)
d12.97 <- d12.97 %>% 
  rename(diam2=diam)
d12 <- merge(d12.90,d12.97,by=c("ID","plot","species","X","Y"),all=TRUE)

# there are some diam1==0 (all with status '4'). Remove them
d12 <- d12[which(is.na(d12$diam1) | d12$diam1>0),]

# remove status.x that are 'missing' or 'dead'
# for time period 1, OK to keep NAs, b/c this means ingrowth
# don't keep NAs for time period 2, b/c not accounting for ingrowth in 
# last sampling period
keep <- c(1,4,6)
d12 <- d12[which(is.na(d12$status.x) | d12$status.x %in% keep),]

# remove stems that have NA for both diam1 and diam2 (i.e., we don't care
# about ingrowth during the third survey) Same for diam2, diam3 - this 
# means something died between first two sampling periods, and we've already
# accounted for that in df d12
d12 <- d12[which(!is.na(d12$diam1) | !is.na(d12$diam2)),]

# ingrowth in first time period is accounted for in d12b, so remove it
# in d12 (i.e., remove diam1==NA)
d12 <- d12[which(!is.na(d12$diam1)),]

# calculate growth rate
d12$growth <- ifelse(!is.na(d12$diam1) & !is.na(d12$diam2),
                      (d12$diam2-d12$diam1)/7,NA)


# if condition code is 2, 3, or 5, diam2 & growth rate should be NA
dead <- c(2,3,5)
d12$diam2 <- ifelse(d12$status.y %in% dead,NA,d12$diam2)
d12$growth <- ifelse(d12$status.y %in% dead,NA,d12$growth)

# add survival column
surv <- c(1,4,6)
d12$surv <- ifelse(d12$status.y %in% surv,1,0)



# plot 13: 89, 97
d13.89 <- d13 %>% 
  filter(year==1989)
d13.97 <- d13 %>% 
  filter(year==1997)

d13.89 <- d13.89 %>% 
  select(-year)
d13.89 <- d13.89 %>% 
  rename(diam1=diam)
d13.97 <- d13.97 %>% 
  select(-year)
d13.97 <- d13.97 %>% 
  rename(diam2=diam)
d13 <- merge(d13.89,d13.97,by=c("ID","plot","species","X","Y"),all=TRUE)



# remove status.x that are 'missing' or 'dead'
# for time period 1, OK to keep NAs, b/c this means ingrowth
# don't keep NAs for time period 2, b/c not accounting for ingrowth in 
# last sampling period
keep <- c(1,4,6)
d13 <- d13[which(is.na(d13$status.x) | d13$status.x %in% keep),]

# remove stems that have NA for both diam1 and diam2 (i.e., we don't care
# about ingrowth during the third survey) Same for diam2, diam3 - this 
# means something died between first two sampling periods, and we've already
# accounted for that in df d13
d13 <- d13[which(!is.na(d13$diam1) | !is.na(d13$diam2)),]

# ingrowth in first time period is accounted for in d13b, so remove it
# in d13 (i.e., remove diam1==NA)
d13 <- d13[which(!is.na(d13$diam1)),]

# calculate growth rate
d13$growth <- ifelse(!is.na(d13$diam1) & !is.na(d13$diam2),
                      (d13$diam2-d13$diam1)/8,NA)


# if condition code is 2, 3, or 5, diam2 & growth rate should be NA
dead <- c(2,3,5)
d13$diam2 <- ifelse(d13$status.y %in% dead,NA,d13$diam2)
d13$growth <- ifelse(d13$status.y %in% dead,NA,d13$growth)

# add survival column
surv <- c(1,4,6)
d13$surv <- ifelse(d13$status.y %in% surv,1,0)




# plot 14: 90, 97
d14.90 <- d14 %>% 
  filter(year==1990)
d14.97 <- d14 %>% 
  filter(year==1997)

d14.90 <- d14.90 %>% 
  select(-year)
d14.90 <- d14.90 %>% 
  rename(diam1=diam)
d14.97 <- d14.97 %>% 
  select(-year)
d14.97 <- d14.97 %>% 
  rename(diam2=diam)
d14 <- merge(d14.90,d14.97,by=c("ID","plot","species","X","Y"),all=TRUE)


# there are some diam1==0 (all with status '4'). Remove them
d14 <- d14[which(is.na(d14$diam1) | d14$diam1>0),]

# remove status.x that are 'missing' or 'dead'
# for time period 1, OK to keep NAs, b/c this means ingrowth
# don't keep NAs for time period 2, b/c not accounting for ingrowth in 
# last sampling period
keep <- c(1,4,6)
d14 <- d14[which(is.na(d14$status.x) | d14$status.x %in% keep),]

# remove stems that have NA for both diam1 and diam2 (i.e., we don't care
# about ingrowth during the third survey) Same for diam2, diam3 - this 
# means something died between first two sampling periods, and we've already
# accounted for that in df d14
d14 <- d14[which(!is.na(d14$diam1) | !is.na(d14$diam2)),]

# ingrowth in first time period is accounted for in d14b, so remove it
# in d14 (i.e., remove diam1==NA)
d14 <- d14[which(!is.na(d14$diam1)),]

# calculate growth rate
d14$growth <- ifelse(!is.na(d14$diam1) & !is.na(d14$diam2),
                      (d14$diam2-d14$diam1)/7,NA)


# if condition code is 2, 3, or 5, diam2 & growth rate should be NA
dead <- c(2,3,5)
d14$diam2 <- ifelse(d14$status.y %in% dead,NA,d14$diam2)
d14$growth <- ifelse(d14$status.y %in% dead,NA,d14$growth)

# add survival column
surv <- c(1,4,6)
d14$surv <- ifelse(d14$status.y %in% surv,1,0)



# plot 91: 91, 98
d91.91 <- d91 %>% 
  filter(year==1991)
d91.98 <- d91 %>% 
  filter(year==1998)

d91.91 <- d91.91 %>% 
  select(-year)
d91.91 <- d91.91 %>% 
  rename(diam1=diam)
d91.98 <- d91.98 %>% 
  select(-year)
d91.98 <- d91.98 %>% 
  rename(diam2=diam)
d91 <- merge(d91.91,d91.98,by=c("ID","plot","species","X","Y"),all=TRUE)


# IF there are some diam1==0 (all with status '4'). Remove them (there shouldn't be)
#d91 <- d91[which(is.na(d91$diam1) | d91$diam1>0),]

# remove status.x that are 'missing' or 'dead'
# for time period 1, OK to keep NAs, b/c this means ingrowth
# don't keep NAs for time period 2, b/c not accounting for ingrowth in 
# last sampling period
keep <- c(1,4,6)
d91 <- d91[which(is.na(d91$status.x) | d91$status.x %in% keep),]

# remove stems that have NA for both diam1 and diam2 (i.e., we don't care
# about ingrowth during the third survey) Same for diam2, diam3 - this 
# means something died between first two sampling periods, and we've already
# accounted for that in df d91
d91 <- d91[which(!is.na(d91$diam1) | !is.na(d91$diam2)),]


# ingrowth in first time period is accounted for in d91b, so remove it
# in d91 (i.e., remove diam1==NA)
d91 <- d91[which(!is.na(d91$diam1)),]

# calculate growth rate
d91$growth <- ifelse(!is.na(d91$diam1) & !is.na(d91$diam2),
                      (d91$diam2-d91$diam1)/7,NA)


# if condition code is 2, 3, or 5, diam2 & growth rate should be NA
dead <- c(2,3,5)
d91$diam2 <- ifelse(d91$status.y %in% dead,NA,d91$diam2)
d91$growth <- ifelse(d91$status.y %in% dead,NA,d91$growth)

# add survival column
surv <- c(1,4,6)
d91$surv <- ifelse(d91$status.y %in% surv,1,0)



# plot 93: 91, 98
d93.91 <- d93 %>% 
  filter(year==1991)
d93.98 <- d93 %>% 
  filter(year==1998)

d93.91 <- d93.91 %>% 
  select(-year)
d93.91 <- d93.91 %>% 
  rename(diam1=diam)
d93.98 <- d93.98 %>% 
  select(-year)
d93.98 <- d93.98 %>% 
  rename(diam2=diam)
d93 <- merge(d93.91,d93.98,by=c("ID","plot","species","X","Y"),all=TRUE)

# there are some diam1==0 (all with status '4'). Remove them
d93 <- d93[which(is.na(d93$diam1) | d93$diam1>0),]

# remove status.x that are 'missing' or 'dead'
keep <- c(1,4,6)
d93 <- d93[which(d93$status.x %in% keep),]


d93$growth <- ifelse(!is.na(d93$diam1) & !is.na(d93$diam2),
                      (d93$diam2-d93$diam1)/7,NA)



# if condition code is 2, 3, or 5, diam2 & growth rate should be NA
dead <- c(2,3,5)
d93$diam2 <- ifelse(d93$status.y %in% dead,NA,d93$diam2)
d93$growth <- ifelse(d93$status.y %in% dead,NA,d93$growth)

# add survival column
surv <- c(1,4,6)
d93$surv <- ifelse(d93$status.y %in% surv,1,0)



# plot 97: 90, 98
d97.90 <- d97 %>% 
  filter(year==1990)
d97.98 <- d97 %>% 
  filter(year==1998)

d97.90 <- d97.90 %>% 
  select(-year)
d97.90 <- d97.90 %>% 
  rename(diam1=diam)
d97.98 <- d97.98 %>% 
  select(-year)
d97.98 <- d97.98 %>% 
  rename(diam2=diam)
d97 <- merge(d97.90,d97.98,by=c("ID","plot","species","X","Y"),all=TRUE)


# IF there are some diam1==0 (all with status '4'). Remove them (shouldn't be)
#d97 <- d97[which(is.na(d97$diam1) | d97$diam1>0),]

# remove status.x that are 'missing' or 'dead'
# for time period 1, OK to keep NAs, b/c this means ingrowth
# don't keep NAs for time period 2, b/c not accounting for ingrowth in 
# last sampling period
keep <- c(1,4,6)
d97 <- d97[which(is.na(d97$status.x) | d97$status.x %in% keep),]

# remove stems that have NA for both diam1 and diam2 (i.e., we don't care
# about ingrowth during the third survey) Same for diam2, diam3 - this 
# means something died between first two sampling periods, and we've already
# accounted for that in df d97
d97 <- d97[which(!is.na(d97$diam1) | !is.na(d97$diam2)),]

# ingrowth in first time period is accounted for in d97b, so remove it
# in d97 (i.e., remove diam1==NA)
d97 <- d97[which(!is.na(d97$diam1)),]

# calculate growth rate
d97$growth <- ifelse(!is.na(d97$diam1) & !is.na(d97$diam2),
                      (d97$diam2-d97$diam1)/8,NA)

# if condition code is 2, 3, or 5, diam2 & growth rate should be NA
dead <- c(2,3,5)
d97$diam2 <- ifelse(d97$status.y %in% dead,NA,d97$diam2)
d97$growth <- ifelse(d97$status.y %in% dead,NA,d97$growth)

# add survival column
surv <- c(1,4,6)
d97$surv <- ifelse(d97$status.y %in% surv,1,0)


# combine all plot dataframes into one
dat <- rbind(d12,d13,d14,d91,d93,d97)


# categorize size classes based on FIA designations
# 1 - 2.54cm DBH -> large seedling
# 2.54 - 12.7cm -> sapling
# > 12.7 -> tree

dat <- dat %>% mutate(size_class = ifelse(diam1 <= 2.54, "seed", 
                                  ifelse(diam1 < 12.7 & diam1 > 2.54, "sap", "tree")))


# START OF SPATIAL CODE - MEASURING DISTANCES BETWEEN STEMS (SEED VS. SAP&TREE; SAP VS. TREE)
# AT FOUR DIFFERENT SPATIAL SCALES (5, 10, 15, 20 meters)
##############################################################

# separate size classes and reduce # columns (only need X, Y, ID, plot)
seed <- dat %>% filter(size_class=="seed") %>% select(X, Y, ID, plot)
sap <- dat %>% filter(size_class=="sap") %>% select(X, Y, ID, plot)
tree <- dat %>% filter(size_class=="tree") %>% select(X, Y, ID, plot)
sap_tree <- dat %>% filter(size_class=="sap" | size_class=="tree") %>% select(X, Y, ID, plot)

# separate by plot
seed12 <- seed %>% filter(plot==12) %>% select(-plot)
seed13 <- seed %>% filter(plot==13) %>% select(-plot)
seed14 <- seed %>% filter(plot==14) %>% select(-plot)
seed91 <- seed %>% filter(plot==91) %>% select(-plot)
seed93 <- seed %>% filter(plot==93) %>% select(-plot)
seed97 <- seed %>% filter(plot==97) %>% select(-plot)

sap12 <- sap %>% filter(plot==12) %>% select(-plot)
sap13 <- sap %>% filter(plot==13) %>% select(-plot)
sap14 <- sap %>% filter(plot==14) %>% select(-plot)
sap91 <- sap %>% filter(plot==91) %>% select(-plot)
sap93 <- sap %>% filter(plot==93) %>% select(-plot)
sap97 <- sap %>% filter(plot==97) %>% select(-plot)

tree12 <- tree %>% filter(plot==12) %>% select(-plot)
tree13 <- tree %>% filter(plot==13) %>% select(-plot)
tree14 <- tree %>% filter(plot==14) %>% select(-plot)
tree91 <- tree %>% filter(plot==91) %>% select(-plot)
tree93 <- tree %>% filter(plot==93) %>% select(-plot)
tree97 <- tree %>% filter(plot==97) %>% select(-plot)

sap_tree12 <- sap_tree %>% filter(plot==12) %>% select(-plot)
sap_tree13 <- sap_tree %>% filter(plot==13) %>% select(-plot)
sap_tree14 <- sap_tree %>% filter(plot==14) %>% select(-plot)
sap_tree91 <- sap_tree %>% filter(plot==91) %>% select(-plot)
sap_tree93 <- sap_tree %>% filter(plot==93) %>% select(-plot)
sap_tree97 <- sap_tree %>% filter(plot==97) %>% select(-plot)

# use crossdist function in spatstat to get matrix of distance values between
# all seed-sap/trees, sap-trees, seed-seed, and sap-sap
# first need to specify window of each point pattern (i.e., plot boundaries)
owin12 <- owin(c(0,170),c(0,120))
owin13 <- owin(c(0,140),c(0,140))
owin14 <- owin(poly=list(x=c(0,250,250,225,225,0),y=c(0,0,50,50,100,100)))
owin91 <- owin(c(0,70),c(0,75))
owin93 <- owin(poly=list(x=c(0, 185, 185, 200, 200, 0),y=c(0, 0, 40, 40, 100, 100)))
owin97 <- owin(c(0,256),c(0,256))

# then create point patterns for each plot
seed12pp <- as.ppp(seed12,owin12)
sap12pp <- as.ppp(sap12,owin12)
tree12pp <- as.ppp(tree12,owin12)
sap_tree12pp <- as.ppp(sap_tree12,owin12)

seed13pp <- as.ppp(seed13,owin13)
sap13pp <- as.ppp(sap13,owin13)
tree13pp <- as.ppp(tree13,owin13)
sap_tree13pp <- as.ppp(sap_tree13,owin13)

seed14pp <- as.ppp(seed14,owin14)
sap14pp <- as.ppp(sap14,owin14)
tree14pp <- as.ppp(tree14,owin14)
sap_tree14pp <- as.ppp(sap_tree14,owin14)

seed91pp <- as.ppp(seed91,owin91)
sap91pp <- as.ppp(sap91,owin91)
tree91pp <- as.ppp(tree91,owin91)
sap_tree91pp <- as.ppp(sap_tree91,owin91)

seed93pp <- as.ppp(seed93,owin93)
sap93pp <- as.ppp(sap93,owin93)
tree93pp <- as.ppp(tree93,owin93)
sap_tree93pp <- as.ppp(sap_tree93,owin93)

seed97pp <- as.ppp(seed97,owin97)
sap97pp <- as.ppp(sap97,owin97)
tree97pp <- as.ppp(tree97,owin97)
sap_tree97pp <- as.ppp(sap_tree97,owin97)

# get distances between stems
# plot 12
sap.tree12 <- crossdist(sap12pp,tree12pp)
rownames(sap.tree12) <- sap12pp$marks
colnames(sap.tree12) <- tree12pp$marks

seed.sap_tree12 <- crossdist(seed12pp,sap_tree12pp)
rownames(seed.sap_tree12) <- seed12pp$marks
colnames(seed.sap_tree12) <- sap_tree12pp$marks

seed.seed12 <- crossdist(seed12pp,seed12pp)
rownames(seed.seed12) <- seed12pp$marks
colnames(seed.seed12) <- seed12pp$marks

sap.sap12 <- crossdist(sap12pp, sap12pp)
rownames(sap.sap12) <- sap12pp$marks
colnames(sap.sap12) <- sap12pp$marks

# plot 13
sap.tree13 <- crossdist(sap13pp,tree13pp)
rownames(sap.tree13) <- sap13pp$marks
colnames(sap.tree13) <- tree13pp$marks

seed.sap_tree13 <- crossdist(seed13pp,sap_tree13pp)
rownames(seed.sap_tree13) <- seed13pp$marks
colnames(seed.sap_tree13) <- sap_tree13pp$marks

seed.seed13 <- crossdist(seed13pp,seed13pp)
rownames(seed.seed13) <- seed13pp$marks
colnames(seed.seed13) <- seed13pp$marks

sap.sap13 <- crossdist(sap13pp,sap13pp)
rownames(sap.sap13) <- sap13pp$marks
colnames(sap.sap13) <- sap13pp$marks

# plot 14
sap.tree14 <- crossdist(sap14pp,tree14pp)
rownames(sap.tree14) <- sap14pp$marks
colnames(sap.tree14) <- tree14pp$marks

seed.sap_tree14 <- crossdist(seed14pp,sap_tree14pp)
rownames(seed.sap_tree14) <- seed14pp$marks
colnames(seed.sap_tree14) <- sap_tree14pp$marks

seed.seed14 <- crossdist(seed14pp,seed14pp)
rownames(seed.seed14) <- seed14pp$marks
colnames(seed.seed14) <- seed14pp$marks

sap.sap14 <- crossdist(sap14pp,sap14pp)
rownames(sap.sap14) <- sap14pp$marks
colnames(sap.sap14) <- sap14pp$marks

# plot 91
sap.tree91 <- crossdist(sap91pp,tree91pp)
rownames(sap.tree91) <- sap91pp$marks
colnames(sap.tree91) <- tree91pp$marks

seed.sap_tree91 <- crossdist(seed91pp,sap_tree91pp)
rownames(seed.sap_tree91) <- seed91pp$marks
colnames(seed.sap_tree91) <- sap_tree91pp$marks

seed.seed91 <- crossdist(seed91pp,seed91pp)
rownames(seed.seed91) <- seed91pp$marks
colnames(seed.seed91) <- seed91pp$marks

sap.sap91 <- crossdist(sap91pp,sap91pp)
rownames(sap.sap91) <- sap91pp$marks
colnames(sap.sap91) <- sap91pp$marks

# plot 93
sap.tree93 <- crossdist(sap93pp,tree93pp)
rownames(sap.tree93) <- sap93pp$marks
colnames(sap.tree93) <- tree93pp$marks

seed.sap_tree93 <- crossdist(seed93pp,sap_tree93pp)
rownames(seed.sap_tree93) <- seed93pp$marks
colnames(seed.sap_tree93) <- sap_tree93pp$marks

seed.seed93 <- crossdist(seed93pp,seed93pp)
rownames(seed.seed93) <- seed93pp$marks
colnames(seed.seed93) <- seed93pp$marks

sap.sap93 <- crossdist(sap93pp,sap93pp)
rownames(sap.sap93) <- sap93pp$marks
colnames(sap.sap93) <- sap93pp$marks

# plot 97
sap.tree97 <- crossdist(sap97pp,tree97pp)
rownames(sap.tree97) <- sap97pp$marks
colnames(sap.tree97) <- tree97pp$marks

seed.sap_tree97 <- crossdist(seed97pp,sap_tree97pp)
rownames(seed.sap_tree97) <- seed97pp$marks
colnames(seed.sap_tree97) <- sap_tree97pp$marks

seed.seed97 <- crossdist(seed97pp,seed97pp)
rownames(seed.seed97) <- seed97pp$marks
colnames(seed.seed97) <- seed97pp$marks

sap.sap97 <- crossdist(sap97pp,sap97pp)
rownames(sap.sap97) <- sap97pp$marks
colnames(sap.sap97) <- sap97pp$marks



# STARTING WITH SEEDLING VS. SAP & TREE STEMS
########################################################
# restructure matrices, create df: rowID, colID, distance, then remove
# cases where distance >5m, 10m, 15m, and 20m (into separate dfs)

# plot 12

seed.sap_tree12df <- melt(seed.sap_tree12,id.vars=rownames(seed.sap_tree12))
colnames(seed.sap_tree12df) <- c("seedID","treeID","dist")
seed.sap_tree12df_5 <- seed.sap_tree12df[seed.sap_tree12df$dist<=5, ]
seed.sap_tree12df_10 <- seed.sap_tree12df[seed.sap_tree12df$dist<=10, ]
seed.sap_tree12df_15 <- seed.sap_tree12df[seed.sap_tree12df$dist<=15, ]
seed.sap_tree12df_20 <- seed.sap_tree12df[seed.sap_tree12df$dist<=20, ]


# 5m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree12df_5 <- merge(seed.sap_tree12df_5,d12,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree12df_5 <- seed.sap_tree12df_5 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree12df_5)[3] <- "tree.species"
seed.sap_tree12df_5$treeBA <- (pi*((seed.sap_tree12df_5$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree12df_5 <- merge(seed.sap_tree12df_5,d12,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree12df_5 <- seed.sap_tree12df_5 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree12df_5)[5] <- "seed.species"

seed.sap_tree12df_5$tree.species <- as.character(seed.sap_tree12df_5$tree.species)
seed.sap_tree12df_5$seed.species <- as.character(seed.sap_tree12df_5$seed.species)
seed.sap_tree12df_5$intxn <- ifelse(seed.sap_tree12df_5$seed.species==seed.sap_tree12df_5$tree.species,"con","het")
seed.sap_tree12df_5 <- seed.sap_tree12df_5 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree12df_5 <- aggregate(treeBA~.,data=seed.sap_tree12df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
seed.sap_tree12df_10 <- merge(seed.sap_tree12df_10,d12,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree12df_10 <- seed.sap_tree12df_10 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree12df_10)[3] <- "tree.species"
seed.sap_tree12df_10$treeBA <- (pi*((seed.sap_tree12df_10$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree12df_10 <- merge(seed.sap_tree12df_10,d12,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree12df_10 <- seed.sap_tree12df_10 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree12df_10)[5] <- "seed.species"

seed.sap_tree12df_10$tree.species <- as.character(seed.sap_tree12df_10$tree.species)
seed.sap_tree12df_10$seed.species <- as.character(seed.sap_tree12df_10$seed.species)
seed.sap_tree12df_10$intxn <- ifelse(seed.sap_tree12df_10$seed.species==seed.sap_tree12df_10$tree.species,"con","het")
seed.sap_tree12df_10 <- seed.sap_tree12df_10 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree12df_10 <- aggregate(treeBA~.,data=seed.sap_tree12df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree12df_15 <- merge(seed.sap_tree12df_15,d12,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree12df_15 <- seed.sap_tree12df_15 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree12df_15)[3] <- "tree.species"
seed.sap_tree12df_15$treeBA <- (pi*((seed.sap_tree12df_15$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree12df_15 <- merge(seed.sap_tree12df_15,d12,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree12df_15 <- seed.sap_tree12df_15 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree12df_15)[5] <- "seed.species"

seed.sap_tree12df_15$tree.species <- as.character(seed.sap_tree12df_15$tree.species)
seed.sap_tree12df_15$seed.species <- as.character(seed.sap_tree12df_15$seed.species)
seed.sap_tree12df_15$intxn <- ifelse(seed.sap_tree12df_15$seed.species==seed.sap_tree12df_15$tree.species,"con","het")
seed.sap_tree12df_15 <- seed.sap_tree12df_15 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree12df_15 <- aggregate(treeBA~.,data=seed.sap_tree12df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree12df_20 <- merge(seed.sap_tree12df_20,d12,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree12df_20 <- seed.sap_tree12df_20 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree12df_20)[3] <- "tree.species"
seed.sap_tree12df_20$treeBA <- (pi*((seed.sap_tree12df_20$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree12df_20 <- merge(seed.sap_tree12df_20,d12,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree12df_20 <- seed.sap_tree12df_20 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree12df_20)[5] <- "seed.species"

seed.sap_tree12df_20$tree.species <- as.character(seed.sap_tree12df_20$tree.species)
seed.sap_tree12df_20$seed.species <- as.character(seed.sap_tree12df_20$seed.species)
seed.sap_tree12df_20$intxn <- ifelse(seed.sap_tree12df_20$seed.species==seed.sap_tree12df_20$tree.species,"con","het")
seed.sap_tree12df_20 <- seed.sap_tree12df_20 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree12df_20 <- aggregate(treeBA~.,data=seed.sap_tree12df_20,FUN="sum")




# plot 13
# 5m scale

seed.sap_tree13df <- melt(seed.sap_tree13,id.vars=rownames(seed.sap_tree13))
colnames(seed.sap_tree13df) <- c("seedID","treeID","dist")
seed.sap_tree13df_5 <- seed.sap_tree13df[seed.sap_tree13df$dist<=5, ]
seed.sap_tree13df_10 <- seed.sap_tree13df[seed.sap_tree13df$dist<=10, ]
seed.sap_tree13df_15 <- seed.sap_tree13df[seed.sap_tree13df$dist<=15, ]
seed.sap_tree13df_20 <- seed.sap_tree13df[seed.sap_tree13df$dist<=20, ]

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree13df_5 <- merge(seed.sap_tree13df_5,d13,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree13df_5 <- seed.sap_tree13df_5 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree13df_5)[3] <- "tree.species"
seed.sap_tree13df_5$treeBA <- (pi*((seed.sap_tree13df_5$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree13df_5 <- merge(seed.sap_tree13df_5,d13,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree13df_5 <- seed.sap_tree13df_5 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree13df_5)[5] <- "seed.species"

seed.sap_tree13df_5$tree.species <- as.character(seed.sap_tree13df_5$tree.species)
seed.sap_tree13df_5$seed.species <- as.character(seed.sap_tree13df_5$seed.species)
seed.sap_tree13df_5$intxn <- ifelse(seed.sap_tree13df_5$seed.species==seed.sap_tree13df_5$tree.species,"con","het")
seed.sap_tree13df_5 <- seed.sap_tree13df_5 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree13df_5 <- aggregate(treeBA~.,data=seed.sap_tree13df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
seed.sap_tree13df_10 <- merge(seed.sap_tree13df_10,d13,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree13df_10 <- seed.sap_tree13df_10 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree13df_10)[3] <- "tree.species"
seed.sap_tree13df_10$treeBA <- (pi*((seed.sap_tree13df_10$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree13df_10 <- merge(seed.sap_tree13df_10,d13,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree13df_10 <- seed.sap_tree13df_10 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree13df_10)[5] <- "seed.species"

seed.sap_tree13df_10$tree.species <- as.character(seed.sap_tree13df_10$tree.species)
seed.sap_tree13df_10$seed.species <- as.character(seed.sap_tree13df_10$seed.species)
seed.sap_tree13df_10$intxn <- ifelse(seed.sap_tree13df_10$seed.species==seed.sap_tree13df_10$tree.species,"con","het")
seed.sap_tree13df_10 <- seed.sap_tree13df_10 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree13df_10 <- aggregate(treeBA~.,data=seed.sap_tree13df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree13df_15 <- merge(seed.sap_tree13df_15,d13,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree13df_15 <- seed.sap_tree13df_15 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree13df_15)[3] <- "tree.species"
seed.sap_tree13df_15$treeBA <- (pi*((seed.sap_tree13df_15$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree13df_15 <- merge(seed.sap_tree13df_15,d13,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree13df_15 <- seed.sap_tree13df_15 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree13df_15)[5] <- "seed.species"

seed.sap_tree13df_15$tree.species <- as.character(seed.sap_tree13df_15$tree.species)
seed.sap_tree13df_15$seed.species <- as.character(seed.sap_tree13df_15$seed.species)
seed.sap_tree13df_15$intxn <- ifelse(seed.sap_tree13df_15$seed.species==seed.sap_tree13df_15$tree.species,"con","het")
seed.sap_tree13df_15 <- seed.sap_tree13df_15 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree13df_15 <- aggregate(treeBA~.,data=seed.sap_tree13df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree13df_20 <- merge(seed.sap_tree13df_20,d13,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree13df_20 <- seed.sap_tree13df_20 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree13df_20)[3] <- "tree.species"
seed.sap_tree13df_20$treeBA <- (pi*((seed.sap_tree13df_20$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree13df_20 <- merge(seed.sap_tree13df_20,d13,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree13df_20 <- seed.sap_tree13df_20 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree13df_20)[5] <- "seed.species"

seed.sap_tree13df_20$tree.species <- as.character(seed.sap_tree13df_20$tree.species)
seed.sap_tree13df_20$seed.species <- as.character(seed.sap_tree13df_20$seed.species)
seed.sap_tree13df_20$intxn <- ifelse(seed.sap_tree13df_20$seed.species==seed.sap_tree13df_20$tree.species,"con","het")
seed.sap_tree13df_20 <- seed.sap_tree13df_20 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree13df_20 <- aggregate(treeBA~.,data=seed.sap_tree13df_20,FUN="sum")






# plot 14
# 5m scale

seed.sap_tree14df <- melt(seed.sap_tree14,id.vars=rownames(seed.sap_tree14))
colnames(seed.sap_tree14df) <- c("seedID","treeID","dist")
seed.sap_tree14df_5 <- seed.sap_tree14df[seed.sap_tree14df$dist<=5, ]
seed.sap_tree14df_10 <- seed.sap_tree14df[seed.sap_tree14df$dist<=10, ]
seed.sap_tree14df_15 <- seed.sap_tree14df[seed.sap_tree14df$dist<=15, ]
seed.sap_tree14df_20 <- seed.sap_tree14df[seed.sap_tree14df$dist<=20, ]

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree14df_5 <- merge(seed.sap_tree14df_5,d14,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree14df_5 <- seed.sap_tree14df_5 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree14df_5)[3] <- "tree.species"
seed.sap_tree14df_5$treeBA <- (pi*((seed.sap_tree14df_5$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree14df_5 <- merge(seed.sap_tree14df_5,d14,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree14df_5 <- seed.sap_tree14df_5 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree14df_5)[5] <- "seed.species"

seed.sap_tree14df_5$tree.species <- as.character(seed.sap_tree14df_5$tree.species)
seed.sap_tree14df_5$seed.species <- as.character(seed.sap_tree14df_5$seed.species)
seed.sap_tree14df_5$intxn <- ifelse(seed.sap_tree14df_5$seed.species==seed.sap_tree14df_5$tree.species,"con","het")
seed.sap_tree14df_5 <- seed.sap_tree14df_5 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree14df_5 <- aggregate(treeBA~.,data=seed.sap_tree14df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
seed.sap_tree14df_10 <- merge(seed.sap_tree14df_10,d14,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree14df_10 <- seed.sap_tree14df_10 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree14df_10)[3] <- "tree.species"
seed.sap_tree14df_10$treeBA <- (pi*((seed.sap_tree14df_10$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree14df_10 <- merge(seed.sap_tree14df_10,d14,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree14df_10 <- seed.sap_tree14df_10 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree14df_10)[5] <- "seed.species"

seed.sap_tree14df_10$tree.species <- as.character(seed.sap_tree14df_10$tree.species)
seed.sap_tree14df_10$seed.species <- as.character(seed.sap_tree14df_10$seed.species)
seed.sap_tree14df_10$intxn <- ifelse(seed.sap_tree14df_10$seed.species==seed.sap_tree14df_10$tree.species,"con","het")
seed.sap_tree14df_10 <- seed.sap_tree14df_10 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree14df_10 <- aggregate(treeBA~.,data=seed.sap_tree14df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree14df_15 <- merge(seed.sap_tree14df_15,d14,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree14df_15 <- seed.sap_tree14df_15 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree14df_15)[3] <- "tree.species"
seed.sap_tree14df_15$treeBA <- (pi*((seed.sap_tree14df_15$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree14df_15 <- merge(seed.sap_tree14df_15,d14,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree14df_15 <- seed.sap_tree14df_15 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree14df_15)[5] <- "seed.species"

seed.sap_tree14df_15$tree.species <- as.character(seed.sap_tree14df_15$tree.species)
seed.sap_tree14df_15$seed.species <- as.character(seed.sap_tree14df_15$seed.species)
seed.sap_tree14df_15$intxn <- ifelse(seed.sap_tree14df_15$seed.species==seed.sap_tree14df_15$tree.species,"con","het")
seed.sap_tree14df_15 <- seed.sap_tree14df_15 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree14df_15 <- aggregate(treeBA~.,data=seed.sap_tree14df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree14df_20 <- merge(seed.sap_tree14df_20,d14,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree14df_20 <- seed.sap_tree14df_20 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree14df_20)[3] <- "tree.species"
seed.sap_tree14df_20$treeBA <- (pi*((seed.sap_tree14df_20$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree14df_20 <- merge(seed.sap_tree14df_20,d14,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree14df_20 <- seed.sap_tree14df_20 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree14df_20)[5] <- "seed.species"

seed.sap_tree14df_20$tree.species <- as.character(seed.sap_tree14df_20$tree.species)
seed.sap_tree14df_20$seed.species <- as.character(seed.sap_tree14df_20$seed.species)
seed.sap_tree14df_20$intxn <- ifelse(seed.sap_tree14df_20$seed.species==seed.sap_tree14df_20$tree.species,"con","het")
seed.sap_tree14df_20 <- seed.sap_tree14df_20 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree14df_20 <- aggregate(treeBA~.,data=seed.sap_tree14df_20,FUN="sum")







# plot 91
# 5m scale

seed.sap_tree91df <- melt(seed.sap_tree91,id.vars=rownames(seed.sap_tree91))
colnames(seed.sap_tree91df) <- c("seedID","treeID","dist")
seed.sap_tree91df_5 <- seed.sap_tree91df[seed.sap_tree91df$dist<=5, ]
seed.sap_tree91df_10 <- seed.sap_tree91df[seed.sap_tree91df$dist<=10, ]
seed.sap_tree91df_15 <- seed.sap_tree91df[seed.sap_tree91df$dist<=15, ]
seed.sap_tree91df_20 <- seed.sap_tree91df[seed.sap_tree91df$dist<=20, ]

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree91df_5 <- merge(seed.sap_tree91df_5,d91,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree91df_5 <- seed.sap_tree91df_5 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree91df_5)[3] <- "tree.species"
seed.sap_tree91df_5$treeBA <- (pi*((seed.sap_tree91df_5$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree91df_5 <- merge(seed.sap_tree91df_5,d91,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree91df_5 <- seed.sap_tree91df_5 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree91df_5)[5] <- "seed.species"

seed.sap_tree91df_5$tree.species <- as.character(seed.sap_tree91df_5$tree.species)
seed.sap_tree91df_5$seed.species <- as.character(seed.sap_tree91df_5$seed.species)
seed.sap_tree91df_5$intxn <- ifelse(seed.sap_tree91df_5$seed.species==seed.sap_tree91df_5$tree.species,"con","het")
seed.sap_tree91df_5 <- seed.sap_tree91df_5 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree91df_5 <- aggregate(treeBA~.,data=seed.sap_tree91df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
seed.sap_tree91df_10 <- merge(seed.sap_tree91df_10,d91,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree91df_10 <- seed.sap_tree91df_10 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree91df_10)[3] <- "tree.species"
seed.sap_tree91df_10$treeBA <- (pi*((seed.sap_tree91df_10$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree91df_10 <- merge(seed.sap_tree91df_10,d91,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree91df_10 <- seed.sap_tree91df_10 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree91df_10)[5] <- "seed.species"

seed.sap_tree91df_10$tree.species <- as.character(seed.sap_tree91df_10$tree.species)
seed.sap_tree91df_10$seed.species <- as.character(seed.sap_tree91df_10$seed.species)
seed.sap_tree91df_10$intxn <- ifelse(seed.sap_tree91df_10$seed.species==seed.sap_tree91df_10$tree.species,"con","het")
seed.sap_tree91df_10 <- seed.sap_tree91df_10 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree91df_10 <- aggregate(treeBA~.,data=seed.sap_tree91df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree91df_15 <- merge(seed.sap_tree91df_15,d91,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree91df_15 <- seed.sap_tree91df_15 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree91df_15)[3] <- "tree.species"
seed.sap_tree91df_15$treeBA <- (pi*((seed.sap_tree91df_15$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree91df_15 <- merge(seed.sap_tree91df_15,d91,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree91df_15 <- seed.sap_tree91df_15 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree91df_15)[5] <- "seed.species"

seed.sap_tree91df_15$tree.species <- as.character(seed.sap_tree91df_15$tree.species)
seed.sap_tree91df_15$seed.species <- as.character(seed.sap_tree91df_15$seed.species)
seed.sap_tree91df_15$intxn <- ifelse(seed.sap_tree91df_15$seed.species==seed.sap_tree91df_15$tree.species,"con","het")
seed.sap_tree91df_15 <- seed.sap_tree91df_15 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree91df_15 <- aggregate(treeBA~.,data=seed.sap_tree91df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree91df_20 <- merge(seed.sap_tree91df_20,d91,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree91df_20 <- seed.sap_tree91df_20 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree91df_20)[3] <- "tree.species"
seed.sap_tree91df_20$treeBA <- (pi*((seed.sap_tree91df_20$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree91df_20 <- merge(seed.sap_tree91df_20,d91,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree91df_20 <- seed.sap_tree91df_20 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree91df_20)[5] <- "seed.species"

seed.sap_tree91df_20$tree.species <- as.character(seed.sap_tree91df_20$tree.species)
seed.sap_tree91df_20$seed.species <- as.character(seed.sap_tree91df_20$seed.species)
seed.sap_tree91df_20$intxn <- ifelse(seed.sap_tree91df_20$seed.species==seed.sap_tree91df_20$tree.species,"con","het")
seed.sap_tree91df_20 <- seed.sap_tree91df_20 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree91df_20 <- aggregate(treeBA~.,data=seed.sap_tree91df_20,FUN="sum")





# plot 93
# 5m scale

seed.sap_tree93df <- melt(seed.sap_tree93,id.vars=rownames(seed.sap_tree93))
colnames(seed.sap_tree93df) <- c("seedID","treeID","dist")
seed.sap_tree93df_5 <- seed.sap_tree93df[seed.sap_tree93df$dist<=5, ]
seed.sap_tree93df_10 <- seed.sap_tree93df[seed.sap_tree93df$dist<=10, ]
seed.sap_tree93df_15 <- seed.sap_tree93df[seed.sap_tree93df$dist<=15, ]
seed.sap_tree93df_20 <- seed.sap_tree93df[seed.sap_tree93df$dist<=20, ]

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree93df_5 <- merge(seed.sap_tree93df_5,d93,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree93df_5 <- seed.sap_tree93df_5 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree93df_5)[3] <- "tree.species"
seed.sap_tree93df_5$treeBA <- (pi*((seed.sap_tree93df_5$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree93df_5 <- merge(seed.sap_tree93df_5,d93,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree93df_5 <- seed.sap_tree93df_5 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree93df_5)[5] <- "seed.species"

seed.sap_tree93df_5$tree.species <- as.character(seed.sap_tree93df_5$tree.species)
seed.sap_tree93df_5$seed.species <- as.character(seed.sap_tree93df_5$seed.species)
seed.sap_tree93df_5$intxn <- ifelse(seed.sap_tree93df_5$seed.species==seed.sap_tree93df_5$tree.species,"con","het")
seed.sap_tree93df_5 <- seed.sap_tree93df_5 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree93df_5 <- aggregate(treeBA~.,data=seed.sap_tree93df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
seed.sap_tree93df_10 <- merge(seed.sap_tree93df_10,d93,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree93df_10 <- seed.sap_tree93df_10 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree93df_10)[3] <- "tree.species"
seed.sap_tree93df_10$treeBA <- (pi*((seed.sap_tree93df_10$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree93df_10 <- merge(seed.sap_tree93df_10,d93,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree93df_10 <- seed.sap_tree93df_10 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree93df_10)[5] <- "seed.species"

seed.sap_tree93df_10$tree.species <- as.character(seed.sap_tree93df_10$tree.species)
seed.sap_tree93df_10$seed.species <- as.character(seed.sap_tree93df_10$seed.species)
seed.sap_tree93df_10$intxn <- ifelse(seed.sap_tree93df_10$seed.species==seed.sap_tree93df_10$tree.species,"con","het")
seed.sap_tree93df_10 <- seed.sap_tree93df_10 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree93df_10 <- aggregate(treeBA~.,data=seed.sap_tree93df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree93df_15 <- merge(seed.sap_tree93df_15,d93,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree93df_15 <- seed.sap_tree93df_15 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree93df_15)[3] <- "tree.species"
seed.sap_tree93df_15$treeBA <- (pi*((seed.sap_tree93df_15$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree93df_15 <- merge(seed.sap_tree93df_15,d93,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree93df_15 <- seed.sap_tree93df_15 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree93df_15)[5] <- "seed.species"

seed.sap_tree93df_15$tree.species <- as.character(seed.sap_tree93df_15$tree.species)
seed.sap_tree93df_15$seed.species <- as.character(seed.sap_tree93df_15$seed.species)
seed.sap_tree93df_15$intxn <- ifelse(seed.sap_tree93df_15$seed.species==seed.sap_tree93df_15$tree.species,"con","het")
seed.sap_tree93df_15 <- seed.sap_tree93df_15 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree93df_15 <- aggregate(treeBA~.,data=seed.sap_tree93df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree93df_20 <- merge(seed.sap_tree93df_20,d93,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree93df_20 <- seed.sap_tree93df_20 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree93df_20)[3] <- "tree.species"
seed.sap_tree93df_20$treeBA <- (pi*((seed.sap_tree93df_20$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree93df_20 <- merge(seed.sap_tree93df_20,d93,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree93df_20 <- seed.sap_tree93df_20 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree93df_20)[5] <- "seed.species"

seed.sap_tree93df_20$tree.species <- as.character(seed.sap_tree93df_20$tree.species)
seed.sap_tree93df_20$seed.species <- as.character(seed.sap_tree93df_20$seed.species)
seed.sap_tree93df_20$intxn <- ifelse(seed.sap_tree93df_20$seed.species==seed.sap_tree93df_20$tree.species,"con","het")
seed.sap_tree93df_20 <- seed.sap_tree93df_20 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree93df_20 <- aggregate(treeBA~.,data=seed.sap_tree93df_20,FUN="sum")




# plot 97
# 5m scale

seed.sap_tree97df <- melt(seed.sap_tree97,id.vars=rownames(seed.sap_tree97))
colnames(seed.sap_tree97df) <- c("seedID","treeID","dist")
seed.sap_tree97df_5 <- seed.sap_tree97df[seed.sap_tree97df$dist<=5, ]
seed.sap_tree97df_10 <- seed.sap_tree97df[seed.sap_tree97df$dist<=10, ]
seed.sap_tree97df_15 <- seed.sap_tree97df[seed.sap_tree97df$dist<=15, ]
seed.sap_tree97df_20 <- seed.sap_tree97df[seed.sap_tree97df$dist<=20, ]

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree97df_5 <- merge(seed.sap_tree97df_5,d97,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree97df_5 <- seed.sap_tree97df_5 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree97df_5)[3] <- "tree.species"
seed.sap_tree97df_5$treeBA <- (pi*((seed.sap_tree97df_5$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree97df_5 <- merge(seed.sap_tree97df_5,d97,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree97df_5 <- seed.sap_tree97df_5 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree97df_5)[5] <- "seed.species"

seed.sap_tree97df_5$tree.species <- as.character(seed.sap_tree97df_5$tree.species)
seed.sap_tree97df_5$seed.species <- as.character(seed.sap_tree97df_5$seed.species)
seed.sap_tree97df_5$intxn <- ifelse(seed.sap_tree97df_5$seed.species==seed.sap_tree97df_5$tree.species,"con","het")
seed.sap_tree97df_5 <- seed.sap_tree97df_5 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree97df_5 <- aggregate(treeBA~.,data=seed.sap_tree97df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
seed.sap_tree97df_10 <- merge(seed.sap_tree97df_10,d97,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree97df_10 <- seed.sap_tree97df_10 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree97df_10)[3] <- "tree.species"
seed.sap_tree97df_10$treeBA <- (pi*((seed.sap_tree97df_10$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree97df_10 <- merge(seed.sap_tree97df_10,d97,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree97df_10 <- seed.sap_tree97df_10 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree97df_10)[5] <- "seed.species"

seed.sap_tree97df_10$tree.species <- as.character(seed.sap_tree97df_10$tree.species)
seed.sap_tree97df_10$seed.species <- as.character(seed.sap_tree97df_10$seed.species)
seed.sap_tree97df_10$intxn <- ifelse(seed.sap_tree97df_10$seed.species==seed.sap_tree97df_10$tree.species,"con","het")
seed.sap_tree97df_10 <- seed.sap_tree97df_10 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree97df_10 <- aggregate(treeBA~.,data=seed.sap_tree97df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree97df_15 <- merge(seed.sap_tree97df_15,d97,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree97df_15 <- seed.sap_tree97df_15 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree97df_15)[3] <- "tree.species"
seed.sap_tree97df_15$treeBA <- (pi*((seed.sap_tree97df_15$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree97df_15 <- merge(seed.sap_tree97df_15,d97,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree97df_15 <- seed.sap_tree97df_15 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree97df_15)[5] <- "seed.species"

seed.sap_tree97df_15$tree.species <- as.character(seed.sap_tree97df_15$tree.species)
seed.sap_tree97df_15$seed.species <- as.character(seed.sap_tree97df_15$seed.species)
seed.sap_tree97df_15$intxn <- ifelse(seed.sap_tree97df_15$seed.species==seed.sap_tree97df_15$tree.species,"con","het")
seed.sap_tree97df_15 <- seed.sap_tree97df_15 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree97df_15 <- aggregate(treeBA~.,data=seed.sap_tree97df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows seedID, treeID, species, and diam1 only)
seed.sap_tree97df_20 <- merge(seed.sap_tree97df_20,d97,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree97df_20 <- seed.sap_tree97df_20 %>% select(seedID, treeID, species, diam1)
colnames(seed.sap_tree97df_20)[3] <- "tree.species"
seed.sap_tree97df_20$treeBA <- (pi*((seed.sap_tree97df_20$diam1)/2)^2)

# add info from seed df so we can specify whether stem interaction is con or het
seed.sap_tree97df_20 <- merge(seed.sap_tree97df_20,d97,by.x="seedID",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.sap_tree97df_20 <- seed.sap_tree97df_20 %>% select(seedID, treeID, tree.species, treeBA, species)
colnames(seed.sap_tree97df_20)[5] <- "seed.species"

seed.sap_tree97df_20$tree.species <- as.character(seed.sap_tree97df_20$tree.species)
seed.sap_tree97df_20$seed.species <- as.character(seed.sap_tree97df_20$seed.species)
seed.sap_tree97df_20$intxn <- ifelse(seed.sap_tree97df_20$seed.species==seed.sap_tree97df_20$tree.species,"con","het")
seed.sap_tree97df_20 <- seed.sap_tree97df_20 %>% select(seedID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of seed stem ID and intxn type
seed.sap_tree97df_20 <- aggregate(treeBA~.,data=seed.sap_tree97df_20,FUN="sum")



# 5m
# restructure data - from long to wide (dcast)
seed.sap_treeBA12_5 <- dcast(seed.sap_tree12df_5,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA12_5) <- c("ID","BAc5","BAh5")
seed.sap_treeBA12_5[is.na(seed.sap_treeBA12_5)] <- 0

seed.sap_treeBA13_5 <- dcast(seed.sap_tree13df_5,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA13_5) <- c("ID","BAc5","BAh5")
seed.sap_treeBA13_5[is.na(seed.sap_treeBA13_5)] <- 0

seed.sap_treeBA14_5 <- dcast(seed.sap_tree14df_5,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA14_5) <- c("ID","BAc5","BAh5")
seed.sap_treeBA14_5[is.na(seed.sap_treeBA14_5)] <- 0

seed.sap_treeBA91_5 <- dcast(seed.sap_tree91df_5,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA91_5) <- c("ID","BAc5","BAh5")
seed.sap_treeBA91_5[is.na(seed.sap_treeBA91_5)] <- 0

seed.sap_treeBA93_5 <- dcast(seed.sap_tree93df_5,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA93_5) <- c("ID","BAc5","BAh5")
seed.sap_treeBA93_5[is.na(seed.sap_treeBA93_5)] <- 0

seed.sap_treeBA97_5 <- dcast(seed.sap_tree97df_5,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA97_5) <- c("ID","BAc5","BAh5")
seed.sap_treeBA97_5[is.na(seed.sap_treeBA97_5)] <- 0




# 10m
seed.sap_treeBA12_10 <- dcast(seed.sap_tree12df_10,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA12_10) <- c("ID","BAc10","BAh10")
seed.sap_treeBA12_10[is.na(seed.sap_treeBA12_10)] <- 0

seed.sap_treeBA13_10 <- dcast(seed.sap_tree13df_10,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA13_10) <- c("ID","BAc10","BAh10")
seed.sap_treeBA13_10[is.na(seed.sap_treeBA13_10)] <- 0

seed.sap_treeBA14_10 <- dcast(seed.sap_tree14df_10,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA14_10) <- c("ID","BAc10","BAh10")
seed.sap_treeBA14_10[is.na(seed.sap_treeBA14_10)] <- 0

seed.sap_treeBA91_10 <- dcast(seed.sap_tree91df_10,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA91_10) <- c("ID","BAc10","BAh10")
seed.sap_treeBA91_10[is.na(seed.sap_treeBA91_10)] <- 0

seed.sap_treeBA93_10 <- dcast(seed.sap_tree93df_10,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA93_10) <- c("ID","BAc10","BAh10")
seed.sap_treeBA93_10[is.na(seed.sap_treeBA93_10)] <- 0

seed.sap_treeBA97_10 <- dcast(seed.sap_tree97df_10,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA97_10) <- c("ID","BAc10","BAh10")
seed.sap_treeBA97_10[is.na(seed.sap_treeBA97_10)] <- 0





# 15m 
seed.sap_treeBA12_15 <- dcast(seed.sap_tree12df_15,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA12_15) <- c("ID","BAc15","BAh15")
seed.sap_treeBA12_15[is.na(seed.sap_treeBA12_15)] <- 0

seed.sap_treeBA13_15 <- dcast(seed.sap_tree13df_15,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA13_15) <- c("ID","BAc15","BAh15")
seed.sap_treeBA13_15[is.na(seed.sap_treeBA13_15)] <- 0

seed.sap_treeBA14_15 <- dcast(seed.sap_tree14df_15,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA14_15) <- c("ID","BAc15","BAh15")
seed.sap_treeBA14_15[is.na(seed.sap_treeBA14_15)] <- 0

seed.sap_treeBA91_15 <- dcast(seed.sap_tree91df_15,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA91_15) <- c("ID","BAc15","BAh15")
seed.sap_treeBA91_15[is.na(seed.sap_treeBA91_15)] <- 0

seed.sap_treeBA93_15 <- dcast(seed.sap_tree93df_15,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA93_15) <- c("ID","BAc15","BAh15")
seed.sap_treeBA93_15[is.na(seed.sap_treeBA93_15)] <- 0

seed.sap_treeBA97_15 <- dcast(seed.sap_tree97df_15,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA97_15) <- c("ID","BAc15","BAh15")
seed.sap_treeBA97_15[is.na(seed.sap_treeBA97_15)] <- 0





# 20m
seed.sap_treeBA12_20 <- dcast(seed.sap_tree12df_20,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA12_20) <- c("ID","BAc20","BAh20")
seed.sap_treeBA12_20[is.na(seed.sap_treeBA12_20)] <- 0

seed.sap_treeBA13_20 <- dcast(seed.sap_tree13df_20,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA13_20) <- c("ID","BAc20","BAh20")
seed.sap_treeBA13_20[is.na(seed.sap_treeBA13_20)] <- 0

seed.sap_treeBA14_20 <- dcast(seed.sap_tree14df_20,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA14_20) <- c("ID","BAc20","BAh20")
seed.sap_treeBA14_20[is.na(seed.sap_treeBA14_20)] <- 0

seed.sap_treeBA91_20 <- dcast(seed.sap_tree91df_20,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA91_20) <- c("ID","BAc20","BAh20")
seed.sap_treeBA91_20[is.na(seed.sap_treeBA91_20)] <- 0

seed.sap_treeBA93_20 <- dcast(seed.sap_tree93df_20,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA93_20) <- c("ID","BAc20","BAh20")
seed.sap_treeBA93_20[is.na(seed.sap_treeBA93_20)] <- 0

seed.sap_treeBA97_20 <- dcast(seed.sap_tree97df_20,seedID~intxn,value.var="treeBA")
colnames(seed.sap_treeBA97_20) <- c("ID","BAc20","BAh20")
seed.sap_treeBA97_20[is.na(seed.sap_treeBA97_20)] <- 0





# SAPLING VS. TREE STEMS
###############################################################

# plot 12

sap.tree12df <- melt(sap.tree12,id.vars=rownames(sap.tree12))
colnames(sap.tree12df) <- c("sapID","treeID","dist")
sap.tree12df_5 <- sap.tree12df[sap.tree12df$dist<=5, ]
sap.tree12df_10 <- sap.tree12df[sap.tree12df$dist<=10, ]
sap.tree12df_15 <- sap.tree12df[sap.tree12df$dist<=15, ]
sap.tree12df_20 <- sap.tree12df[sap.tree12df$dist<=20, ]

# 5m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree12df_5 <- merge(sap.tree12df_5,d12,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree12df_5 <- sap.tree12df_5 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree12df_5)[3] <- "tree.species"
sap.tree12df_5$treeBA <- (pi*((sap.tree12df_5$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree12df_5 <- merge(sap.tree12df_5,d12,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree12df_5 <- sap.tree12df_5 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree12df_5)[5] <- "sap.species"

sap.tree12df_5$tree.species <- as.character(sap.tree12df_5$tree.species)
sap.tree12df_5$sap.species <- as.character(sap.tree12df_5$sap.species)
sap.tree12df_5$intxn <- ifelse(sap.tree12df_5$sap.species==sap.tree12df_5$tree.species,"con","het")
sap.tree12df_5 <- sap.tree12df_5 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree12df_5 <- aggregate(treeBA~.,data=sap.tree12df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
sap.tree12df_10 <- merge(sap.tree12df_10,d12,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree12df_10 <- sap.tree12df_10 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree12df_10)[3] <- "tree.species"
sap.tree12df_10$treeBA <- (pi*((sap.tree12df_10$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree12df_10 <- merge(sap.tree12df_10,d12,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree12df_10 <- sap.tree12df_10 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree12df_10)[5] <- "sap.species"

sap.tree12df_10$tree.species <- as.character(sap.tree12df_10$tree.species)
sap.tree12df_10$sap.species <- as.character(sap.tree12df_10$sap.species)
sap.tree12df_10$intxn <- ifelse(sap.tree12df_10$sap.species==sap.tree12df_10$tree.species,"con","het")
sap.tree12df_10 <- sap.tree12df_10 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree12df_10 <- aggregate(treeBA~.,data=sap.tree12df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree12df_15 <- merge(sap.tree12df_15,d12,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree12df_15 <- sap.tree12df_15 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree12df_15)[3] <- "tree.species"
sap.tree12df_15$treeBA <- (pi*((sap.tree12df_15$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree12df_15 <- merge(sap.tree12df_15,d12,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree12df_15 <- sap.tree12df_15 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree12df_15)[5] <- "sap.species"

sap.tree12df_15$tree.species <- as.character(sap.tree12df_15$tree.species)
sap.tree12df_15$sap.species <- as.character(sap.tree12df_15$sap.species)
sap.tree12df_15$intxn <- ifelse(sap.tree12df_15$sap.species==sap.tree12df_15$tree.species,"con","het")
sap.tree12df_15 <- sap.tree12df_15 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree12df_15 <- aggregate(treeBA~.,data=sap.tree12df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree12df_20 <- merge(sap.tree12df_20,d12,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree12df_20 <- sap.tree12df_20 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree12df_20)[3] <- "tree.species"
sap.tree12df_20$treeBA <- (pi*((sap.tree12df_20$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree12df_20 <- merge(sap.tree12df_20,d12,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree12df_20 <- sap.tree12df_20 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree12df_20)[5] <- "sap.species"

sap.tree12df_20$tree.species <- as.character(sap.tree12df_20$tree.species)
sap.tree12df_20$sap.species <- as.character(sap.tree12df_20$sap.species)
sap.tree12df_20$intxn <- ifelse(sap.tree12df_20$sap.species==sap.tree12df_20$tree.species,"con","het")
sap.tree12df_20 <- sap.tree12df_20 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree12df_20 <- aggregate(treeBA~.,data=sap.tree12df_20,FUN="sum")




# plot 13
# 5m scale

sap.tree13df <- melt(sap.tree13,id.vars=rownames(sap.tree13))
colnames(sap.tree13df) <- c("sapID","treeID","dist")
sap.tree13df_5 <- sap.tree13df[sap.tree13df$dist<=5, ]
sap.tree13df_10 <- sap.tree13df[sap.tree13df$dist<=10, ]
sap.tree13df_15 <- sap.tree13df[sap.tree13df$dist<=15, ]
sap.tree13df_20 <- sap.tree13df[sap.tree13df$dist<=20, ]

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree13df_5 <- merge(sap.tree13df_5,d13,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree13df_5 <- sap.tree13df_5 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree13df_5)[3] <- "tree.species"
sap.tree13df_5$treeBA <- (pi*((sap.tree13df_5$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree13df_5 <- merge(sap.tree13df_5,d13,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree13df_5 <- sap.tree13df_5 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree13df_5)[5] <- "sap.species"

sap.tree13df_5$tree.species <- as.character(sap.tree13df_5$tree.species)
sap.tree13df_5$sap.species <- as.character(sap.tree13df_5$sap.species)
sap.tree13df_5$intxn <- ifelse(sap.tree13df_5$sap.species==sap.tree13df_5$tree.species,"con","het")
sap.tree13df_5 <- sap.tree13df_5 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree13df_5 <- aggregate(treeBA~.,data=sap.tree13df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
sap.tree13df_10 <- merge(sap.tree13df_10,d13,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree13df_10 <- sap.tree13df_10 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree13df_10)[3] <- "tree.species"
sap.tree13df_10$treeBA <- (pi*((sap.tree13df_10$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree13df_10 <- merge(sap.tree13df_10,d13,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree13df_10 <- sap.tree13df_10 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree13df_10)[5] <- "sap.species"

sap.tree13df_10$tree.species <- as.character(sap.tree13df_10$tree.species)
sap.tree13df_10$sap.species <- as.character(sap.tree13df_10$sap.species)
sap.tree13df_10$intxn <- ifelse(sap.tree13df_10$sap.species==sap.tree13df_10$tree.species,"con","het")
sap.tree13df_10 <- sap.tree13df_10 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree13df_10 <- aggregate(treeBA~.,data=sap.tree13df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree13df_15 <- merge(sap.tree13df_15,d13,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree13df_15 <- sap.tree13df_15 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree13df_15)[3] <- "tree.species"
sap.tree13df_15$treeBA <- (pi*((sap.tree13df_15$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree13df_15 <- merge(sap.tree13df_15,d13,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree13df_15 <- sap.tree13df_15 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree13df_15)[5] <- "sap.species"

sap.tree13df_15$tree.species <- as.character(sap.tree13df_15$tree.species)
sap.tree13df_15$sap.species <- as.character(sap.tree13df_15$sap.species)
sap.tree13df_15$intxn <- ifelse(sap.tree13df_15$sap.species==sap.tree13df_15$tree.species,"con","het")
sap.tree13df_15 <- sap.tree13df_15 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree13df_15 <- aggregate(treeBA~.,data=sap.tree13df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree13df_20 <- merge(sap.tree13df_20,d13,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree13df_20 <- sap.tree13df_20 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree13df_20)[3] <- "tree.species"
sap.tree13df_20$treeBA <- (pi*((sap.tree13df_20$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree13df_20 <- merge(sap.tree13df_20,d13,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree13df_20 <- sap.tree13df_20 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree13df_20)[5] <- "sap.species"

sap.tree13df_20$tree.species <- as.character(sap.tree13df_20$tree.species)
sap.tree13df_20$sap.species <- as.character(sap.tree13df_20$sap.species)
sap.tree13df_20$intxn <- ifelse(sap.tree13df_20$sap.species==sap.tree13df_20$tree.species,"con","het")
sap.tree13df_20 <- sap.tree13df_20 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree13df_20 <- aggregate(treeBA~.,data=sap.tree13df_20,FUN="sum")






# plot 14
# 5m scale

sap.tree14df <- melt(sap.tree14,id.vars=rownames(sap.tree14))
colnames(sap.tree14df) <- c("sapID","treeID","dist")
sap.tree14df_5 <- sap.tree14df[sap.tree14df$dist<=5, ]
sap.tree14df_10 <- sap.tree14df[sap.tree14df$dist<=10, ]
sap.tree14df_15 <- sap.tree14df[sap.tree14df$dist<=15, ]
sap.tree14df_20 <- sap.tree14df[sap.tree14df$dist<=20, ]

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree14df_5 <- merge(sap.tree14df_5,d14,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree14df_5 <- sap.tree14df_5 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree14df_5)[3] <- "tree.species"
sap.tree14df_5$treeBA <- (pi*((sap.tree14df_5$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree14df_5 <- merge(sap.tree14df_5,d14,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree14df_5 <- sap.tree14df_5 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree14df_5)[5] <- "sap.species"

sap.tree14df_5$tree.species <- as.character(sap.tree14df_5$tree.species)
sap.tree14df_5$sap.species <- as.character(sap.tree14df_5$sap.species)
sap.tree14df_5$intxn <- ifelse(sap.tree14df_5$sap.species==sap.tree14df_5$tree.species,"con","het")
sap.tree14df_5 <- sap.tree14df_5 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree14df_5 <- aggregate(treeBA~.,data=sap.tree14df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
sap.tree14df_10 <- merge(sap.tree14df_10,d14,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree14df_10 <- sap.tree14df_10 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree14df_10)[3] <- "tree.species"
sap.tree14df_10$treeBA <- (pi*((sap.tree14df_10$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree14df_10 <- merge(sap.tree14df_10,d14,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree14df_10 <- sap.tree14df_10 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree14df_10)[5] <- "sap.species"

sap.tree14df_10$tree.species <- as.character(sap.tree14df_10$tree.species)
sap.tree14df_10$sap.species <- as.character(sap.tree14df_10$sap.species)
sap.tree14df_10$intxn <- ifelse(sap.tree14df_10$sap.species==sap.tree14df_10$tree.species,"con","het")
sap.tree14df_10 <- sap.tree14df_10 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree14df_10 <- aggregate(treeBA~.,data=sap.tree14df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree14df_15 <- merge(sap.tree14df_15,d14,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree14df_15 <- sap.tree14df_15 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree14df_15)[3] <- "tree.species"
sap.tree14df_15$treeBA <- (pi*((sap.tree14df_15$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree14df_15 <- merge(sap.tree14df_15,d14,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree14df_15 <- sap.tree14df_15 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree14df_15)[5] <- "sap.species"

sap.tree14df_15$tree.species <- as.character(sap.tree14df_15$tree.species)
sap.tree14df_15$sap.species <- as.character(sap.tree14df_15$sap.species)
sap.tree14df_15$intxn <- ifelse(sap.tree14df_15$sap.species==sap.tree14df_15$tree.species,"con","het")
sap.tree14df_15 <- sap.tree14df_15 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree14df_15 <- aggregate(treeBA~.,data=sap.tree14df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree14df_20 <- merge(sap.tree14df_20,d14,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree14df_20 <- sap.tree14df_20 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree14df_20)[3] <- "tree.species"
sap.tree14df_20$treeBA <- (pi*((sap.tree14df_20$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree14df_20 <- merge(sap.tree14df_20,d14,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree14df_20 <- sap.tree14df_20 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree14df_20)[5] <- "sap.species"

sap.tree14df_20$tree.species <- as.character(sap.tree14df_20$tree.species)
sap.tree14df_20$sap.species <- as.character(sap.tree14df_20$sap.species)
sap.tree14df_20$intxn <- ifelse(sap.tree14df_20$sap.species==sap.tree14df_20$tree.species,"con","het")
sap.tree14df_20 <- sap.tree14df_20 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree14df_20 <- aggregate(treeBA~.,data=sap.tree14df_20,FUN="sum")







# plot 91
# 5m scale

sap.tree91df <- melt(sap.tree91,id.vars=rownames(sap.tree91))
colnames(sap.tree91df) <- c("sapID","treeID","dist")
sap.tree91df_5 <- sap.tree91df[sap.tree91df$dist<=5, ]
sap.tree91df_10 <- sap.tree91df[sap.tree91df$dist<=10, ]
sap.tree91df_15 <- sap.tree91df[sap.tree91df$dist<=15, ]
sap.tree91df_20 <- sap.tree91df[sap.tree91df$dist<=20, ]

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree91df_5 <- merge(sap.tree91df_5,d91,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree91df_5 <- sap.tree91df_5 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree91df_5)[3] <- "tree.species"
sap.tree91df_5$treeBA <- (pi*((sap.tree91df_5$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree91df_5 <- merge(sap.tree91df_5,d91,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree91df_5 <- sap.tree91df_5 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree91df_5)[5] <- "sap.species"

sap.tree91df_5$tree.species <- as.character(sap.tree91df_5$tree.species)
sap.tree91df_5$sap.species <- as.character(sap.tree91df_5$sap.species)
sap.tree91df_5$intxn <- ifelse(sap.tree91df_5$sap.species==sap.tree91df_5$tree.species,"con","het")
sap.tree91df_5 <- sap.tree91df_5 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree91df_5 <- aggregate(treeBA~.,data=sap.tree91df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
sap.tree91df_10 <- merge(sap.tree91df_10,d91,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree91df_10 <- sap.tree91df_10 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree91df_10)[3] <- "tree.species"
sap.tree91df_10$treeBA <- (pi*((sap.tree91df_10$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree91df_10 <- merge(sap.tree91df_10,d91,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree91df_10 <- sap.tree91df_10 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree91df_10)[5] <- "sap.species"

sap.tree91df_10$tree.species <- as.character(sap.tree91df_10$tree.species)
sap.tree91df_10$sap.species <- as.character(sap.tree91df_10$sap.species)
sap.tree91df_10$intxn <- ifelse(sap.tree91df_10$sap.species==sap.tree91df_10$tree.species,"con","het")
sap.tree91df_10 <- sap.tree91df_10 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree91df_10 <- aggregate(treeBA~.,data=sap.tree91df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree91df_15 <- merge(sap.tree91df_15,d91,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree91df_15 <- sap.tree91df_15 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree91df_15)[3] <- "tree.species"
sap.tree91df_15$treeBA <- (pi*((sap.tree91df_15$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree91df_15 <- merge(sap.tree91df_15,d91,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree91df_15 <- sap.tree91df_15 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree91df_15)[5] <- "sap.species"

sap.tree91df_15$tree.species <- as.character(sap.tree91df_15$tree.species)
sap.tree91df_15$sap.species <- as.character(sap.tree91df_15$sap.species)
sap.tree91df_15$intxn <- ifelse(sap.tree91df_15$sap.species==sap.tree91df_15$tree.species,"con","het")
sap.tree91df_15 <- sap.tree91df_15 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree91df_15 <- aggregate(treeBA~.,data=sap.tree91df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree91df_20 <- merge(sap.tree91df_20,d91,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree91df_20 <- sap.tree91df_20 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree91df_20)[3] <- "tree.species"
sap.tree91df_20$treeBA <- (pi*((sap.tree91df_20$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree91df_20 <- merge(sap.tree91df_20,d91,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree91df_20 <- sap.tree91df_20 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree91df_20)[5] <- "sap.species"

sap.tree91df_20$tree.species <- as.character(sap.tree91df_20$tree.species)
sap.tree91df_20$sap.species <- as.character(sap.tree91df_20$sap.species)
sap.tree91df_20$intxn <- ifelse(sap.tree91df_20$sap.species==sap.tree91df_20$tree.species,"con","het")
sap.tree91df_20 <- sap.tree91df_20 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree91df_20 <- aggregate(treeBA~.,data=sap.tree91df_20,FUN="sum")





# plot 93
# 5m scale

sap.tree93df <- melt(sap.tree93,id.vars=rownames(sap.tree93))
colnames(sap.tree93df) <- c("sapID","treeID","dist")
sap.tree93df_5 <- sap.tree93df[sap.tree93df$dist<=5, ]
sap.tree93df_10 <- sap.tree93df[sap.tree93df$dist<=10, ]
sap.tree93df_15 <- sap.tree93df[sap.tree93df$dist<=15, ]
sap.tree93df_20 <- sap.tree93df[sap.tree93df$dist<=20, ]

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree93df_5 <- merge(sap.tree93df_5,d93,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree93df_5 <- sap.tree93df_5 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree93df_5)[3] <- "tree.species"
sap.tree93df_5$treeBA <- (pi*((sap.tree93df_5$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree93df_5 <- merge(sap.tree93df_5,d93,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree93df_5 <- sap.tree93df_5 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree93df_5)[5] <- "sap.species"

sap.tree93df_5$tree.species <- as.character(sap.tree93df_5$tree.species)
sap.tree93df_5$sap.species <- as.character(sap.tree93df_5$sap.species)
sap.tree93df_5$intxn <- ifelse(sap.tree93df_5$sap.species==sap.tree93df_5$tree.species,"con","het")
sap.tree93df_5 <- sap.tree93df_5 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree93df_5 <- aggregate(treeBA~.,data=sap.tree93df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
sap.tree93df_10 <- merge(sap.tree93df_10,d93,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree93df_10 <- sap.tree93df_10 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree93df_10)[3] <- "tree.species"
sap.tree93df_10$treeBA <- (pi*((sap.tree93df_10$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree93df_10 <- merge(sap.tree93df_10,d93,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree93df_10 <- sap.tree93df_10 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree93df_10)[5] <- "sap.species"

sap.tree93df_10$tree.species <- as.character(sap.tree93df_10$tree.species)
sap.tree93df_10$sap.species <- as.character(sap.tree93df_10$sap.species)
sap.tree93df_10$intxn <- ifelse(sap.tree93df_10$sap.species==sap.tree93df_10$tree.species,"con","het")
sap.tree93df_10 <- sap.tree93df_10 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree93df_10 <- aggregate(treeBA~.,data=sap.tree93df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree93df_15 <- merge(sap.tree93df_15,d93,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree93df_15 <- sap.tree93df_15 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree93df_15)[3] <- "tree.species"
sap.tree93df_15$treeBA <- (pi*((sap.tree93df_15$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree93df_15 <- merge(sap.tree93df_15,d93,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree93df_15 <- sap.tree93df_15 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree93df_15)[5] <- "sap.species"

sap.tree93df_15$tree.species <- as.character(sap.tree93df_15$tree.species)
sap.tree93df_15$sap.species <- as.character(sap.tree93df_15$sap.species)
sap.tree93df_15$intxn <- ifelse(sap.tree93df_15$sap.species==sap.tree93df_15$tree.species,"con","het")
sap.tree93df_15 <- sap.tree93df_15 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree93df_15 <- aggregate(treeBA~.,data=sap.tree93df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree93df_20 <- merge(sap.tree93df_20,d93,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree93df_20 <- sap.tree93df_20 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree93df_20)[3] <- "tree.species"
sap.tree93df_20$treeBA <- (pi*((sap.tree93df_20$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree93df_20 <- merge(sap.tree93df_20,d93,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree93df_20 <- sap.tree93df_20 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree93df_20)[5] <- "sap.species"

sap.tree93df_20$tree.species <- as.character(sap.tree93df_20$tree.species)
sap.tree93df_20$sap.species <- as.character(sap.tree93df_20$sap.species)
sap.tree93df_20$intxn <- ifelse(sap.tree93df_20$sap.species==sap.tree93df_20$tree.species,"con","het")
sap.tree93df_20 <- sap.tree93df_20 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree93df_20 <- aggregate(treeBA~.,data=sap.tree93df_20,FUN="sum")




# plot 97
# 5m scale

sap.tree97df <- melt(sap.tree97,id.vars=rownames(sap.tree97))
colnames(sap.tree97df) <- c("sapID","treeID","dist")
sap.tree97df_5 <- sap.tree97df[sap.tree97df$dist<=5, ]
sap.tree97df_10 <- sap.tree97df[sap.tree97df$dist<=10, ]
sap.tree97df_15 <- sap.tree97df[sap.tree97df$dist<=15, ]
sap.tree97df_20 <- sap.tree97df[sap.tree97df$dist<=20, ]

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree97df_5 <- merge(sap.tree97df_5,d97,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree97df_5 <- sap.tree97df_5 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree97df_5)[3] <- "tree.species"
sap.tree97df_5$treeBA <- (pi*((sap.tree97df_5$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree97df_5 <- merge(sap.tree97df_5,d97,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree97df_5 <- sap.tree97df_5 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree97df_5)[5] <- "sap.species"

sap.tree97df_5$tree.species <- as.character(sap.tree97df_5$tree.species)
sap.tree97df_5$sap.species <- as.character(sap.tree97df_5$sap.species)
sap.tree97df_5$intxn <- ifelse(sap.tree97df_5$sap.species==sap.tree97df_5$tree.species,"con","het")
sap.tree97df_5 <- sap.tree97df_5 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree97df_5 <- aggregate(treeBA~.,data=sap.tree97df_5,FUN="sum")


# 10m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
sap.tree97df_10 <- merge(sap.tree97df_10,d97,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree97df_10 <- sap.tree97df_10 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree97df_10)[3] <- "tree.species"
sap.tree97df_10$treeBA <- (pi*((sap.tree97df_10$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree97df_10 <- merge(sap.tree97df_10,d97,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree97df_10 <- sap.tree97df_10 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree97df_10)[5] <- "sap.species"

sap.tree97df_10$tree.species <- as.character(sap.tree97df_10$tree.species)
sap.tree97df_10$sap.species <- as.character(sap.tree97df_10$sap.species)
sap.tree97df_10$intxn <- ifelse(sap.tree97df_10$sap.species==sap.tree97df_10$tree.species,"con","het")
sap.tree97df_10 <- sap.tree97df_10 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree97df_10 <- aggregate(treeBA~.,data=sap.tree97df_10,FUN="sum")


# 15m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree97df_15 <- merge(sap.tree97df_15,d97,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree97df_15 <- sap.tree97df_15 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree97df_15)[3] <- "tree.species"
sap.tree97df_15$treeBA <- (pi*((sap.tree97df_15$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree97df_15 <- merge(sap.tree97df_15,d97,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree97df_15 <- sap.tree97df_15 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree97df_15)[5] <- "sap.species"

sap.tree97df_15$tree.species <- as.character(sap.tree97df_15$tree.species)
sap.tree97df_15$sap.species <- as.character(sap.tree97df_15$sap.species)
sap.tree97df_15$intxn <- ifelse(sap.tree97df_15$sap.species==sap.tree97df_15$tree.species,"con","het")
sap.tree97df_15 <- sap.tree97df_15 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree97df_15 <- aggregate(treeBA~.,data=sap.tree97df_15,FUN="sum")


# 20m scale

# add info from tree df so we know diameters for tree stems, then calculate BA for tree stems
# (keep rows sapID, treeID, species, and diam1 only)
sap.tree97df_20 <- merge(sap.tree97df_20,d97,by.x="treeID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree97df_20 <- sap.tree97df_20 %>% select(sapID, treeID, species, diam1)
colnames(sap.tree97df_20)[3] <- "tree.species"
sap.tree97df_20$treeBA <- (pi*((sap.tree97df_20$diam1)/2)^2)

# add info from sap df so we can specify whether stem interaction is con or het
sap.tree97df_20 <- merge(sap.tree97df_20,d97,by.x="sapID",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.tree97df_20 <- sap.tree97df_20 %>% select(sapID, treeID, tree.species, treeBA, species)
colnames(sap.tree97df_20)[5] <- "sap.species"

sap.tree97df_20$tree.species <- as.character(sap.tree97df_20$tree.species)
sap.tree97df_20$sap.species <- as.character(sap.tree97df_20$sap.species)
sap.tree97df_20$intxn <- ifelse(sap.tree97df_20$sap.species==sap.tree97df_20$tree.species,"con","het")
sap.tree97df_20 <- sap.tree97df_20 %>% select(sapID, treeBA, intxn)

# aggregate to get total BA of tree stems for each combination of sap stem ID and intxn type
sap.tree97df_20 <- aggregate(treeBA~.,data=sap.tree97df_20,FUN="sum")



# 5m
# restructure data - from long to wide (dcast)
sap.treeBA12_5 <- dcast(sap.tree12df_5,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA12_5) <- c("ID","BAc5","BAh5")
sap.treeBA12_5[is.na(sap.treeBA12_5)] <- 0
BA12_5 <- rbind(seed.sap_treeBA12_5, sap.treeBA12_5)

sap.treeBA13_5 <- dcast(sap.tree13df_5,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA13_5) <- c("ID","BAc5","BAh5")
sap.treeBA13_5[is.na(sap.treeBA13_5)] <- 0
BA13_5 <- rbind(seed.sap_treeBA13_5, sap.treeBA13_5)

sap.treeBA14_5 <- dcast(sap.tree14df_5,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA14_5) <- c("ID","BAc5","BAh5")
sap.treeBA14_5[is.na(sap.treeBA14_5)] <- 0
BA14_5 <- rbind(seed.sap_treeBA14_5, sap.treeBA14_5)

sap.treeBA91_5 <- dcast(sap.tree91df_5,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA91_5) <- c("ID","BAc5","BAh5")
sap.treeBA91_5[is.na(sap.treeBA91_5)] <- 0
BA91_5 <- rbind(seed.sap_treeBA91_5, sap.treeBA91_5)

sap.treeBA93_5 <- dcast(sap.tree93df_5,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA93_5) <- c("ID","BAc5","BAh5")
sap.treeBA93_5[is.na(sap.treeBA93_5)] <- 0
BA93_5 <- rbind(seed.sap_treeBA93_5, sap.treeBA93_5)

sap.treeBA97_5 <- dcast(sap.tree97df_5,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA97_5) <- c("ID","BAc5","BAh5")
sap.treeBA97_5[is.na(sap.treeBA97_5)] <- 0
BA97_5 <- rbind(seed.sap_treeBA97_5, sap.treeBA97_5)





# 10m
sap.treeBA12_10 <- dcast(sap.tree12df_10,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA12_10) <- c("ID","BAc10","BAh10")
sap.treeBA12_10[is.na(sap.treeBA12_10)] <- 0
BA12_10 <- rbind(seed.sap_treeBA12_10, sap.treeBA12_10)

sap.treeBA13_10 <- dcast(sap.tree13df_10,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA13_10) <- c("ID","BAc10","BAh10")
sap.treeBA13_10[is.na(sap.treeBA13_10)] <- 0
BA13_10 <- rbind(seed.sap_treeBA13_10, sap.treeBA13_10)

sap.treeBA14_10 <- dcast(sap.tree14df_10,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA14_10) <- c("ID","BAc10","BAh10")
sap.treeBA14_10[is.na(sap.treeBA14_10)] <- 0
BA14_10 <- rbind(seed.sap_treeBA14_10, sap.treeBA14_10)

sap.treeBA91_10 <- dcast(sap.tree91df_10,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA91_10) <- c("ID","BAc10","BAh10")
sap.treeBA91_10[is.na(sap.treeBA91_10)] <- 0
BA91_10 <- rbind(seed.sap_treeBA91_10, sap.treeBA91_10)

sap.treeBA93_10 <- dcast(sap.tree93df_10,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA93_10) <- c("ID","BAc10","BAh10")
sap.treeBA93_10[is.na(sap.treeBA93_10)] <- 0
BA93_10 <- rbind(seed.sap_treeBA93_10, sap.treeBA93_10)

sap.treeBA97_10 <- dcast(sap.tree97df_10,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA97_10) <- c("ID","BAc10","BAh10")
sap.treeBA97_10[is.na(sap.treeBA97_10)] <- 0
BA97_10 <- rbind(seed.sap_treeBA97_10, sap.treeBA97_10)






# 15m 
sap.treeBA12_15 <- dcast(sap.tree12df_15,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA12_15) <- c("ID","BAc15","BAh15")
sap.treeBA12_15[is.na(sap.treeBA12_15)] <- 0
BA12_15 <- rbind(seed.sap_treeBA12_15, sap.treeBA12_15)

sap.treeBA13_15 <- dcast(sap.tree13df_15,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA13_15) <- c("ID","BAc15","BAh15")
sap.treeBA13_15[is.na(sap.treeBA13_15)] <- 0
BA13_15 <- rbind(seed.sap_treeBA13_15, sap.treeBA13_15)

sap.treeBA14_15 <- dcast(sap.tree14df_15,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA14_15) <- c("ID","BAc15","BAh15")
sap.treeBA14_15[is.na(sap.treeBA14_15)] <- 0
BA14_15 <- rbind(seed.sap_treeBA14_15, sap.treeBA14_15)

sap.treeBA91_15 <- dcast(sap.tree91df_15,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA91_15) <- c("ID","BAc15","BAh15")
sap.treeBA91_15[is.na(sap.treeBA91_15)] <- 0
BA91_15 <- rbind(seed.sap_treeBA91_15, sap.treeBA91_15)

sap.treeBA93_15 <- dcast(sap.tree93df_15,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA93_15) <- c("ID","BAc15","BAh15")
sap.treeBA93_15[is.na(sap.treeBA93_15)] <- 0
BA93_15 <- rbind(seed.sap_treeBA93_15, sap.treeBA93_15)

sap.treeBA97_15 <- dcast(sap.tree97df_15,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA97_15) <- c("ID","BAc15","BAh15")
sap.treeBA97_15[is.na(sap.treeBA97_15)] <- 0
BA97_15 <- rbind(seed.sap_treeBA97_15, sap.treeBA97_15)





# 20m
sap.treeBA12_20 <- dcast(sap.tree12df_20,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA12_20) <- c("ID","BAc20","BAh20")
sap.treeBA12_20[is.na(sap.treeBA12_20)] <- 0
BA12_20 <- rbind(seed.sap_treeBA12_20, sap.treeBA12_20)

sap.treeBA13_20 <- dcast(sap.tree13df_20,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA13_20) <- c("ID","BAc20","BAh20")
sap.treeBA13_20[is.na(sap.treeBA13_20)] <- 0
BA13_20 <- rbind(seed.sap_treeBA13_20, sap.treeBA13_20)

sap.treeBA14_20 <- dcast(sap.tree14df_20,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA14_20) <- c("ID","BAc20","BAh20")
sap.treeBA14_20[is.na(sap.treeBA14_20)] <- 0
BA14_20 <- rbind(seed.sap_treeBA14_20, sap.treeBA14_20)

sap.treeBA91_20 <- dcast(sap.tree91df_20,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA91_20) <- c("ID","BAc20","BAh20")
sap.treeBA91_20[is.na(sap.treeBA91_20)] <- 0
BA91_20 <- rbind(seed.sap_treeBA91_20, sap.treeBA91_20)

sap.treeBA93_20 <- dcast(sap.tree93df_20,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA93_20) <- c("ID","BAc20","BAh20")
sap.treeBA93_20[is.na(sap.treeBA93_20)] <- 0
BA93_20 <- rbind(seed.sap_treeBA93_20, sap.treeBA93_20)

sap.treeBA97_20 <- dcast(sap.tree97df_20,sapID~intxn,value.var="treeBA")
colnames(sap.treeBA97_20) <- c("ID","BAc20","BAh20")
sap.treeBA97_20[is.na(sap.treeBA97_20)] <- 0
BA97_20 <- rbind(seed.sap_treeBA97_20, sap.treeBA97_20)



BA12 <- merge(BA12_5, BA12_10, by="ID", all = TRUE)
BA12 <- merge(BA12, BA12_15, by="ID", all = TRUE)
BA12 <- merge(BA12, BA12_20, by="ID", all = TRUE)
BA12[is.na(BA12)] <- 0
BA12$plot <- 12

BA13 <- merge(BA13_5, BA13_10, by="ID", all = TRUE)
BA13 <- merge(BA13, BA13_15, by="ID", all = TRUE)
BA13 <- merge(BA13, BA13_20, by="ID", all = TRUE)
BA13[is.na(BA13)] <- 0
BA13$plot <- 13

BA14 <- merge(BA14_5, BA14_10, by="ID", all = TRUE)
BA14 <- merge(BA14, BA14_15, by="ID", all = TRUE)
BA14 <- merge(BA14, BA14_20, by="ID", all = TRUE)
BA14[is.na(BA14)] <- 0
BA14$plot <- 14

BA91 <- merge(BA91_5, BA91_10, by="ID", all = TRUE)
BA91 <- merge(BA91, BA91_15, by="ID", all = TRUE)
BA91 <- merge(BA91, BA91_20, by="ID", all = TRUE)
BA91[is.na(BA91)] <- 0
BA91$plot <- 91

BA93 <- merge(BA93_5, BA93_10, by="ID", all = TRUE)
BA93 <- merge(BA93, BA93_15, by="ID", all = TRUE)
BA93 <- merge(BA93, BA93_20, by="ID", all = TRUE)
BA93[is.na(BA93)] <- 0
BA93$plot <- 93

BA97 <- merge(BA97_5, BA97_10, by="ID", all = TRUE)
BA97 <- merge(BA97, BA97_15, by="ID", all = TRUE)
BA97 <- merge(BA97, BA97_20, by="ID", all = TRUE)
BA97[is.na(BA97)] <- 0
BA97$plot <- 97


# bind dfs together
BA <- rbind(BA12, BA13, BA14, BA91, BA93, BA97)



# SEEDLING VS. SEEDLING interactions (3m scale only)
###############################################################

# plot 12
seed.seed12df <- melt(seed.seed12,id.vars=rownames(seed.seed12))
colnames(seed.seed12df) <- c("seedID1","seedID2","dist")
seed.seed12df <- seed.seed12df[seed.seed12df$dist<=3, ]

# remove rows that calculate distance between same individual
seed.seed12df <- seed.seed12df[seed.seed12df$seedID1 != seed.seed12df$seedID2,]

# add info from seed df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
seed.seed12df <- merge(seed.seed12df,d12,by.x="seedID2",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed12df <- seed.seed12df %>% select(seedID1, seedID2, species)
colnames(seed.seed12df)[3] <- "species2"

# add info from seed df so we can specify whether stem interaction is con or het
seed.seed12df <- merge(seed.seed12df,d12,by.x="seedID1",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed12df <- seed.seed12df %>% select(seedID1, seedID2, species2, species)
colnames(seed.seed12df)[4] <- "species1"

seed.seed12df$species2 <- as.character(seed.seed12df$species2)
seed.seed12df$species1 <- as.character(seed.seed12df$species1)
seed.seed12df$intxn <- ifelse(seed.seed12df$species1==seed.seed12df$species2,"con","het")
seed.seed12df <- seed.seed12df %>% select(seedID1, intxn)

# aggregate to get count of con and het seedling stems
seed.seed12df$count <- 1
seed.seed12df <- aggregate(count~.,data=seed.seed12df,FUN="sum")





# plot 13

seed.seed13df <- melt(seed.seed13,id.vars=rownames(seed.seed13))
colnames(seed.seed13df) <- c("seedID1","seedID2","dist")
seed.seed13df <- seed.seed13df[seed.seed13df$dist<=3, ]

# remove rows that calculate distance between same individual
seed.seed13df <- seed.seed13df[seed.seed13df$seedID1 != seed.seed13df$seedID2,]

# add info from seed df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
seed.seed13df <- merge(seed.seed13df,d13,by.x="seedID2",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed13df <- seed.seed13df %>% select(seedID1, seedID2, species)
colnames(seed.seed13df)[3] <- "species2"

# add info from seed df so we can specify whether stem interaction is con or het
seed.seed13df <- merge(seed.seed13df,d13,by.x="seedID1",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed13df <- seed.seed13df %>% select(seedID1, seedID2, species2, species)
colnames(seed.seed13df)[4] <- "species1"

seed.seed13df$species2 <- as.character(seed.seed13df$species2)
seed.seed13df$species1 <- as.character(seed.seed13df$species1)
seed.seed13df$intxn <- ifelse(seed.seed13df$species1==seed.seed13df$species2,"con","het")
seed.seed13df <- seed.seed13df %>% select(seedID1, intxn)

# aggregate to get count of con and het seedling stems
seed.seed13df$count <- 1
seed.seed13df <- aggregate(count~.,data=seed.seed13df,FUN="sum")




# plot 14

seed.seed14df <- melt(seed.seed14,id.vars=rownames(seed.seed14))
colnames(seed.seed14df) <- c("seedID1","seedID2","dist")
seed.seed14df <- seed.seed14df[seed.seed14df$dist<=3, ]

# remove rows that calculate distance between same individual
seed.seed14df <- seed.seed14df[seed.seed14df$seedID1 != seed.seed14df$seedID2,]

# add info from seed df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
seed.seed14df <- merge(seed.seed14df,d14,by.x="seedID2",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed14df <- seed.seed14df %>% select(seedID1, seedID2, species)
colnames(seed.seed14df)[3] <- "species2"

# add info from seed df so we can specify whether stem interaction is con or het
seed.seed14df <- merge(seed.seed14df,d14,by.x="seedID1",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed14df <- seed.seed14df %>% select(seedID1, seedID2, species2, species)
colnames(seed.seed14df)[4] <- "species1"

seed.seed14df$species2 <- as.character(seed.seed14df$species2)
seed.seed14df$species1 <- as.character(seed.seed14df$species1)
seed.seed14df$intxn <- ifelse(seed.seed14df$species1==seed.seed14df$species2,"con","het")
seed.seed14df <- seed.seed14df %>% select(seedID1, intxn)

# aggregate to get count of con and het seedling stems
seed.seed14df$count <- 1
seed.seed14df <- aggregate(count~.,data=seed.seed14df,FUN="sum")





# plot 91

seed.seed91df <- melt(seed.seed91,id.vars=rownames(seed.seed91))
colnames(seed.seed91df) <- c("seedID1","seedID2","dist")
seed.seed91df <- seed.seed91df[seed.seed91df$dist<=3, ]

# remove rows that calculate distance between same individual
seed.seed91df <- seed.seed91df[seed.seed91df$seedID1 != seed.seed91df$seedID2,]

# add info from seed df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
seed.seed91df <- merge(seed.seed91df,d91,by.x="seedID2",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed91df <- seed.seed91df %>% select(seedID1, seedID2, species)
colnames(seed.seed91df)[3] <- "species2"

# add info from seed df so we can specify whether stem interaction is con or het
seed.seed91df <- merge(seed.seed91df,d91,by.x="seedID1",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed91df <- seed.seed91df %>% select(seedID1, seedID2, species2, species)
colnames(seed.seed91df)[4] <- "species1"

seed.seed91df$species2 <- as.character(seed.seed91df$species2)
seed.seed91df$species1 <- as.character(seed.seed91df$species1)
seed.seed91df$intxn <- ifelse(seed.seed91df$species1==seed.seed91df$species2,"con","het")
seed.seed91df <- seed.seed91df %>% select(seedID1, intxn)

# aggregate to get count of con and het seedling stems
seed.seed91df$count <- 1
seed.seed91df <- aggregate(count~.,data=seed.seed91df,FUN="sum")





# plot 93

seed.seed93df <- melt(seed.seed93,id.vars=rownames(seed.seed93))
colnames(seed.seed93df) <- c("seedID1","seedID2","dist")
seed.seed93df <- seed.seed93df[seed.seed93df$dist<=3, ]

# remove rows that calculate distance between same individual
seed.seed93df <- seed.seed93df[seed.seed93df$seedID1 != seed.seed93df$seedID2,]

# add info from seed df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
seed.seed93df <- merge(seed.seed93df,d93,by.x="seedID2",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed93df <- seed.seed93df %>% select(seedID1, seedID2, species)
colnames(seed.seed93df)[3] <- "species2"

# add info from seed df so we can specify whether stem interaction is con or het
seed.seed93df <- merge(seed.seed93df,d93,by.x="seedID1",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed93df <- seed.seed93df %>% select(seedID1, seedID2, species2, species)
colnames(seed.seed93df)[4] <- "species1"

seed.seed93df$species2 <- as.character(seed.seed93df$species2)
seed.seed93df$species1 <- as.character(seed.seed93df$species1)
seed.seed93df$intxn <- ifelse(seed.seed93df$species1==seed.seed93df$species2,"con","het")
seed.seed93df <- seed.seed93df %>% select(seedID1, intxn)

# aggregate to get count of con and het seedling stems
seed.seed93df$count <- 1
seed.seed93df <- aggregate(count~.,data=seed.seed93df,FUN="sum")





# plot 97

seed.seed97df <- melt(seed.seed97,id.vars=rownames(seed.seed97))
colnames(seed.seed97df) <- c("seedID1","seedID2","dist")
seed.seed97df <- seed.seed97df[seed.seed97df$dist<=3, ]

# remove rows that calculate distance between same individual
seed.seed97df <- seed.seed97df[seed.seed97df$seedID1 != seed.seed97df$seedID2,]

# add info from seed df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
seed.seed97df <- merge(seed.seed97df,d97,by.x="seedID2",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed97df <- seed.seed97df %>% select(seedID1, seedID2, species)
colnames(seed.seed97df)[3] <- "species2"

# add info from seed df so we can specify whether stem interaction is con or het
seed.seed97df <- merge(seed.seed97df,d97,by.x="seedID1",by.y="ID",all.x=TRUE,all.y=FALSE)
seed.seed97df <- seed.seed97df %>% select(seedID1, seedID2, species2, species)
colnames(seed.seed97df)[4] <- "species1"

seed.seed97df$species2 <- as.character(seed.seed97df$species2)
seed.seed97df$species1 <- as.character(seed.seed97df$species1)
seed.seed97df$intxn <- ifelse(seed.seed97df$species1==seed.seed97df$species2,"con","het")
seed.seed97df <- seed.seed97df %>% select(seedID1, intxn)

# aggregate to get count of con and het seedling stems
seed.seed97df$count <- 1
seed.seed97df <- aggregate(count~.,data=seed.seed97df,FUN="sum")






# restructure data - from long to wide (dcast)
seed.seed_n_12 <- dcast(seed.seed12df,seedID1~intxn,value.var="count")
colnames(seed.seed_n_12) <- c("ID","n_con","n_het")
seed.seed_n_12[is.na(seed.seed_n_12)] <- 0

seed.seed_n_13 <- dcast(seed.seed13df,seedID1~intxn,value.var="count")
colnames(seed.seed_n_13) <- c("ID","n_con","n_het")
seed.seed_n_13[is.na(seed.seed_n_13)] <- 0

seed.seed_n_14 <- dcast(seed.seed14df,seedID1~intxn,value.var="count")
colnames(seed.seed_n_14) <- c("ID","n_con","n_het")
seed.seed_n_14[is.na(seed.seed_n_14)] <- 0

seed.seed_n_91 <- dcast(seed.seed91df,seedID1~intxn,value.var="count")
colnames(seed.seed_n_91) <- c("ID","n_con","n_het")
seed.seed_n_91[is.na(seed.seed_n_91)] <- 0

seed.seed_n_93 <- dcast(seed.seed93df,seedID1~intxn,value.var="count")
colnames(seed.seed_n_93) <- c("ID","n_con","n_het")
seed.seed_n_93[is.na(seed.seed_n_93)] <- 0

seed.seed_n_97 <- dcast(seed.seed97df,seedID1~intxn,value.var="count")
colnames(seed.seed_n_97) <- c("ID","n_con","n_het")
seed.seed_n_97[is.na(seed.seed_n_97)] <- 0


# create plot column
seed.seed_n_12$plot <- 12
seed.seed_n_13$plot <- 13
seed.seed_n_14$plot <- 14
seed.seed_n_91$plot <- 91
seed.seed_n_93$plot <- 93
seed.seed_n_97$plot <- 97

# bind dfs together
seed.seed_n <- rbind(seed.seed_n_12, seed.seed_n_13, seed.seed_n_14, 
                     seed.seed_n_91, seed.seed_n_93, seed.seed_n_97)







# SAPLING VS. SAPLING (5m scale only)

# plot 12
sap.sap12df <- melt(sap.sap12,id.vars=rownames(sap.sap12))
colnames(sap.sap12df) <- c("sapID1","sapID2","dist")
sap.sap12df <- sap.sap12df[sap.sap12df$dist<=5, ]

# remove rows that calculate distance between same individual
sap.sap12df <- sap.sap12df[sap.sap12df$sapID1 != sap.sap12df$sapID2,]

# add info from sap df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
sap.sap12df <- merge(sap.sap12df,d12,by.x="sapID2",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap12df <- sap.sap12df %>% select(sapID1, sapID2, species)
colnames(sap.sap12df)[3] <- "species2"

# add info from sap df so we can specify whether stem interaction is con or het
sap.sap12df <- merge(sap.sap12df,d12,by.x="sapID1",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap12df <- sap.sap12df %>% select(sapID1, sapID2, species2, species)
colnames(sap.sap12df)[4] <- "species1"

sap.sap12df$species2 <- as.character(sap.sap12df$species2)
sap.sap12df$species1 <- as.character(sap.sap12df$species1)
sap.sap12df$intxn <- ifelse(sap.sap12df$species1==sap.sap12df$species2,"con","het")
sap.sap12df <- sap.sap12df %>% select(sapID1, intxn)

# aggregate to get count of con and het sapling stems
sap.sap12df$count <- 1
sap.sap12df <- aggregate(count~.,data=sap.sap12df,FUN="sum")





# plot 13

sap.sap13df <- melt(sap.sap13,id.vars=rownames(sap.sap13))
colnames(sap.sap13df) <- c("sapID1","sapID2","dist")
sap.sap13df <- sap.sap13df[sap.sap13df$dist<=5, ]

# remove rows that calculate distance between same individual
sap.sap13df <- sap.sap13df[sap.sap13df$sapID1 != sap.sap13df$sapID2,]

# add info from sap df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
sap.sap13df <- merge(sap.sap13df,d13,by.x="sapID2",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap13df <- sap.sap13df %>% select(sapID1, sapID2, species)
colnames(sap.sap13df)[3] <- "species2"

# add info from sap df so we can specify whether stem interaction is con or het
sap.sap13df <- merge(sap.sap13df,d13,by.x="sapID1",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap13df <- sap.sap13df %>% select(sapID1, sapID2, species2, species)
colnames(sap.sap13df)[4] <- "species1"

sap.sap13df$species2 <- as.character(sap.sap13df$species2)
sap.sap13df$species1 <- as.character(sap.sap13df$species1)
sap.sap13df$intxn <- ifelse(sap.sap13df$species1==sap.sap13df$species2,"con","het")
sap.sap13df <- sap.sap13df %>% select(sapID1, intxn)

# aggregate to get count of con and het sapling stems
sap.sap13df$count <- 1
sap.sap13df <- aggregate(count~.,data=sap.sap13df,FUN="sum")




# plot 14

sap.sap14df <- melt(sap.sap14,id.vars=rownames(sap.sap14))
colnames(sap.sap14df) <- c("sapID1","sapID2","dist")
sap.sap14df <- sap.sap14df[sap.sap14df$dist<=5, ]

# remove rows that calculate distance between same individual
sap.sap14df <- sap.sap14df[sap.sap14df$sapID1 != sap.sap14df$sapID2,]

# add info from sap df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
sap.sap14df <- merge(sap.sap14df,d14,by.x="sapID2",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap14df <- sap.sap14df %>% select(sapID1, sapID2, species)
colnames(sap.sap14df)[3] <- "species2"

# add info from sap df so we can specify whether stem interaction is con or het
sap.sap14df <- merge(sap.sap14df,d14,by.x="sapID1",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap14df <- sap.sap14df %>% select(sapID1, sapID2, species2, species)
colnames(sap.sap14df)[4] <- "species1"

sap.sap14df$species2 <- as.character(sap.sap14df$species2)
sap.sap14df$species1 <- as.character(sap.sap14df$species1)
sap.sap14df$intxn <- ifelse(sap.sap14df$species1==sap.sap14df$species2,"con","het")
sap.sap14df <- sap.sap14df %>% select(sapID1, intxn)

# aggregate to get count of con and het sapling stems
sap.sap14df$count <- 1
sap.sap14df <- aggregate(count~.,data=sap.sap14df,FUN="sum")





# plot 91

sap.sap91df <- melt(sap.sap91,id.vars=rownames(sap.sap91))
colnames(sap.sap91df) <- c("sapID1","sapID2","dist")
sap.sap91df <- sap.sap91df[sap.sap91df$dist<=5, ]

# remove rows that calculate distance between same individual
sap.sap91df <- sap.sap91df[sap.sap91df$sapID1 != sap.sap91df$sapID2,]

# add info from sap df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
sap.sap91df <- merge(sap.sap91df,d91,by.x="sapID2",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap91df <- sap.sap91df %>% select(sapID1, sapID2, species)
colnames(sap.sap91df)[3] <- "species2"

# add info from sap df so we can specify whether stem interaction is con or het
sap.sap91df <- merge(sap.sap91df,d91,by.x="sapID1",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap91df <- sap.sap91df %>% select(sapID1, sapID2, species2, species)
colnames(sap.sap91df)[4] <- "species1"

sap.sap91df$species2 <- as.character(sap.sap91df$species2)
sap.sap91df$species1 <- as.character(sap.sap91df$species1)
sap.sap91df$intxn <- ifelse(sap.sap91df$species1==sap.sap91df$species2,"con","het")
sap.sap91df <- sap.sap91df %>% select(sapID1, intxn)

# aggregate to get count of con and het sapling stems
sap.sap91df$count <- 1
sap.sap91df <- aggregate(count~.,data=sap.sap91df,FUN="sum")





# plot 93

sap.sap93df <- melt(sap.sap93,id.vars=rownames(sap.sap93))
colnames(sap.sap93df) <- c("sapID1","sapID2","dist")
sap.sap93df <- sap.sap93df[sap.sap93df$dist<=5, ]

# remove rows that calculate distance between same individual
sap.sap93df <- sap.sap93df[sap.sap93df$sapID1 != sap.sap93df$sapID2,]

# add info from sap df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
sap.sap93df <- merge(sap.sap93df,d93,by.x="sapID2",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap93df <- sap.sap93df %>% select(sapID1, sapID2, species)
colnames(sap.sap93df)[3] <- "species2"

# add info from sap df so we can specify whether stem interaction is con or het
sap.sap93df <- merge(sap.sap93df,d93,by.x="sapID1",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap93df <- sap.sap93df %>% select(sapID1, sapID2, species2, species)
colnames(sap.sap93df)[4] <- "species1"

sap.sap93df$species2 <- as.character(sap.sap93df$species2)
sap.sap93df$species1 <- as.character(sap.sap93df$species1)
sap.sap93df$intxn <- ifelse(sap.sap93df$species1==sap.sap93df$species2,"con","het")
sap.sap93df <- sap.sap93df %>% select(sapID1, intxn)

# aggregate to get count of con and het sapling stems
sap.sap93df$count <- 1
sap.sap93df <- aggregate(count~.,data=sap.sap93df,FUN="sum")





# plot 97

sap.sap97df <- melt(sap.sap97,id.vars=rownames(sap.sap97))
colnames(sap.sap97df) <- c("sapID1","sapID2","dist")
sap.sap97df <- sap.sap97df[sap.sap97df$dist<=5, ]

# remove rows that calculate distance between same individual
sap.sap97df <- sap.sap97df[sap.sap97df$sapID1 != sap.sap97df$sapID2,]

# add info from sap df so we can specify whether stem interaction is con or het
# (keep rows sapID, treeID, species, and diam1 only)
sap.sap97df <- merge(sap.sap97df,d97,by.x="sapID2",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap97df <- sap.sap97df %>% select(sapID1, sapID2, species)
colnames(sap.sap97df)[3] <- "species2"

# add info from sap df so we can specify whether stem interaction is con or het
sap.sap97df <- merge(sap.sap97df,d97,by.x="sapID1",by.y="ID",all.x=TRUE,all.y=FALSE)
sap.sap97df <- sap.sap97df %>% select(sapID1, sapID2, species2, species)
colnames(sap.sap97df)[4] <- "species1"

sap.sap97df$species2 <- as.character(sap.sap97df$species2)
sap.sap97df$species1 <- as.character(sap.sap97df$species1)
sap.sap97df$intxn <- ifelse(sap.sap97df$species1==sap.sap97df$species2,"con","het")
sap.sap97df <- sap.sap97df %>% select(sapID1, intxn)

# aggregate to get count of con and het sapling stems
sap.sap97df$count <- 1
sap.sap97df <- aggregate(count~.,data=sap.sap97df,FUN="sum")






# restructure data - from long to wide (dcast)
sap.sap_n_12 <- dcast(sap.sap12df,sapID1~intxn,value.var="count")
colnames(sap.sap_n_12) <- c("ID","n_con","n_het")
sap.sap_n_12[is.na(sap.sap_n_12)] <- 0

sap.sap_n_13 <- dcast(sap.sap13df,sapID1~intxn,value.var="count")
colnames(sap.sap_n_13) <- c("ID","n_con","n_het")
sap.sap_n_13[is.na(sap.sap_n_13)] <- 0

sap.sap_n_14 <- dcast(sap.sap14df,sapID1~intxn,value.var="count")
colnames(sap.sap_n_14) <- c("ID","n_con","n_het")
sap.sap_n_14[is.na(sap.sap_n_14)] <- 0

sap.sap_n_91 <- dcast(sap.sap91df,sapID1~intxn,value.var="count")
colnames(sap.sap_n_91) <- c("ID","n_con","n_het")
sap.sap_n_91[is.na(sap.sap_n_91)] <- 0

sap.sap_n_93 <- dcast(sap.sap93df,sapID1~intxn,value.var="count")
colnames(sap.sap_n_93) <- c("ID","n_con","n_het")
sap.sap_n_93[is.na(sap.sap_n_93)] <- 0

sap.sap_n_97 <- dcast(sap.sap97df,sapID1~intxn,value.var="count")
colnames(sap.sap_n_97) <- c("ID","n_con","n_het")
sap.sap_n_97[is.na(sap.sap_n_97)] <- 0


# create plot column
sap.sap_n_12$plot <- 12
sap.sap_n_13$plot <- 13
sap.sap_n_14$plot <- 14
sap.sap_n_91$plot <- 91
sap.sap_n_93$plot <- 93
sap.sap_n_97$plot <- 97

# bind dfs together
sap.sap_n <- rbind(sap.sap_n_12, sap.sap_n_13, sap.sap_n_14, 
                     sap.sap_n_91, sap.sap_n_93, sap.sap_n_97)

# combine seedling counts with sapling counts
counts <- rbind(seed.seed_n, sap.sap_n)

# add counts and tree basal areas to main dataframe
dat2 <- merge(dat, counts, by=c("ID","plot"), all = TRUE)
dat2 <- merge(dat2, BA, by=c("ID","plot"), all = TRUE)

# there are 10 observations where seedling/sapling doesn't have any values
# for con/het densities. probably due to X, Y coordinate issues, so remove them
remove <- dat2 %>% filter(is.na(BAc20)) %>% 
  filter(size_class=="sap" | size_class=="seed") %>% 
  select(ID, plot)

dat2 <- anti_join(dat2, remove, by=c("ID","plot"))

# remove stems from main df that aren't seedlings or saplings
dat2 <- dat2 %>% filter(size_class != "tree")


# We need an edge correction to remove bias of stems on edges of plots that 
# have low density measures. Remove stems <20m from plot edges
# original plot windows are:
#win12 <- owin(c(0,170),c(0,120))
#win13 <- owin(c(0,140),c(0,140))
#win14 <- owin(poly=list(x=c(0,250,250,225,225,0),y=c(0,0,50,50,100,100)))
#win91 <- owin(c(0,70),c(0,75))
#win93 <- owin(poly=list(x=c(0, 185, 185, 200, 200, 0),y=c(0, 0, 40, 40, 100, 100)))
#win97 <- owin(c(0,256),c(0,256))

# modify plot windows. spatstat will throw a warning that stems are being left out. 
win12 <- owin(c(20,150),c(20,100))
d12 <- dat2 %>% 
  filter(plot==12) %>% 
  select(X,Y,ID,plot)
d12pp <- as.ppp(d12,owin(win12))
ext12 <- d12pp$marks

win13 <- owin(c(20,120),c(20,120))
d13 <- dat2 %>% 
  filter(plot==13) %>% 
  select(X,Y,ID,plot)
d13pp <- as.ppp(d13,owin(win13))
ext13 <- d13pp$marks

win14 <- owin(poly=list(x=c(20,230,230,205,205,20),y=c(20,20,30,30,80,80)))
d14 <- dat2 %>% 
  filter(plot==14) %>% 
  select(X,Y,ID,plot)
d14pp <- as.ppp(d14,owin(win14))
ext14 <- d14pp$marks

win91 <- owin(c(20,50),c(20,55))
d91 <- dat2 %>% 
  filter(plot==91) %>% 
  select(X,Y,ID,plot)
d91pp <- as.ppp(d91,owin(win91))
ext91 <- d91pp$marks

win93 <- owin(poly=list(x=c(20, 165, 165, 180, 180, 20),y=c(20, 20, 40, 40, 80, 80)))
d93 <- dat2 %>% 
  filter(plot==93) %>% 
  select(X,Y,ID,plot)
d93pp <- as.ppp(d93,owin(win93))
ext93 <- d93pp$marks

win97 <- owin(c(20,236),c(20,236))
d97 <- dat2 %>% 
  filter(plot==97) %>% 
  select(X,Y,ID,plot)
d97pp <- as.ppp(d97,owin(win97))
ext97 <- d97pp$marks

correct <- rbind(ext12,ext13,ext14,ext91,ext93,ext97)
dat3 <- merge(dat2, correct, by=c("ID","plot"), all.y=TRUE, all.x=FALSE)



# ADD PROXY FOR LIGHT AVAILABILITY: "GAPPINESS"
# EXTRACTED FROM SPATIAL LOCATIONS OF LARGE STEM DEATHS DURING SURVEY PERIOD, use spatstat

win12 <- owin(c(0,170),c(0,120))
win13 <- owin(c(0,140),c(0,140))
win14 <- owin(poly=list(x=c(0,250,250,225,225,0),y=c(0,0,50,50,100,100)))
win91 <- owin(c(0,70),c(0,75))
win93 <- owin(poly=list(x=c(0, 185, 185, 200, 200, 0),y=c(0, 0, 40, 40, 100, 100)))
win97 <- owin(c(0,256),c(0,256))

# all seedling/sapling stems are <12.7cm, so for taller tree deaths, can use those stems
# that are larger than 12.7cm DBH
death <- dat %>% filter(!is.na(diam1) & diam1>12.7) %>% 
  filter(surv==0) %>% 
  select(plot, X, Y, diam1)

# separate by plot
death12 <- death[death$plot==12, c("X","Y","diam1")]
death13 <- death[death$plot==13, c("X","Y","diam1")]
death14 <- death[death$plot==14, c("X","Y","diam1")]
death91 <- death[death$plot==91, c("X","Y","diam1")]
death93 <- death[death$plot==93, c("X","Y","diam1")]
death97 <- death[death$plot==97, c("X","Y","diam1")]

# smooth kernels
death12pp <- as.ppp(death12,win12)
death12fun <- Smoothfun(death12pp,sigma=10,edge=TRUE)

death13pp <- as.ppp(death13,win13)
death13fun <- Smoothfun(death13pp,sigma=10,edge=TRUE)

death14pp <- as.ppp(death14,win14)
death14fun <- Smoothfun(death14pp,sigma=10,edge=TRUE)

death91pp <- as.ppp(death91,win91)
death91fun <- Smoothfun(death91pp,sigma=10,edge=TRUE)

death93pp <- as.ppp(death93,win93)
death93fun <- Smoothfun(death93pp,sigma=10,edge=TRUE)

death97pp <- as.ppp(death97,win97,sigma=10,edge=TRUE)
death97fun <- Smoothfun(death97pp,sigma=10,edge=TRUE)


# create df for each plot, which will be used to extract interpolated 'gappiness' measure
gap12 <- dat3[dat3$plot==12,]
gap13 <- dat3[dat3$plot==13,]
gap14 <- dat3[dat3$plot==14,]
gap91 <- dat3[dat3$plot==91,]
gap93 <- dat3[dat3$plot==93,]
gap97 <- dat3[dat3$plot==97,]

# gives you gappiness measure at each x, y point in df
# duplicated points OK
g12 <- death12fun(gap12$X, gap12$Y)
g13 <- death13fun(gap13$X, gap13$Y)
g14 <- death14fun(gap14$X, gap14$Y)
g91 <- death91fun(gap91$X, gap91$Y)
g93 <- death93fun(gap93$X, gap93$Y)
g97 <- death97fun(gap97$X, gap97$Y)

# add gappiness column to df
gap12 <- cbind(gap12, g12)
colnames(gap12)[23] <- "gap"

gap13 <- cbind(gap13, g13)
colnames(gap13)[23] <- "gap"

gap14 <- cbind(gap14, g14)
colnames(gap14)[23] <- "gap"

gap91 <- cbind(gap91, g91)
colnames(gap91)[23] <- "gap"

gap93 <- cbind(gap93, g93)
colnames(gap93)[23] <- "gap"

gap97 <- cbind(gap97, g97)
colnames(gap97)[23] <- "gap"


# bind dfs together
dat4 <- rbind(gap12, gap13, gap14, gap91, gap93, gap97)

# there are a few seed/sap that don't have neighboring seed/saps; 
# convert those NAs to 0s
dat4$n_con[is.na(dat4$n_con)] <- 0
dat4$n_het[is.na(dat4$n_het)] <- 0


# ADD SHADE TOLERANCE AND MYCORRHIZAL TYPE BY SPECIES
###########################################

# myc type from variety of resources
ecm <- c('CACA', 'CACR', 'CAGL', 'CAOV', 'CATO', 'FAGR', 'QUAL', 'QUMI', 'QURU', 'QUVE')
am <- c('ACRU', 'CECA','CELA', 'CHVI','COFL', 'FRAX', 'ILDE', 'JUVI', 'LIST', 'LITU', 'MAGR', 
        'MORU', 'PRSE', 'ULAL', 'ULAM', 'ULRU')
eric <- c('OXAR')

dat4 <- dat4 %>% mutate(myc = ifelse(species %in% ecm, 'ecm', 
                                     ifelse(species %in% am, 'am', 
                                            ifelse(species %in% eric, 'eric',
                                                   'other'))))

dat4$species <- as.character(dat4$species)

# shade tolerance from: Niinemets & Valladares 2006 TOlerance to shade, drought, and waterlogging of temperate northern hemisphere trees and shrubs
shade_dat <- read.csv("shade_tolerance.csv", stringsAsFactors = FALSE, strip.white = TRUE)

shade_dat <- shade_dat %>% 
  mutate(species=recode(species, "Carya tomentosa" = "Carya alba"),
         species=recode(species, "Celtis laevigata" = "Celtis laevigata var. laevigata"),
         species=recode(species, "Cercis canadensis" = "Cercis canadensis var. canadensis"),
         species=recode(species, "Ilex opaca" = "Ilex opaca var. opaca"),
         species=recode(species, "Morus rubra" = "Morus rubra var. rubra"), 
         species=recode(species, "Ostrya virginiana" = "Ostrya virginiana var. virginiana"),
         species=recode(species, "Quercus prinus" = "Quercus michauxii"))

# CAN LUMP CAOL (CARYA OVALIS) WITH CAGL (only two CAOL stems, though)

# download the plot data that contains full species names; then you can 
# merge shade tolerances with main dataframe
s12 <- read.csv("MFP_12.csv", stringsAsFactors = FALSE)
s13 <- read.csv("MFP_13.csv", stringsAsFactors = FALSE)
s14 <- read.csv("MFP_14.csv", stringsAsFactors = FALSE)
s93 <- read.csv("MFP_93.csv", stringsAsFactors = FALSE)
s91 <- read.csv("MFP_91.csv", stringsAsFactors = FALSE)
s97 <- read.csv("MFP_97.csv", stringsAsFactors = FALSE)
species_names <- rbind(s12,s13,s14,s93,s91,s97)

species_names <- species_names %>% 
  select(species = SpeciesCode, sciname = ScientificName) %>% 
  distinct()

shade <- merge(species_names, shade_dat, by.x="sciname", by.y="species", 
                       all.x = TRUE, all.y = FALSE)

# Missing shade tolerance for CACA (Carya carolinae-septentrionalis); add this
# manually - should be v. similar to shade tolerance of Carya ovata (CAOV) == 3.40
# (Robert Peet, pers communication)

# Missing shade tol for FRAX (Fraxinus spp.); add this manually - take mean shade
# tol of F. americana and F. pennsylvanicum == 3.5 + 2.46 / 2 == 2.98
# (Robert Peet, pers communication)

shade$shade <- ifelse(shade$species=="CACA", 3.40, 
                      ifelse(shade$species=="FRAX", 2.98, shade$shade))

shade <- shade %>% select(-sciname)

# add shade tolerance data to main dataframe
dat5 <- merge(dat4, shade, by="species", all.x = TRUE, all.y = FALSE)



# CREATE SPECIES LIST of those species that you want to include in analysis
# you'll want trees (not shrubs or tree-shrubs), and species w/ >= 20 obs
###########################################
specieslist <- dat5 %>% 
  group_by(species) %>% 
  summarise(diameter=mean(diam1,na.rm=TRUE), count=n())


# categorize species into growth habit based on USDA Plants
# remove inappropriate species (invasive spp., vines, shrubs)
remove <- c('LISI','CARY','PRSP','EUAM','UNKN','COAM')
shrub.trees <- c('AESY','AMAR','ASTR','CACR','CECA','CELA','CEOC','CHVI','COFL','HAVI','ILAM','ILDE','ILOP','LIBE','OSVI','OXAR','PRAM','PRSE','QUSH','SAAL','STGR','VIPR','VIRU')
trees <- c('ACBA','ACRU','CACA','CAGL','CAOL','CAOV','CATO','DIVI','FAGR','FRAX','JUNI','JUVI','LIST','LITU','MAGR','MORU','NYSY','PIEC','PITA','QUAL','QUCO','QUFA','QUFP','QULY','QUMI','QUPH','QURU','QUST','QUVE','ULAL','ULAM','ULRU')

specieslist <- specieslist %>% filter(!species %in% remove)
specieslist$habit <- ifelse(specieslist$species %in% shrub.trees,"shrub-tree",
                            ifelse(specieslist$species %in% trees,"tree","oops"))

# add habit info to main dataframe
dat5 <- merge(dat5, specieslist, by="species", all.x = TRUE)


