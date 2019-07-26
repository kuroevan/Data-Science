#=====================================================================================
# NYTimes Click Through Rate
# Author: Evangelos Giakoumakis
#=====================================================================================

library(ggplot2)

# set the data pth to fileLocation "http://stat.columbia.edu/~rachel/datasets/nyt1.csv"
fileLocation <- "http://stat.columbia.edu/~rachel/datasets/nyt1.csv"

# place data in local set

nyt_my <- read.csv(url(fileLocation))

# Get the variable names
names(nyt_my) 

## get value of data.frame
str(nyt_my)

head(nyt_my)

# Create a new variable ageGroup that categorizes age into following groups: 
#     < 18, 18–24, 25–34, 35–44, 45–54, 55–64 and 65+.
nyt_my$AgeGroup <- cut(nyt_my$Age, c(-Inf, 18, 24, 34, 44, 54, 64, Inf))

levels(nyt_my$AgeGroup) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
head(nyt_my)
dim(nyt_my)

Imp1<-subset(nyt_my, Impressions == 0)
dim(Imp1)

# Use subset of data where Impressions > 0 in your data set.
Imp2<-subset(nyt_my, Impressions > 0)
dim(Imp2)

# Create a new variable called click-through-rate (CTR = click/impression)
Imp2$CTR <- Imp2$Clicks/Imp2$Impressions

head(Imp2$CTR)
Imp2$CTRGroup <- cut(Imp2$CTR, c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf))

# Define a new variable to segment users based on click -through-rate (CTR) behavior.
# 0<=CTR< 0.2, 0.2<=CTR <0.4, 0.4<= CTR<0.6, 0.6<=CTR<0.8, CTR>0.8
levels(Imp2$CTRGroup) <- c("0<=CTR< 0.2", "0.2<=CTR <0.4", "0.4<= CTR<0.6", "0.6<=CTR<0.8", "CTR>0.8")
head(Imp2)

# Impressions density plot for nyt_my
#ggplot(nyt_my,aes(x=Impressions, colour=AgeGroup))+geom_density()

# Plot distributions of number impressions and click-through-rate (CTR = click/impression) 
#       for the age groups, that is, x: Impression, fill: AgeGroup
ggplot(nyt_my, aes(x=nyt_my$Impressions, fill=AgeGroup)) +  geom_histogram(binwidth=1)

#CTR density plot for Clicks>0, group by day
ggplot(Imp2,aes(x=CTR, colour=AgeGroup))+geom_density()


# get the total number of Gender, Impressions, Clicks and Signed_In
sapply(Imp2[,2:5] ,sum) 

# get the mean of Age, Gender, Impressions, Clicks, CTR
clms <- c(1,2,3,4,7)
sapply(Imp2[,clms] ,mean)

#get the means by AgeGroup
#summaryBy(Gender+Signed_In+Impressions+Clicks+CTR~AgeGroup, data=Imp2)

# Create table of the CTRGroup vs AgeGroup
table(Imp2$CTRGroup, Imp2$AgeGroup)

# Plot CTR density for Clicks>0, group by colour=AgeGroup
ggplot(subset(Imp2, Clicks>0),aes(x=CTR, colour=AgeGroup))+geom_density()
