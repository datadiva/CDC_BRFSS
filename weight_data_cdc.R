####BMI in the US from 1984-2011
# Behaviorial Risk Factor annual survey data:http://www.cdc.gov/brfss/annual_data/annual_data.htm
# BRFSS general information: http://www.cdc.gov/brfss/data_documentation/index.htm 


#libraries 
library(plyr)
library(ggplot2)
library(gmodels)
library(reshape2)
library(tidyr)


####read in datasets
all.orig <- read.csv(file="./AllData_1984_to_2011_all.csv", head=TRUE, sep=",")
state.codes <- read.csv(file="./state_mapping.csv", head=TRUE, sep=",")

all<-all.orig
colnames(all) <- tolower(colnames(all))


####Transform and clean variables
#Merge state.codes
all <- merge(all, state.codes, by="state")

#Recode missing data
all$year[all$year<=83] <- NA
all$age[all$age<10]<- NA


all$age.cat[all$age <35] <- "18-34"
all$age.cat[all$age >=35 & all$age<50] <- "35-49"
all$age.cat[all$age >=50 & all$age<65] <- "50-64"
all$age.cat[all$age >=65 & all$age<80] <- "65-79"
all$age.cat[all$age >=80] <- "80+"

all$weight[all$weight >=777 | all$weight <= 20] <- NA
all$yr <- ifelse(all$year <1900,
                    (all$year + 1900), all$year) 


all$height[all$height <300 | all$height >711 ] <- NA
all$height[all$height >=412 & all$height <500 ] <- NA
all$height[all$height >=512 & all$height <600 ] <- NA
all$height[all$height >=612 & all$height <700 ] <- NA

#Calculate height (currently height is recorded as 605 = 6'5")
all$ht.ft <- substr(as.character(all$height), 1, 1)
all$ht.in <- substr(as.character(all$height), 2, 3)
all$ht <- as.numeric(all$ht.ft)*12 + as.numeric(all$ht.in)

#Calculate bmi
all$bmi <- (all$weight/all$ht^2)*703
all$bmi <- round(all$bmi, 1)

#Remove bmi that are excessively out of range
all$bmi[all$bmi <10 | all$bmi >200] <- NA

hist(all$bmi)

all.clean <- na.omit(all)

keep <- c("state", "state.name", "yr", "age", "age.cat", "sex", "bmi")
all <- all.clean[keep]


#Data with BMI averaged by state, year, sex, and age.cat 
all.c <- ddply(all, .(state, yr, sex, age.cat), summarise, mean.bmi=round(mean(bmi),1))







