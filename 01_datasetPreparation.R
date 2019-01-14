## Script dataset preparation for 
## "The behavioral phenotype of early life adversity: a 3-level meta-analysis of preclinical studies"

## Author: Valeria Bonapersona
## Supervision: Caspar J van Lissa

## for questions, contact valeria @ v.bonapersona-2@umcutrecht.nl


# Environment preparation -------------------------------------------------

rm(list = ls()) #clean environment

#Libraries
library(metafor) 
library(dplyr) 
library(ggplot2) 


#import full dataset 
data <- read.csv("MAB_Data_osf.csv", sep = ";") 

# Select relevant parts of dataset ----------------------------------------
## prepare dataset
data$exp <- as.factor(paste0(data$id, data$nest)) #variable to identify experiments

#@ keep only included studies, and a selection of variables
data %>%
  filter(select == "1",
         is.na(exclude),
         dataFrom %in% c("email", "ruler", "paper")) %>%
  select(exp, id, author, year, journal, 
         
         species, strainGrouped, origin, sex, ageWeek, #animals
         
         model, mTimeStart, mTimeEnd, mHoursAve, #models
         mCageGrouped, mLDGrouped, mRepetition, mControlGrouped,
         
         hit2Grouped, #theories
         
         testAuthorGrouped, testLDGrouped, varAuthorGrouped, #tests
         waterT, waterTcate, freezingType, retentionGrouped, 
         
         directionGrouped, #qualitative
         
         effectSizeCorrection, cut_nC, n_notRetriev, #stats info
         nC, meanC, sdC, seC, 
         nE, meanE, sdE, seE,
         
         dataFrom, #data
         
         seqGeneration, baseline, allocation, housing, blindExp, 
         control, outAss, outBlind, incData) %>%
  droplevels() -> data


# Missing values ----------------------------------------------------------
#Age: missing values sustituted by median
data$ageWeekNum <- data$ageWeek
data[data$ageWeekNum == "unclear",]$ageWeekNum <- NA
data$ageWeekNum <- as.numeric(as.character(data$ageWeekNum))
data[data$ageWeek == "unclear",]$ageWeek <- median(data$ageWeekNum, na.rm = TRUE)

#Water temperature: missing values substituted by median
data$waterTNum <- data$waterT
data[data$waterTNum %in% c("NS", "notApplicable"),]$waterTNum <- NA
data$waterTNum <- as.numeric(as.character(data$waterTNum))
data[data$waterT == "NS",]$waterTNum <- median(data$waterTNum, na.rm = TRUE)
data[data$waterT == "NS",]$waterT <- median(data$waterTNum, na.rm = TRUE)


#categorical missing: substitute with most common category 
categorical_mode <- function(x){
  names(table(x))[which.max(table(x))]
}

data[data$origin == "NS",]$origin <- categorical_mode(data$origin)
data[data$mCageGrouped == "NS",]$mCageGrouped <- categorical_mode(data$mCageGrouped)
data[data$mLDGrouped == "NS",]$mLDGrouped <- categorical_mode(data$mLDGrouped)
data[data$mControlGrouped %in% c("NS","high"),]$mControlGrouped <- "AFR" #combine LG control with AFR



# Code variables of interest ----------------------------------------------
data$each <- c(1:length(data$exp))

##other variables
data$mHoursAve <- data$mHoursAve * (data$mTimeEnd - data$mTimeStart)
data$mTimeLength <- data$mTimeEnd - data$mTimeStart #length of ELS model variable
data$speciesStrainGrouped <- as.factor(paste(data$species, data$strainGrouped, sep = "_"))

##identify studies who were both blinded and randomised
data$blindRand <- ifelse(data$blindExp == "Y" & data$seqGeneration == "Y", 1, 0)
data$blindRand <- as.factor(data$blindRand)

#correct model variable
data$model <- as.character(data$model)
data[data$mRepetition %in% c("O", "twice"),]$model <- "MD" #to distinguish between maternal separation and depravation
data$model <- as.factor(data$model)

##quantify amount of potential bias: 1 point for "N", .5 for "NS"
var <- c("seqGeneration", "baseline", "allocation", "housing","blindExp", "outAss", "control", "outBlind", "incData")
data$bias <- rowSums(data[, var] == "N") + .5*rowSums(data[, var] == "NS")


# Categorization domains --------------------------------------------------
#import dataset with categorization 
cate <- read.csv("MAB_BehaviourTests_Variables_osf.csv", sep = ";")

#Now I want to merge the information of the categorization file with the dataset
cate$tV <- as.factor(paste(cate$Test, cate$Variable, sep = "_"))
data$tV <- as.factor(paste(data$testAuthorGrouped,
                           data$varAuthorGrouped, 
                           sep = "_"))

setdiff(data$tV, cate$tV) #check all variables have a categorization

#match information from categorization dataset to data
data <- data.frame(data, cate[match(data$tV, cate$tV), c("anxiety", "sLearning", "nsLearning", 
                                                         "social", "direcHyp", "noMeta")])
#rename "direcHyp" to "multiply"
names(data)[match("direcHyp", names(data))] <- "multiply"

## Manual fixes to categorization
#MWM categorization according to water temperature
data$sLearning <- ifelse(data$waterT != "notApplicable" & data$waterTNum >= 24 , 0, data$sLearning) #water warmer than 24C is not sLearning
data$nsLearning <- ifelse(data$waterT != "notApplicable" & data$waterTNum > 26, 1, data$nsLearning) #warm water (26C) becomes nsLearning
data$noMeta <- ifelse(data$waterT != "notApplicable" & data$waterTNum >= 24 & 
                        data$waterTNum <= 26, 1, data$noMeta) #if water temperature between 24 and 26C, not included



#create a variable for all domains
domain_levels <- c("anxiety", "sLearning", "nsLearning", "social", "noMeta")
data$domain <- apply(data[, domain_levels], 1, function(x){domain_levels[which(x == 1)]})

#data$domain <- as.factor(data$domain)   
data$domain <- factor(data$domain, levels = c("anxiety", "sLearning", 
                                              "nsLearning", "social", "noMeta")) #order levels



# Corrections to statistical measurements -------------------------------------------------------------
#N correction
data$nC <- ifelse(!is.na(data$cut_nC), data$nC/2, data$nC) ##cut N.C in half if same control used by two experimental groups

##papers in which N not reported >> mean of other papers
data$nC[is.na(data$nC)] <- round(mean(data$nC, na.rm = TRUE))
data$nE[is.na(data$nE)] <- round(mean(data$nE, na.rm = TRUE))

#calculate SD for all comparisons
data$sdC <- ifelse(!is.na(data$effectSizeCorrection), data$seC/6, #correction for IQ range
                   ifelse(is.na(data$sdC), (data$seC * sqrt(data$nC)), data$sdC)) 

data$sdE <- ifelse(!is.na(data$effectSizeCorrection), data$seE/6, #correction for IQ range
                   ifelse(is.na(data$sdE), (data$seE * sqrt(data$nE)), data$sdE)) 

data$seC <- ifelse(!is.na(data$effectSizeCorrection), (data$sdC / sqrt(data$nC)), data$seC) 
data$seC <- ifelse(is.na(data$seC), (data$sdC / sqrt(data$nC)), data$seC) 
data$seE <- ifelse(!is.na(data$effectSizeCorrection), (data$sdE / sqrt(data$nE)), data$seE) 
data$seE <- ifelse(is.na(data$seE), (data$sdE / sqrt(data$nE)), data$seE) 


##correction for qualitative interpretation direction (for systematic review graphs)
data$directionQual <- (as.numeric(factor(data$directionGrouped, 
                                         levels = c("decrease", "ns", "increase"))) - 2) #convert direction reported by studies to numeric

data$directionQual <- data$directionQual * data$multiply #correct direction of effects reported by studies according to categorization rules

data$directionGrouped <- ifelse(is.na(data$directionQual), "notRetrievable",
                                ifelse(data$directionQual == -1, "decrease",
                                       ifelse(data$directionQual == 1, "increase", "ns"))) #convert numeric to interpretation

#convert decrease with increase and viceversa for nsLearning and social
data$directionGrouped <- ifelse(data$domain %in% c("nsLearning", "social") & data$directionGrouped == "decrease", 
                                "increase",
                                ifelse(data$domain %in% c("nsLearning", "social") & data$directionGrouped == "increase", 
                                       "decrease", data$directionGrouped))

data$directionGrouped <- as.factor(data$directionGrouped)


# Calculation effect size and checks --------------------------------------
##calculate effect size
data <- escalc(m1i = meanE, sd1i = sdE, n1i = nE, 
               m2i = meanC, sd2i = sdC, n2i = nC, 
               measure = "SMD", method = "HE",
               data = data)

#dat$yi <- ifelse(data$each %% 2 == 0, dat$yi * -1, dat$yi) ##for blinding

data$yi <- data$yi * data$multiply #give all effect sizes the correct direction



# Save resulting dataset --------------------------------------------------

data <- data %>% droplevels() #drop missing levels

save(data, file = "data.RData") #save


dataRepo <- data %>% 
  select(ageWeekNum, waterTNum, tV, speciesStrainGrouped) %>% #variables removed because redundant
  droplevels()
  
#write.csv(dataRepo, file = "MAB_Data&Explanation.csv") ##file uploaded to repository

