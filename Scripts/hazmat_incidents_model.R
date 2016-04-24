setwd("~/Documents/Sideprojects/bayeshack-transportation-hazmat/data")
library(randomForest)
library(plyr)
library(ggplot2)
library(reshape2)

## This function splits the dataset into training and test sets

splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}


## Read Hazardous Incident dataset
hazmat <- read.csv("hazmat_incidents.csv", na.strings="NA")

h_temp <- hazmat

## Subset only the features we are interested in
h_temp <- h_temp[, c("Quantity.Released", "Unit.of.Measure","Cont1.Package.Capacity", "Cont2.Package.Capacity","Hazardous.Class", "Total.Amount.of.Damages", "Mode.of.Transportation", "HMIS.Serious.Incident.Ind")]

h_temp <- subset(h_temp, Mode.of.Transportation != "FREIGHT FORWARDER")

colnames(h_temp) <- c("qty_released", "unit", "cont1_qty", "cont2_qty", "h.class", "damages", "mode", "incident")

## These units refer to gas, liquid and solid states of materials
h_temp <- subset(h_temp, unit == "GCF" | unit == "LGA" | unit == "SLB")

h_temp$total_qty <- as.numeric(h_temp$cont1_qty) + as.numeric(h_temp$cont2_qty)

liquid_hazmat <- subset(h_temp, unit == "LGA")
gas_hazmat <- subset(h_temp, unit == "GCF")
solid_hazmat <- subset(h_temp, unit == "SLB")

## Convert all the states into tons
## For liquid we are using the density of petroleum fuels to convert 1 gallon to tons
liquid_hazmat$qty_released = liquid_hazmat$qty_released * 0.0315 ## Converting gallons to tons
liquid_hazmat$total_qty = liquid_hazmat$total_qty * 0.0315 ## Converting gallons to tons

## For gas we are using the density of natural gas to convert 1 gallon to tons
gas_hazmat$qty_released = gas_hazmat$qty_released * 0.000259 ## Converting cubic feet of NG to tons
gas_hazmat$total_qty = gas_hazmat$total_qty * 0.000259 

## For solids, the conversion is from pounds to tons
solid_hazmat$qty_released = solid_hazmat$qty_released * 0.0005 ## converting pounds to tons
solid_hazmat$total_qty = solid_hazmat$total_qty * 0.0005

## Combine all the different hazard material states into one file
hz2 <- rbind(solid_hazmat, liquid_hazmat, gas_hazmat)

## Shuffle the dataset so that the hazard materials are distributed randomly between solid, liquid and gas
hz2 <- hz2[sample(nrow(hz2)), ]
hz2 <- hz2[sample(nrow(hz2)), ]

## Conver all values in the dataset to character from factor
hz2 <- data.frame(lapply(hz2, as.character), stringsAsFactors=FALSE)

## Remove rows that have NA values from the dataset
hz2 <- hz2[complete.cases(hz2),]

## Calculating percentage leak
hz2$perc_leak <- as.numeric(hz2$qty_released)/as.numeric(hz2$total_qty)
hz2$total_worth <- (1/hz2$perc_leak) * as.numeric(hz2$damages)

## Convert values to numeric values
hz2$total_worth = as.numeric(hz2$total_worth)
hz2$damages = as.numeric(hz2$damages)

## Code incident as 0 or 1 
## O means no incident
hz2$incident[hz2$incident == 'No'] <- 0
hz2$incident[hz2$incident == 'Yes'] <- 1

## Replace mode of transportation with category values
## Categorized based on FAF database
hz2$mode[hz2$mode == 'Air'] <- 4
hz2$mode[hz2$mode == 'Highway'] <- 1
hz2$mode[hz2$mode == 'OTHER'] <- 7
hz2$mode[hz2$mode == 'Rail'] <- 2
hz2$mode[hz2$mode == 'Water'] <- 3


## Replace hazard class with category values
## The categories are Explosives: 1, Flammable: 2, Poisonous: 3, Radioactive: 4, and Misc: 5
hz2$h.class[hz2$h.class == 'COMBUSTIBLE LIQUID'] <- 2
hz2$h.class[hz2$h.class == 'CORROSIVE MATERIAL'] <- 2
hz2$h.class[hz2$h.class == 'DANGEROUS WHEN WET MATERIAL'] <- 2
hz2$h.class[hz2$h.class == 'EXPLOSIVE FIRE HAZARD'] <- 1
hz2$h.class[hz2$h.class == 'EXPLOSIVE MASS EXPLOSION HAZARD'] <- 1
hz2$h.class[hz2$h.class == 'EXPLOSIVE NO BLAST HAZARD'] <- 1
hz2$h.class[hz2$h.class == 'EXPLOSIVE PROJECTION HAZARD'] <- 1
hz2$h.class[hz2$h.class == 'EXPLOSIVES  CLASS A'] <- 1
hz2$h.class[hz2$h.class == 'EXPLOSIVES  CLASS B'] <- 1
hz2$h.class[hz2$h.class == 'EXPLOSIVES  CLASS C'] <- 1
hz2$h.class[hz2$h.class == 'FLAMMABLE - COMBUSTIBLE LIQUID'] <- 2
hz2$h.class[hz2$h.class == 'FLAMMABLE GAS'] <- 2
hz2$h.class[hz2$h.class == 'FLAMMABLE SOLID'] <- 2
hz2$h.class[hz2$h.class == 'FLAMMABLE SOLID (PRE 1991)'] <- 2
hz2$h.class[hz2$h.class == 'INFECTIOUS SUBSTANCE (ETIOLOGIC)'] <- 3
hz2$h.class[hz2$h.class == 'IRRITATING MATERIAL'] <- 3
hz2$h.class[hz2$h.class == 'MISCELLANEOUS HAZARDOUS MATERIAL'] <- 5
hz2$h.class[hz2$h.class == 'NONFLAMMABLE COMPRESSED GAS'] <- 5
hz2$h.class[hz2$h.class == 'ORGANIC PEROXIDE'] <- 1
hz2$h.class[hz2$h.class == 'OTHER REGULATED MATERIAL CLASS D'] <- 5
hz2$h.class[hz2$h.class == 'OXIDIZER'] <- 2
hz2$h.class[hz2$h.class == 'POISONOUS GAS'] <- 3
hz2$h.class[hz2$h.class == 'POISONOUS MATERIALS'] <- 3
hz2$h.class[hz2$h.class == 'RADIOACTIVE MATERIAL'] <- 4
hz2$h.class[hz2$h.class == 'SPONTANEOUSLY COMBUSTIBLE'] <- 2
hz2$h.class[hz2$h.class == 'VERY INSENSITIVE EXPLOSIVE'] <- 1

## Remove any hazardous class that does not have a description 
hz2 <- subset(hz2, h.class != "")

## Subset features to do the model
hz3 <- hz2[,c(5,7,8,9,11)]

## Convert categories to factors, and the rest of the values to numeric
hz3[,1] <- as.factor(hz3[,1])
hz3[,2] <- as.factor(hz3[,2])
hz3[,3] <- as.factor(hz3[,3])
hz3[,4] <- as.numeric(hz3[,4])
hz3[,5] <- as.numeric(hz3[,5])

## Remove any Nan or Inf values from the numeric columns
hz3[,4][which(is.nan(hz3[,4]))] = NA
hz3[,4][which(hz3[,4]==Inf)] = NA

hz3[,5][which(is.nan(hz3[,5]))] = NA
hz3[,5][which(hz3[,5]==Inf)] = NA

## Remove any missing row from the dataset
hz3 <- hz3[complete.cases(hz3),]

## Split the dataset into test set and training set
splits <- splitdf(hz3, seed=808)
training <- splits$trainset
testing <- splits$testset

## Use the training set to fit incident data on the features using Random Forest algorithm
fit <- randomForest(incident~., data=training, importance=TRUE, ntree=1000)

summary(fit)

## Use the test data to predict the incidents
testing$pred <- predict(fit, testing[,-3])

## Compare the incidents in test data with the predictions to obtain accuracy
accuracy <- sum(testing$incident == testing$pred)/nrow(testing)


## Read FAF dataset
freight_final <- read.csv("freight_citynames.csv")

## Convert the commodity value from million dollars to USD
freight_final$value_2012 <- freight_final$value_2012 *1000000 ## convert from M$ to $

#freight_final[,9] <- as.factor(freight_final[,9])


## subset the dataset for the modes present in the hazard incident dataset
## 1: Truck, 2: Rail, 3: Water, 4: Air, 5: Other
f2 <- subset(freight_final, dms_mode == 1 | dms_mode == 2 | dms_mode == 3 |
               dms_mode == 4 | dms_mode == 7)

## subset the dataset for different commodity types
## For the detailed set of commodity codes vs. descriptions, see: 
## http://www.ops.fhwa.dot.gov/freight/freight_analysis/faf/faf3/userguide/index.htm
f2 <- subset(f2, sctg2 == 15 | sctg2 == 16 | sctg2 == 17 |
                          sctg2 == 18 | sctg2 == 20 | sctg2 == 23 | sctg2 == 14 | 
                          sctg2 == 21 | sctg2 == 22 | sctg2 == 24 | sctg2 == 35 | sctg2 == 41 | sctg2 == 99)


## Classify the commodity types into the 5 categories described above
f2$sctg2[f2$sctg2 == 23] <- 1
f2$sctg2[f2$sctg2 == 20] <- 1

f2$sctg2[f2$sctg2 == 15] <- 2
f2$sctg2[f2$sctg2 == 16] <- 2
f2$sctg2[f2$sctg2 == 17] <- 2
f2$sctg2[f2$sctg2 == 18] <- 2


f2$sctg2[f2$sctg2 == 14] <- 3
f2$sctg2[f2$sctg2 == 21] <- 3
f2$sctg2[f2$sctg2 == 22] <- 3
f2$sctg2[f2$sctg2 == 24] <- 3

f2$sctg2[f2$sctg2 == 35] <- 4

f2$sctg2[f2$sctg2 == 41] <- 5
f2$sctg2[f2$sctg2 == 99] <- 5

## Trade_type ==1 reflects the domestic freight movement
f2 <- subset(f2, trade_type == 1)

## subset the data to relevant features
f3 <- f2[,c(2,3,9,11,13,14,15,16)]

## Rearrange the data 
f3 <- f3[,c(4,3,1,2,5,6,7,8)]

colnames(f3) <- c("h.class", "mode", "total_qty", "total_worth", "FAF.Origin.City", "FAF.Origin.State", "FAF.Dest.City", "FAF.Dest.State")

## change categories to factors
f3[,1] <- as.factor(f3[,1])
f3[,2] <- as.factor(f3[,2])

## Use the following code only when RandomForest gives an error to fit the model
#levels(f3[,1]) <- levels(training[,1])
#levels(f3[,2]) <- levels(training[,2])

## predict the incidents for the FAF dataset
f3$incident <- predict(fit, f3[,-c(5,6,7,8)])

## Write the predicted dataset into a CSV file
write.csv(f3, "freight_2012.csv")

