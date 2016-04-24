"
  Prompt:          How can data stop accidents before they happen?
  Link to Project: http://bayeshack.org/transportation.html

  Link(s) to data: https://github.com/bayesimpact/bayeshack-transportation-hazmat

  Purpose: this file finds unique cities in each data set along with their frequency
           to reverse look up their latitude and longitude using Google maps and
           MapQuest API's. 

           Finding unique locations and their frequency was important to ensure that
           the latitude and longitude for each location was retrieved once to ensure
           efficiency of data lookup and avoid reaching quota's from using each sites'
           services.

           The refined/ simplified data was outputed in a csv file so that an accompanying 
           python script retrieved it and looked up the coordinates. 

           We limited the locations to cities due to quotas of services and the widespread
           frequency of cities in datasets over routes
"


library(plyr)

############DATA FOR ACCIDENTS#########################################
hazmat  <- read.csv("hazmat_incidents.csv")
freight <- read.csv("freight_data.csv")
citycodes <- read.csv("city_codes.csv")

incidentData <- read.csv("freight_incidentsummary_2012.csv")
#######################################################################

############DATA FOR STATES############################################
stateNames          <- read.csv("states.txt", header = FALSE, sep="\n")
stateAbbreviations  <- read.csv("abbrev.txt", header = FALSE, sep="\n")

stateNames          <- toupper(stateNames$V1)
stateAbbreviations  <- toupper(stateAbbreviations$V1)

size <- max(length(stateNames), length(stateAbbreviations)) 

stateNames         <- c(stateNames, rep(NA, size - length(stateNames)))
stateAbbreviations <- c(stateAbbreviations, rep(NA, size - length(stateAbbreviations)))

STATES <- data.frame(stateNames, stateAbbreviations)
colnames(STATES) <- c("full", "abbrev")
#######################################################################

FreqOfData <- function(data){
  X <- data.frame(table(data))
  Y <- X[X$Freq != 0, ]
  
  return(Y)
}
AbbrevToFull <- function(data, REFERENCE){ 
  colnames(REFERENCE) <- c("full", "abbrev")
  
  X <- vector(mode="numeric", nrow(data))
  
  for(i in 1:nrow(data)){
    if(is.null(data$State[i]) | is.na(data$State[i])){
      print(paste("ERROR1", i))
      next
    } else{
      
      Data  <- as.character(data$State[i])
      REF   <- na.omit(STATES$abbrev)
      
      for(j in 1:length(REF)){
        if(is.null(REFERENCE$abbrev[j]) | is.na(REFERENCE$abbrev[j])){print(paste("ERROR2",j, i)); next }
        
        else{
          StateAbbrev <- as.character(REFERENCE$abbrev[j])
          if(Data == StateAbbrev){X[i] <- as.character(REFERENCE$full[j])} 
        }
      }
    }
  }
  
  X <- data.frame(data$City, X, data$Freq)
  colnames(X) <- c("City", "State", "Freq")
  return(X)
}
StateIntersect <- function(dataset1, dataset2){
  colnames(dataset1) <- c("City", "State", "Freq")
  colnames(dataset2) <- c("City", "State", "Freq")
  
  size <- min(nrow(dataset1), nrow(dataset2))
  
  X <- vector(mode="numeric", size)
  FREQ <- vector(mode="numeric", size)
  CITY <- vector(mode="numeric", size)
  
  
  for(i in 1:nrow(dataset1)){
    found <- TRUE
    if(is.null(dataset1$State[i]) | is.na(dataset1$State[i])){
      print(paste("ERROR1", i))
      next
    } else { 
      State1  <- as.character(dataset1$State[i])
      
      for(j in 1:nrow(dataset2)){
        if(is.null(dataset2$State[j]) | is.na(dataset2$State[j])){print(paste("ERROR2",j, i)); next }
        else{
          State2 <- as.character(dataset2$State[j])
          if(State1 == State2){
            found <- FALSE
            X[i] <- State1
            FREQ[i] <- as.numeric(dataset1$Freq[i])
            break
          } else {
            found <- TRUE
          } 
        }
      }
      if(found){print(State2)}
    }
  }
  
  X <- data.frame(X, FREQ)
  colnames(X) <- c("State", "Freq")
  return(X)
}
FilterStates <- function(dataset, states){
  if(length(states) <= 1){return(subset(dataset, Origin.City != states[1]))}
  else{
    dataset <- subset(dataset, Origin.City != states[1])
    states  <- states[2:length(states)]
    return(FilterStates(dataset, states))
  }
}
StateDisjoint <- function(dataset1, dataset2){
  colnames(dataset1) <- c("City", "State", "Freq")
  colnames(dataset2) <- c("City", "State", "Freq")
  
  setdiff(as.character(dataset2$State), as.character(dataset1$State))
}
GeoDataFramer <- function(data){
  address <- data$Address
  frequen <- data$Freq
  size <- length(data) #number of rows
  lat  <- rep(0, size) #latitude
  long <- rep(0, size) #longitude
  
  return(data.frame(address, lat, long, frequen))
}
CombineSubAddress <- function(data){  
  FullAddress <- paste(data$City, data$State, sep = ", ")
  
  X <- data.frame(FullAddress, data$Freq)
  colnames(X) <- c("Address", "Freq")
  return(X)
} 

citycodes <- citycodes[,c(1:3)]; colnames(citycodes) <- c("dms_orig", "FAF.Regions.Orig", "FAF.State.Orig")
destcitycodes <- citycodes;      colnames(destcitycodes) <- c("dms_dest", "FAF.Regions.Dest", "FAF.State.Dest")

pp <- join(freight, citycodes,     by="dms_orig" )
pp <- join(pp,      destcitycodes, by ="dms_dest")

"--------------------------------Incident Data for Hazmat-----------------------------------"

HazmatHighway <- subset(hazmat, Mode.of.Transportation == "Highway" & Total.Amount.of.Damages > 0 & Incident.State != "")

data <- HazmatHighway

#Get the Incident of the hazmat 
City      <- as.character(data$Incident.City)
State     <- as.character(data$Incident.State)
I_Addresses <- data.frame(City, State)

I_Summary <- FreqOfData(I_Addresses)
I_Summary <- CombineSubAddress(I_Summary)
I_Summary <- GeoDataFramer(I_Summary)

for(i in 1:120){ 
  FILE <- paste("HAZMAT/HazmatIncidents", i, ".csv", sep="")
  write.csv(head(I_Summary, n=100), file=FILE)
  I_Summary <- I_Summary[100:nrow(I_Summary),]
} 

"--------------------------------Origin Data for Freight-----------------------------------"

#Get the origin of the hazmat 
val <- data$Origin.City;  H_OriginCity    <- as.character(val)
val <- data$Origin.State; H_OriginState   <- as.character(val)
H_OriginAddress <- data.frame(H_OriginCity, H_OriginState)

H_Summary <- FreqOfData(H_OriginAddress)

#get the origin of the freight 
data <- pp
F_OriginCity    <- as.character(data$FAF.Regions.Orig)
F_OriginState   <- as.character(data$FAF.State.Orig)
F_OriginAddress <- data.frame(F_OriginCity, F_OriginState)

F_Summary <- FreqOfData(F_OriginAddress)

##########FIND INTERSECTION & DISJOINT BEWTEEN STATES####################
colnames(H_Summary) <- c("City", "State", "Freq")
colnames(F_Summary) <- c("City", "State", "Freq")

F_Summary <- AbbrevToFull(F_Summary, STATES)

IntersectStates <- StateIntersect(F_Summary, H_Summary)
DisjointStates  <- StateDisjoint(F_Summary, H_Summary)

newHighway <- FilterStates(HazmatHighway, DisjointStates)

##########Find Unique Cities for Freight####################
H_CITY   <- FreqOfData(H_OriginAddress)
colnames(H_CITY) <- c("City", "State", "Freq")

F_CITY   <- FreqOfData(F_OriginAddress)
colnames(F_CITY) <- c("City", "State", "Freq")

remove <- grep("Remainder of State", F_CITY$City)
F_CITY <- F_CITY[-remove, ]


F_CITY <- AbbrevToFull(F_CITY, STATES)
F_CITY <- CombineSubAddress(F_CITY)
colnames(F_CITY) <- c("Address", "Freq")

F_CITY <- GeoDataFramer(F_CITY)
write.csv(F_CITY, file="Freight.csv")

"--------------------------------Incident Data for Freight-----------------------------------"
#get the origin of the freight 
data <- incidentData

F_OriginCity     <- as.character(data$FAF.Origin.City)
F_OriginState    <- as.character(data$FAF.Origin.State)
F_OriginIncident <- data$incidents
F_OriginAddress <- na.omit(data.frame(F_OriginCity, F_OriginState, F_OriginIncident))

##########FREIGHT INCIDENT DATA####################
colnames(F_OriginAddress) <- c("City", "State", "Freq")

# remove <- grep("Remainder of State", F_CITY$City)
# F_CITY <- F_CITY[-remove, ]

F_CITY <- AbbrevToFull(F_OriginAddress, STATES)
F_CITY <- CombineSubAddress(F_CITY)
colnames(F_CITY) <- c("Address", "Freq")

F_CITY <- GeoDataFramer(F_CITY)
colnames(F_CITY) <- c("Address", "Latitude", "Longitude", "Incidents")
write.csv(F_CITY, file="Freight2012.csv")