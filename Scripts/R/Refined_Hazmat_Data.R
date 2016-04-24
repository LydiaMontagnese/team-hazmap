"
  Prompt:          How can data stop accidents before they happen?
  Link to Project: http://bayeshack.org/transportation.html

  Link(s) to data: https://github.com/bayesimpact/bayeshack-transportation-hazmat

  Purpose: this file subsets data to groups on the scale of having routes to cities to highways and to states   
"

#library("ggmap", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")   #do not use (max 2500 addresses/day)
#library("RDSTK", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")   #street2coordinates() is not working

hazmat  <- read.csv("hazmat_incidents.csv")

#subset data to just addresses
Route <- as.character(hazmat$Incident.Route) 
City  <- as.character(hazmat$Incident.City)
State <- as.character(hazmat$Incident.State)
addresses   <- data.frame(Route, City, State)

#Subset the data into Streets, Cities, State and HighWays
StreetAddress <- subset(addresses, (Route != "" & Route != " " & Route != "UNKNOWN") & (City != "UNKNOWN"))
CityAddress   <- subset(addresses, (Route == "" | Route == " " | Route == "UNKNOWN") & (City != "" & City != " " & City != "UNKNOWN"))
StateAddress  <- subset(addresses,  Route == ""  & (City == ""  | City == " "   | City == "UNKNOWN") & State != "")
Highways      <- subset(addresses,  Route != ""  & (City == ""  | City == " "   | City == "UNKNOWN"))

#How Much Data was retained? (I found missing 162 values from original dataset mostly due to no information about location)
DataPercentRetention <- (nrow(StreetAddress) + nrow(CityAddress) + nrow(StateAddress) + nrow(Highways))/ nrow(addresses)

#Concatenate Street, City & State Addresses
CombineSubAddress <- function(data){ 

  if(data$City[1]  == "UNKNOWN" & data$Route[1] != "") {FullAddress <- paste(data$Route, data$State, sep = ", ")}
  else if(data$City[1]  == "UNKNOWN") {FullAddress <- paste(data$State)}
  else if(data$Route[1] == "")        {FullAddress <- paste(data$City, data$State, sep = ", ")}
  else                                {FullAddress <-paste(data$Route, data$City,  data$State, sep = ", ")}

  return(FullAddress)
}

STREETAddress <- CombineSubAddress(StreetAddress) 
CITYAddress   <- CombineSubAddress(CityAddress)
STATEAddress  <- CombineSubAddress(StateAddress) 
HIGHWAYS      <- CombineSubAddress(Highways)

DataFramer <- function(data){
  address <- data
  size <- length(data) #number of rows
  lat  <- rep(0, size) #latitude
  long <- rep(0, size) #longitude
  
  return(data.frame(address, lat, long))
}

route   <- DataFramer(STREETAddress)
city    <- DataFramer(CITYAddress)
state   <- DataFramer(STATEAddress)
highway <- DataFramer(HIGHWAYS)

locations <- rbind(route, city, state, highway)

write.csv(locations, file="test.txt")