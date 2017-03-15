###    Setup workspace
#Identify user
username <- Sys.getenv("USERNAME")

#Create file path to Git repository
dir <- paste("C:/Users/", username, "/Documents/GitHub/Quality-Control---Fuel-Moisture", sep = "")

#Set working directory
setwd(dir)

##  Load Packages

##  Load Data
fuel = data.frame(read.csv("tbl_fuelMoisture.csv"))
sample_units = data.frame(read.csv("tbl_unit.csv"))
sample_episodes = data.frame(read.csv("tbl_unitSamplingEpisode.csv"))

## Remove unnecessary data from sample_units object
## This will make file easier to view
sample_units <- sample_units[,-c(5:12, 14:17, 21:24)]

##  Add Unit Name
unitID <- merge(sample_episodes[,1:2], sample_units[,1:3], by = "unitID")
unitID <- unitID[,-3]
unitID <- unitID[,-1]
fuel <- merge(fuel , unitID , by = "unitSamplingEpisodeID")


###Logical Boundaries
OutL1 <- fuel[fuel$grossWetWt_g - fuel$wetContWt_g < fuel$grossDryWt_g - fuel$dryContWt_g,]
OutL1 <- OutL1[!is.na(OutL1$fuelMoistureID),]
OutL2 <- fuel[fuel$grossWetWt_g < fuel$wetContWt_g,]
OutL2 <- OutL2[!is.na(OutL2$fuelMoistureID),]
OutL3 <- fuel[fuel$grossDryWt_g < fuel$dryContWt_g,]
OutL3 <- OutL3[!is.na(OutL3$fuelMoistureID),]


##  Add Error Message and Output to OutLog
if(nrow(OutL1) > 0) {OutL1$Error <- "Logical Bound (NetDryWt > NetWetWt)"; OutLog <- rbind(OutL1)}
if(nrow(OutL2) > 0) {OutL2$Error <- "Logical Bound (DryContWt > GrossDryWt)"; OutLog <- rbind(OutLog, OutL2)}
if(nrow(OutL3) > 0) {OutL3$Error <- "Logical Bound (WetContWt > GrossWetWet)"; OutLog <- rbind(OutLog, OutL3)}



