###    Setup workspace
#Identify user
username <- Sys.getenv("USERNAME")

#Create file path to Git repository
dir <- paste("C:/Users/", username, "/Documents/GitHub/Quality-Control---Fuel-Moisture", sep = "")

#Set working directory
setwd(dir)

##  Load Packages
library(dplyr)

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

##Add net weights and fuel moisture content
fuel$NetWet <- fuel$grossWetWt_g - fuel$wetContWt_g
fuel$NetDry <- fuel$grossDryWt_g - fuel$dryContWt_g
fuel$MoistureWt <- fuel$NetWet - fuel$NetDry
fuel$FMpercent <- (fuel$MoistureWt / fuel$NetDry) * 100

###Boxplots
types <- as.data.frame(table(fuel$sampleMaterialID))
colnames(types) <-c("sampleMaterialID", "Freq")
boxplot(FMpercent ~ sampleMaterialID, data = fuel, xlab = "sampleMaterialID", ylab = "Fuel moisture (%)", main = "Fuel moisture percent by sample type", plot = FALSE)

###Fuel Moisture by catagory
mean.byID <- summarize(group_by(fuel, sampleMaterialID), median(FMpercent, na.rm = TRUE))
counts <- as.data.frame(table(fuel$sampleMaterialID))

FM.byID <- cbind(counts, mean.byID[,2])
colnames(FM.byID) <- c("sampleMaterialID", "n", "Mean fuel moisture (%)")

###Logical Boundaries
OutL1 <- fuel[fuel$NetWet < fuel$NetDry,]
OutL1 <- OutL1[!is.na(OutL1$fuelMoistureID),]
OutL2 <- fuel[fuel$grossWetWt_g < fuel$wetContWt_g,]
OutL2 <- OutL2[!is.na(OutL2$fuelMoistureID),]
OutL3 <- fuel[fuel$grossDryWt_g < fuel$dryContWt_g,]
OutL3 <- OutL3[!is.na(OutL3$fuelMoistureID),]
OutL4 <- fuel[fuel$FMpercent < 0,]
OutL4 <- OutL4[!is.na(OutL4$FMpercent),]


##  Add Error Message and Output to OutLog
if(nrow(OutL1) > 0) {OutL1$Error <- "Logical Bound (NetDryWt > NetWetWt)"; OutLog <- rbind(OutL1)}
if(nrow(OutL2) > 0) {OutL2$Error <- "Logical Bound (DryContWt > GrossDryWt)"; OutLog <- rbind(OutLog, OutL2)}
if(nrow(OutL3) > 0) {OutL3$Error <- "Logical Bound (WetContWt > GrossWetWet)"; OutLog <- rbind(OutLog, OutL3)}
if(nrow(OutL4) > 0) {OutL4$Error <- "Logical Bound (FuelMoisture < 0)"; OutLog <- rbind(OutLog, OutL4)}


###Regression
##Net Dry ~ New Wet
fuelNA <- fuel[!is.na(fuel$MoistureWt),]

mod1 <- lm(fuelNA$NetDry ~fuelNA$NetWet)
res1 <- rstandard(mod1)
OutR1 <-fuelNA
OutR1 <- OutR1[abs(res1) > 3, ]
res2 <- res1[abs(res1) > 3]

plot(fuelNA$NetWet, fuelNA$NetDry, xlab = "Net wet wt (g)", ylab = "Net dry wt (g)", main = "Net wet wt vs. Net dry wt")
abline(mod1)
r2 = summary(mod1)$adj.r.squared
p = summary(mod1)$coefficients[2,4]
rp = vector('expression', 2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2, dig = 3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(p, digits = 3))) [2]
legend('bottomright', legend = rp, bty = 0)

#Outliers with Standardized Residuals > |3|
plot(fuelNA$NetWet, res1, xlab = "Wet wt (g)", ylab = "Standardized Residuals", main = "Outliers with Standardized Residuals > |3|")
abline(3, 0)
abline(-3, 0)
points(OutR1$NetWet, res2, pch = 21, col = "red", bg = "red")

##Regression
mod1 <- lm(fuelNA$NetDry ~fuelNA$NetWet, subset = )

##  Add Error Message and Output to OutReg
if(nrow(OutR1) > 0) {OutR1$Error <- "Regression: NetDry~NetWet (StdRes > |3|)"; OutReg <- rbind(OutR1)}





###Compile and Export All Outliers to Overstory_Outliers.xls
FuelMoisture_Outliers <- data.frame()
if(nrow(OutLog) > 0) {FuelMoisture_Outliers <- rbind(OutLog)}
if(nrow(OutReg) > 0) {FuelMoisture_Outliers <- rbind(FuelMoisture_Outliers, OutReg)}


#Date and time stamp on output file
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
             tz = "", usetz = FALSE)

if(nrow(FuelMoisture_Outliers) > 0) {write.csv(FuelMoisture_Outliers, file = paste(dt, "_", tm, "_", username, 
                                                                             "_FuelMoisture_Outliers.csv", 
                                                                             sep = ""))}