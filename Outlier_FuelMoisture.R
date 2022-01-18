########################################################
########################################################
#     Fuel Moisture: Quality Control/Data Cleaning     #
#                   Anthony Martinez                   #
#                       May 2017                       #
########################################################
########################################################

###   Initial Setup   ###
# Identify user
username <- Sys.getenv("USERNAME")

# Create file path to Git repository
dir <- paste("C:/Users/", username, "/Documents/GitHub/Quality-Control---Fuel-Moisture", sep = "")

# Set working directory
setwd(dir)

# Load Packages
library(dplyr)
library(HH)
library(xlsx)

# Load Data
fuel <- data.frame(read.csv("tbl_fuelMoisture.csv"))
fuel$sampleMaterialID <- as.factor(fuel$sampleMaterialID)
sample_units <- data.frame(read.csv("tbl_unit.csv"))
sample_episodes <- data.frame(read.csv("tbl_unitSamplingEpisode.csv"))

###   Data Cleanning   ###
# Remove unnecessary data from sample_units object
#    This will make file easier to view
sample_units <- sample_units[,-c(5:12, 14:17, 21:24)]

# Add Unit Name
unitID <- merge(sample_episodes[,1:2], sample_units[,1:3], by = "unitID")
unitID <- unitID[,-3]
unitID <- unitID[,-1]
fuel <- merge(fuel , unitID , by = "unitSamplingEpisodeID")

# Add net weights and fuel moisture content
fuel$NetWet <- fuel$grossWetWt_g - fuel$wetContWt_g
fuel$NetDry <- fuel$grossDryWt_g - fuel$dryContWt_g
fuel$MoistureWt <- fuel$NetWet - fuel$NetDry
fuel$FMpercent <- (fuel$MoistureWt / fuel$NetDry) * 100

###   Boxplots   ###
types <- as.data.frame(table(fuel$sampleMaterialID))
colnames(types) <-c("sampleMaterialID", "Freq")
boxplot(FMpercent ~ sampleMaterialID, data = fuel, xlab = "sampleMaterialID", ylab = "Fuel moisture (%)", main = "Fuel moisture percent by sample type", plot = T)

### Fuel Moisture by catagory
mean.byID <- summarize(group_by(fuel, sampleMaterialID), mean(FMpercent, na.rm = TRUE))
counts <- as.data.frame(table(fuel$sampleMaterialID))
sd.byID <- summarize(group_by(fuel, sampleMaterialID), sd(FMpercent, na.rm = TRUE))
se.byID <- sd.byID$`sd(FMpercent, na.rm = TRUE)` / sqrt(counts$Freq)
FM.byID <- cbind(counts, mean.byID[,2], se.byID)
colnames(FM.byID) <- c("sampleMaterialID", "n", "Mean fuel moisture (%)", "SE")
FM.byID <- FM.byID[!is.na(FM.byID$`Mean fuel moisture (%)`),]

###   Outlier Analysis   ###
##  Logical Boundaries  ##
OutL1 <- fuel[fuel$NetWet < fuel$NetDry,]
OutL1 <- OutL1[!is.na(OutL1$fuelMoistureID),]
OutL2 <- fuel[fuel$grossWetWt_g < fuel$wetContWt_g,]
OutL2 <- OutL2[!is.na(OutL2$fuelMoistureID),]
OutL3 <- fuel[fuel$grossDryWt_g < fuel$dryContWt_g,]
OutL3 <- OutL3[!is.na(OutL3$fuelMoistureID),]
OutL4 <- fuel[fuel$FMpercent < 0,]
OutL4 <- OutL4[!is.na(OutL4$FMpercent),]

# Add Error Message and Output to Error Log (OutLog)
if(nrow(OutL1) > 0) {OutL1$Error <- "Logical Bound (NetDryWt > NetWetWt)"; OutLog <- rbind(OutL1)}
if(nrow(OutL2) > 0) {OutL2$Error <- "Logical Bound (DryContWt > GrossDryWt)"; OutLog <- rbind(OutLog, OutL2)}
if(nrow(OutL3) > 0) {OutL3$Error <- "Logical Bound (WetContWt > GrossWetWet)"; OutLog <- rbind(OutLog, OutL3)}
if(nrow(OutL4) > 0) {OutL4$Error <- "Logical Bound (FuelMoisture < 0)"; OutLog <- rbind(OutLog, OutL4)}

##  Regression  ##
# Net Dry ~ New Wet
fuelNA <- fuel[!is.na(fuel$MoistureWt),]
mod1 <- lm(fuelNA$NetDry ~fuelNA$NetWet)

# Plot linear model for overall dataset
plot(fuelNA$NetWet, fuelNA$NetDry, xlab = "Net wet wt (g)", ylab = "Net dry wt (g)", main = "Net wet wt vs. Net dry wt")
abline(mod1)
r2 = summary(mod1)$adj.r.squared
p = summary(mod1)$coefficients[2,4]
rp = vector('expression', 2)
rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2, dig = 3)))[2]
rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(p, digits = 3))) [2]
legend('bottomright', legend = rp, bty = 0)

# Residual analysis for overall model
plot(mod1$fit, mod1$resid, xlab = "x", ylab = "Residuals", main = "Residual Plot") #heteroscedastic
abline(h=0)
plot(fuelNA$NetDry, rstudent(mod1), xlab="Fitted Values", ylab="Studentized residuals", 
     main = "Linear Model Studentized residuals", ylim= c(-3,3))
abline(h=c(0,2,-2), lty=c(1,2,2))

# Leverage points for overall model
hh <- as.data.frame(case(mod1))
points(fuelNA$NetDry[hh$dffits > 2*sqrt(sum(hh$h)/length(fuelNA$NetWet))], rstudent(mod1)[hh$dffits > 2*sqrt(sum(hh$h)/length(fuelNA$NetWet))], pch = 16, col = "blue")

# Compile outliers for overall model
OutRo1 <- fuelNA[abs(rstudent(mod1)) > 3, ]
OutRo1 <- OutRo1[!is.na(OutRo1$fuelMoistureID),]

# Compile leverage points for overall model
OutRl1 <- fuelNA[hh$dffits > 2*sqrt(sum(hh$h)/length(fuelNA$NetWet)),]
OutRl1 <- OutRl1[!is.na(OutRl1$fuelMoistureID),]

# Add Error Message and Output to Error Log (OutLog)
if(nrow(OutRl1) > 0) {OutRl1$Error <- "Regression: Leverage Point (NetWet ~ NetDry)"}
if(nrow(OutRo1) > 0) {OutRo1$Error <- "Regression: |Studentized residuals| > 3 (NetWet ~ NetDry)"}

##  Compute outliers and leverage points for models  ##
# Calculated by surface material type (SM)
SMid <- sort(unique(fuelNA$sampleMaterialID))
for(i in 1:length(SMid)){
  #separate data by surface material type
  assign(paste0("fuel.SM",SMid[i]), fuelNA[fuelNA$sampleMaterialID == SMid[i],])
  #create linear models for each dataset (NetDry ~ NetWet)
  assign(paste0("mod.SM",SMid[i]), lm(NetDry ~ NetWet, data = get(paste0("fuel.SM",SMid[i]))))
  #plot linear models
  plot(get(paste0("fuel.SM",SMid[i]))$NetWet, get(paste0("fuel.SM",SMid[i]))$NetDry, 
       main = paste0("mod.SM",SMid[i]), xlab = "Net wet wt (g)", ylab = "Net dry wt (g)")
  abline(get(paste0("mod.SM",SMid[i])))
     
  # Add r^2 and p value to plots
  r2 = summary(get(paste0("mod.SM",SMid[i])))$adj.r.squared
  p = summary(get(paste0("mod.SM",SMid[i])))$coefficients[2,4]
  rp = vector('expression', 2)
  rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(r2, dig = 3)))[2]
  rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(p, digits = 3))) [2]
  legend('bottomright', legend = rp, bty = 0)
  
  # Compile outliers for regression (by SM type)
  assign(paste0("OutRo", 1+i), get(paste0("fuel.SM",SMid[i]))[abs(rstudent(get(paste0("mod.SM",SMid[i])))) > 3,])
     
  # Add Error text
  assign("Error", rep("Regression: |Studentized residuals| > 3 (NetWet ~ NetDry: within SM type)",length(get(paste0("OutRo", 1+i))[,1])))
  assign(paste0("OutRo", 1+i), cbind(get(paste0("OutRo", 1+i)), Error, row.names= NULL))
  
  # Compile leverage points for regression (by SM type)
  assign(paste0("hh.SM",SMid[i]), as.data.frame(case(get(paste0("mod.SM",SMid[i])))))
  assign(paste0("OutRl", 1+i), get(paste0("fuel.SM",SMid[i]))[get(paste0("hh.SM",SMid[i]))$dffits > 2*sqrt(sum(get(paste0("hh.SM",SMid[i]))$h)/length(get(paste0("fuel.SM",SMid[i]))$NetWet)),])
  
  # Add Error Message and Output to Error Log (OutLog)
  assign("Error", rep("Regression: Leverage Point (NetWet ~ NetDry: within SM type)",length(get(paste0("OutRl", 1+i))[,1])))
  assign(paste0("OutRl", 1+i), cbind(get(paste0("OutRl", 1+i)), Error, row.names= NULL))
}

# Compile outliers and leverage points
OutRO <- bind_rows(mget(ls(pattern = "OutRo")))
OutRL <- bind_rows(mget(ls(pattern = "OutRl")))
# Remove observations from regression error list that are not leverage points
OutReg <- OutRO[OutRO$fuelMoistureID %in% OutRL$fuelMoistureID ,]

###   Compile and Export All Outliers to FuelMositure_Outliers.xls   ###
FuelMoisture_Outliers <- data.frame()
if(nrow(OutLog) > 0) {FuelMoisture_Outliers <- rbind(OutLog)}
if(nrow(OutReg) > 0) {FuelMoisture_Outliers <- rbind(FuelMoisture_Outliers, OutReg)}

# Produce an error summary
if(nrow(FuelMoisture_Outliers) > 0) {
  errorsummary <- as.data.frame(table(FuelMoisture_Outliers$Error))
  colnames(errorsummary) <- c("Error", "Frequency")
  errorcount <- matrix(c("Total errors", length(FuelMoisture_Outliers$Error)),nrow=1, ncol=2)
  obscount <- matrix(c("Total measured observations", (length(fuelNA$fuelMoistureID))),nrow=1, ncol=2)
  errorrate <- matrix(c("Error rate (errors per observation)", round(as.numeric(errorcount[,2]) / as.numeric(obscount[,2]), 3)) ,nrow=1, ncol=2)
  b <- matrix(c(rep.int(NA, 2)), nrow=1, ncol=2)
  FuelMoisturesummary <-data.frame()
  FuelMoisturesummary1 <- as.data.frame(rbind(b, errorcount, obscount, errorrate))
  colnames(FuelMoisturesummary1) <- c("Error", "Frequency")
  FuelMoisturesummary <- rbind(errorsummary, FuelMoisturesummary1)
  FuelMoisturesummary[,2] <- as.numeric(FuelMoisturesummary[,2])}

# Date and time stamp on output file
dt <- Sys.Date()
tm <- format(Sys.time(), format = "%H.%M.%S", 
  tz = "", usetz = FALSE)

# Output to Excel file
if(nrow(FuelMoisture_Outliers) > 0) {
  write.xlsx(FuelMoisturesummary, file = paste0(dt, "_", tm, "_", username, "_FuelMoisture_Outliers.xlsx"),
             sheetName = "Summary", row.names = FALSE, col.names = TRUE, showNA = FALSE)
  write.xlsx(FuelMoisture_Outliers, file = paste0(dt, "_", tm, "_", username, "_FuelMoisture_Outliers.xlsx"),
             sheetName = "Errors", row.names = FALSE, col.names = TRUE, showNA = FALSE, append = TRUE)}

