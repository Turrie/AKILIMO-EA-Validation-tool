
require(plyr)
require(dplyr)
require(tidyr)
require(webshot)
library(plyr)
library(dplyr)
library(tidyr)
library(htmlwidgets)
library(viridis) 
library(ggplot2)
library(scales)
library(lubridate)
library(tidyverse)
library(data.table)
library(DT)
library(RCurl)
library(zoo)

#setwd("C:/Users/User/Documents/ACAI/EA Tools/ValActivityTool - server/getEAandHHdata")

source("functions.R")

## NOTE; whenever the recent data from ONA is downloaded, we should run the function_getData.R, The following two lines of code
 #source('functions_getData.R')

########################################################################################

userspwd <- read.csv("Input/data/EA_MonitoringAccess.csv")

dsHH <- readRDS("Input/sourceData/dsHH.RDS")
dsEA <- readRDS("Input/sourceData/dsEA.RDS")
dataVAl_FR <- readRDS("Input/sourceData/dataVAl_FR.RDS")
dataVAl_IC <- readRDS("Input/sourceData/dataVAl_IC.RDS")
dataVAl_PP <- readRDS("Input/sourceData/dataVAl_PP.RDS")
dataVAl_SPHS <- readRDS("Input/sourceData/dataVAl_SPHS.RDS")
dataVAl_CIS <- readRDS("Input/sourceData/dataVAl_CIS.RDS")
VAl_FR <- readRDS("Input/sourceData/VAl_FR.RDS")
VAl_IC <- readRDS("Input/sourceData/VAl_IC.RDS")
VAl_dataIC <- readRDS("Input/sourceData/VAl_dataIC.RDS")
VAl_PP <- readRDS("Input/sourceData/VAl_PP.RDS")
VAl_SPHS <- readRDS("Input/sourceData/VAl_SPHS.RDS")
PRA_SPHS <- readRDS("Input/sourceData/PRA_SPHS.RDS")
VAL_CIS <- readRDS("Input/sourceData/VAl_CIS.RDS")
dsICactual <- readRDS("Input/sourceData/dsICactual.RDS")
dsFRactual <- readRDS("Input/sourceData/dsFRactual.RDS")
dsPPactual <- readRDS("Input/sourceData/dsPPactual.RDS")
dsEAHH <- readRDS("Input/sourceData/dsEAHH.RDS")
multipleHHName <- readRDS("Input/sourceData/multipleHHName.RDS")
multipleHHID <- readRDS("Input/sourceData/multipleHHID.RDS")
#multipleEAName <- readRDS("Input/sourceData/multipleEAName.RDS")
#oneEADupHH <- readRDS("Input/sourceData/oneEADupHHRDS")
#dsEAHH_DI <- readRDS("Input/sourceData/dsEAHH_DI.RDS")
DsFRdue <- readRDS("Input/sourceData/DsFRdue.RDS")
FR_dstchecked <- readRDS("Input/sourceData/FR_dstchecked.RDS")
#dataVAL_PP_TZ <- readRDS("Input/sourceData/dataVAL_PP_TZ.RDS")
wksdtplot <- readRDS("Input/sourceData/wksdtplot.RDS")
FRpoints <- readRDS("Input/sourceData/FRpoints.RDS")
dsfrecom <- readRDS("Input/sourceData/dsfrecom.RDS")
FRpointsdt2 <- readRDS('Input/sourceData/FRpointsdt2.RDS')
DsICdue <- readRDS("Input/sourceData/DsICdue.RDS")
IC_dstchecked <- readRDS("Input/sourceData/IC_dstchecked.RDS")
icwksdata <- readRDS("Input/sourceData/icwksdata.RDS")
ICpoints2 <- readRDS("Input/sourceData/ICpoints2.RDS")
dsicrecom <- readRDS("Input/sourceData/dsicrecom.RDS")
ICpointsdt2 <- readRDS("Input/sourceData/ICpointsdt2.RDS")
DsPPdue <- readRDS("Input/sourceData/DsPPdue.RDS")
PP_dstchecked <- readRDS("Input/sourceData/PP_dstchecked.RDS")
ppwksdata <- readRDS("Input/sourceData/ppwksdata.RDS")
PPpoints <- readRDS("Input/sourceData/PPpoints.RDS")
PPpointsdt2 <- readRDS("Input/sourceData/PPpointsdt2.RDS")
dspprecom <- readRDS("Input/sourceData/dspprecom.RDS")
#dsEAHH_DI <- readRDS("Input/sourceData/dsEAHH_DI.RDS")

DsSPNGdue <- readRDS("Input/sourceData/DsSPNGdue.RDS")
SPNG_dstchecked <- readRDS("Input/sourceData/SPNG_dstchecked.RDS")
SPNGwksdata <- readRDS("Input/sourceData/SPNGwksdata.RDS")
SPNGpoints <- readRDS("Input/sourceData/SPNGpoints.RDS")
SPNGpointsdt2 <- readRDS("Input/sourceData/SPNGpointsdt2.RDS")

DsSPHSdue <- readRDS("Input/sourceData/DsSPHSdue.RDS")
SP_dstchecked <- readRDS("Input/sourceData/SP_dstchecked.RDS")
SPHSwksdata <- readRDS("Input/sourceData/SPHSwksdata.RDS")
SPHSpoints <- readRDS("Input/sourceData/SPHSpoints.RDS")
SPHSpointsdt2 <- readRDS("Input/sourceData/SPHSpointsdt2.RDS")
dssphsrecom <- readRDS("Input/sourceData/dssphsrecom.RDS")
DsCISdue <- readRDS("Input/sourceData/DsCISdue.RDS")
CIS_dstchecked <- readRDS("Input/sourceData/CIS_dstchecked.RDS")
wksCISdtplot <- readRDS("Input/sourceData/wksCISdtplot.RDS")
CISpoints <- readRDS("Input/sourceData/CISpoints.RDS")
CISpointsdt2 <- readRDS("Input/sourceData/CISpointsdt2.RDS")
dsCISrecom <- readRDS("Input/sourceData/dsCISrecom.RDS")
data_FR <- readRDS("Input/sourceData/data_FR.RDS")
#data_PP <- readRDS("Input/sourceData/data_PP.RDS")
#data_SP <- readRDS("Input/sourceData/data_SP.RDS")
data_CIS <- readRDS("Input/sourceData/data_CIS.RDS")
DsPPTZdue <- readRDS("Input/sourceData/DsPPTZdue.RDS")
PPTZ_dstchecked <- readRDS("Input/sourceData/PPTZ_dstchecked.RDS")
PPTZwksdata <- readRDS("Input/sourceData/PPTZwksdata.RDS")
PPTZpoints <- readRDS("Input/sourceData/PPTZpoints.RDS")
PPTZpointsdt2 <- readRDS("Input/sourceData/PPTZpointsdt2.RDS")
dsPPTZactual <- readRDS("Input/sourceData/dsPPTZactual.RDS")
dat_PP_TZ <- readRDS("Input/sourceData/dat_PP_TZ.RDS")
VAl_PP_TZ <- readRDS("Input/sourceData/VAl_PP_TZ.RDS")

dsPPTZrecom <- readRDS("Input/sourceData/dsPPTZrecom.RDS")


#ACHHTZ001259
########################################################################################
## google sheet processed data
########################################################################################
# EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
# EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
# EAmonitoringIssue1 <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Remove", ])
# EAmonitoringIssue1$EAHHUC <- paste(EAmonitoringIssue1$EAID, EAmonitoringIssue1$EA_Name, EAmonitoringIssue1$HHID, EAmonitoringIssue1$HH_Name, EAmonitoringIssue1$useCase, sep="")
# dsEAHH$EAHHUC <- paste(dsEAHH$EAID, dsEAHH$EA_Name, dsEAHH$HHID, dsEAHH$HH_Name, dsEAHH$useCase, sep="")
# dsEAHH <- droplevels(dsEAHH[!dsEAHH$EAHHUC %in% EAmonitoringIssue1$EAHHUC, ])
# dsEAHH <- subset(dsEAHH, select = -c(EAHHUC))
# 
# EAmonitoringIssue2 <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Keep", ])
# EAmonitoringIssue2 <- subset(EAmonitoringIssue2, select = -c(Status, data_issue))
# 
# EAmonitoringIssue2 <- EAmonitoringIssue2[, colnames(dsEAHH)]
# dsEAHH <- rbind(dsEAHH, EAmonitoringIssue2)

dsEAHHPP <- droplevels(dsEAHH[dsEAHH$useCase == "PP", ])
dsEAHHnotPP <- droplevels(dsEAHH[!dsEAHH$useCase == "PP", ])

dsEAptnCAV <- read.csv("Input/data/dsBPP_CAVAII_EAs.csv")
dsEAptnOYC <- read.csv("Input/data/dsBPP_OYSCGA_EAs.csv")

dsEAHHPP$EA_Partner <- ifelse(dsEAHHPP$EAID %in% dsEAptnCAV$EAID, "CAVAII", as.character(dsEAHHPP$EA_Partner))
dsEAHHPP$EA_Partner <- ifelse(dsEAHHPP$EAID %in% dsEAptnOYC$EAID, "OYSCGA", as.character(dsEAHHPP$EA_Partner))


dsEAHH <- rbind(dsEAHHPP, dsEAHHnotPP)

# dsEAHHna <- droplevels(dsEAHH[is.na(dsEAHH$EAID), ])
dsEAHHnotNA <- droplevels(dsEAHH[!is.na(dsEAHH$EAID), ])
# dsEAHHna <- droplevels(dsEAHH[dsEAHH$EAID == "", ])
dsEAHH <- dsEAHHnotNA

