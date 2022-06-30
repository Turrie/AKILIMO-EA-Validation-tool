

#' run this fucntion every time after updating the ONA data (downloading ONA data to your PC) 
#'  it writes out the processed data to make to tool faster for users
dataProcessing <- function(){
  

  #setwd("C:/Users/User/Documents/ACAI/EA Tools/ValActivityTool")
  
  wd <- "C:/Users/User/Documents/ACAI/EA Tools/ValActivityTool"
  source("clean_IDs.R")
  source("functions.R")
  
  #read in the EA and HH registration data and subset
  dsEA <- dropGroupNames(read.csv(paste(wd, "data/Register_EA.csv", sep="/")))
  dsHH <- dropGroupNames(read.csv(paste(wd, "data/Register_HH.csv", sep="/")))
  
  EAHH_cleanData <- Clean_EAHH_Ids(dsEA, dsHH)
  dsEA <- EAHH_cleanData[[1]]
  dsHH <- EAHH_cleanData[[2]]
  
  dsEA <- unique(subset(dsEA, select=c(EAID, EA_Name, phoneNrEA, country, partner)))
  dsHH <- subset(dsHH, select=c(EAID, HHID, HH_Name, genderHH, phoneNrHH, Latitude, Longitude, country, useCaseHH, region, state, zone ))
  
  dsEA <- unique(dsEA)
  dsHH <- unique(dsHH)
  
  dsHH <- droplevels(dsHH[!dsHH$EAID=="", ])
  
  #merge dsEA AND dsHH
  dsEAHH <- droplevels(unique(merge(dsEA, dsHH, by=c("EAID", "country"))))
  dsEAHH<-dsEAHH[!(dsEAHH$EA_Name %in% c("Halima Faki")), ] 
  head(dsEAHH)

  #correct Margaret Asuo name mispell
  dsEAHH$EA_Name<- ifelse(dsEAHH$EA_Name %in% "Wargaret Asuo", "Margaret Asuo", as.character(dsEAHH$EA_Name))
  
  dsEAHH$region_state <- ifelse(dsEAHH$region=="", as.character(dsEAHH$state), as.character(dsEAHH$region))
  dsEAHH$region_state2<-gsub("NG.", "", dsEAHH$region_state)
  dsEAHH$region_state3<-gsub("TZ.", "", dsEAHH$region_state2)
  #write.csv(dsEAHH, "dsEAHH_gen.csv")
  dsEAHH <- dsEAHH[, c( "EAID","EA_Name","phoneNrEA", "partner","HHID","HH_Name", "phoneNrHH","useCaseHH","Latitude","Longitude", "country", "region_state3")]
  colnames(dsEAHH) <- c( "EAID","EA_Name","EA_PhoneNr", "EA_Partner","HHID","HH_Name", "HH_PhoneNr","useCase","Latitude","Longitude", "Country", "region_state")
  dsEAHH <- unique(dsEAHH)
  unique(dsEAHH$HHID)
  head(dsEAHH)
  #write.csv(dsEAHH, "dsEAHH.csv")

  #Read Validation-data
  dataVAl_FR <- read.csv("data/dataVAL_FR.csv")## actual planting date
  dataVAl_IC <- read.csv("data/dataVAL_IC.csv")
  dataVAl_PP <- read.csv("data/dataVAL_PP.csv")
  dataVAl_SPHS <- read.csv("data/dataVAL_SPHS.csv")
  dataVAl_CIS <- read.csv("data/dataVAL_CIS.csv")
  dataVAL_PP_TZ <- read.csv("data/dataVAL_PP_TZ.csv")
  userspwd <- read.csv("data/EA_MonitoringAccess.csv")
  
  VAl_FR <- read.csv("data/VAL_FR.csv")
  VAl_IC <- read.csv("data/VAL_IC.csv")
  VAl_PP <- read.csv("data/VAL_PP.csv")
  VAl_CIS <- read.csv("data/VAL_CIS.csv")
  VAl_PP_TZ <- read.csv("data/VAL_PP_TZ.csv")
  
  # merge the SPHS data
  # fls <- paste(wd, "/data/", list.files(path=paste(wd, "/data", sep=""), pattern="VAL_SPHS_"), sep="")
  # fls_TZ <- fls[grep("TZ", fls)]
  # fls_NG <- fls[-grep("TZ", fls)]
  # dst_TZ <- dropGroupNames(do.call(rbind, lapply(fls_TZ, function(x) read.csv(x))))
  # dst_NG <- dropGroupNames(do.call(rbind, lapply(fls_NG, function(x) read.csv(x))))
  # dst_TZ$season <- 'NA'
  # dst_TZ <- dst_TZ[, colnames(dst_NG)]
  # dst <- rbind(dst_TZ, dst_NG)
  
  VAl_SPHS_KW <- read.csv("data/VAL_SPHS_KW.csv",stringsAsFactors=FALSE)
  VAl_SPHS_OG <- read.csv("data/VAL_SPHS_OG.csv",stringsAsFactors=FALSE)
  VAl_SPHS_ON <- read.csv("data/VAL_SPHS_ON.csv",stringsAsFactors=FALSE)
  VAl_SPHS_OY <- read.csv("data/VAL_SPHS_OY.csv",stringsAsFactors=FALSE)
  
  VAl_SPHS_TZEZ <- read.csv("data/VAL_SPHS_TZEZ.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZLZE <- read.csv("data/VAL_SPHS_TZLZE.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZLZW <- read.csv("data/VAL_SPHS_TZLZW.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZSZ <- read.csv("data/VAL_SPHS_TZSZ.csv",stringsAsFactors=FALSE)
  
  
  VAl_SPHS_NG <- rbind(VAl_SPHS_KW, VAl_SPHS_OG, VAl_SPHS_ON, VAl_SPHS_OY)
  VAl_SPHS_TZ <- rbind(VAl_SPHS_TZEZ, VAl_SPHS_TZLZE, VAl_SPHS_TZLZW, VAl_SPHS_TZSZ)
  VAl_SPHS_NG <- VAl_SPHS_NG[, colnames(VAl_SPHS_TZ)]
  VAl_SPHS <- rbind(VAl_SPHS_NG, VAl_SPHS_TZ)
  # dsSPHS <- VAl_SPHS
  # 
  # VAl_SPHS <- dst
  PRA_SPHS <- read.csv("data/PRA_SPHS.csv")
  #
  # #add variety name for the case of IC	then merge this with DsEAHH
  # dataVAl_IC$HHID_VAR <- ifelse(dataVAl_IC$purpose.maizeVarietySelect=="Ikom_White", paste(dataVAl_IC$HHID, "_Ikom_White", sep=""), "NA")
  # #dataVAl_IC$HHID <- paste(dataVAl_IC$HHID, ifelse(dataVAl_IC$purpose.maizeVarietySelect=="Ikom_White", "_Ikom_White", ""), sep="")
 
  
  # 
  # #write.csv(dataVAl_IC, "IKOM_IC.csv")
  # Ikomdata <- dataVAl_IC[!dataVAl_IC$HHID_VAR == "NA", ]
  # dsEAHH$HHID <- ifelse(dsEAHH$HHID %in% Ikomdata$HHID, Ikomdata$HHID_VAR, as.character(dsEAHH$HHID))
  # #write.csv(dsEAHH, "dsIKOM_IC.csv")
  # dsEAHH$region_state<-gsub("NG.", "", dsEAHH$region_state)
  # dsEAHH$region_state<-gsub("TZ.", "", dsEAHH$region_state)
  # unique(dsEAHH$HHID)
  # dataVAl_IC$HHID <- paste(dataVAl_IC$HHID, ifelse(dataVAl_IC$purpose.maizeVarietySelect=="Ikom_White", "_Ikom_White", ""), sep="")
 
   #############################################################################
  ## Data issues
  ## count multiple  HHName per HHID
  HHIDName <- unique(dsEAHH[, c("HHID", "HH_Name")])
  chhid <- as.data.frame(table(HHIDName$HHID))
  multipleHHName <- dsEAHH[dsEAHH$HHID %in% chhid[chhid$Freq >1, ]$Var1, ]
  multipleHHName$data_issue <- "multipleHHName"
  
  ## count multiple HHID per hhname
 
  HHIDName <- unique(dsEAHH[, c("HHID", "HH_Name")])
  chhname <- as.data.frame(table(HHIDName$HH_Name))
  multipleHHID <- dsEAHH[dsEAHH$HH_Name %in% chhname[chhname$Freq >1, ]$Var1, ]
  multipleHHID$data_issue <- "multipleHHID"
  
  # count multiple EAID per EAname
  EAIDName <- unique(dsEAHH[, c("EAID", "EA_Name")])
  ceaname <- as.data.frame(table(EAIDName$EA_Name))
  multipleEAID <- dsEAHH[dsEAHH$EA_Name %in% ceaname[ceaname$Freq >1, ]$Var1, ]
  multipleEAID$dataissue <- "multipleEAID"
  
  ## count multiple EA per HH
  EAIDHHID <- unique(dsEAHH[, c("EAID", "HHID", "useCase")])
  chh <- as.data.frame(table(EAIDHHID$HHID))
  multipleHH <- dsEAHH[dsEAHH$HHID %in% chh[chh$Freq >1, ]$Var1, ]
  oneEADupHH <- NULL
  for(hids in unique(multipleHH$HHID)){
    hdhid <- multipleHH[multipleHH$HHID == hids, ]
    if(length(unique(hdhid$useCase)) > 1){
      oneEADupHH <- rbind(oneEADupHH, multipleHH)
    }
  }
  oneEADupHH$data_issue <- "multipleEAforHH"
  
  # dsEAHH_DI <- rbind(multipleHHName, multipleHHID, multipleEAName, oneEADupHH)
  # dsEAHH_DI$Latitude <- round(dsEAHH_DI$Latitude, digits=3)
  # dsEAHH_DI$Longitude <- round(dsEAHH_DI$Longitude, digits=3)
  # dsEAHH_DI <- unique(dsEAHH_DI)
  # dsEAHH_DI$Status <- "Keep"
  ########################################################################################
  ## google sheet processed data
  ########################################################################################
  EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
  EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
  EAmonitoringIssue <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Remove", ])
  EAmonitoringIssue$EAHHUC <- paste(EAmonitoringIssue$EAID, EAmonitoringIssue$EA_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$useCase, sep="")
  dsEAHH$EAHHUC <- paste(dsEAHH$EAID, dsEAHH$EA_Name, dsEAHH$HHID, dsEAHH$HH_Name, dsEAHH$useCase, sep="")
  dsEAHH <- droplevels(dsEAHH[!dsEAHH$EAHHUC %in% EAmonitoringIssue$EAHHUC, ])
  ########################################################################################
  # FR data
  ########################################################################################
  
  data_FR <- dataVAl_FR
  VAl_dataFR <- VAl_FR
  names(data_FR)[grepl('\\.', names(data_FR))] <- sub('.*\\.', '', names(data_FR)[grepl('\\.', names(data_FR))])
  #data_FR <- filterSingleSubmission(data_FR, ID, recent=TRUE) #same for HH registration submissions
  useCase = "FR"
  cleanVALFR(useCase = "FR", wd, recent=TRUE)
  
  FRreg_HH <- droplevels(data_FR[!data_FR$HHID %in% dsHH$HHID, ])#HHs not registered
  FRreg_EA <- droplevels(data_FR[!data_FR$EAID %in% dsEA$EAID, ])#EAs not registered
  library(lubridate)
  
  
  dat <- data_FR
  dst <-  VAl_dataFR
  #fix planting date issue
  pldateissue_FR <-  unique(dst[, c("HHID", "plantingDate")])
  plfr <- as.data.frame(table(pldateissue_FR$HHID))
  VAl_FR_dupPlDate <- droplevels(dst[dst$HHID %in% plfr[plfr$Freq >1, ]$Var1, ])
  dst <- droplevels(dst[!dst$HHID %in% plfr[plfr$Freq >1, ]$Var1, ])
  VAl_FR_dupPlDate <- getLatestPlDate(VAl_FR_dupPlDate)
  dst <- rbind(dst, VAl_FR_dupPlDate)
  
  #subset actual dates
  dsFRactual <- unique(subset(dat, select=c(HHID, plantingDate, gappingDate, 
                                            dateFertilizer1, dateFertilizer2,
                                            dateWeeding1, dateWeeding2, dateWeeding3, harvestDate)))
  colnames(dsFRactual) <- c("HHID",  "actualplantingdate", "gapping_date", "fertlizerapp1", "fertlizerapp2", "weeding1", "weeding2", 
                            "weeding3", "harvestdate"  )
  dsFRactual <- EventLatestDate(usecasedata=dsFRactual, usecase="FR")
  colnames(dsFRactual) <- c("HHID",  "actualplantingdate", "gapping_date", "fertlizerapp1", "fertlizerapp2", "weeding1", "weeding2", "weeding3", "harvestdate"  )
  
  
  dsFRplant <- unique(subset(VAl_dataFR, select=c(HHID, plantingDate, season)))
  colnames(dsFRplant) <- c("HHID", "Plannedplantingdate", "season")
  dsFRplant[is.na(dsFRplant$season),]$season <- 1
  
  #convert factor to date to enable ordering
  dsFRplant$f0m <- lubridate::month(mdy(dsFRplant$Plannedplantingdate))
  dsFRplant$f0d <- lubridate::day(mdy(dsFRplant$Plannedplantingdate))
  dsFRplant$f0y <- lubridate::year(mdy(dsFRplant$Plannedplantingdate))
  dsFRplant$f0dmy <- ifelse(is.na(dsFRplant$f0m), NA, paste(dsFRplant$f0y, dsFRplant$f0m,  dsFRplant$f0d,    sep = "/"))
  dsFRplant$f0dmy <-  as.Date(dsFRplant$f0dmy)
  
  dsFRplant <- dsFRplant[, c(1,3,7)]
  colnames(dsFRplant) <- c("HHID", "season", "Plannedplantingdate")
  head(dsFRplant)
  
  dsFRplant$seasonhh <- paste(dsFRplant$HHID, dsFRplant$season, sep = "_")
 
  dsFRplant <- unique(dsFRplant)
  
  #pick latest date
  dsFRplant2 <- aggregate(dsFRplant$Plannedplantingdate, list(dsFRplant$HHID, dsFRplant$season, dsFRplant$seasonhh), max) 
  head(dsFRplant2)
  #dsFRplant2 <- dsFRplant[dsFRplant$Plannedplantingdate %in% latest$x,]
  
  dsFRplanned <- dsFRplant2
  colnames(dsFRplanned) <- c("HHID", "season","seasonhh", "Plannedplantingdate")
  
  #convert dates back again to factor
  dsFRplanned$Plannedplantingdate <-  as.factor(dsFRplanned$Plannedplantingdate)
  dsFRplanned$f0mn <- lubridate::month(ymd(dsFRplanned$Plannedplantingdate))
  
  dsFRplanned$f0m <- month.abb[dsFRplanned$f0mn]
  dsFRplanned$f0d <- lubridate::day(ymd(dsFRplanned$Plannedplantingdate))
  dsFRplanned$f0y <- lubridate::year(ymd(dsFRplanned$Plannedplantingdate))
  dsFRplanned$f0sep <- paste( dsFRplanned$f0d, "," ,  sep = "")
  
  dsFRplanned$f0ymd <- ifelse(is.na(dsFRplanned$f0m), NA, paste(dsFRplanned$f0m,  dsFRplanned$f0sep, dsFRplanned$f0y, sep = " "))
  
  
  dsFRplanned <- dsFRplanned[, c(1,2,3,10)]
  colnames(dsFRplanned) <- c("HHID", "season", "seasonhh", "Plannedplantingdate")
  
  dsFRheatmap <- droplevels(unique(merge(dsFRplanned, dsFRactual, by=c("HHID"))))
  
  dsFRheatmap$Plannedplantingdate <- as.factor(dsFRheatmap$Plannedplantingdate)
  
  
  # dsFRplanned <- unique(subset(VAl_dataFR, select=c(HHID, plantingDate)))
  # colnames(dsFRplanned) <- c("HHID", "Plannedplantingdate")
  # dsFRheatmap <- droplevels(unique(merge(dsFRplanned, dsFRactual, by=c("HHID"))))
 
  head(dsFRheatmap)
  #replace missing actual planting dates with planned planting date
  dsFRheatmaphid1 <- NULL
  for( hids in unique(dsFRheatmap$HHID)){
    dsFRheatmaphid <- droplevels(dsFRheatmap[dsFRheatmap$HHID == hids, ])
    if(all(unique(dsFRheatmaphid$actualplantingdate) == '')){
      dsFRheatmaphid$actualplantingdate <- ifelse(dsFRheatmaphid$actualplantingdate == '', as.character(dsFRheatmaphid$Plannedplantingdate), as.character(dsFRheatmaphid$actualplantingdate))
    }else{
      ap <- unique(dsFRheatmaphid[dsFRheatmaphid$actualplantingdate != "", ]$actualplantingdate)
      dsFRheatmaphid$actualplantingdate <- as.character(ap)
    }
    dsFRheatmaphid1 <- rbind(dsFRheatmaphid1, dsFRheatmaphid)
  }
  dsFRheatmap <- dsFRheatmaphid1
  
  #solve dates into days of the year
  head(dsFRheatmap)
  actual <- solvedates(colNr = 5, usecasedata = dsFRheatmap)
  planned <- solvedates(colNr = 4, usecasedata = dsFRheatmap)
  gap <- solvedates(colNr = 6, usecasedata = dsFRheatmap)
  fert1 <- solvedates(colNr = 7, usecasedata = dsFRheatmap)
  fert2 <- solvedates(colNr = 8, usecasedata = dsFRheatmap)
  weed1 <- solvedates(colNr = 9, usecasedata = dsFRheatmap)
  weed2 <- solvedates(colNr = 10, usecasedata = dsFRheatmap)
  weed3 <- solvedates(colNr = 11, usecasedata = dsFRheatmap)
  
  harvestdater <- solvedates(colNr = 12, usecasedata = dsFRheatmap)
  if(!is.null(harvestdater)){
    harvestdater<- subset(harvestdater, select = c(HHID,harvestdate ))
  }else{
    harvestdater <- as.data.frame(matrix(ncol=1, nrow=nrow(dsFRheatmap)), data="NA")
    colnames(harvestdater) <- c("harvestdate" )
    harvestdater$HHID <- dsFRheatmap$HHID
  }
  
  #merge solved days of the year
  fr0 <- dplyr::select(dsFRheatmap, HHID, season, seasonhh)
  fr_n <- unique(merge(fr0, actual, by="HHID"), all=TRUE)
  fr1 <- unique(merge(fr_n, planned, by="HHID"), all=TRUE)
  fr2 <- unique(merge(fr1, gap, by='HHID', all=TRUE))
  fr3 <- unique(merge(fr2, fert1, by='HHID', all=TRUE))
  fr4 <- unique(merge(fr3, fert2, by='HHID', all=TRUE))
  fr5 <- unique(merge(fr4, weed1, by='HHID', all=TRUE))
  fr6 <- unique(merge(fr5, weed2, by='HHID', all=TRUE))
  fr7 <- unique(merge(fr6, weed3, by='HHID', all=TRUE))
  fr8 <- unique(merge(fr7, harvestdater, by='HHID', all=TRUE))
  # if(!is.null(harvestdater)){
  #   dsFRheatmapdates <- unique(merge(fr7, harvestdater, by='HHID', all=TRUE))
  # }else{
  #   dsFRheatmapdates <- fr7
  # }
  
  dsFRheatmapdates <- fr8
  head(dsFRheatmapdates)
  
  
  #deduct solved days of the year against actual planting dates
  dsFRheatmapdates$planting <- as.numeric(dsFRheatmapdates$actualplantingdate) - as.numeric(dsFRheatmapdates$Plannedplantingdate)   
  dsFRheatmapdates$gappin <- as.numeric(dsFRheatmapdates$gapping_date) - as.numeric(dsFRheatmapdates$actualplantingdate)
  dsFRheatmapdates$fertilizr1 <- as.numeric(dsFRheatmapdates$fertlizerapp1) - as.numeric(dsFRheatmapdates$actualplantingdate)
  dsFRheatmapdates$fertilizr2 <- as.numeric(dsFRheatmapdates$fertlizerapp2) - as.numeric(dsFRheatmapdates$actualplantingdate)
  dsFRheatmapdates$weedin1 <- as.numeric(dsFRheatmapdates$weeding1) - as.numeric(dsFRheatmapdates$actualplantingdate)
  dsFRheatmapdates$weedin2 <- as.numeric(dsFRheatmapdates$weeding2) - as.numeric(dsFRheatmapdates$actualplantingdate)
  dsFRheatmapdates$weedin3 <- as.numeric(dsFRheatmapdates$weeding3) - as.numeric(dsFRheatmapdates$actualplantingdate)
  dsFRheatmapdates$harvest <- as.numeric(dsFRheatmapdates$harvestdate) - as.numeric(dsFRheatmapdates$actualplantingdate)
  
  head(dsFRheatmapdates)
  
  colnames (dsFRheatmapdates)<- c("HHID", "season", "seasonhh", "actualplantingdate", "plannedplantingdate",  "actualgappingdate", "fertilizerdate1","fertilizerdate2", "weedingdate1",
                                  "weedingdate2", "weedingdate3","harvestdate", "planting", "gappin", "fertilizr1", "fertilizr2", "weedin1", "weedin2", "weedin3", "harvest")
  
  #get EA names from dsEAHH
  dsFRheatmap2 <- droplevels(unique(merge(dsFRheatmapdates, dsEAHH, by=c("HHID"))))
  head(dsFRheatmap2)
  drops <- c("EAID.y","EA_PhoneNr", "EA_Partner", "HH_Name", "HH_PhoneNr", "useCase", "Latitude", "Longitude", "Country", "region_state")
  dsfr1 <- (dsFRheatmap2[ , !(names(dsFRheatmap2) %in% drops)])
  dsfr <- dsfr1
  
  #identify activities not done
  dsfr$gapping <- ifelse(is.na(dsfr$gappin) & !is.na(dsfr$fertilizr1), -999, dsfr$gappin)
  dsfr$fertilizer1 <- ifelse(is.na(dsfr$fertilizr1) & !is.na(dsfr$fertilizr2), -999, dsfr$fertilizr1)
  dsfr$fertilizer2 <-  ifelse(is.na(dsfr$fertilizr2) & !is.na(dsfr$weedin1), -999, dsfr$fertilizr2)
  dsfr$weeding1 <-  ifelse(is.na(dsfr$weedin1) & !is.na(dsfr$weedin2), -999, dsfr$weedin1)
  dsfr$weeding2 <-  ifelse(is.na(dsfr$weedin2) & !is.na(dsfr$weedin3), -999, dsfr$weedin2)
  dsfr$weeding3 <-  ifelse(is.na(dsfr$weedin3) & !is.na(dsfr$harvest), -999, dsfr$weedin3)
  head(dsfr)
  
  drops <- c("gappin", "fertilizr1", "fertilizr2", "weedin1", "weedin2", "weedin3")
  dsfr2 <- (dsfr[ , !(names(dsfr) %in% drops)])
  head(dsfr2)
  
  dsfr3 <- subset(dsfr2, select=c("HHID", "season", "seasonhh", "EAID", "EA_Name",  "planting", "gapping", "fertilizer1", "fertilizer2", "weeding1", "weeding2", "weeding3", "harvest"))
  
  #reshape actual dates
  dsFRss1 <- subset(dsfr2, select=c(HHID, season, seasonhh, plannedplantingdate, actualplantingdate, actualgappingdate, fertilizerdate1, fertilizerdate2, weedingdate1, weedingdate2, weedingdate3, harvestdate))
  dsFRss2 <- subset(dsfr2, select=c(HHID, season, seasonhh, planting, harvest, EAID,  EA_Name, gapping, fertilizer1, fertilizer2, weeding1, weeding2, weeding3))
  #dsFRss2 <- dsFRss2[!dsFRss2$EAID == "ACEANG000023", ]
  dsFReshapess1 <- suppressWarnings({dsFRss1 %>% gather(actualEvents, actualDates, actualplantingdate, actualgappingdate, fertilizerdate1, fertilizerdate2, weedingdate1, weedingdate2, weedingdate3, harvestdate)})
  dsFReshapess2 <- suppressWarnings({dsFRss2 %>% gather(events, dates,planting, gapping, fertilizer1, fertilizer2, weeding1, weeding2, weeding3, harvest)})
  dsFReshapess2$dates <- as.numeric(dsFReshapess2$dates)
  head(dsFReshapess1)
  
  #merge actual dates and solved dates
  dsFReshapess1$events <- gsub("actual", "", dsFReshapess1$actualEvents)
  dsFReshapess1$events <- gsub("date", "", dsFReshapess1$events)
  dsFReshape <- merge(dsFReshapess1, dsFReshapess2)
  dsFReshape <- unique(dsFReshape)
  head(dsFReshape)
  
  names(dsFReshape) <- c('HHID', "season", "seasonhh", 'events', "plannedplantingdate", 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 'DatesBnPlantingEvent')
  hhdata <- dsFReshape
  hhdata$actualDates <- as.Date(hhdata$actualDates, origin = "1970-01-01")
 
  head(hhdata)
  
  #get status of events
  getstatus<- NULL
  for(h in unique(hhdata$seasonhh)){
    #maxDate <- max(hhdata$Dates, na.rm=TRUE)
    hhdatass <- subset(hhdata, seasonhh == h)
    if(!all(is.na(hhdatass$DatesBnPlantingEvent))){
      
      
      hhdatass$status <- ifelse(hhdatass$events %in% c("planting")  & hhdatass$DatesBnPlantingEvent  == 0, "On-time",
                                ifelse(hhdatass$events %in% c("planting")  & (hhdatass$DatesBnPlantingEvent  < 0 & hhdatass$DatesBnPlantingEvent > -50), "Earlier",
                                       ifelse(hhdatass$events %in% c("planting")  & hhdatass$DatesBnPlantingEvent  > 0, "Done late",
                                              ifelse(hhdatass$events %in% c("planting")  & hhdatass$DatesBnPlantingEvent  == -999, "Not done", 
                                                     
                                                     ifelse(hhdatass$events %in% c("gapping") & (hhdatass$DatesBnPlantingEvent < 28 & hhdatass$DatesBnPlantingEvent > -50), "Earlier", 
                                                            ifelse(hhdatass$events %in% c("gapping") & hhdatass$DatesBnPlantingEvent  == 28 , "On-time", 
                                                                   ifelse(hhdatass$events %in% c("gapping") & hhdatass$DatesBnPlantingEvent > 28 , "Done late",
                                                                          ifelse(hhdatass$events %in% c("gapping")  & hhdatass$DatesBnPlantingEvent  == -999, "Not done", 
                                                                                 ifelse(hhdatass$events %in% c("gapping")  & hhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                        
                                                                                        ifelse(hhdatass$events %in% c("fertilizer1") & (hhdatass$DatesBnPlantingEvent < 14  & hhdatass$DatesBnPlantingEvent > -50), "Earlier", 
                                                                                               ifelse(hhdatass$events %in% c("fertilizer1") & hhdatass$DatesBnPlantingEvent   >= 14 & hhdatass$DatesBnPlantingEvent <= 42 , "On-time", 
                                                                                                      ifelse(hhdatass$events %in% c("fertilizer1") & hhdatass$DatesBnPlantingEvent > 42 , "Done late",
                                                                                                             
                                                                                                             ifelse(hhdatass$events %in% c("fertilizer1")  & hhdatass$DatesBnPlantingEvent  == -999, "Not done", 
                                                                                                                    ifelse(hhdatass$events %in% c("fertilizer1")  & hhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                           
                                                                                                                           ifelse(hhdatass$events %in% c("fertilizer2") & (hhdatass$DatesBnPlantingEvent < 56 & hhdatass$DatesBnPlantingEvent > -50), "Earlier", 
                                                                                                                                  ifelse(hhdatass$events %in% c("fertilizer2") & hhdatass$DatesBnPlantingEvent  >= 56 & hhdatass$DatesBnPlantingEvent <= 84 , "On-time", 
                                                                                                                                         ifelse(hhdatass$events %in% c("fertilizer2") & hhdatass$DatesBnPlantingEvent > 84 , "Done late",
                                                                                                                                                ifelse(hhdatass$events %in% c("fertilizer2") & hhdatass$DatesBnPlantingEvent == -999 , "Not done",   
                                                                                                                                                       ifelse(hhdatass$events %in% c("fertilizer2")  & hhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                              
                                                                                                                                                              
                                                                                                                                                              ifelse(hhdatass$events %in% c("weeding1") & hhdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                                                                                     ifelse(hhdatass$events %in% c("weeding1")  & hhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                            ifelse(hhdatass$events %in% c("weeding1") & hhdatass$DatesBnPlantingEvent == -999 , "Not done",                                                                                                                                                   
                                                                                                                                                                                   
                                                                                                                                                                                   ifelse(hhdatass$events %in% c("weeding2") & hhdatass$DatesBnPlantingEvent > 0, "On-time",
                                                                                                                                                                                          ifelse(hhdatass$events %in% c("weeding2")  & hhdatass$Dates == -100, "To be done",
                                                                                                                                                                                                 ifelse(hhdatass$events %in% c("weeding2") & hhdatass$DatesBnPlantingEvent == -999 , "Not done",                               
                                                                                                                                                                                                        
                                                                                                                                                                                                        ifelse(hhdatass$events %in% c("weeding3") & hhdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                                                                                                                               ifelse(hhdatass$events %in% c("weeding3")  & hhdatass$Dates == -100, "To be done",
                                                                                                                                                                                                                      ifelse(hhdatass$events %in% c("weeding3") & hhdatass$DatesBnPlantingEvent == -999 , "Not done",                                                                                                                                                                            
                                                                                                                                                                                                                             
                                                                                                                                                                                                                             ifelse(hhdatass$events %in% c("harvest") & hhdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                                                                                                                                                    ifelse(hhdatass$events %in% c("harvest")  & hhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                           as.character(hhdatass$status)))))))))))))))))))))))))))))))
      
      hhdatass <- replace(hhdatass, hhdatass == -100, NA)
      suppressWarnings({getstatus <- rbind(getstatus, hhdatass)})
    }
  } 
  
  
  hhdata <- getstatus
  
  hhdata$status <- ifelse(is.na(hhdata$DatesBnPlantingEvent), "To be done", as.character(hhdata$status))
  hhdata$status <- ifelse(hhdata$DatesBnPlantingEvent == "", "To be done", as.character(hhdata$status))
  hhdata$status <- ifelse(is.na(hhdata$status), "To be done", as.character(hhdata$status))
  hhdata$actualmn <- lubridate::month(ymd(hhdata$actualDates))
  hhdata$month  <- month.abb[hhdata$actualmn]
  hhdata$actuald <- lubridate::day(ymd(hhdata$actualDates))
  hhdata$actualday <- lubridate::wday(ymd(hhdata$actualDates))
  hhdata$actualYR <- lubridate::year(ymd(hhdata$actualDates))
  
  head(hhdata)
  
  hhdata$datetbd <- ifelse(!is.na(hhdata$actuald), paste(hhdata$actualYR, hhdata$actualmn, hhdata$actuald, sep = "/"), NA)
  hhdata$datetbd2 <- as.Date(hhdata$datetbd)
  #hhdata <- select(hhdata, -c(plannedplantingdate))
  hhdata <- unique(hhdata)
  
  hhdata <- droplevels(hhdata[!hhdata$HHID == "ACHHNG002777", ])
  hhdata <- droplevels(hhdata[!hhdata$HHID == "ACHHNG002780", ])
  
  #projected dates
  DsFRdue <- NULL
  for(hids in unique(hhdata$seasonhh)){
    hd <- hhdata[hhdata$seasonhh == hids, ]
    hdPl <- hd[hd$events == 'planting', ]
    
    hdG <- hd[hd$events == 'gapping', ]
    if(is.na(hdG$datetbd2)){
      hdG$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 28
    }
    hdF1 <- hd[hd$events == 'fertilizer1', ]
    if(is.na(hdF1$datetbd2)){
      hdF1$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 28
    }
    hdF2 <- hd[hd$events == 'fertilizer2', ]
    if(is.na(hdF2$datetbd2)){
      hdF2$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 70

    }
    hdW1 <- hd[hd$events == 'weeding1', ]
    if(is.na(hdW1$datetbd2)){
      hdW1$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 84
    }
    
    hdW2 <- hd[hd$events == 'weeding2', ]
    if(is.na(hdW2$datetbd2)){
      hdW2$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 168
    }
    
    hdW3 <- hd[hd$events == 'weeding3', ]
    if(is.na(hdW3$datetbd2)){
      hdW3$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 252
    }
    
    hdh <- hd[hd$events == 'harvest', ]
    
    DsFRdue <- suppressWarnings({rbind(DsFRdue, hdPl, hdG, hdF1, hdF2, hdW1,hdW2,hdW3, hdh)})
  }
  
  
  names(DsFRdue) <- c('HHID', 'season', 'seasonhh', 'Events', 'plannedplantinddates', 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 'DatesBnPlantingEvent', 'status', 
                      'actualmn','month', 'actuald', 'actualday', 'actualYR', 'datetbd', 'datetbd2')
  DsFRdue <- unique(DsFRdue)
  head(DsFRdue)
  DsFRdue <- droplevels(DsFRdue[!is.na(DsFRdue$HHID), ] )
  DsFRdue$status <- ifelse(is.na(DsFRdue$DatesBnPlantingEvent), "To be done", as.character(DsFRdue$status))
  DsFRdue$status <- ifelse(DsFRdue$status == "To be done"& DsFRdue$datetbd2 < Sys.Date(), "Overdue", as.character(DsFRdue$status))
  DsFRdue$status <- ifelse(is.na(DsFRdue$DatesBnPlantingEvent), "To be done", as.character(DsFRdue$status))
  
  # ADDING column for DST run
  DsFRdue$datetbd2 <- as.character(DsFRdue$datetbd2)
  
  hhidFR_TT <- droplevels(DsFRdue [DsFRdue $HHID %in% VAl_FR$HHID, ])
  hhidFR_TF <- droplevels(VAl_FR[!VAl_FR$HHID %in% DsFRdue $HHID, ])## dst is run but no data is submitted
  hhidFR_FT <- droplevels(DsFRdue[!DsFRdue$HHID %in% VAl_FR$HHID, ])## dst is not run but there is data
  
  FR_TT <- NULL
  for(hhids in unique(hhidFR_TT$HHID)){
    hd <- hhidFR_TT[hhidFR_TT$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "True"
    hd1$Events <- "DST run"
    hd1$status <- "To be done"
    FR_TT <- rbind(FR_TT, hd, hd1)
  }
  
  FR_TF <- NULL
  for(eaids in unique(hhidFR_TF$EAID)){
    edata <- DsFRdue [DsFRdue $EAID == eaids, ]
    hh_nodata <- unique(hhidFR_TF[hhidFR_TF$EAID == eaids, ]$HHID)
    if(length(hh_nodata) > 0){
      asas <- edata[1:length(hh_nodata),]
      asas$HHID <- hh_nodata
      asas$Events <- "DST run"
      asas$actualEvents <- "DST run"
      asas$status <- "To be done"
      asas$datetbd2 <- "True"
      FR_TF <- rbind(FR_TF, edata, asas)
    }
  }
  
  FR_dstchecked <- rbind(FR_TT, FR_TF)
  unique(FR_dstchecked$EAID)
  head(FR_dstchecked)
  
  ############
  #METRICS
  # length(unique(FR_dstchecked$HHID))
  # length(unique(FR_dstchecked$EAID))
  # FR_dstchecked$COUNTRY <- ifelse(grepl("ACHHTZ", FR_dstchecked$HHID), "TZ", "NG")
  # TZFR <- FR_dstchecked[FR_dstchecked$COUNTRY == "TZ", ]
  # length(unique(TZFR$HHID))
  # length(unique(TZFR$EAID))
  
 
  ##################################################################################################################################################################################
  #FR points data
  ######################################################################################
  dataVAl_FR <- read.csv("data/dataVAL_FR.csv")
  head(dataVAl_FR)
  
  
  # pointsdat <- unique(subset(dataVAl_FR, select=c(HHID, EAID,SubmissionDate, purpose.event, plantingDetails.plantingDate, gappingDetails.gappingDate, 
  #                                                 fertilizer1.dateFertilizer1, fertilizer2.dateFertilizer2,
  #                                                 weedingDetails.dateWeeding1, weedingDetails.dateWeeding2, weedingDetails.dateWeeding3, harvest.harvestDate)))
  
  
  pointsdat <- unique(subset(dataVAl_FR, select=c(HHID, EAID,SubmissionDate, event, plantingDate, gappingDate, 
                                                  dateFertilizer1, dateFertilizer2,
                                                  dateWeeding1, dateWeeding2, dateWeeding3, harvestDate)))
  
  colnames(pointsdat) <- c("HHID", "EAID", "subdate", "p_event" , "actualplantingdate", "gapping_date", "fertlizerapp1", "fertlizerapp2", "weeding1", "weeding2", "weeding3", "cassavahvstdate"  )
  pointsdat <- droplevels(unique(merge(pointsdat, dsEA, by=c("EAID"))))
  head(pointsdat)
  
  library(lubridate)	
  # ADDING column for DST run
  VAl_FRpts <- subset(VAl_FR, select = c(HHID, SubmissionDate))
  VAl_FRpts$SubmissionDate <- mdy_hms(VAl_FRpts$SubmissionDate)
  VAl_FRpts$SubmissionDate <- as.Date(as.POSIXct(VAl_FRpts$SubmissionDate, format = "%m/%d/%Y"))
  head(VAl_FRpts)
  VAl_FRpts <- VAl_FRpts[!VAl_FRpts$HHID == "", ]
  pts_valFR <- merge(pointsdat, VAl_FRpts, by=c("HHID") )
  head(pts_valFR)
  
  
  FR_runs <- NULL
  for(hhids in unique(pts_valFR$HHID)){
    hd <- pts_valFR[pts_valFR$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$p_event <- "DST run"
    FR_runs <- rbind(FR_runs, hd, hd1)
  }
  
  head(FR_runs)
  pointsdt <-FR_runs
  
  pointsdt$EA_Name<- ifelse(pointsdt$EA_Name %in% "Wargaret Asuo", "Margaret Asuo", as.character(pointsdt$EA_Name))
  pointsdt$subdate <-mdy_hms(pointsdt$subdate)
  pointsdt$subdate <- as.Date(as.POSIXct(pointsdt$subdate, format = "%m/%d/%Y"))
  pointsdt$actualmn <- lubridate::month(ymd(pointsdt$subdate))
  
  pointsdt$month  <- month.abb[pointsdt$actualmn]
  pointsdt$actuald <- lubridate::day(ymd(pointsdt$subdate))
  pointsdt$actualday <- lubridate::wday(ymd(pointsdt$subdate))
  pointsdt$actualYR <- lubridate::year(ymd(pointsdt$subdate))
  pointsdt$submn <- lubridate::month(ymd(pointsdt$SubmissionDate))
  
  pointsdt$submonth  <- month.abb[pointsdt$submn]
  pointsdt$subd <- lubridate::day(ymd(pointsdt$SubmissionDate))
  pointsdt$subday <- lubridate::wday(ymd(pointsdt$SubmissionDate))
  pointsdt$subYR <- lubridate::year(ymd(pointsdt$SubmissionDate))
  head(pointsdt)
  
  pointsdt <- unique(pointsdt)
  
  pointsdt$event1 <-as.numeric(ifelse ((!is.na(pointsdt$subdate)) &  (pointsdt$p_event == "event1"), "2", "0"))
  pointsdt$event2 <- as.numeric(ifelse ((!is.na(pointsdt$subdate)) &  (pointsdt$p_event == "event2"), "2", "0"))
  pointsdt$event3 <- as.numeric(ifelse ((!is.na(pointsdt$subdate)) &  (pointsdt$p_event == "event3"), "2", "0"))
  pointsdt$event4 <- as.numeric(ifelse ((!is.na(pointsdt$subdate)) &  (pointsdt$p_event == "event4"), "2", "0"))
  pointsdt$event5 <- as.numeric(ifelse ((!is.na(pointsdt$subdate)) &  (pointsdt$p_event == "event5"), "2", "0"))
  pointsdt$event6 <- as.numeric(ifelse ((!is.na(pointsdt$subdate)) &  (pointsdt$p_event == "event6"), "2", "0"))
  pointsdt$event7 <- as.numeric(ifelse ((!is.na(pointsdt$subdate)) &  (pointsdt$p_event == "event7"), "2", "0"))
  pointsdt$event8 <- as.numeric(ifelse ((!is.na(pointsdt$subdate)) &  (pointsdt$p_event == "event8"), "10", "0"))
  pointsdt$DST_run <- as.numeric(ifelse ((!is.na(pointsdt$SubmissionDate)) &  (pointsdt$p_event == "DST run"), "2", "0"))
  
  FRpoints <- subset(pointsdt, select = c(HHID, EAID, EA_Name, event1, event2, event3, event4, event5, event6, event7, event8, DST_run ))
  FRpoints <- droplevels(FRpoints[!FRpoints$HHID == "", ])
  FRpoints <- droplevels(FRpoints[!FRpoints$EAID == "", ])
  head(FRpoints)
  
  library(dplyr)
  FRpoints <- FRpoints %>% group_by(HHID, EAID, EA_Name) %>% summarise_at(1:9, sum)
  FRpoints <- data.table::setDT(FRpoints)[HHID!="ACHHNG123456" & HHID!="ACHHNG000000"]
  FRpoints = unique(FRpoints)
  head(FRpoints)
  
  #Sum points
  FRpoints$Totalpoints <- FRpoints$event1 + FRpoints$event2 + FRpoints$event3 + FRpoints$event4 + FRpoints$event5 + FRpoints$event6 + FRpoints$event7 + FRpoints$event8 + FRpoints$DST_run
  
  #points by month and EA_NAME
  head(pointsdt)
  dsfrpnts1 <- subset(pointsdt, select=c(EAID, actualYR, subYR, month, submonth, event1, event2, event3, event4, event5, event6, event7, event8, DST_run))
  dsfrpnts2 <- dsfrpnts1 %>% gather(events, points,event1, event2, event3, event4, event5, event6, event7, event8, DST_run)
  dsfrpnts2$month[dsfrpnts2$events == "DST_run"] <- dsfrpnts2$submonth
  dsfrpnts2$actualYR[dsfrpnts2$events == "DST_run"] <- dsfrpnts2$subYR
  head(dsfrpnts2)
  
  dsfrpnts2 <- subset(dsfrpnts2, select=c(EAID, events, actualYR, month, points))
  colnames (dsfrpnts2) <- c("EAID", "Events", "Year", "month", "points")
  dsfrpnts2 <- droplevels(dsfrpnts2[!is.na(dsfrpnts2$month), ])
  dsfrpnts3 <- dsfrpnts2 %>% mutate(i = row_number()) %>% spread(month, points)
  dsfrpnts3 <- subset(dsfrpnts3, select = -c(i))
  
  head(dsfrpnts3)
  #fix all calendar months not available in data
  Notavailmonths <- month.abb[!month.abb %in% colnames(dsfrpnts3)]
  x <-  suppressWarnings({as.data.frame(matrix(ncol=length(Notavailmonths), nrow=nrow(dsfrpnts3), data=as.numeric('NA')))})
  colnames(x)<-  Notavailmonths 
  dsfrpnts4 <- cbind(dsfrpnts3, x)
  
  #summarize points
  FRpointsdt <- ddply(dsfrpnts4, .(EAID, Events, Year), summarize,
                      Jan = sum(Jan, na.rm = TRUE),
                      Feb = sum(Feb, na.rm = TRUE),
                      Mar = sum(Mar, na.rm = TRUE),
                      Apr = sum(Apr, na.rm = TRUE),
                      May = sum(May, na.rm = TRUE),
                      Jun = sum(Jun, na.rm = TRUE),
                      Jul = sum(Jul, na.rm = TRUE),
                      Aug = sum(Aug, na.rm = TRUE),
                      Sep = sum(Sep, na.rm = TRUE),
                      Oct = sum(Oct, na.rm = TRUE),
                      Nov = sum(Nov, na.rm = TRUE),
                      Dec = sum(Dec, na.rm = TRUE))
  
  
  FRpointsdt2 <- ddply(FRpointsdt, .(EAID,Events, Year,  Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
                       summarize, Total = sum(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec))
  
  FRpointsdt3 <- ddply(FRpointsdt, .(EAID, Year),
                       summarize, Jan = sum(Jan), Feb=sum(Feb), Mar=sum(Mar), Apr=sum(Apr), May=sum(May), Jun=sum(Jun), Jul=sum(Jul), Aug=sum(Aug), 
                       Sep=sum(Sep), Oct=sum(Oct), Nov=sum(Nov), Dec=sum(Dec))
  FRpointsdt3$Events <- "Total"
  FRpointsdt3$Total <- ""
  FRpointsdt3 <- FRpointsdt3[, colnames(FRpointsdt2)]
  
  FRpointsdt2 <- FRpointsdt2[FRpointsdt2$Total >0, ]
  FRpointsdt2 <- rbind(FRpointsdt2, FRpointsdt3)
  
  FRpointsdt2 <- droplevels(FRpointsdt2[!FRpointsdt2$EAID == "", ])
  head(FRpointsdt2)
  ############################################################################################################################################
  ######################################################################################################################################
  #FR calendar
  ########################################################################################################################################
  
  dsfrwks2 <- DsFRdue
  
  #get days/weeks/months/year from actual dates
  dsfrwks2$planting1wk <- lubridate::week(ymd(dsfrwks2$plannedplantinddates))
  dsfrwks2$eventwk <- lubridate::week(ymd(dsfrwks2$datetbd2))
  
  dsfrwks2$planting1yr <- lubridate::year(ymd(dsfrwks2$plannedplantinddates))
  
  frwkhh <- dsfrwks2
  
  #deduct weeks of the year from planting week
  frwkhh$status2 <- ifelse(frwkhh$eventwk == -999, -999, frwkhh$eventwk - frwkhh$planting1wk)
  head(frwkhh)
  
  drops <- c( "DatesBnPlantingEvent", "DiffToday") 
  frwkhh <- (frwkhh[ , !(names(frwkhh) %in% drops)])
  head(frwkhh)
  
  colnames(frwkhh) <- c('HHID', 'season', 'seasonhh', 'events', 'plannedplanting', 'actualEvents', "actualDates", "EAID","EA_Name", "status", "actualmn","month","actuald", "actualday", "actualYR",   
                        "datetbd",   "datetbd2", "planting1wk", "weeks", "planting1yr", "status2")
  head(frwkhh)
  
  
  wksdtplot <- frwkhh
  wksdtplot$dueyear <- lubridate::year(wksdtplot$datetbd2)
  
  
  wksdtplot$index <- 1:nrow(wksdtplot)
  
  toBeDue <- droplevels(wksdtplot[wksdtplot$datetbd2 > Sys.Date() & wksdtplot$dueyear ==  lubridate::year(Sys.Date()), ] )
  
  
  if (nrow(toBeDue) > 0) {
    toBeDue$todayweek <-  as.numeric(lubridate::week(Sys.Date()))
    toBeDue$t2 <- as.numeric(as.character(toBeDue$weeks)) - toBeDue$todayweek
    toBeDue <- toBeDue[toBeDue$t2 <= 2, ]
    #toBeDone <- toBeDue[toBeDue$t2 > 2, ]
    if (nrow(toBeDue) > 0) {
      toBeDue$status <- 'Due soon'
      toBeDue$month <- as.factor(month.abb[lubridate::month(ymd(toBeDue$datetbd2))])
      wksdtplot <- wksdtplot[!wksdtplot$index %in% toBeDue$index, ]
      wksdtplot <- rbind(wksdtplot, toBeDue[, colnames(wksdtplot)])
    }
    # if (nrow(toBeDone) > 0) {
    #   toBeDone$status <- 'To be done'
    #   toBeDone$month <- as.factor(month.abb[lubridate::month(ymd(toBeDone$datetbd2))])
    #   wksdtplot <- wksdtplot[!wksdtplot$index %in% toBeDone$index, ]
    #   wksdtplot <- rbind(wksdtplot, toBeDone[, colnames(wksdtplot)])
    # }
    
  }
  
  wksdtplot$status <- ifelse(is.na(wksdtplot$status), "To be done", as.character(wksdtplot$status))
  wksdtplot <- droplevels(wksdtplot[!is.na(wksdtplot$HHID), ])
  wksdtplot$status <- ifelse(wksdtplot$status == "To be done" & Sys.Date() > wksdtplot$datetbd2, "Not done", as.character(wksdtplot$status))
  head(wksdtplot)
  
  
  ############################################################################################################
  #IC
  #######################################################################
  useCase = "IC"
  ID = "HHID"
  
  data_IC <- dataVAl_IC
  VAl_dataIC <- VAl_IC
  names(data_IC)[grepl('\\.', names(data_IC))] <- sub('.*\\.', '', names(data_IC)[grepl('\\.', names(data_IC))])
  #data_FR <- filterSingleSubmission(data_FR, ID, recent=TRUE) #same for HH registration submissions
  
  cleanVALIC(useCase = "IC", wd, recent=TRUE)	
  
  head(data_IC)
  ICreg_HH <- droplevels(data_IC[!data_IC$HHID %in% dsHH$HHID, ])#HHs not registered
  ICreg_EA <- droplevels(data_IC[!data_IC$EAID %in% dsEA$EAID, ])#EAs not registered
  library(lubridate)
  
  #	VAl_dataIC <- VAl_IC
  pldateissue_IC <-  unique(VAl_dataIC[, c("HHID", "plantingDate")])
  plic <- as.data.frame(table(pldateissue_IC$HHID))
  plic[plic$Freq >1, ]
  VAl_IC_dupPlDate <- droplevels(VAl_dataIC[VAl_dataIC$HHID %in% plic[plic$Freq >1, ]$Var1, ])
  VAl_dataIC <- droplevels(VAl_dataIC[!VAl_dataIC$HHID %in% plic[plic$Freq >1, ]$Var1, ])
  VAl_IC_dupPlDate <- getLatestPlDate(VAl_IC_dupPlDate)
  VAl_dataIC <- rbind(VAl_dataIC, VAl_IC_dupPlDate)
  
  ### taking latest dates per event
  dsICactual <- unique(subset(data_IC, select=c(HHID, plantingDate, maizeReseedingDate, maizeThinningDate, dateFertilizer0, dateFertilizer1, cassavaGappingDate,
                                                dateWeeding1, dateWeeding2, dateWeeding3, dateMaizeHarvest, dateCassavaHarvest )))
  colnames(dsICactual) <- c("HHID", "actualplantingdate", "reseedingdate", "thinningdate", "fertlizerapp0", "fertilizerdate1", "gapping_date",
                            "weeding1", "weeding2", "weeding3", "maizehvstdate", "cassavahvstdate")
  dsICactual$cassavahvstdate <- ifelse(is.na(dsICactual$cassavahvstdate), "", as.character(dsICactual$cassavahvstdate) )
  
  dsICactual <- EventLatestDate(usecasedata=dsICactual, usecase="IC")
  colnames(dsICactual) <- c("HHID", "actualplantingdate", "reseedingdate", "thinningdate", "fertlizerdate0", "fertilizerdate1", "gappingdateIC",
                            "weedingIC1", "weedingIC2", "weedingIC3", "maizehvstdate", "cassavahvstdate")
  
  head(dsICactual)
  dsICplant <- unique(subset(VAl_dataIC, select=c(HHID, plantingDate, season)))
  colnames(dsICplant) <- c("HHID", "Plannedplantingdate", "season")
  #convert factor to date to enable ordering
  dsICplant$f0m <- lubridate::month(mdy(dsICplant$Plannedplantingdate))
  dsICplant$f0d <- lubridate::day(mdy(dsICplant$Plannedplantingdate))
  dsICplant$f0y <- lubridate::year(mdy(dsICplant$Plannedplantingdate))
  dsICplant$f0dmy <- ifelse(is.na(dsICplant$f0m), NA, paste(dsICplant$f0y, dsICplant$f0m,  dsICplant$f0d,    sep = "/"))
  dsICplant$f0dmy <-  as.Date(dsICplant$f0dmy)
  dsICplant[is.na(dsICplant$season),]$season <- 1
  dsICplant <- dsICplant[, c(1,7,3)]
  colnames(dsICplant) <- c("HHID", "Plannedplantingdate", "season")
  head(dsICplant)
  
  #pick latest date
  dsICplant2 <- aggregate(dsICplant$Plannedplantingdate, list(dsICplant$HHID, dsICplant$season), max) 
  
  #dsICplant2 <- dsICplant[dsICplant$Plannedplantingdate %in% latest$x,]
  
  dsICplanned <- dsICplant2
  colnames(dsICplanned) <- c("HHID",  "season", "Plannedplantingdate")
  head(dsICplanned)
  #convert dates back again to factor
  
  dsICplanned$Plannedplantingdate <-  as.factor(dsICplanned$Plannedplantingdate)
  dsICplanned$f0mn <- lubridate::month(ymd(dsICplanned$Plannedplantingdate))
  
  dsICplanned$f0m <- month.abb[dsICplanned$f0mn]
  dsICplanned$f0d <- lubridate::day(ymd(dsICplanned$Plannedplantingdate))
  dsICplanned$f0y <- lubridate::year(ymd(dsICplanned$Plannedplantingdate))
  dsICplanned$f0sep <- paste( dsICplanned$f0d, "," ,  sep = "")
  
  dsICplanned$f0ymd <- ifelse(is.na(dsICplanned$f0m), NA, paste(dsICplanned$f0m,  dsICplanned$f0sep, dsICplanned$f0y, sep = " "))
  
  
  dsICplanned <- dsICplanned[, c(1,2,9)]
  colnames(dsICplanned) <- c("HHID", "season", "Plannedplantingdate")
  
  dsICheatmap <- droplevels(unique(merge(dsICplanned, dsICactual, by=c("HHID"))))
  
  dsICheatmap$Plannedplantingdate <- as.factor(dsICheatmap$Plannedplantingdate)
  
  
  # # dsICplanned <- unique(subset(VAl_dataIC, select=c(HHID, plantingDate)))
  # # colnames(dsICplanned) <- c("HHID", "Plannedplantingdate")
  # # dsICheatmap <- droplevels(unique(merge(dsICplanned, dsICactual, by=c("HHID"))))
  # 
  # head(dsICheatmap)
  # #VAl_dataIC$HHID <- paste(VAl_dataIC$HHID, ifelse(VAl_dataIC$maizeVarietySelect=="Ikom_White", "_Ikom_White", ""), sep="")
  # 
  # dsICplanned <- unique(subset(VAl_dataIC, select=c(HHID, plantingDate, season)))
  # colnames(dsICplanned) <- c("HHID", "Plannedplantingdate", "season")
  # dsICplanned[is.na(dsICplanned$season),]$season <- 1 
  # dsICheatmap <- droplevels(unique(merge(dsICplanned, dsICactual, by=c("HHID"))))
  
  head(dsICheatmap)
  dsICheatmap$actualplantingdate <- as.character(dsICheatmap$actualplantingdate)
  
  #replace missing actual planting dates with planned planting date
  dsICheatmaphid1 <- NULL
  for( hids in unique(dsICheatmap$HHID)){
    dsICheatmaphid <- droplevels(dsICheatmap[dsICheatmap$HHID == hids, ])
    if(all(unique(dsICheatmaphid$actualplantingdate) == '')){
      dsICheatmaphid$actualplantingdate <- ifelse(dsICheatmaphid$actualplantingdate == '', as.character(dsICheatmaphid$Plannedplantingdate), as.character(dsICheatmaphid$actualplantingdate))
    }else{
      
      dsICheatmaphid$actualplantingdate <- as.character(unique(dsICheatmaphid[dsICheatmaphid$actualplantingdate !='', ]$actualplantingdate))
    }
    dsICheatmaphid1 <- rbind(dsICheatmaphid1, dsICheatmaphid)
  }
  dsICheatmap <- dsICheatmaphid1
  
  head(dsICheatmap)
  
  unique(dsICheatmap$HHID)
  
  seasoninfo <- subset(dsICheatmap, select = c("HHID", "season"))
  #dsICheatmap$Plannedplantingdate <- as.POSIXlt(dsICheatmap$Plannedplantingdate, format="%b %d, %Y %H:%M:%S", tz="GMT")
  
  #solve dates into days of the year
  plannedIC <- solvedates(colNr = 3, usecasedata=dsICheatmap)
  # colnames(Plannedplantingdate) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'Plannedplantingdate', 'plndOrigdate')
  # plannedIC<- subset(Plannedplantingdate, select = c(HHID,Plannedplantingdate,plndOrigdate ))
  head(plannedIC)
  
  actualIC <- solvedates(colNr = 4 , usecasedata=dsICheatmap)
  # colnames(actualplantingdateIC) <- c('HHID', 'month', 'date', 'year', 'leapyear',  'actualplantingdate', 'actualOrigdate')
  # actualIC<- subset(actualplantingdateIC, select = c(HHID,actualplantingdate,actualOrigdate ))
  head(actualIC)
  
  reseedIC <- solvedates(colNr = 5, usecasedata=dsICheatmap)
  # colnames(reseed_date) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'reseedingdate', 'Origreseedate')
  # reseedIC<- subset(reseed_date, select = c(HHID,reseedingdate, Origreseedate ))
  head(reseedIC)
  
  thinningIC <- solvedates(colNr = 6, usecasedata=dsICheatmap)
  # colnames(thinning_date) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'thinning_dateIC', 'Origthinning_date')
  # thinningIC<- subset(thinning_date, select = c(HHID,thinning_dateIC, Origthinning_date ))
  head(thinningIC)
  
  fert0IC <- solvedates(colNr = 7, usecasedata=dsICheatmap)
  head(fert0IC)
  
  fert1IC <- solvedates(colNr = 8, usecasedata=dsICheatmap)
  
  head(fert1IC)
  
  gapIC <- solvedates(colNr = 9, usecasedata=dsICheatmap)
  
  head(gapIC)
  
  weedIC1 <- solvedates(colNr = 10, usecasedata=dsICheatmap)
  
  
  head(weedIC1)
  
  weedIC2 <- solvedates(colNr = 11, usecasedata=dsICheatmap)
  
  head(weedIC2)
  
  weedIC3 <- solvedates(colNr = 12, usecasedata=dsICheatmap)
  
  head(weedIC3)
  
  maizehvstdater <- solvedates(colNr = 13, usecasedata=dsICheatmap)
  # colnames(maizehvstdate) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'maizehvstdate', 'Origmzaharvestdate')
  # maizehvstdater<- subset(maizehvstdate, select = c(HHID, maizehvstdate, Origmzaharvestdate ))
  head(maizehvstdater)
  str(maizehvstdater)
  
  cassavahvstdate <- solvedates(colNr = 14, usecasedata=dsICheatmap)
  
  
  #merge solved days of the year
  ic <- unique(merge(seasoninfo, actualIC, by="HHID"), all=TRUE)
  ic1 <- unique(merge(ic, plannedIC, by="HHID"), all=TRUE)
  ic2 <- unique(merge(ic1, reseedIC, by='HHID', all=TRUE))
  ic3 <- unique(merge(ic2, thinningIC, by='HHID', all=TRUE))
  ic4 <- unique(merge(ic3, gapIC, by='HHID', all=TRUE))
  ic5 <- unique(merge(ic4, fert0IC, by='HHID', all=TRUE))
  ic6 <- unique(merge(ic5, weedIC1, by='HHID', all=TRUE))
  ic7 <- unique(merge(ic6, weedIC2, by='HHID', all=TRUE))
  ic8 <- unique(merge(ic7, weedIC3, by='HHID', all=TRUE))
  
  ic9 <- unique(merge(ic8, maizehvstdater, by='HHID', all=TRUE))
  ic10 <- unique(merge(ic9, cassavahvstdate, by='HHID', all=TRUE))
  
  
  # if(!is.null(cassavahvstdate)){
  # 	dsICheatmapdates <- unique(merge(ic9, cassavahvstdate, by='HHID', all=TRUE))
  # }else{
  # 	dsICheatmapdates <- ic9
  # }
  
  dsICheatmapdates <- ic10
  
  unique(dsICheatmapdates$HHID)
  head(dsICheatmapdates)
  #dsICheatmapdates <- (Reduce(function(x, y) merge(x, y, all=TRUE),list(actualIC, plannedIC, reseedIC, thinningIC, gapIC, fert1IC, weedIC1, weedIC2, weedIC3, maizehvstdater, by="HHID"))) #replace harvestdateic
  
  #deduct solved days of the year against actual planting dates
  dsICheatmapdates$planting <- as.numeric(dsICheatmapdates$actualplantingdate) - as.numeric(dsICheatmapdates$Plannedplantingdate)
  dsICheatmapdates$reseedng <- as.numeric(dsICheatmapdates$reseedingdate) - as.numeric(dsICheatmapdates$actualplantingdate)
  dsICheatmapdates$thnning <- as.numeric(dsICheatmapdates$thinningdate) - as.numeric(dsICheatmapdates$actualplantingdate)
  dsICheatmapdates$gappng <- as.numeric(dsICheatmapdates$gappingdateIC) - as.numeric(dsICheatmapdates$actualplantingdate)
  dsICheatmapdates$fertlzer1 <- as.numeric(dsICheatmapdates$fertlizerdate0) - as.numeric(dsICheatmapdates$actualplantingdate)
  dsICheatmapdates$weedn1 <- as.numeric(dsICheatmapdates$weedingIC1) - as.numeric(dsICheatmapdates$actualplantingdate)
  dsICheatmapdates$weedn2 <- as.numeric(dsICheatmapdates$weedingIC2) - as.numeric(dsICheatmapdates$actualplantingdate)
  dsICheatmapdates$weedn3 <- as.numeric(dsICheatmapdates$weedingIC3) - as.numeric(dsICheatmapdates$actualplantingdate)
  dsICheatmapdates$maizeharvest <- as.numeric(dsICheatmapdates$maizehvstdate) - as.numeric(dsICheatmapdates$actualplantingdate)
  dsICheatmapdates$cassharvest <- as.numeric(dsICheatmapdates$cassavahvstdate) - as.numeric(dsICheatmapdates$actualplantingdate)
  # 
  
  #get EA names from dsEAHH
  # ROR <- dataVAl_IC[!dataVAl_IC$HHID_VAR == "NA", ]
  # dsEAHH$HHID <- ifelse(dsEAHH$HHID %in% ROR$HHID, ROR$HHID_VAR, as.character(dsEAHH$HHID))
  # dsEAHH$region_state<-gsub("NG.", "", dsEAHH$region_state)
  # dsEAHH$region_state<-gsub("TZ.", "", dsEAHH$region_state)
  # unique(dsEAHH$HHID)
  # 
  # DFR <- dsICheatmapdates[!dsICheatmapdates$HHID %in% dsEAHH$HHID, ]
  # 
  dsICheatmap2 <- droplevels(unique(merge(dsICheatmapdates, dsEAHH, by=c("HHID"))))
  unique(dsICheatmap2$HHID)
  unique(dsICheatmap2$EAID)
  head(dsICheatmap2)
  drops <- c("EA_PhoneNr", "EA_Partner", "HH_Name", "HH_PhoneNr", "useCase", "Latitude", "Longitude", "Country", "region_state")
  dsIC <- (dsICheatmap2[ , !(names(dsICheatmap2) %in% drops)])
  
  #identify activities not done
  dsIC$reseeding <- ifelse(is.na(dsIC$reseedng) & (!is.na(dsIC$thnning) | !is.null(dsIC$gappng)), -999, dsIC$reseedng)
  dsIC$thinning <- ifelse(is.na(dsIC$thnning) & (!is.null(dsIC$gappng) | !is.null(dsIC$fertlzer1)) , -999, dsIC$thinning)
  dsIC$gapping <-  ifelse(is.null(dsIC$gappng) & (!is.null(dsIC$fertlzer1) | !is.null(dsIC$weedn1)), -999, dsIC$gappng)
  dsIC$fertilizer0 <-  ifelse(is.null(dsIC$fertlzer1) & (!is.null(dsIC$weedn1) | !is.null(dsIC$weedn2)), -999, dsIC$fertlzer1)
  dsIC$weeding1 <-  ifelse(is.null(dsIC$weedn1) & (!is.null(dsIC$weedn2) | !is.null(dsIC$weedn3)), -999, dsIC$weedn1)
  dsIC$weeding2 <-  ifelse(is.null(dsIC$weedn2) & (!is.null(dsIC$weedn3) | !is.na(dsIC$maizeharvest)), -999, dsIC$weedn2)
  dsIC$weeding3 <- ifelse(is.na(dsIC$weedn3) & !is.na(dsIC$maizeharvest), -999, dsIC$weedn3)
  #dsIC$weeding3 <- ifelse(is.na(dsIC$weedn3) & !is.na(dsIC$maizeharvest), -999, dsIC$weedn3)
  
  head(dsIC)
  
  seasoninfo <- subset(dsIC, select = c("HHID", "season"))
  unique(dsIC$HHID)
  drops <- c( "reseedng", "thnning", "gappng", "fertlzer1", "weedn1", "weedn2", "weedn3")
  dsIC2 <- (dsIC[ , !(names(dsIC) %in% drops)])
  
  #reshape actual dates
  
  dsICss1 <- subset(dsIC2, select=c(HHID, actualplantingdate, Plannedplantingdate, reseedingdate, thinningdate, gappingdateIC, fertlizerdate0, weedingIC1, weedingIC2, weedingIC3,
                                    maizehvstdate,cassavahvstdate
  ))
  colnames(dsICss1) <- c('HHID', 'actualplanneddate', 'actualplantingdate', 'actualreseeding', 'actualthinning', 'actualgapping', 'fertilizer0date',
                         'actualweeding1', 'actualweeding2', 'actualweeding3', 'actualmaizeharvest', 'actualcassharvest')
  dsICss2 <- subset(dsIC2, select=c(HHID, EAID,  EA_Name, season, planting, reseeding, thinning, gapping, fertilizer0, weeding1, weeding2,
                                    weeding3, maizeharvest, cassharvest))
  
  dsICRss1 <- suppressWarnings({dsICss1 %>% gather(actualEvents, actualDates, actualplantingdate, actualreseeding, actualthinning, actualgapping, fertilizer0date, actualweeding1, 
                                                   actualweeding2, actualweeding3, actualmaizeharvest, actualcassharvest)})
  
  dsICss2 <- dsICss2[, c("HHID","EAID", "EA_Name", "season", "planting","reseeding", "thinning", "gapping", "fertilizer0", "weeding1", 
                         "weeding2", "weeding3", "maizeharvest", "cassharvest")]
  dsICRss2 <- dsICss2 %>% gather(events, dates, planting:cassharvest)
  dsICRss2$dates <- as.numeric(dsICRss2$dates)
  
  #merge actual dates and solved dates
  dsICRss1$events <- gsub("actual", "", dsICRss1$actualEvents)
  
  dsICRss1$events <- gsub("date", "", dsICRss1$events)
  dsICreshape <- merge(unique(dsICRss1), dsICRss2, by=c("HHID", "events"))
  dim(dsICreshape)
  
  
  dsICreshape = unique(dsICreshape)
  head(dsICreshape)
  unique(dsICreshape$HHID)
  
  #dsICreshape$diffdate <- today()-anytime::anydate(as.character(dsICreshape$actualDates))
  names(dsICreshape) <- c('HHID', 'events', 'plannedplantingdate' , 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 'season', 'DatesBnPlantingEvent')
  ichhdata <- dsICreshape
  ichhdata$actualDates <- as.Date((ichhdata$actualDates), origin = "1970-01-01")
  
  
  head(ichhdata)
  #get status of events
  
  geticstatus<- NULL
  for(h in unique(ichhdata$HHID)){
    ichhdatass <- subset(ichhdata, HHID == h)
    if(!all(is.na(ichhdatass$DatesBnPlantingEvent))){
      #ichhdatass <- replace(ichhdatass, is.na(ichhdatass), -100)
      
      
      ichhdatass$status <- ifelse(ichhdatass$events %in% c("planting")  & ichhdatass$DatesBnPlantingEvent  == 0, "On-time",
                                  ifelse(ichhdatass$events %in% c("planting")  & ichhdatass$DatesBnPlantingEvent  < 0 & !ichhdatass$DatesBnPlantingEvent == -999, "Earlier",
                                         ifelse(ichhdatass$events %in% c("planting")  & ichhdatass$DatesBnPlantingEvent  > 0, "Done later",
                                                ifelse(ichhdatass$events %in% c("planting")  & ichhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                       ifelse(ichhdatass$events %in% c("reseeding") & ichhdatass$DatesBnPlantingEvent < 21  & ichhdatass$DatesBnPlantingEvent > -50, "Earlier",
                                                              ifelse(ichhdatass$events %in% c("reseeding") & ichhdatass$DatesBnPlantingEvent  == 21 , "On-time",
                                                                     ifelse(ichhdatass$events %in% c("reseeding") & ichhdatass$DatesBnPlantingEvent > 21 , "Done later",
                                                                            ifelse(ichhdatass$events %in% c("reseeding")  & ichhdatass$DatesBnPlantingEvent  == -999, "Not done",                               
                                                                                   ifelse(ichhdatass$events %in% c("reseeding")  & ichhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                          
                                                                                          ifelse(ichhdatass$events %in% c("thinning") & ichhdatass$DatesBnPlantingEvent < 21  & !ichhdatass$DatesBnPlantingEvent == -999, "Earlier",
                                                                                                 ifelse(ichhdatass$events %in% c("thinning") & ichhdatass$DatesBnPlantingEvent  == 21 , "On-time",
                                                                                                        ifelse(ichhdatass$events %in% c("thinning") & ichhdatass$DatesBnPlantingEvent > 21 , "Done later",
                                                                                                               ifelse(ichhdatass$events %in% c("thinning")  & ichhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                                                                                      ifelse(ichhdatass$events %in% c("thinning")  & ichhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                             
                                                                                                                             ifelse(ichhdatass$events %in% c("gapping") & ichhdatass$DatesBnPlantingEvent < 28  & !ichhdatass$DatesBnPlantingEvent == -999, "Earlier",
                                                                                                                                    ifelse(ichhdatass$events %in% c("gapping") & ichhdatass$DatesBnPlantingEvent  == 28 , "On-time",
                                                                                                                                           ifelse(ichhdatass$events %in% c("gapping") & ichhdatass$DatesBnPlantingEvent > 28 , "Done later",
                                                                                                                                                  ifelse(ichhdatass$events %in% c("gapping")  & ichhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                                                                                                                         ifelse(ichhdatass$events %in% c("gapping")  & ichhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                
                                                                                                                                                                ifelse(ichhdatass$events %in% c("fertilizer0") & ichhdatass$DatesBnPlantingEvent < 35  & !ichhdatass$DatesBnPlantingEvent == -999, "Earlier",
                                                                                                                                                                       ifelse(ichhdatass$events %in% c("fertilizer0") & ichhdatass$DatesBnPlantingEvent  == 35, "On-time",
                                                                                                                                                                              ifelse(ichhdatass$events %in% c("fertilizer0") & ichhdatass$DatesBnPlantingEvent > 35 , "Done later",
                                                                                                                                                                                     ifelse(ichhdatass$events %in% c("fertilizer0")  & ichhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                                                                                                                                                            ifelse(ichhdatass$events %in% c("fertilizer0")  & ichhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                                   
                                                                                                                                                                                                   
                                                                                                                                                                                                   ifelse(ichhdatass$events %in% c("weeding1") & ichhdatass$DatesBnPlantingEvent >= 0, "On-time",
                                                                                                                                                                                                          ifelse(ichhdatass$events %in% c("weeding1")  & ichhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                                                 ifelse(ichhdatass$events %in% c("weeding1")  & ichhdatass$DatesBnPlantingEvent  == -999, "Not done",  	
                                                                                                                                                                                                                        ifelse(ichhdatass$events %in% c("weeding2") & ichhdatass$DatesBnPlantingEvent >= 0, "On-time",
                                                                                                                                                                                                                               ifelse(ichhdatass$events %in% c("weeding2") & ichhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                                                                      ifelse(ichhdatass$events %in% c("weeding2")  & ichhdatass$DatesBnPlantingEvent  == -999, "Not done",                                                                                                                                                                                                                                
                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                             ifelse(ichhdatass$events %in% c("weeding3") & ichhdatass$DatesBnPlantingEvent >= 0, "On-time",
                                                                                                                                                                                                                                                    ifelse(ichhdatass$events %in% c("weeding3")  & ichhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                                                                                           ifelse(ichhdatass$events %in% c("weeding3")  & ichhdatass$DatesBnPlantingEvent  == -999, "Not done",                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                  ifelse(ichhdatass$events %in% c("maizeharvest") & ichhdatass$DatesBnPlantingEvent  >= 0 & ichhdatass$DatesBnPlantingEvent > -50, "On-time",
                                                                                                                                                                                                                                                                         ifelse(ichhdatass$events %in% c("maizeharvest")  & ichhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                ifelse(ichhdatass$events %in% c("cassharvest") & ichhdatass$DatesBnPlantingEvent > 0 & ichhdatass$DatesBnPlantingEvent > -50, "On-time",
                                                                                                                                                                                                                                                                                       ifelse(ichhdatass$events %in% c("cassharvest")  & ichhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                              as.character(ichhdatass$status))))))))))))))))))))))))))))))))))))))
      
      ichhdatass <- replace(ichhdatass, ichhdatass == -100, NA)
      geticstatus <- rbind(geticstatus, ichhdatass)
    }
  }
  
  ichhdata <- geticstatus
  
  head(ichhdata)
  ichhdata$status <- ifelse(is.na(ichhdata$DatesBnPlantingEvent), "To be done", as.character(ichhdata$status))
  ichhdata$status <- ifelse(ichhdata$DatesBnPlantingEvent == "", "To be done", as.character(ichhdata$status))
  #calculating due dates for IC
  
  #ichhdata$actualDates <- as.Date(ichhdata$actualDates) 
  ichhdata$actualmn <- lubridate::month(ymd(ichhdata$actualDates))
  
  ichhdata$month  <- month.abb[ichhdata$actualmn]
  ichhdata$actuald <- lubridate::day(ymd(ichhdata$actualDates))
  ichhdata$actualday <- lubridate::wday(ymd(ichhdata$actualDates))
  ichhdata$actualYR <- lubridate::year(ymd(ichhdata$actualDates))
  head(ichhdata)
  
  ichhdata$datetbd <- ifelse(!is.na(ichhdata$actuald), paste(ichhdata$actualYR, ichhdata$actualmn, ichhdata$actuald, sep = "/"), NA)
  
  ichhdata$datetbd2 <- as.Date(ichhdata$datetbd) 
  
  
  DsICdue <- NULL
  for(hids in unique(ichhdata$HHID)){
    hd <- ichhdata[ichhdata$HHID == hids, ]
    hdpl <- hd[hd$events == 'planting', ]
    
    hdG <- hd[hd$events == 'gapping', ]
    if(is.na(hdG$datetbd2)){
      hdG$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 28
    }
    
    hdT <- hd[hd$events == 'thinning', ]
    if(is.na(hdT$datetbd2)){
      hdT$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 21
    }
    hdR <- hd[hd$events == 'reseeding', ]
    if(is.na(hdR$datetbd2)){
      hdR$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 21
    }
    # 
    hdF1 <- hd[hd$events == 'fertilizer0', ]
    if(is.na(hdF1$datetbd2)){
      hdF1$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 21
    }
    # 
    # hdF2 <- hd[hd$events == 'fertilizer2', ]
    # if(is.na(hdF2$datetbd2)){
    #   hdF2$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 35
    #   
    # }
    hdW1 <- hd[hd$events == 'weeding1', ]
    if(is.na(hdW1$datetbd2)){
      hdW1$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 35
    }
    
    hdW2 <- hd[hd$events == 'weeding2', ]
    if(is.na(hdW2$datetbd2)){
      hdW2$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 168
    }
    
    hdW3 <- hd[hd$events == 'weeding3', ]
    if(is.na(hdW3$datetbd2)){
      hdW3$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 252
    }
    
    hdharvM <- hd[hd$events == 'maizeharvest', ]
    if(is.na(hdharvM$datetbd2)){
      hdharvM$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 70
    }
    
    
    
    hdharvC <- hd[hd$events == 'cassharvest', ]
    
    DsICdue <- suppressWarnings({rbind(DsICdue, hdpl, hdT, hdR, hdG,hdF1, hdW1,hdW2,hdW3, hdharvM, hdharvC)})
  }
  
  names(DsICdue) <- c('HHID', 'Events', 'plannedplanting', 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 'season', 'DatesBnPlantingEvent', 'status', 
                      'actualmn', 'month', 'actuald', 'actualday', 'actualYR',   'datetbd',   'datetbd2')
  
  #DsICdue <- merge(unique(DsICdue), seasoninfo, by=c("HHID"))
  
  
  head(DsICdue)
  
  DsICdue$status <- ifelse(is.na(DsICdue$DatesBnPlantingEvent), "To be done", as.character(DsICdue$status))
  DsICdue$status <- ifelse(DsICdue$status == "To be done"& DsICdue$datetbd2 < Sys.Date(), "Overdue", as.character(DsICdue$status))
  DsICdue <- droplevels(DsICdue[!is.na(DsICdue$HHID), ] )
  DsICdue$status <- ifelse(is.na(DsICdue$DatesBnPlantingEvent), "To be done", as.character(DsICdue$status))
  
  # ADDING column for DST run
  DsICdue$datetbd2 <- as.character(DsICdue$datetbd2)
  
  hhidIC_TT <- droplevels(DsICdue[DsICdue$HHID %in% VAl_dataIC$HHID, ])
  hhidIC_TF <- droplevels(VAl_dataIC[!VAl_dataIC$HHID %in% DsICdue$HHID, ])## dst is run but no data is submitted
  hhidIC_FT <- droplevels(DsICdue[!DsICdue$HHID %in% VAl_dataIC$HHID, ])## dst is not run but there is data
  
  IC_TT <- NULL
  for(hhids in unique(hhidIC_TT$HHID)){
    hd <- hhidIC_TT[hhidIC_TT$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "True"
    hd1$Events <- "DST run"
    hd1$status <- "To be done"
    IC_TT <- rbind(IC_TT, hd, hd1)
  }
  
  
  
  # eaids <- "ACEANG000123"
  IC_TF <- NULL
  for(eaids in unique(hhidIC_TF$EAID)){
    edata <- DsICdue[DsICdue$EAID == h, ]
    hh_nodata <- unique(hhidIC_TF[hhidIC_TF$EAID == eaids, ]$HHID)
    if(length(hh_nodata) > 0){
      asas <- edata[1:length(hh_nodata),]
      asas$HHID <- hh_nodata
      asas$Events <- "DST run"
      asas$actualEvents <- "DST run"
      asas$status <- "To be done"
      asas$datetbd2 <- "True"
      IC_TF <- rbind(IC_TF, edata, asas)
    }
  }
  
  IC_dstchecked <- rbind(IC_TT, IC_TF)
  IC_dstchecked <- unique(IC_dstchecked)
  
  ############
  #METRICS
  # length(unique(IC_dstchecked$HHID))
  # length(unique(IC_dstchecked$EAID))
  # IC_dstchecked$COUNTRY <- ifelse(grepl("ACHHTZ", IC_dstchecked$HHID), "TZ", "NG")
  # TZIC <- IC_dstchecked[IC_dstchecked$COUNTRY == "TZ", ]
  # length(unique(TZIC$HHID))
  # length(unique(TZIC$EAID))

  
  
  ####################################################################################### 
  #IC calendar
  
  dsicwks2 <- DsICdue
  
  #get days/weeks/months/year from actual dates 
  dsicwks2$planting1wk <- lubridate::week(ymd(dsicwks2$plannedplanting))
  dsicwks2$eventwk <- lubridate::week(ymd(dsicwks2$datetbd2))
  
  dsicwks2$planting1yr <- lubridate::year(ymd(dsicwks2$plannedplanting))
  
  icwkhh <- dsicwks2
  
  ##deduct weeks of the year from planting week
  icwkhh$status2 <- ifelse(icwkhh$eventwk == -999, -999, icwkhh$eventwk - icwkhh$planting1wk)
  head(icwkhh)
  
  # icwkhh[icwkhh$EAID== 'ACEANG000037' & icwkhh$HHID == 'ACHHNG001554',]
  
  drops <- c( "DatesBnPlantingEvent", "DiffToday") 
  icwkhh <- (icwkhh[ , !(names(icwkhh) %in% drops)])
  head(icwkhh)
  
  colnames(icwkhh) <- c('HHID', 'events', 'plannedplanting', 'actualEvents', "actualDates", "EAID","EA_Name", "season", "status", "actualmn","month","actuald", "actualday", "actualYR",   
                        "datetbd",   "datetbd2", "planting1wk", "weeks", "planting1yr", "status2")
  
  icwksdata <- icwkhh
  
  #get due soon dates
  icwksdata$dueyear <- lubridate::year(icwksdata$datetbd2)
  # icwksdata$diffsys <- icwksdata$datetbd2 > Sys.Date() & icwksdata$dueyear ==  lubridate::year(Sys.Date())
  # icwksdata$status <- ifelse(icwksdata$diffsys == 'TRUE', "Due soon", as.character(icwksdata$status))
  icwksdata$index <- 1:nrow(icwksdata)
  
  toBeDue <- droplevels(icwksdata[icwksdata$datetbd2 > Sys.Date() & icwksdata$dueyear ==  lubridate::year(Sys.Date()), ] )
  
  
  if (nrow(toBeDue) > 0) {
    toBeDue$todayweek <-  as.numeric(lubridate::week(Sys.Date()))
    toBeDue$t2 <- as.numeric(as.character(toBeDue$weeks)) - toBeDue$todayweek
    toBeDue <- toBeDue[toBeDue$t2 <= 2, ]
    toBeDue <- droplevels(toBeDue[!is.na(toBeDue$HHID), ])
    toBeDone <- toBeDue[toBeDue$t2 > 2, ]
    
    if (nrow(toBeDue) > 0) {
      toBeDue$status <- 'Due soon'
      toBeDue$month <- as.factor(month.abb[lubridate::month(ymd(toBeDue$datetbd2))]) 
      icwksdata <- icwksdata[!icwksdata$index %in% toBeDue$index, ]
      icwksdata <- rbind(icwksdata, toBeDue[, colnames(icwksdata)]) 
    }
    # if (nrow(toBeDone) > 0) {
    #    toBeDone$status <- 'To be done'
    #   toBeDone$month <- as.factor(month.abb[lubridate::month(ymd(toBeDone$datetbd2))]) 
    #   icwksdata <- icwksdata[!icwksdata$index %in% toBeDone$index, ]
    #    icwksdata <- rbind(icwksdata, toBeDone[, colnames(icwksdata)]) 
    #  }
    
  }
  
  
  icwksdata <- droplevels(icwksdata[!is.na(icwksdata$HHID), ])
  icwksdata$status <- ifelse(icwksdata$status == "To be done" & Sys.Date() > icwksdata$datetbd2, "Not done", as.character(icwksdata$status))
  icwksdata$status <- ifelse(is.na(icwksdata$status), "To be done", as.character(icwksdata$status))
  head(icwksdata)
  
  
  ########################################################################################
  #ICPoints
  #######################################################################################
  pointdVAl_IC <- data_IC
  pointdVAl_IC <- droplevels(pointdVAl_IC[pointdVAl_IC$HHID %in% IC_dstchecked$HHID, ])
  
  pointsicdat <- unique(subset(pointdVAl_IC, select=c(HHID, EAID, SubmissionDate, event, plantingDate, maizeReseedingDate, maizeThinningDate, 
                                                      dateFertilizer1, cassavaGappingDate, dateWeeding1, dateWeeding2, dateWeeding3, 
                                                      dateMaizeHarvest, dateCassavaHarvest )))
  
  colnames(pointsicdat) <- c("HHID", "EAID", "subdate", "p_event", "actualplantingdate", "reseedingdate", "thinningdate", "fertlizerapp1", 
                             "gapping_date", "weeding1", "weeding2", "weeding3", "maizehvstdate", "cassavahvstdate")
  
  
  pointsicdat <- droplevels(unique(merge(pointsicdat, dsEA, by=c("EAID"))))
  
  library(lubridate)	
  # ADDING column for DST run
  VAl_ICpts <- subset(VAl_dataIC, select = c(HHID, SubmissionDate))
  VAl_ICpts$SubmissionDate <- mdy_hms(VAl_ICpts$SubmissionDate)
  VAl_ICpts$SubmissionDate <- as.Date(as.POSIXct(VAl_ICpts$SubmissionDate, format = "%m/%d/%Y"))
  head(VAl_ICpts)
  pts_valIC <- merge(pointsicdat, VAl_ICpts, by=c("HHID") )
  
  
  IC_runs <- NULL
  for(hhids in unique(pts_valIC$HHID)){
    hd <- pts_valIC[pts_valIC$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$p_event <- "DST run"
    IC_runs <- rbind(IC_runs, hd, hd1)
  }
  
  head(IC_runs)
  
  pointsic <-IC_runs
  
  pointsic$EA_Name<- ifelse(pointsic$EA_Name %in% "Wargaret Asuo", "Margaret Asuo", as.character(pointsic$EA_Name))
  pointsic$subdate <-mdy_hms(pointsic$subdate)
  pointsic$subdate <- as.Date(as.POSIXct(pointsic$subdate, format = "%m/%d/%Y"))
  pointsic$actualmn <- lubridate::month(ymd(pointsic$subdate))
  pointsic$month  <- month.abb[pointsic$actualmn]
  pointsic$actuald <- lubridate::day(ymd(pointsic$subdate))
  pointsic$actualday <- lubridate::wday(ymd(pointsic$subdate))
  pointsic$actualYR <- lubridate::year(ymd(pointsic$subdate))
  head(pointsic)
  
  pointsic$submn <- lubridate::month(ymd(pointsic$SubmissionDate ))
  pointsic$submonth  <- month.abb[pointsic$submn]
  pointsic$subd <- lubridate::day(ymd(pointsic$SubmissionDate ))
  pointsic$subday <- lubridate::wday(ymd(pointsic$SubmissionDate ))
  pointsic$subYR <- lubridate::year(ymd(pointsic$SubmissionDate ))
  head(pointsic)
  
  pointsic <- unique(pointsic)
  
  pointsic$event1 <-as.numeric(ifelse ((!is.na(pointsic$subdate)) &  (pointsic$p_event == "event1"), "2", "0"))
  pointsic$event2 <- as.numeric(ifelse ((!is.na(pointsic$subdate)) &  (pointsic$p_event == "event2"), "2", "0"))
  pointsic$event3 <- as.numeric(ifelse ((!is.na(pointsic$subdate)) &  (pointsic$p_event == "event3"), "2", "0"))
  pointsic$event4 <- as.numeric(ifelse ((!is.na(pointsic$subdate)) &  (pointsic$p_event == "event4"), "2", "0"))
  pointsic$event5 <- as.numeric(ifelse ((!is.na(pointsic$subdate)) &  (pointsic$p_event == "event5"), "9", "0"))
  pointsic$event6 <- as.numeric(ifelse ((!is.na(pointsic$subdate)) &  (pointsic$p_event == "event6"), "2", "0"))
  pointsic$event7 <- as.numeric(ifelse ((!is.na(pointsic$subdate)) &  (pointsic$p_event == "event7"), "2", "0"))
  pointsic$event8 <- as.numeric(ifelse ((!is.na(pointsic$subdate)) &  (pointsic$p_event == "event8"), "10", "0"))
  pointsic$DST_run <- as.numeric(ifelse ((!is.na(pointsic$SubmissionDate )) &  (pointsic$p_event == "DST run"), "2", "0"))
  
  
  pointsEVENT <- subset(pointsic, select = c(HHID, EAID, EA_Name, event1, event2, event3, event4, event5, event6, event7, event8, DST_run ))
  head(pointsEVENT)
  
  ICpoints <- pointsEVENT %>% group_by(HHID, EAID, EA_Name) %>% summarise_at(1:9, sum)
  ICpoints <- data.table::setDT(ICpoints)[HHID!="ACHHNG123456" & HHID!="ACHHNG000000"]
  ICpoints2 = unique(ICpoints)
  
  ICpoints2$Totalpoints <- ICpoints2$event1+ICpoints2$event2+ICpoints2$event3+ ICpoints2$event4+ICpoints2$event5+ICpoints2$event6 +ICpoints2$event7 +ICpoints2$event8 +ICpoints2$DST_run
  
  head(ICpoints2)#ICpoints <- subset(ICpoints, select= c(HHID, EAID, EA_Name,planting, event2, event3,weeding2, weeding3, harvestM,harvestC, Totalpoints))
  dsICpnts1 <- subset(pointsic, select=c(EAID, EAID, actualYR, subYR, month, submonth, event1, event2, event3, event4, event5, event6, event7, event8, DST_run))
  dsICpnts2 <- dsICpnts1 %>% gather(events, points,event1, event2, event3, event4, event5, event6, event7, event8, DST_run)
  dsICpnts2$month[dsICpnts2$events == "DST_run"] <- dsICpnts2$submonth
  dsICpnts2$actualYR[dsICpnts2$events == "DST_run"] <- dsICpnts2$subYR
  
  head(dsICpnts2)
  
  dsICpnts2 <- subset(dsICpnts2, select=c(EAID, events, actualYR, month, points))
  colnames (dsICpnts2) <- c("EAID", "Events", "Year", "month", "points")
  
  dsICpnts2 <- droplevels(dsICpnts2[!is.na(dsICpnts2$month), ])
  
  dsICpnts3 <- dsICpnts2 %>% mutate(i = row_number()) %>% spread(month, points)
  dsICpnts3 <- subset(dsICpnts3, select = -c(i))
  
  head(dsICpnts3)
  #fix all calendar months not available in data
  Notavailmonths <- month.abb[!month.abb %in% colnames(dsICpnts3)]
  x <-  suppressWarnings({as.data.frame(matrix(ncol=length(Notavailmonths), nrow=nrow(dsICpnts3), data=as.numeric('NA')))})
  colnames(x)<-  Notavailmonths 
  dsICpnts4 <- cbind(dsICpnts3, x)
  
  #summarize points
  ICpointsdt <- ddply(dsICpnts4, .(EAID, Events, Year), summarize,
                      Jan = sum(Jan, na.rm = TRUE),
                      Feb = sum(Feb, na.rm = TRUE),
                      Mar = sum(Mar, na.rm = TRUE),
                      Apr = sum(Apr, na.rm = TRUE),
                      May = sum(May, na.rm = TRUE),
                      Jun = sum(Jun, na.rm = TRUE),
                      Jul = sum(Jul, na.rm = TRUE),
                      Aug = sum(Aug, na.rm = TRUE),
                      Sep = sum(Sep, na.rm = TRUE),
                      Oct = sum(Oct, na.rm = TRUE),
                      Nov = sum(Nov, na.rm = TRUE),
                      Dec = sum(Dec, na.rm = TRUE))
  
  
  ICpointsdt2 <- ddply(ICpointsdt, .(EAID,Events, Year,  Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
                       summarize, Total = sum(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec))
  
  ICpointsdt3 <- ddply(ICpointsdt, .(EAID, Year),
                       summarize, Jan = sum(Jan), Feb=sum(Feb), Mar=sum(Mar), Apr=sum(Apr), May=sum(May), Jun=sum(Jun), Jul=sum(Jul), Aug=sum(Aug), 
                       Sep=sum(Sep), Oct=sum(Oct), Nov=sum(Nov), Dec=sum(Dec))
  ICpointsdt3$Events <- "Total"
  ICpointsdt3$Total <- ""
  ICpointsdt3 <- ICpointsdt3[, colnames(ICpointsdt2)]
  
  ICpointsdt2 <- ICpointsdt2[ICpointsdt2$Total >0, ]
  ICpointsdt2 <- rbind(ICpointsdt2, ICpointsdt3)
  head(ICpointsdt2)
  
  #ACHHNG002541_Ikom_White
  
  ##########################################################################################################################################
  #PP DATA
  ##########################################################################################################################################
  
  useCase = "PP"
  ID = "HHID"

  dat_PP <- dataVAl_PP
  dst_PP <- VAl_PP
  names(dat_PP)[grepl('\\.', names(dat_PP))] <- sub('.*\\.', '', names(dat_PP)[grepl('\\.', names(dat_PP))])
  #dat_PP <- filterSingleSubmission(dat_PP, ID="HHID",recent=TRUE)
  
  cleanVALPP(useCase = "PP", wd, recent=TRUE)	
  
  head(dat_PP)
  PPreg_HH <- droplevels(dat_PP[!dat_PP$HHID %in% dsHH$HHID, ])#HHs not registered
  PPreg_EA <- droplevels(dat_PP[!dat_PP$EAID %in% dsEA$EAID, ])#EAs not registered
  
  library(lubridate)
  
  
  pldateissue_PP <-  unique(dst_PP[, c("HHID", "plantingDate")])
  plpp <- as.data.frame(table(pldateissue_PP$HHID))
  VAl_PP_dupPlDate <- droplevels(dst_PP[dst_PP$HHID %in% plpp[plpp$Freq >1, ]$Var1, ])
  VAl_PP <- droplevels(dst_PP[!dst_PP$HHID %in% plpp[plpp$Freq >1, ]$Var1, ])
  VAl_PP_dupPlDate <- getLatestPlDate(VAl_PP_dupPlDate)
  dst_PP <- rbind(dst_PP, VAl_PP_dupPlDate)
  # 
  
  #PP data add season variable
  #VAl_PP <- VAl_PP[,-which(names(VAl_PP)=="harrow_plot3")[1]] #dropping accidentally column with duplicated variable name
  dst_PP[is.na(dst_PP$season),]$season <- 1 #replace NA values by 1 (season variable did not exist in season 1)
  # VAl_PP <- filterSingleSubmission(VAl_PP, ID=c("HHID", "season"), recent=recent) #retains only the most recent DST submission per season
  # VAl_PP <- merge(VAl_PP, subset(dsEA, select=c(EAID, firstNameEA, surNameEA, phoneNrEA)), all.x=TRUE) #add EA details
  # VAl_PP <- merge(VAl_PP, subset(dsHH, select=c(HHID, firstNameHH, surNameHH, phoneNrHH)), all.x=TRUE) #add HH details
  
  dsPPactual <- unique(subset(dat_PP, select=c(HHID,  plantingDate, gappingDate,
                                               dateWeeding1, dateWeeding2, dateWeeding3, harvestDate)))
  colnames(dsPPactual) <- c("HHID", "actualplantingdate", "gapping_date", "weeding1", "weeding2", "weeding3", "cassavahvstdate")
  dsPPactual <- EventLatestDate(usecasedata=dsPPactual, usecase="PP")
  colnames(dsPPactual) <- c("HHID",  "actualplantingdate", "gappingdate", "weedingdate1", "weedingdate2", "weedingdate3", "harvestdate")
  
  dsPPplant <- unique(subset(dst_PP, select=c(HHID, season, plantingDate)))
  colnames(dsPPplant) <- c("HHID", "season", "Plannedplantingdate")
  
  #convert factor to date to enable ordering
  dsPPplant$f0m <- lubridate::month(mdy(dsPPplant$Plannedplantingdate))
  dsPPplant$f0d <- lubridate::day(mdy(dsPPplant$Plannedplantingdate))
  dsPPplant$f0y <- lubridate::year(mdy(dsPPplant$Plannedplantingdate))
  dsPPplant$f0dmy <- ifelse(is.na(dsPPplant$f0m), NA, paste(dsPPplant$f0y, dsPPplant$f0m,  dsPPplant$f0d,    sep = "/"))
  dsPPplant$f0dmy <-  as.Date(dsPPplant$f0dmy)
  
  dsPPplant <- dsPPplant[, c(1,2,7)]
  colnames(dsPPplant) <- c("HHID", "season", "Plannedplantingdate")
  head(dsPPplant)

  #pick latest date
  dsPPplant2 <- aggregate(dsPPplant$Plannedplantingdate, list(dsPPplant$HHID, dsPPplant$season), max) 
  
  dsPPplanned <- dsPPplant2
  colnames(dsPPplanned) <- c("HHID",  "season", "Plannedplantingdate")
  head(dsPPplanned)
  
     #convert dates back again to factor
  dsPPplanned$Plannedplantingdate <-  as.factor(dsPPplanned$Plannedplantingdate)
  dsPPplanned$f0mn <- lubridate::month(ymd(dsPPplanned$Plannedplantingdate))
  
  dsPPplanned$f0m <- month.abb[dsPPplanned$f0mn]
  dsPPplanned$f0d <- lubridate::day(ymd(dsPPplanned$Plannedplantingdate))
  dsPPplanned$f0y <- lubridate::year(ymd(dsPPplanned$Plannedplantingdate))
  dsPPplanned$f0sep <- paste( dsPPplanned$f0d, "," ,  sep = "")
  
  dsPPplanned$f0ymd <- ifelse(is.na(dsPPplanned$f0m), NA, paste(dsPPplanned$f0m,   dsPPplanned$f0sep, dsPPplanned$f0y, sep = " "))
  
  dsPPplanned <- dsPPplanned[, c(1,2,9)]
  colnames(dsPPplanned) <- c("HHID","season", "Plannedplantingdate")
  
  dsPPheatmap <- droplevels(unique(merge(dsPPplanned, dsPPactual, by=c("HHID"))))
  
  dsPPheatmap$Plannedplantingdate <- as.factor(dsPPheatmap$Plannedplantingdate)
  

  
  #replace missing actual planting dates with planned planting date
  dsPPheatmaphid1 <- NULL
  for( hids in unique(dsPPheatmap$HHID)){
    dsPPheatmaphid <- droplevels(dsPPheatmap[dsPPheatmap$HHID == hids, ])
    if(all(unique(dsPPheatmaphid$actualplantingdate) == '')){
      dsPPheatmaphid$actualplantingdate <- ifelse(dsPPheatmaphid$actualplantingdate == '', as.character(dsPPheatmaphid$Plannedplantingdate), as.character(dsPPheatmaphid$actualplantingdate))
    }else{
      aap <- unique(dsPPheatmaphid[dsPPheatmaphid$actualplantingdate != "", ]$actualplantingdate)
      dsPPheatmaphid$actualplantingdate <- as.character(aap)
    }
    dsPPheatmaphid1 <- rbind(dsPPheatmaphid1, dsPPheatmaphid)
  }
  dsPPheatmap <- dsPPheatmaphid1
  head(dsPPheatmap)
  #solve dates into days of the year
  plannedPP <- solvedates(colNr = 3, usecasedata=dsPPheatmap)
  # colnames(Plannedplantingdate) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'Plannedplantingdate', 'plndOrigdate')
  # plannedPP<- subset(Plannedplantingdate, select = c(HHID,Plannedplantingdate,plndOrigdate ))
  head(plannedPP)
  
  actualPP <- solvedates(colNr = 4, usecasedata=dsPPheatmap)
  # colnames(actualplantingdatePP) <- c('HHID', 'month', 'date', 'year', 'leapyear',  'actualplantingdate', 'actualOrigdate')
  # actualPP<- subset(actualplantingdatePP, select = c(HHID,actualplantingdate,actualOrigdate ))
  head(actualPP)
  
  gapPP <- solvedates(colNr = 5, usecasedata=dsPPheatmap)
  # colnames(gapping_date) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'gappingdatePP', 'Origgapdate')
  # gapPP<- subset(gapping_date, select = c(HHID,gappingdatePP, Origgapdate ))
  head(gapPP)
  
  weedPP1 <- solvedates(colNr = 6, usecasedata=dsPPheatmap)
  # colnames(weeding1) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'weedingPP1', 'Origweed1date')
  # weedPP1<- subset(weeding1, select = c(HHID,weedingPP1, Origweed1date))
  head(weedPP1)
  
  weedPP2 <- solvedates(colNr = 7, usecasedata=dsPPheatmap)
  # colnames(weeding2) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'weedingPP2', 'Origweed2date')
  # weedPP2<- subset(weeding2, select = c(HHID,weedingPP2, Origweed2date ))
  head(weedPP2)
  
  weedPP3 <- solvedates(colNr = 8, usecasedata=dsPPheatmap)
  # colnames(weeding3) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'weedingPP3', 'Origweed3date')
  # weedPP3 <- subset(weeding3, select = c(HHID, weedingPP3, Origweed3date ))
  head(weedPP3)
  
  harvestPP <- solvedates(colNr = 9, usecasedata=dsPPheatmap)
  # if(!is.null(harvestPP) ){
  #   #colnames(harvestpp) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'harvestdatePP', 'Origharvestdate')
  #   harvestPP <- subset(harvestPP, select = c(HHID, HarvestPP))
    head(harvestPP)
  # }else{
  #   harvestPP <- as.data.frame(matrix(ncol=2, nrow=nrow(dsPPheatmap)), data="NA")
  #   colnames(harvestPP) <- c("HHID", "harvestdate")
  #   harvestPP$HHID <- dsPPheatmap$HHID
  # }
  
  #merge solved days of the year
  season <- select(dsPPheatmap, HHID, season)
  pp0 <- unique(merge(season, actualPP, by="HHID", all=TRUE))
  pp1 <- unique(merge(pp0, plannedPP, by="HHID", all=TRUE))
  pp2 <- unique(merge(pp1, gapPP, by="HHID", all=TRUE))
  pp3 <- unique(merge(pp2, weedPP1, by="HHID", all=TRUE))
  pp4 <- unique(merge(pp3, weedPP2, by="HHID", all=TRUE))
  pp5 <- unique(merge(pp4, weedPP3, by="HHID", all=TRUE))
  pp6 <- unique(merge(pp5, harvestPP, by= "HHID", all=TRUE))
  
  # if(!is.null(harvestPP)){
  # 	dsPPheatmapdates <- merge(pp5, harvestPP, by="HHID")
  # }else{
  # 	dsPPheatmapdates <- pp5
  # }
  dsPPheatmapdates <- pp6
  dsPPheatmapdates$planting <- as.numeric(dsPPheatmapdates$actualplantingdate) - as.numeric(dsPPheatmapdates$Plannedplantingdate) 
  dsPPheatmapdates$gappingPP <- as.numeric(dsPPheatmapdates$gappingdate) - as.numeric(dsPPheatmapdates$actualplantingdate)
  dsPPheatmapdates$weeding1PP <- as.numeric(dsPPheatmapdates$weedingdate1) - as.numeric(dsPPheatmapdates$actualplantingdate)
  dsPPheatmapdates$weeding2PP <- as.numeric(dsPPheatmapdates$weedingdate2) - as.numeric(dsPPheatmapdates$actualplantingdate)
  dsPPheatmapdates$weeding3PP <- as.numeric(dsPPheatmapdates$weedingdate3) - as.numeric(dsPPheatmapdates$actualplantingdate)
  dsPPheatmapdates$harvest <- as.numeric(dsPPheatmapdates$harvestdate) - as.numeric(dsPPheatmapdates$actualplantingdate)
  
  head(dsPPheatmapdates)
  
  heatmapPP <-  dsPPheatmapdates
  dsPPheatmap2 <- droplevels(unique(merge(heatmapPP, dsEAHH, by=c("HHID"))))
  head(dsPPheatmap2)
  drops <- c("EAID.y","EA_PhoneNr", "EA_Partner", "HH_Name", "HH_PhoneNr", "useCase", "Latitude", "Longitude", "Country", "region_state")
  dsPP <- (dsPPheatmap2[ , !(names(dsPPheatmap2) %in% drops)])
  
  #identify activities not done
  dsPP$gapping <-  ifelse(is.na(dsPP$gappingPP) & (!is.na(dsPP$weeding1PP) | !is.na(dsPP$weeding2PP)), -999, dsPP$gappingPP)
  dsPP$weeding1 <-  ifelse(is.na(dsPP$weeding1PP) & (!is.na(dsPP$weeding2PP) | !is.na(dsPP$weeding3PP)), -999, dsPP$weeding1PP)
  dsPP$weeding2 <-  ifelse(is.na(dsPP$weeding2PP) & (!is.na(dsPP$weeding3PP) | !is.na(dsPP$harvest)), -999, dsPP$weeding2PP)
  dsPP$weeding3 <-  ifelse(is.na(dsPP$weeding3PP) & !is.na(dsPP$harvest), -999, dsPP$weeding3PP)
  
  drops <- c("plantingPP", "gappingPP", "weeding1PP", "weeding2PP", "weeding3PP")
  dsPP2 <- (dsPP[ , !(names(dsPP) %in% drops)])
  head(dsPP2)
  #reshape actual dates
  dsPPss1 <- subset(dsPP2, select=c(HHID, season, actualplantingdate, Plannedplantingdate, gappingdate, weedingdate1, weedingdate2, weedingdate3, harvestdate))
  colnames(dsPPss1) <- c('HHID', 'season', 'actualplantingdate', ' plannedplanting','actualgapping', 'actualweeding1', 'actualweeding2','actualweeding3', 'actualharvest')
  dsPPss2 <- subset(dsPP2, select=c(HHID, season, planting, EAID,  EA_Name, gapping, weeding1, weeding2, weeding3, harvest ))
  colnames(dsPPss2) <- c('HHID', 'season', 'planting', ' EAID','EA_Name', "gapping", 'weeding1', 'weeding2','weeding3', 'harvest')
  
  dsPPRss1 <- suppressWarnings({dsPPss1 %>% gather(actualEvents, actualDates, actualplantingdate, actualgapping, actualweeding1, actualweeding2, actualweeding3, actualharvest)})
  dsPPRss2 <- suppressWarnings({dsPPss2 %>% gather(events, dates, planting, gapping, weeding1, weeding2, weeding3, harvest)})
  dsPPRss2$dates <- as.numeric(dsPPRss2$dates)
  #merge actual dates and solved dates
  dsPPRss1$events <- gsub("actual", "", dsPPRss1$actualEvents)
  
  dsPPRss1$events <- gsub("date", "", dsPPRss1$events)
  dsPPreshape <- merge(dsPPRss1, dsPPRss2)
  dim(dsPPreshape)
  
  dsPPreshape <- unique(dsPPreshape)
  head(dsPPreshape)
  
  #dsPPreshape$diffdate <- today()-anytime::anydate(as.character(dsPPreshape$actualDates))
  names(dsPPreshape) <- c('HHID', 'season', 'events', 'plannedplanting', 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 'DatesBnPlantingEvent')
  PPhhdata <- dsPPreshape
  PPhhdata$actualDates <- as.Date(PPhhdata$actualDates)
  #get status for events 
  
  getstatus<- NULL
  for(h in unique(PPhhdata$HHID)){
    PPhhdatass <- subset(PPhhdata, HHID == h)
    if(!all(is.na(PPhhdatass$DatesBnPlantingEvent))){
      #hhdatass <- subset(hhdata, HHID == "ACHHNG000151")
      #PPhhdatass <- replace(PPhhdatass, is.na(PPhhdatass), -100)
      
      PPhhdatass$status <- ifelse(PPhhdatass$events %in% c("planting")  & PPhhdatass$DatesBnPlantingEvent  == 0, "On-time",
                                  ifelse(PPhhdatass$events %in% c("planting")  & PPhhdatass$DatesBnPlantingEvent  < 0 & !PPhhdatass$DatesBnPlantingEvent == -999, "Earlier",
                                         ifelse(PPhhdatass$events %in% c("planting")  & PPhhdatass$DatesBnPlantingEvent  > 0, "Done later",
                                                ifelse(PPhhdatass$events %in% c("planting")  & PPhhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                       
                                                       ifelse(PPhhdatass$events %in% c("gapping")   & PPhhdatass$DatesBnPlantingEvent < 28  & !PPhhdatass$DatesBnPlantingEvent == -999, "Earlier",
                                                              ifelse(PPhhdatass$events %in% c("gapping")   & PPhhdatass$DatesBnPlantingEvent  == 28 , "On-time",
                                                                     ifelse(PPhhdatass$events %in% c("gapping")   & PPhhdatass$DatesBnPlantingEvent > 28 , "Done later",
                                                                            ifelse(PPhhdatass$events %in% c("gapping")   & PPhhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                                                   ifelse(PPhhdatass$events %in% c("gapping")   & PPhhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                          
                                                                                          ifelse(PPhhdatass$events %in% c("weeding1")  & PPhhdatass$DatesBnPlantingEvent < 56  & !PPhhdatass$DatesBnPlantingEvent == -999, "Earlier",
                                                                                                 ifelse(PPhhdatass$events %in% c("weeding1")  & PPhhdatass$DatesBnPlantingEvent  == 56, "On-time",
                                                                                                        ifelse(PPhhdatass$events %in% c("weeding1")  & PPhhdatass$DatesBnPlantingEvent > 56 , "Done later",
                                                                                                               ifelse(PPhhdatass$events %in% c("weeding1")  & PPhhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                                                                                      ifelse(PPhhdatass$events %in% c("weeding1")  & PPhhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                             
                                                                                                                             ifelse(PPhhdatass$events %in% c("weeding2")  & PPhhdatass$DatesBnPlantingEvent > 0, "On-time",
                                                                                                                                    ifelse(PPhhdatass$events %in% c("weeding2")  & PPhhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                           ifelse(PPhhdatass$events %in% c("weeding2")  & PPhhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                                                                                                                  ifelse(PPhhdatass$events %in% c("weeding3")  & PPhhdatass$DatesBnPlantingEvent > 0, "On-time",
                                                                                                                                                         ifelse(PPhhdatass$events %in% c("weeding3")  & PPhhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                ifelse(PPhhdatass$events %in% c("weeding3")  & PPhhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                                                                                                                                       
                                                                                                                                                                       ifelse(PPhhdatass$events %in% c("harvest")   & PPhhdatass$DatesBnPlantingEvent > 0, "On-time",
                                                                                                                                                                              ifelse(PPhhdatass$events %in% c("harvest")   & PPhhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                     
                                                                                                                                                                                     as.character(PPhhdatass$status)))))))))))))))))))))))
      
      PPhhdatass <- replace(PPhhdatass, PPhhdatass == -100, NA)
      getstatus <- rbind(getstatus, PPhhdatass)
    }
  }
  
  
  PPhhdata <- getstatus
  head(PPhhdata)
  #dsPPR <- PPhhdata
  
  #calculating due dates for PP
  #PPhhdata$actualDates <- as.Date(PPhhdata$actualDates) 
  PPhhdata$status <- ifelse(is.na(PPhhdata$DatesBnPlantingEvent), "To be done", as.character(PPhhdata$status))
  PPhhdata$status <- ifelse(PPhhdata$DatesBnPlantingEvent == "", "To be done", as.character(PPhhdata$status))
  PPhhdata$status <- ifelse(is.na(PPhhdata$status), "To be done", as.character(PPhhdata$status))
  
  PPhhdata$actualmn <- lubridate::month(ymd(PPhhdata$actualDates))
  
  PPhhdata$month  <- month.abb[PPhhdata$actualmn]
  PPhhdata$actuald <- lubridate::day(ymd(PPhhdata$actualDates))
  PPhhdata$actualday <- lubridate::wday(ymd(PPhhdata$actualDates))
  PPhhdata$actualYR <- lubridate::year(ymd(PPhhdata$actualDates))
  head(PPhhdata)
  
  PPhhdata  <- unique(PPhhdata)
  PPhhdata$datetbd <- ifelse(!is.na(PPhhdata$actuald), paste(PPhhdata$actualYR, PPhhdata$actualmn, PPhhdata$actuald, sep = "/"), NA)
  
  PPhhdata$datetbd2 <- as.Date(PPhhdata$datetbd) 
  PPhhdata <- droplevels(PPhhdata[!PPhhdata$HHID == "ACHHNG002151", ])
  PPhhdata <- droplevels(PPhhdata[!PPhhdata$HHID == "ACHHNG002410", ])
  PPhhdata <- droplevels(PPhhdata[!PPhhdata$HHID == "ACHHNG004091", ])

  
  DsPPdue <- NULL
  for(hids in unique(PPhhdata$HHID)){
    hd <- PPhhdata[PPhhdata$HHID == hids, ]
    hdpl <- hd[hd$events == 'planting', ]
    
    hdG <- hd[hd$events == 'gapping', ]
    if(is.na(hdG$datetbd2)){
      hdG$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 28
    }
    
    hdW1 <- hd[hd$events == 'weeding1', ]
    if(is.na(hdW1$datetbd2)){
      hdW1$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 84
    }
    
    hdW2 <- hd[hd$events == 'weeding2', ]
    if(is.na(hdW2$datetbd2)){
      hdW2$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 168
    }
    
    hdW3 <- hd[hd$events == 'weeding3', ]
    if(is.na(hdW3$datetbd2)){
      hdW3$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 168
    }
    
    hdv <- hd[hd$events == 'harvest', ]
    if(is.na(hdv$datetbd2)){
      hdv$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 365
    }
    
    
    DsPPdue <- suppressWarnings({rbind(DsPPdue,hdpl, hdG, hdW1,hdW2,hdW3,hdv)})
  }
  
  names(DsPPdue) <- c('HHID', 'season', 'Events', 'plannedplantingdate', 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 'DatesBnPlantingEvent','status', 
                      'actualmn', 'month', 'actuald', 'actualday', 'actualYR',   'datetbd',   'datetbd2')
  head(DsPPdue)
  
  
  DsPPdue$status <- ifelse(DsPPdue$status == "To be done"& DsPPdue$datetbd2 < Sys.Date(), "Overdue", as.character(DsPPdue$status))
  DsPPdue <- droplevels(DsPPdue[!is.na(DsPPdue$HHID), ] )
  
  DsPPdue$status <- ifelse(is.na(DsPPdue$DatesBnPlantingEvent), "To be done", as.character(DsPPdue$status))
  
  # ADDING column for DST run
  DsPPdue$datetbd2 <- as.character(DsPPdue$datetbd2)
  
  hhid_TT <- droplevels(DsPPdue[DsPPdue$HHID %in% dst_PP$HHID, ])
  hhid_TF <- droplevels(dst_PP[!dst_PP$HHID %in% DsPPdue$HHID, ])## dst is run but no data is submitted
  hhid_FT <- droplevels(DsPPdue[!DsPPdue$HHID %in% dst_PP$HHID, ])## dst is not run but there is data
  
  PP_TT <- NULL
  for(hhids in unique(hhid_TT$HHID)){
    hd <- hhid_TT[hhid_TT$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "True"
    hd1$Events <- "DST run"
    hd1$status <- "To be done"
    PP_TT <- rbind(PP_TT, hd, hd1)
  }
  
  
  # eaids <- "ACEANG000123"
  PP_TF <- NULL
  for(eaids in unique(hhid_TF$EAID)){
    edata <- DsPPdue[DsPPdue$EAID == eaids, ]
    hh_nodata <- unique(hhid_TF[hhid_TF$EAID == eaids, ]$HHID)
    if(length(hh_nodata) > 0){
      asas <- edata[1:length(hh_nodata),]
      asas$HHID <- hh_nodata
      asas$Events <- "DST run"
      asas$actualEvents <- "DST run"
      asas$status <- "To be done"
      asas$datetbd2 <- "True"
      PP_TF <- rbind(PP_TF, edata, asas)
    }
  }
  
  PP_dstchecked <- rbind(PP_TT, PP_TF)
  PP_dstchecked <- PP_dstchecked[!is.na(PP_dstchecked$HHID), ]
  
  #METRICS
  # length(unique(PP_dstchecked$HHID))
  # length(unique(PP_dstchecked$EAID))
  # PP_dstchecked$COUNTRY <- ifelse(grepl("ACHHTZ", PP_dstchecked$HHID), "TZ", "NG")
  # TZPP <- PP_dstchecked[PP_dstchecked$COUNTRY == "TZ", ]
  # length(unique(TZPP$HHID))
  # length(unique(TZPP$EAID))
  
  ###################################################
  #PPpoints
  ###################################################
  dataVAl_PP <- read.csv("data/dataVAL_PP.csv")
  
  pointsppdat <- unique(subset(dataVAl_PP, select=c(HHID, EAID, SubmissionDate, event, plantingDate, gappingDate,
                                                    dateWeeding1, dateWeeding2, dateWeeding3, harvestDate)))
  colnames(pointsppdat) <- c("HHID", "EAID", "subdate", "p_event", "actualplantingdate", "gapping_date", "weeding1", "weeding2", "weeding3", "cassavahvstdate")
  pointsppdat <- droplevels(unique(merge(pointsppdat, dsEA, by=c("EAID"))))
  
  library(lubridate)	
  # ADDING column for DST run
  VAl_PPpts <- subset(VAl_PP, select = c(HHID, SubmissionDate))
  VAl_PPpts$SubmissionDate <- mdy_hms(VAl_PPpts$SubmissionDate)
  VAl_PPpts$SubmissionDate <- as.Date(as.POSIXct(VAl_PPpts$SubmissionDate, format = "%m/%d/%Y"))
  head(VAl_PPpts)
  pts_valPP <- merge(pointsppdat, VAl_PPpts, by=c("HHID") )
  
  
  PP_runs <- NULL
  for(hhids in unique(pts_valPP$HHID)){
    hd <- pts_valPP[pts_valPP$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$p_event <- "DST run"
    PP_runs <- rbind(PP_runs, hd, hd1)
  }
  
  
  head(PP_runs)
  
  pointspp <-PP_runs
  pointspp$EA_Name<- ifelse(pointspp$EA_Name %in% "Wargaret Asuo", "Margaret Asuo", as.character(pointspp$EA_Name))
  
  #fixing dates
  pointspp$subdate <-mdy_hms(pointspp$subdate)
  pointspp$subdate <- as.Date(as.POSIXct(pointspp$subdate, format = "%m/%d/%Y"))
  pointspp$actualmn <- lubridate::month(ymd(pointspp$subdate))
  
  pointspp$month  <- month.abb[pointspp$actualmn]
  pointspp$actuald <- lubridate::day(ymd(pointspp$subdate))
  pointspp$actualday <- lubridate::wday(ymd(pointspp$subdate))
  pointspp$actualYR <- lubridate::year(ymd(pointspp$subdate))
  
  pointspp$submn <- lubridate::month(ymd(pointspp$SubmissionDate))
  
  pointspp$submonth  <- month.abb[pointspp$submn]
  pointspp$subd <- lubridate::day(ymd(pointspp$SubmissionDate))
  pointspp$subday <- lubridate::wday(ymd(pointspp$SubmissionDate))
  pointspp$subYR <- lubridate::year(ymd(pointspp$SubmissionDate))
  head(pointspp)
  
  pointspp <- unique(pointspp)
  
  #Omit data for events not done
  #
  pointspp$event1 <-as.numeric(ifelse ((!is.na(pointspp$subdate)) &  (pointspp$p_event == "event1"), "2", "0"))
  #
  pointspp$event2 <- as.numeric(ifelse ((!is.na(pointspp$subdate)) &  (pointspp$p_event == "event2"), "4", "0"))
  pointspp$event3 <- as.numeric(ifelse ((!is.na(pointspp$subdate)) &  (pointspp$p_event == "event3"), "2", "0"))
  pointspp$event4 <- as.numeric(ifelse ((!is.na(pointspp$subdate)) &  (pointspp$p_event == "event4"), "4", "0"))
  pointspp$event5 <- as.numeric(ifelse ((!is.na(pointspp$subdate)) &  (pointspp$p_event == "event5"), "9", "0"))
  pointspp$event6 <- as.numeric(ifelse ((!is.na(pointspp$subdate)) &  (pointspp$p_event == "event6"), "2", "0"))
  pointspp$event7 <- as.numeric(ifelse ((!is.na(pointspp$subdate)) &  (pointspp$p_event == "event7"), "10", "0"))
  pointspp$DST_run <- as.numeric(ifelse ((!is.na(pointspp$SubmissionDate)) &  (pointspp$p_event == "DST run"), "2", "0"))
  
  
  ptspp <- subset(pointspp, select = c(HHID, EAID, EA_Name, event1, event2, event3, event4, event5, event6, event7, DST_run))
  head(ptspp)
  
  
  library(dplyr)
  PPpoints <- ptspp %>% group_by(HHID, EAID, EA_Name) %>% summarise_at(1:8, sum)
  PPpoints <- data.table::setDT(PPpoints)[HHID!="ACHHNG123456" & HHID!="ACHHNG000000"]
  PPpoints = unique(PPpoints)
  
  PPpoints$Totalpoints <- PPpoints$event1+PPpoints$event2+PPpoints$event3+ PPpoints$event4+PPpoints$event5+PPpoints$event6 +PPpoints$event7 + PPpoints$DST_run 
  head(PPpoints)
  library(tidyr)
  dsPPpnts1 <- subset(pointspp, select=c(EAID, actualYR, subYR, month, submonth, event1, event2, event3, event4, event5, event6, event7, DST_run))
  dsPPpnts2 <- dsPPpnts1 %>% gather(events, points,event1, event2, event3, event4, event5, event6, event7, DST_run)
  dsPPpnts2$month[dsPPpnts2$events == "DST_run"] <- dsPPpnts2$submonth
  dsPPpnts2$actualYR[dsPPpnts2$events == "DST_run"] <- dsPPpnts2$subYR
  
  head(dsPPpnts2)
  
  dsPPpnts2 <- subset(dsPPpnts2, select=c(EAID, events, actualYR, month, points))
  colnames (dsPPpnts2) <- c("EAID", "Events", "Year", "month", "points")
  
  
  dsPPpnts2 <- droplevels(dsPPpnts2[!is.na(dsPPpnts2$month), ])
  
  dsPPpnts3 <- dsPPpnts2 %>% dplyr::mutate(i = row_number()) %>% spread(month, points)
  dsPPpnts3 <- subset(dsPPpnts3, select = -c(i))
  
  head(dsPPpnts3)
  
  
  #fix all calendar months not available in data
  
  Notavailmonths <- month.abb[!month.abb %in% colnames(dsPPpnts3)]
  x <- suppressWarnings({as.data.frame(matrix(ncol=length(Notavailmonths), nrow=nrow(dsPPpnts3), data=as.numeric('NA')))})
  colnames(x)<-  Notavailmonths 
  dsPPpnts4 <- cbind(dsPPpnts3, x)
  head(dsPPpnts4)
  
  #summarize points
  PPpointsdt <- ddply(dsPPpnts4, .(EAID, Events, Year), summarize,
                      Jan = sum(Jan, na.rm = TRUE),
                      Feb = sum(Feb, na.rm = TRUE),
                      Mar = sum(Mar, na.rm = TRUE),
                      Apr = sum(Apr, na.rm = TRUE),
                      May = sum(May, na.rm = TRUE),
                      Jun = sum(Jun, na.rm = TRUE),
                      Jul = sum(Jul, na.rm = TRUE),
                      Aug = sum(Aug, na.rm = TRUE),
                      Sep = sum(Sep, na.rm = TRUE),
                      Oct = sum(Oct, na.rm = TRUE),
                      Nov = sum(Nov, na.rm = TRUE),
                      Dec = sum(Dec, na.rm = TRUE))
  
  PPpointsdt2 <- ddply(PPpointsdt, .(EAID,Events, Year,  Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
                       summarize, Total = sum(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec))
  
  PPpointsdt3 <- ddply(PPpointsdt, .(EAID, Year),
                       summarize, Jan = sum(Jan), Feb=sum(Feb), Mar=sum(Mar), Apr=sum(Apr), May=sum(May), Jun=sum(Jun), Jul=sum(Jul), Aug=sum(Aug), 
                       Sep=sum(Sep), Oct=sum(Oct), Nov=sum(Nov), Dec=sum(Dec))
  PPpointsdt3$Events <- "Total"
  PPpointsdt3$Total <- ""
  PPpointsdt3 <- PPpointsdt3[, colnames(PPpointsdt2)]
  
  PPpointsdt2 <- PPpointsdt2[PPpointsdt2$Total >0, ]
  PPpointsdt2 <- rbind(PPpointsdt2, PPpointsdt3)
  
  head(PPpointsdt2)
  
  #################################################################################################################################################
  #PP weeks
  head(DsPPdue)
  dsppwks2 <- DsPPdue
  dsppwks2[is.na(dsppwks2$season),]$season <- 1
  #get days/weeks/months/year from actual dates 
  dsppwks2$planting1wk <- lubridate::week(ymd(dsppwks2$plannedplanting))
  dsppwks2$eventwk <- lubridate::week(ymd(dsppwks2$datetbd2))
  
  dsppwks2$planting1yr <- lubridate::year(ymd(dsppwks2$plannedplanting))
  
  ppwkhh <- dsppwks2
  
  ##deduct weeks of the year from planting week
  ppwkhh$status2 <- ifelse(ppwkhh$eventwk == -999, -999, ppwkhh$eventwk - ppwkhh$planting1wk)
  head(ppwkhh)
  
  drops <- c( "DatesBnPlantingEvent", "DiffToday") 
  ppwkhh <- (ppwkhh[ , !(names(ppwkhh) %in% drops)])
  head(ppwkhh)
  
  colnames(ppwkhh) <- c('HHID', 'season', 'events', 'plannedplanting', 'actualEvents', "actualDates", "EAID","EA_Name", "status", "actualmn","month","actuald", "actualday", "actualYR",   
                        "datetbd",   "datetbd2", "planting1wk", "weeks", "planting1yr", "status2")
  
  
  ppwksdata <- ppwkhh
  
  ppwksdata$dueyear <- lubridate::year(ppwksdata$datetbd2)
  # icwksdata$diffsys <- ppwksdata$datetbd2 > Sys.Date() & ppwksdata$dueyear ==  lubridate::year(Sys.Date())
  # ppwksdata$status <- ifelse(ppwksdata$diffsys == 'TRUE', "Due soon", as.character(ppwksdata$status))
  ppwksdata$index <- 1:nrow(ppwksdata)
  
  toBeDue <- droplevels(ppwksdata[ppwksdata$datetbd2 > Sys.Date() & ppwksdata$dueyear ==  lubridate::year(Sys.Date()), ] )

  
  if (nrow(toBeDue) > 0) {
    toBeDue$todayweek <-  as.numeric(lubridate::week(Sys.Date()))
    toBeDue$t2 <- as.numeric(as.character(toBeDue$weeks)) - toBeDue$todayweek
    toBeDue <- toBeDue[toBeDue$t2 <= 2, ]
    #toBeDone <- toBeDue[toBeDue$t2 > 2, ]
    if (nrow(toBeDue) > 0) {
      toBeDue$status <- 'Due soon'
      toBeDue$month <- as.factor(month.abb[lubridate::month(ymd(toBeDue$datetbd2))]) 
      ppwksdata <- ppwksdata[!ppwksdata$index %in% toBeDue$index, ]
      ppwksdata <- rbind(ppwksdata, toBeDue[, colnames(ppwksdata)]) 
    }
    # if (nrow(toBeDone) > 0) {
    #   toBeDone$status <- 'To be done'
    #   toBeDone$month <- as.factor(month.abb[lubridate::month(ymd(toBeDone$datetbd2))]) 
    #   icwksdata <- icwksdata[!icwksdata$index %in% toBeDone$index, ]
    #   icwksdata <- rbind(icwksdata, toBeDone[, colnames(icwksdata)]) 
    # }
    
  }
  
  
  ppwksdata <- droplevels(ppwksdata[!is.na(ppwksdata$HHID), ])
  ppwksdata$status <- ifelse(ppwksdata$status == "To be done" & Sys.Date() > ppwksdata$datetbd2, "Not done", as.character(ppwksdata$status))
  ppwksdata$status <- ifelse(is.na(ppwksdata$status), "To be done", as.character(ppwksdata$status))

  head(ppwksdata)
  
  ########################################################################################
  # SPHS data
  ########################################################################################
  
   VAl_SP <- read.csv("data/VAL_SPHS_KW.csv")
  # #VAl_SP <- rbind(VAl_SP, read.csv("data/VAL_SPHS_KW.csv"))
  # VAl_SP <- rbind(VAl_SP, read.csv("data/VAL_SPHS_OG.csv"))
  # VAl_SP <- rbind(VAl_SP, read.csv("data/VAL_SPHS_ON.csv"))
  # VAl_SP <- rbind(VAl_SP, read.csv("data/VAL_SPHS_OY.csv"))
  # 
  # VAl_SP <- rbind(VAl_SP, read.csv("data/VAL_SPHS_TZEZ.csv"))
  # VAl_SP <- rbind(VAl_SP, read.csv("data/VAL_SPHS_TZLZE.csv"))
  # VAl_SP <- rbind(VAl_SP, read.csv("data/VAL_SPHS_TZLZW.csv"))
  # VAl_SP <- rbind(VAl_SP, read.csv("data/VAL_SPHS_TZSZ.csv"))
  
  # 
  # fls <- paste(wd, "/data/", list.files(path=paste(wd, "/data", sep=""), pattern="VAL_SPHS_"), sep="")
  # fls_TZ <- fls[grep("TZ", fls)]
  # fls_NG <- fls[-grep("TZ", fls)]
  # dst_TZ <- dropGroupNames(do.call(rbind, lapply(fls_TZ, function(x) read.csv(x))))
  # dst_NG <- dropGroupNames(do.call(rbind, lapply(fls_NG, function(x) read.csv(x))))
  # dst_TZ$season <- 'NA'
  # dst_TZ <- dst_TZ[, colnames(dst_NG)]
  # dst <- rbind(dst_TZ, dst_NG)
  # 

  VAl_SPHS_KW <- read.csv("data/VAL_SPHS_KW.csv",stringsAsFactors=FALSE)
  VAl_SPHS_OG <- read.csv("data/VAL_SPHS_OG.csv",stringsAsFactors=FALSE)
  VAl_SPHS_ON <- read.csv("data/VAL_SPHS_ON.csv",stringsAsFactors=FALSE)
  VAl_SPHS_OY <- read.csv("data/VAL_SPHS_OY.csv",stringsAsFactors=FALSE)
  
  VAl_SPHS_TZEZ <- read.csv("data/VAL_SPHS_TZEZ.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZLZE <- read.csv("data/VAL_SPHS_TZLZE.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZLZW <- read.csv("data/VAL_SPHS_TZLZW.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZSZ <- read.csv("data/VAL_SPHS_TZSZ.csv",stringsAsFactors=FALSE)
  
  
  VAl_SPHS_NG <- rbind(VAl_SPHS_KW, VAl_SPHS_OG, VAl_SPHS_ON, VAl_SPHS_OY)
  VAl_SPHS_TZ <- rbind(VAl_SPHS_TZEZ, VAl_SPHS_TZLZE, VAl_SPHS_TZLZW, VAl_SPHS_TZSZ)
  VAl_SPHS_NG <- VAl_SPHS_NG[, colnames(VAl_SPHS_TZ)]
  VAl_SPHS <- rbind(VAl_SPHS_NG, VAl_SPHS_TZ)
  dst <- VAl_SPHS
  dsSPHSp <- VAl_SPHS
  
  useCase = "SPHS"
  dat_SP <- dataVAl_SPHS
  dst_SP <- dst
  names(dat_SP)[grepl('\\.', names(dat_SP))] <- sub('.*\\.', '', names(dat_SP)[grepl('\\.', names(dat_SP))])
  
  cleanVALSP(useCase = "SPHS", wd, recent=TRUE)
  head(dat_SP)
  
  
  # SPreg_HH <- dat_SP[!dat_SP$HHID %in% dsHH$HHID, ]#HHs not registered
  # SPreg_EA <- dat_SP[!dat_SP$EAID %in% dsEA$EAID, ]#EAs not registered
  # SPreg <- droplevels(dat_SP[!dat_SP$EAID %in% dsEAHH$EAID, ])#EAs not registered

  #find latest dates for planting 
  pldateissue_VAl_SP <-  unique(dst_SP[, c("HHID", "plantingDate")])
  plsp <- as.data.frame(table(pldateissue_VAl_SP$HHID))
  plsp[plsp$Freq >1, ]
  VAl_SP_dupPlDate <- droplevels(dst_SP[dst_SP$HHID %in% plsp[plsp$Freq >1, ]$Var1, ])
  dst_SP <- droplevels(dst_SP[!dst_SP$HHID %in% plsp[plsp$Freq >1, ]$Var1, ])
  VAl_SP_dupPlDate <- getLatestPlDate(VAl_SP_dupPlDate)
  dst_SP <- rbind(dst_SP, VAl_SP_dupPlDate)
  
  
  # dsSPHSactual <- unique(subset(dataVAl_SPHS, select=c(HHID,  plantingDetails.plantingDate, gappingDetails.gappingDate,weedingDetails.dateWeeding1, weedingDetails.dateWeeding2, weedingDetails.dateWeeding3, harvestDate.intHarvestDate_CON, harvestDate.intHarvestDate_REC, effHarvestDate_REC,effHarvestDate_CON )))
  # 
  dsSPHSactual <- unique(subset(dat_SP, select=c(HHID,  plantingDate, gappingDate, dateWeeding1, dateWeeding2, dateWeeding3, intHarvestDate_CON, 
                                                 intHarvestDate_REC, effHarvestDate_CON, effHarvestDate_REC)))

 
  #SHOULD WE INCLUDE THE EFF HARVEST DATES?
  colnames(dsSPHSactual) <- c("HHID",  "actualplantingdate", "gapping_date", "weeding1", "weeding2", "weeding3", "intharvestdateCON", 
                              "intharvestdateREC", "effharvestdateCON", "effharvestdateREC"  )
  
  dsSPHSactual <- EventLatestDate(usecasedata=dsSPHSactual, usecase="SP")
  colnames(dsSPHSactual) <- c("HHID", "actualplantingdate", "gappingdate", "weedingdate1", "weedingdate2", "weedingdate3", "intharvestdateCON", 
                              "intharvestdateREC", "effharvestdateCON", "effharvestdateREC" )
  head(dsSPHSactual)
  dsSPplant <- unique(subset(dst_SP, select=c(HHID, plantingDate, season)))
  colnames(dsSPplant) <- c("HHID", "Plannedplantingdate", "season")
  #convert factor to date to enable ordering
  dsSPplant$f0m <- lubridate::month(mdy(dsSPplant$Plannedplantingdate))
  dsSPplant$f0d <- lubridate::day(mdy(dsSPplant$Plannedplantingdate))
  dsSPplant$f0y <- lubridate::year(mdy(dsSPplant$Plannedplantingdate))
  dsSPplant$f0dmy <- ifelse(is.na(dsSPplant$f0m), NA, paste(dsSPplant$f0y, dsSPplant$f0m,  dsSPplant$f0d,    sep = "/"))
  dsSPplant$f0dmy <-  as.Date(dsSPplant$f0dmy)
  dsSPplant[is.na(dsSPplant$season),]$season <- 1
  dsSPplant <- dsSPplant[, c(1,7,3)]
  colnames(dsSPplant) <- c("HHID", "Plannedplantingdate", "season")
  head(dsSPplant)
  
  #pick latest date
  dsSPplant2 <- aggregate(dsSPplant$Plannedplantingdate, list(dsSPplant$HHID, dsSPplant$season), max) 
  
  #dsSPplant2 <- dsSPplant[dsSPplant$Plannedplantingdate %in% latest$x,]
  VAl_SPpts
  dsSPplanned <- dsSPplant2
  colnames(dsSPplanned) <- c("HHID",  "season", "Plannedplantingdate")
  head(dsSPplanned)

  
  dsSPplanned$Plannedplantingdate <-  as.factor(dsSPplanned$Plannedplantingdate)
  dsSPplanned$f0mn <- lubridate::month(ymd(dsSPplanned$Plannedplantingdate))
  
  dsSPplanned$f0m <- month.abb[dsSPplanned$f0mn]
  dsSPplanned$f0d <- lubridate::day(ymd(dsSPplanned$Plannedplantingdate))
  dsSPplanned$f0y <- lubridate::year(ymd(dsSPplanned$Plannedplantingdate))
  dsSPplanned$f0sep <- paste( dsSPplanned$f0d, "," ,  sep = "")
  
  dsSPplanned$f0ymd <- ifelse(is.na(dsSPplanned$f0m), NA, paste(dsSPplanned$f0m,  dsSPplanned$f0sep, dsSPplanned$f0y, sep = " "))
  
  
  dsSPplanned <- dsSPplanned[, c(1,2,9)]
  colnames(dsSPplanned) <- c("HHID", "season", "Plannedplantingdate")
  
  dsSPHSheatmap <- droplevels(unique(merge(dsSPplanned, dsSPHSactual, by=c("HHID"))))
  
  dsSPHSheatmap$Plannedplantingdate <- as.factor(dsSPHSheatmap$Plannedplantingdate)
  #replace missing actual planting dates with planned planting date
  
  # dsSPHSheatmap <- droplevels(unique(merge(dsSPHSplanned, dsSPHSactual, by=c("HHID"))))
  str(dsSPHSheatmap)
  head(dsSPHSheatmap)
  
  dsSPHSheatmaphid1 <- NULL
  for( hids in unique(dsSPHSheatmap$HHID)){
    dsSPHSheatmaphid <- droplevels(dsSPHSheatmap[dsSPHSheatmap$HHID == hids, ])
    if(all(unique(dsSPHSheatmaphid$actualplantingdate) == '')){
      dsSPHSheatmaphid$actualplantingdate <- ifelse(dsSPHSheatmaphid$actualplantingdate == '', as.character(dsSPHSheatmaphid$Plannedplantingdate), 
                                                    as.character(dsSPHSheatmaphid$actualplantingdate))
    }else{
      
      aasp <- unique(dsSPHSheatmaphid[dsSPHSheatmaphid$actualplantingdate != "", ]$actualplantingdate)
      dsSPHSheatmaphid$actualplantingdate <- as.character(aasp)
    }
    dsSPHSheatmaphid1 <- rbind(dsSPHSheatmaphid1, dsSPHSheatmaphid)
  }
  dsSPHSheatmap <- dsSPHSheatmaphid1
  
  
  seasoninfo <- subset(dsSPHSheatmap, select = c("HHID", "season"))
  #solve dates into days of the year
  head(dsSPHSheatmap)
  actual <- solvedates(colNr = 4, usecasedata = dsSPHSheatmap)
  head(actual)
  
  planned <- solvedates(colNr = 3, usecasedata = dsSPHSheatmap)
  head(planned)
  
  gap <- solvedates(colNr = 5, usecasedata = dsSPHSheatmap)
  colnames(gap) <- c("HHID", "gapping_date")
  head(gap)
  
  weed1 <- solvedates(colNr = 6, usecasedata = dsSPHSheatmap)
  colnames(weed1) <- c("HHID", "weeding_date1")
  head(weed1)

  
  weed2 <- solvedates(colNr = 7, usecasedata = dsSPHSheatmap)
  colnames(weed2) <- c("HHID", "weeding_date2")
  head(weed2)
  
  weed3 <- solvedates(colNr = 8, usecasedata = dsSPHSheatmap)
  colnames(weed3) <- c("HHID", "weeding_date3")
  head(weed3)
  
  inthvstcon <- solvedates(colNr = 9, usecasedata=dsSPHSheatmap)
  # if(!is.null(hvstcon)){
  #   #colnames(cassavahvstdate) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'cassavahvstdate', 'Origcasshvstdate')
  #   hvstcon<- subset(hvstcon, select = c(HHID, hvstcon))
  # }else{
  #   hvstcon <- as.data.frame(matrix(ncol=1, nrow=nrow(dsSPHSheatmap)), data="NA")
  #   colnames(hvstcon) <- c("hvstcon")
  #   hvstcon$HHID <- dsSPHSheatmap$HHID
 

  # }
  head(inthvstcon)
  inthvstrec <- solvedates(colNr = 10, usecasedata=dsSPHSheatmap)
  # if(!is.null(hvstrec)){
  #   #colnames(cassavahvstdate) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'cassavahvstdate', 'Origcasshvstdate')
  #   hvstrec<- subset(hvstrec, select = c(HHID, hvstrec))
  # }else{
  #   hvstrec <- as.data.frame(matrix(ncol=1, nrow=nrow(dsSPHSheatmap)), data="NA")
  #   colnames(hvstrec) <- c("hvstrec")
  #   hvstrec$HHID <- dsSPHSheatmap$HHID
  # }
  head(inthvstrec)
  
  effhvstdateCON <- solvedates(colNr = 11, usecasedata = dsSPHSheatmap)
  colnames(effhvstdateCON) <- c("HHID", "effharvestdateCON")
  head(effhvstdateCON)
  
  effhvstdateREC <- solvedates(colNr = 12, usecasedata = dsSPHSheatmap)
  colnames(effhvstdateREC) <- c("HHID", "effharvestdateREC")
  head(effhvstdateREC)
  
  #merge solved days of the year
  SPHS <- NULL
  SPHS <- unique(merge(seasoninfo, actual, by="HHID"), all=TRUE)
  SPHS1 <- unique(merge(SPHS, planned, by="HHID"), all=TRUE)
  SPHS2 <- unique(merge(SPHS1, gap, by='HHID', all=TRUE))
  SPHS3 <- unique(merge(SPHS2, weed1, by='HHID', all=TRUE))
  SPHS4 <- unique(merge(SPHS3, weed2, by='HHID', all=TRUE))
  SPHS5 <- unique(merge(SPHS4, weed3, by='HHID', all=TRUE))
  SPHS6 <- merge(SPHS5, inthvstcon, by="HHID", all=TRUE)
  SPHS7 <- merge(SPHS6, inthvstrec, by="HHID", all=TRUE)
  SPHS8 <- merge(SPHS7, effhvstdateCON, by="HHID", all=TRUE)
  SPHS9 <- merge(SPHS8, effhvstdateREC, by="HHID", all=TRUE)
  dsSPHSheatmapdates <- SPHS9
  
  head(dsSPHSheatmapdates)
  
  #deduct solved days of the year against actual planting dates
  dsSPHSheatmapdates$planting <- as.numeric(dsSPHSheatmapdates$actualplantingdate) - as.numeric(dsSPHSheatmapdates$Plannedplantingdate)   
  dsSPHSheatmapdates$gappin <- as.numeric(dsSPHSheatmapdates$gapping_date) - as.numeric(dsSPHSheatmapdates$actualplantingdate)
  dsSPHSheatmapdates$weedin1 <- as.numeric(dsSPHSheatmapdates$weeding_date1) - as.numeric(dsSPHSheatmapdates$actualplantingdate)
  dsSPHSheatmapdates$weedin2 <- as.numeric(dsSPHSheatmapdates$weeding_date2) - as.numeric(dsSPHSheatmapdates$actualplantingdate)
  dsSPHSheatmapdates$weedin3 <- as.numeric(dsSPHSheatmapdates$weeding_date3) - as.numeric(dsSPHSheatmapdates$actualplantingdate)

  dsSPHSheatmapdates$effharvestREC <- dsSPHSheatmapdates$effharvestdateREC - dsSPHSheatmapdates$actualplantingdate
  dsSPHSheatmapdates$effharvestCON <- dsSPHSheatmapdates$effharvestdateCON - dsSPHSheatmapdates$actualplantingdate
  dsSPHSheatmapdates$intharvestCON <- dsSPHSheatmapdates$intharvestdateCON - dsSPHSheatmapdates$actualplantingdate
  dsSPHSheatmapdates$intharvestREC <- dsSPHSheatmapdates$intharvestdateREC - dsSPHSheatmapdates$actualplantingdate
  
  head(dsSPHSheatmapdates)
  
  #heatmapSPHS <- subset.data.SPHSame(dsSPHSheatmapdates, select=c(HHID, plndOrigdate, actualOrigdate,Origgapdate, Origfert1date,Origfert2date, 
  #Origweed1date,Origweed2date, Origweed3date,Origharvestdate, planting, gappin, 
  #fertilizr1, fertilizr2, weedin1, weedin2, weedin3, harvest ))
  #colnames (dsSPHSheatmapdates)<- c("HHID", "plannedplantingdate", "actualplantingdate", "actualgappingdate", "fertilizerdate1","fertilizerdate2", "weedingdate1",
  #"weedingdate2", "weedingdate3","harvestdate", "planting", "gappin", "fertilizr1", "fertilizr2", "weedin1", "weedin2", "weedin3", "harvest")
  unique(dsSPHSheatmapdates)
  
  #get EA names from dsEAHH
  dsEAHH <- unique(dsEAHH)
  
  dsSPHSheatmap2 <- droplevels(unique(merge(dsSPHSheatmapdates, dsEAHH, by=c("HHID"))))
  head(dsSPHSheatmap2)
  drops <- c("EAID.y","EA_PhoneNr", "EA_Partner", "HH_Name", "HH_PhoneNr", "useCase", "Latitude", "Longitude", "Country", "region_state")
  dsSPHS1 <- (dsSPHSheatmap2[ , !(names(dsSPHSheatmap2) %in% drops)])
  
  head(dsSPHS1)
  dsSPHS <- dsSPHS1
  
  #identify activities not done
  dsSPHS$gapping <- ifelse(is.na(dsSPHS$gappin) & (!is.na(dsSPHS$weedin1) | !is.na(dsSPHS$weedin2)), -999, dsSPHS$gappin)
  
  dsSPHS$weeding1 <-  ifelse(is.na(dsSPHS$weedin1) & (!is.na(dsSPHS$weedin2) | !is.na(dsSPHS$weedin3)), -999, dsSPHS$weedin1)
  dsSPHS$weeding2 <-  ifelse(is.na(dsSPHS$weedin2) & (!is.na(dsSPHS$weedin3) | !is.na(dsSPHS$effharvestCON)), -999, dsSPHS$weedin2)
  dsSPHS$weeding3 <-  ifelse(is.na(dsSPHS$weedin3) & !is.na(dsSPHS$effharvestCON), -999, dsSPHS$weedin3)
  head(dsSPHS)
  
  drops <- c("gappin", "weedin1", "weedin2", "weedin3")
  dsSPHS2 <- (dsSPHS[ , !(names(dsSPHS) %in% drops)])
  head(dsSPHS2)
  
  # dsSPHS3 <- subset(dsSPHS2, select=c("HHID", "season", "EAID", "EA_Name",  "planting", "gapping", "weeding1", "weeding2", "weeding3", 
  #                                     "intharvestCON", "intharvestREC", "effharvestCON",  "effharvestREC"))
  # 
  
  #reshape actual dates
  dsSPHSss1 <- subset(dsSPHS2, select=c(HHID, season, actualplantingdate, Plannedplantingdate,  gapping_date, weeding_date1, weeding_date2, weeding_date3,intharvestdateCON, intharvestdateREC, effharvestdateCON, effharvestdateREC))
  
  colnames(dsSPHSss1) <- c("HHID", "season", "actualplantingdate", "plannedplantingdate", "actualgappingdate", "weedingdate1", "weedingdate2", "weedingdate3", "intharvestdateCON", "intharvestdateREC", "effharvestdateCON", "effharvestdateREC")
  
  dsSPHSss2 <- subset(dsSPHS2, select=c(HHID, EAID, EA_Name, season, planting, gapping, weeding1, weeding2, weeding3, effharvestREC, effharvestCON, intharvestCON, intharvestREC ))
  dsSPHSeshapess1 <- suppressWarnings({dsSPHSss1 %>% gather(actualEvents, actualDates, actualplantingdate, actualgappingdate, weedingdate1, weedingdate2, weedingdate3, intharvestdateCON, intharvestdateREC, effharvestdateCON, effharvestdateREC)})

  dsSPHSeshapess2 <- suppressWarnings({dsSPHSss2 %>% gather(events, dates,planting, gapping, weeding1, weeding2, weeding3, effharvestREC, effharvestCON, intharvestCON, intharvestREC )})
  dsSPHSeshapess2$dates <- as.numeric(dsSPHSeshapess2$dates)
 
  #merge actual dates and solved dates
  dsSPHSeshapess1$events <- gsub("actual", "", dsSPHSeshapess1$actualEvents)
  
  dsSPHSeshapess1$events <- gsub("date", "", dsSPHSeshapess1$events)
  dsSPHSeshape <- merge(dsSPHSeshapess1, dsSPHSeshapess2)
  dim(dsSPHSeshape)
  
  dsSPHSeshape <- unique(dsSPHSeshape)
  head(dsSPHSeshape)
  
  #dsSPHSeshape$diffdate <- today()-anytime::anydate(as.character(dsSPHSeshape$actualDates))
  names(dsSPHSeshape) <- c('HHID', 'season', 'events', "plannedplantingdate", 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 'DatesBnPlantingEvent')
  sphsdata <- dsSPHSeshape
  sphsdata$actualDates <- as.Date(sphsdata$actualDates)

  #get status of events
  getspstatus<- NULL
  for(h in unique(sphsdata$HHID)){
    #maxDate <- max(sphsdata$Dates, na.rm=TRUE)
    sphsdatass <- subset(sphsdata, HHID == h)
    if(!all(is.na(sphsdatass$DatesBnPlantingEvent))){
      #sphsdatass <- subset(sphsdata, HHID == "ACHHNG000151")
      #sphsdatass <- replace(sphsdatass, is.na(sphsdatass), -100)
      
      sphsdatass$status <- ifelse(sphsdatass$events %in% c("planting")  & sphsdatass$DatesBnPlantingEvent  == 0, "On-time",
                                  ifelse(sphsdatass$events %in% c("planting")  & sphsdatass$DatesBnPlantingEvent  < 0 & !sphsdatass$DatesBnPlantingEvent == -999, "Earlier",
                                         ifelse(sphsdatass$events %in% c("planting")  & sphsdatass$DatesBnPlantingEvent  > 0, "Done late",
                                                ifelse(sphsdatass$events %in% c("planting")  & sphsdatass$DatesBnPlantingEvent  == -999, "Not done", 
                                                       
                                                       
                                                       ifelse(sphsdatass$events %in% c("gapping") & sphsdatass$DatesBnPlantingEvent < 28 & !sphsdatass$DatesBnPlantingEvent == -999, "Earlier", 
                                                              ifelse(sphsdatass$events %in% c("gapping") & sphsdatass$DatesBnPlantingEvent  == 28 , "On-time", 
                                                                     ifelse(sphsdatass$events %in% c("gapping") & sphsdatass$DatesBnPlantingEvent > 28 , "Done late",
                                                                            ifelse(sphsdatass$events %in% c("gapping")  & sphsdatass$DatesBnPlantingEvent  == -999, "Not done", 
                                                                                   ifelse(sphsdatass$events %in% c("gapping")  & sphsdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                          
                                                                                          ifelse(sphsdatass$events %in% c("weeding1") & sphsdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                 ifelse(sphsdatass$events %in% c("weeding1")  & sphsdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                        ifelse(sphsdatass$events %in% c("weeding1") & sphsdatass$DatesBnPlantingEvent == -999 , "Not done",                                                                                                                                                   
                                                                                                               
                                                                                                               ifelse(sphsdatass$events %in% c("weeding2") & sphsdatass$DatesBnPlantingEvent > 0, "On-time",
                                                                                                                      ifelse(sphsdatass$events %in% c("weeding2")  & sphsdatass$Dates == -100, "To be done",
                                                                                                                             ifelse(sphsdatass$events %in% c("weeding2") & sphsdatass$DatesBnPlantingEvent == -999 , "Not done",                               
                                                                                                                                    ifelse(sphsdatass$events %in% c("weeding3") & sphsdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                                                           ifelse(sphsdatass$events %in% c("weeding3")  & sphsdatass$Dates == -100, "To be done",
                                                                                                                                                  ifelse(sphsdatass$events %in% c("weeding3") & sphsdatass$DatesBnPlantingEvent == -999 , "Not done",                                                                                                                                                                            
                                                                                                                                                         
                                                                                                                                                         ifelse(sphsdatass$events %in% c("effharvestCON") & sphsdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                                                                                ifelse(sphsdatass$events %in% c("effharvestCON")  & sphsdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                       
                                                                                                                                                                       ifelse(sphsdatass$events %in% c("effharvestREC") & sphsdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                                                                                              ifelse(sphsdatass$events %in% c("effharvestREC")  & sphsdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                     
                                                                                                                                                                                     ifelse(sphsdatass$events %in% c("intharvestCON") & sphsdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                                                                                                            ifelse(sphsdatass$events %in% c("intharvestCON")  & sphsdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                                   
                                                                                                                                                                                                   ifelse(sphsdatass$events %in% c("intharvestREC") & sphsdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                                                                                                                          ifelse(sphsdatass$events %in% c("intharvestREC")  & sphsdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                     
                                                                                                                                                                                     as.character(sphsdatass$status)))))))))))))))))))))))))))                                                                                                                                                             
      
      
      sphsdatass <- replace(sphsdatass, sphsdatass == -100, NA)
      suppressWarnings({getspstatus <- rbind(getspstatus, sphsdatass)})
    }
  } 
  
  
  sphsdata <- getspstatus
  
  
  sphsdata$status <- ifelse(is.na(sphsdata$DatesBnPlantingEvent), "To be done", as.character(sphsdata$status))
  sphsdata$status <- ifelse(is.na(sphsdata$status), "To be done", as.character(sphsdata$status))
  head(sphsdata)
  
  sphsdata$actualmn <- lubridate::month(ymd(sphsdata$actualDates))
  
  sphsdata$month  <- month.abb[sphsdata$actualmn]
  sphsdata$actuald <- lubridate::day(ymd(sphsdata$actualDates))
  sphsdata$actualday <- lubridate::wday(ymd(sphsdata$actualDates))
  sphsdata$actualYR <- lubridate::year(ymd(sphsdata$actualDates))
  
  head(sphsdata)
  
  sphsdata$datetbd <- ifelse(!is.na(sphsdata$actuald), paste(sphsdata$actualYR, sphsdata$actualmn, sphsdata$actuald, sep = "/"), NA)
  
  sphsdata$datetbd2 <- as.Date(sphsdata$datetbd)
  
  
  #fix projected harvest dates for CON and REC
  dsSPHS <- read.csv("data/VAL_SPHS_KW.csv")
  #dsSPHS <- rbind(dsSPHS, read.csv("data/VAL_SPHS_KW.csv"))
  dsSPHS <- rbind(dsSPHS, read.csv("data/VAL_SPHS_OG.csv"))
  dsSPHS <- rbind(dsSPHS, read.csv("data/VAL_SPHS_ON.csv"))
  dsSPHS <- rbind(dsSPHS, read.csv("data/VAL_SPHS_OY.csv"))
  
  dsSPHS <- rbind(dsSPHS, read.csv("data/VAL_SPHS_TZEZ.csv"))
  dsSPHS <- rbind(dsSPHS, read.csv("data/VAL_SPHS_TZLZE.csv"))
  dsSPHS <- rbind(dsSPHS, read.csv("data/VAL_SPHS_TZLZW.csv"))
  dsSPHS <- rbind(dsSPHS, read.csv("data/VAL_SPHS_TZSZ.csv"))
  
  # 
  names(dsSPHS)[grepl('\\.', names(dsSPHS))] <- sub('.*\\.', '', names(dsSPHS)[grepl('\\.', names(dsSPHS))])
  # 
  # pldateissue_dsSPHS <-  unique(dsSPHS[, c("HHID", "plantingDate")])
  # plsp <- as.data.frame(table(pldateissue_dsSPHS$HHID))
  # plsp[plsp$Freq >1, ]
  # dsSPHS_dupPlDate <- droplevels(dsSPHS[dsSPHS$HHID %in% plsp[plsp$Freq >1, ]$Var1, ])
  # dsSPHS <- droplevels(dsSPHS[!dsSPHS$HHID %in% plsp[plsp$Freq >1, ]$Var1, ])
  # dsSPHS_dupPlDate <- getLatestPlDate(dsSPHS_dupPlDate)
  # dsSPHS <- rbind(dsSPHS, dsSPHS_dupPlDate)
  
  
  pldateissue_dsSPHS <-  unique(dsSPHS[, c("HHID", "plantingDate")])
  plsp <- as.data.frame(table(pldateissue_dsSPHS$HHID))
  plsp[plsp$Freq >1, ]
  dsSPHS_dupPlDate <- droplevels(dsSPHS[dsSPHS$HHID %in% plsp[plsp$Freq >1, ]$Var1, ])
  dsSPHS <- droplevels(dsSPHS[!dsSPHS$HHID %in% plsp[plsp$Freq >1, ]$Var1, ])
  dsSPHS_dupPlDate <- getLatestPlDate(dsSPHS_dupPlDate)
  dsSPHS <- rbind(dsSPHS, dsSPHS_dupPlDate)
  
  hvstdate <- subset(dsSPHS, select = c (HHID,harvestDate,optHarvest))
  
  #hvstdate[hvstdate$HHID == "ACHHNG001401", ]
  
  latestharvest <- NULL
  for(hids in unique(hvstdate$HHID)){
    hd <- hvstdate[hvstdate$HHID ==  hids, ]
    hd$actuald <- lubridate::day(mdy(hd$harvestDate))
    hd$actualmn <- lubridate::month(mdy(hd$harvestDate))
    hd$actualYR <- lubridate::year(mdy(hd$harvestDate))
    hd$harvestDate2 <- as.Date(paste(hd$actualYR, hd$actualmn, hd$actuald, sep = "/"))
    hd <- hd[order(hd$harvestDate2), ]
    hd <- hd[nrow(hd), ]
    
    
    #hd$harv1 <- hd$harvestDate2 %m+% months(hd$optHarvest)
    hd$harv1 <- hd$harvestDate2 + 14 * (hd$optHarvest)
    
    hd <- subset(hd, select = c(HHID, harvestDate2, optHarvest,  harv1))
    
    colnames(hd) <-  c('HHID',  'hvstCONdate', 'optHarvest', 'hvstRECdate')
    
    latestharvest <- rbind(latestharvest, hd)
  }
  
  
  
  sphsdata2 <- unique(merge(sphsdata, latestharvest, by='HHID'))
  
  head(sphsdata2)
  
  #  	#sphsdata$date3 <- sphsdata$datetbd2 - sphsdata$actualDates
  DsSPHSdue <- NULL
  for(hids in unique(sphsdata2$HHID)){
    hd <- sphsdata2[sphsdata2$HHID ==  hids, ]
    hdpl <- hd[hd$events == 'planting', ]
    
    hdG <- hd[hd$events == 'gapping', ]
    if(is.na(hdG$datetbd2)){
      hdG$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 28
    }
    
    hdW1 <- hd[hd$events == 'weeding1', ]
    if(is.na(hdW1$datetbd2)){
      hdW1$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 84
    }
    
    hdW2 <- hd[hd$events == 'weeding2', ]
    if(is.na(hdW2$datetbd2)){
      hdW2$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 168
    }
    
    hdW3 <- hd[hd$events == 'weeding3', ]
    if(is.na(hdW3$datetbd2)){
      hdW3$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 252
    }
    
    # hdC <- hd[hd$events == 'harvestCON', ]
    # if(is.na(hdC$datetbd2)){
    #   hdC$datetbd2 <- unique(hd$hvstCONdate)
    # }
    hdintCON <- hd[hd$events == 'intharvestCON', ]
    
    hdintREC <- hd[hd$events == 'intharvestREC', ]
    
    hdeffCON <- hd[hd$events == 'effharvestCON', ]
    
    hdeffREC <- hd[hd$events == 'effharvestREC', ]
   
    
    
    DsSPHSdue <- suppressWarnings({rbind(DsSPHSdue,hdpl, hdG, hdW1,hdW2,hdW3,hdintCON,hdintREC,hdeffCON, hdeffREC )})
  }
  

  names(DsSPHSdue) <- c('HHID', 'season', 'Events', 'plannedplantingdate', 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 
                        'DatesBnPlantingEvent','status',  'actualmn', 'month', 'actuald', 'actualday', 'actualYR',  
                        'datetbd',   'datetbd2', 'hvstCONdate', 'optHarvest', 'hvstRECdate')
  
  DsSPHSdue <- unique(DsSPHSdue)
  head(DsSPHSdue)
  DsSPHSdue$status <- ifelse(DsSPHSdue$Events=='intharvestCON',  'Control', as.character(DsSPHSdue$status))
  DsSPHSdue$status <- ifelse(DsSPHSdue$Events=='intharvestREC',  'Recommended', as.character(DsSPHSdue$status))
  DsSPHSdue$status <- ifelse(DsSPHSdue$status == "To be done"& DsSPHSdue$datetbd2 < Sys.Date(), "Overdue", as.character(DsSPHSdue$status))
  DsSPHSdue$season <- ifelse(DsSPHSdue$season == "NA", 1, as.character(DsSPHSdue$season))
  DsSPHSdue <- droplevels(DsSPHSdue[!is.na(DsSPHSdue$HHID), ] )
  
  # ADDING column for DST run
  DsSPHSdue$datetbd2 <- as.character(DsSPHSdue$datetbd2)
  hhidSP_TT <- droplevels(DsSPHSdue[DsSPHSdue$HHID %in% dst_SP$HHID, ])
  hhidSP_TF <- droplevels(dst_SP[!dst_SP$HHID %in% DsSPHSdue$HHID, ])## dst is run but no data is submitted
  hhidSP_FT <- droplevels(DsSPHSdue[!DsSPHSdue$HHID %in% dst_SP$HHID, ])## dst is not run but there is data
  
  SP_TT <- NULL
  for(hhids in unique(hhidSP_TT$HHID)){
    hd <- hhidSP_TT[hhidSP_TT$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "True"
    hd1$Events <- "DST run"
    hd1$status <- "To be done"
    SP_TT <- rbind(SP_TT, hd, hd1)
  }
  
  
  # eaids <- "ACEANG000123"
  SP_TF <- NULL
  for(eaids in unique(hhidSP_TF$EAID)){
    edata <- DsSPHSdue[DsSPHSdue$EAID == eaids, ]
    hh_nodata <- unique(hhidSP_TF[hhidSP_TF$EAID == eaids, ]$HHID)
    if(length(hh_nodata) > 0){
      asas <- edata[1:length(hh_nodata),]
      asas$HHID <- hh_nodata
      asas$Events <- "DST run"
      asas$actualEvents <- "DST run"
      asas$status <- "To be done"
      asas$datetbd2 <- "True"
      SP_TF <- rbind(SP_TF, edata, asas)
    }
  }
  
  SP_dstchecked <- rbind(SP_TT, SP_TF)
  unique(SP_dstchecked$EAID)
  unique(SP_dstchecked$EAID)
  SP_dstchecked[SP_dstchecked$status == "Recommended", ]
  SP_dstchecked <- droplevels(SP_dstchecked[!is.na(SP_dstchecked$HHID), ])
  SP_dstchecked <- unique(SP_dstchecked)
  SP_dstchecked <- droplevels(SP_dstchecked[!is.na(SP_dstchecked$HHID), ])
  SP_dstchecked[SP_dstchecked$Events == "DST run", ]
  head(SP_dstchecked)
  
  #METRICS
  # length(unique(SP_dstchecked$HHID))
  # length(unique(SP_dstchecked$EAID))
  # SP_dstchecked$COUNTRY <- ifelse(grepl("ACHHTZ", SP_dstchecked$HHID), "TZ", "NG")
  # TZSP <- SP_dstchecked[SP_dstchecked$COUNTRY == "TZ", ]
  # length(unique(TZSP$HHID))
  # length(unique(TZSP$EAID))
 
  #################################################################################################################################################
  #SPHS weeks
  # head(SP_dstchecked)
  # SP_dstchecked$datetbd2 <- as.Date(SP_dstchecked$datetbd2)
  dsSPHSwks2 <- DsSPHSdue
  
  #get days/weeks/months/year from actual dates 
  dsSPHSwks2$planting1wk <- lubridate::week(ymd(dsSPHSwks2$plannedplantingdate))
  dsSPHSwks2$eventwk <- lubridate::week(ymd(dsSPHSwks2$datetbd2))
  dsSPHSwks2$planting1yr <- lubridate::year(ymd(dsSPHSwks2$plannedplantingdate))
  
  SPHSwkhh <- dsSPHSwks2
  
  ##deduct weeks of the year from planting week
  SPHSwkhh$status2 <- ifelse(SPHSwkhh$eventwk == -999, -999, SPHSwkhh$eventwk - SPHSwkhh$planting1wk)
  head(SPHSwkhh)
  
  drops <- c( "DatesBnPlantingEvent", "DiffToday") 
  SPHSwkhh <- (SPHSwkhh[ , !(names(SPHSwkhh) %in% drops)])
  head(SPHSwkhh)
  
  colnames(SPHSwkhh) <- c('HHID', 'season', 'events', 'plannedplanting', 'actualEvents', "actualDates", "EAID","EA_Name", "status", "actualmn","month","actuald", 
                          "actualday", "actualYR","datetbd", "datetbd2", 'hvstCONdate',  'optHarvest',  'hvstRECdate',"planting1wk", "weeks", "planting1yr", "status2")
  
  
  SPHSwksdata <- SPHSwkhh
  
  SPHSwksdata$dueyear <- lubridate::year(SPHSwksdata$datetbd2)
  # icwksdata$diffsys <- SPHSwksdata$datetbd2 > Sys.Date() & SPHSwksdata$dueyear ==  lubridate::year(Sys.Date())
  # SPHSwksdata$status <- ifelse(SPHSwksdata$diffsys == 'TRUE', "Due soon", as.character(SPHSwksdata$status))
  SPHSwksdata$index <- 1:nrow(SPHSwksdata)
  
  toBeDue <- droplevels(SPHSwksdata[SPHSwksdata$datetbd2 > Sys.Date() & SPHSwksdata$dueyear ==  lubridate::year(Sys.Date()), ] )
  
  
  if (nrow(toBeDue) > 0) {
    toBeDue$todayweek <-  as.numeric(lubridate::week(Sys.Date()))
    toBeDue$t2 <- as.numeric(as.character(toBeDue$weeks)) - toBeDue$todayweek
    toBeDue <- toBeDue[toBeDue$t2 <= 2, ]
    #toBeDone <- toBeDue[toBeDue$t2 > 2, ]
    if (nrow(toBeDue) > 0) {
      toBeDue$status <- 'Due soon'
      toBeDue$month <- as.factor(month.abb[lubridate::month(ymd(toBeDue$datetbd2))]) 
      SPHSwksdata <- SPHSwksdata[!SPHSwksdata$index %in% toBeDue$index, ]
      SPHSwksdata <- rbind(SPHSwksdata, toBeDue[, colnames(SPHSwksdata)]) 
    }
    # if (nrow(toBeDone) > 0) {
    #   toBeDone$status <- 'To be done'
    #   toBeDone$month <- as.factor(month.abb[lubridate::month(ymd(toBeDone$datetbd2))]) 
    #   icwksdata <- icwksdata[!icwksdata$index %in% toBeDone$index, ]
    #   icwksdata <- rbind(icwksdata, toBeDone[, colnames(icwksdata)]) 
    # }
    
  }
  
  
  SPHSwksdata <- droplevels(SPHSwksdata[!is.na(SPHSwksdata$HHID), ])
  SPHSwksdata <- droplevels(SPHSwksdata[!is.na(SPHSwksdata$HHID), ])
  SPHSwksdata$status <- ifelse(SPHSwksdata$status == "To be done" & Sys.Date() > SPHSwksdata$datetbd2, "Not done", as.character(SPHSwksdata$status))
  SPHSwksdata$status <- ifelse(is.na(SPHSwksdata$status), "To be done", as.character(SPHSwksdata$status))
  # head(icwksdata)
  # 
  # ppwksdata <- droplevels(ppwksdata[!is.na(ppwksdata$month) , ])
  # 
  # ppwksdata$diffsys <- ppwksdata$datetbd2 > Sys.Date() & ppwksdata$actualYR ==  lubridate::year(Sys.Date())
  # ppwksdata$status <- ifelse(ppwksdata$diffsys == 'TRUE', "Due soon", as.character(ppwksdata$status))
  
  head(SPHSwksdata)
  
  ###################################################
  #SPHS points
  ###################################################
  
  # pldateissue_SPHS <-  unique(VAl_SPHS[, c("HHID", "plantingDate")])
  # plsp <- as.data.frame(table(pldateissue_SPHS$HHID))
  # VAl_SPHS_dupPlDate <- droplevels(VAl_SPHS[VAl_SPHS$HHID %in% plsp[plsp$Freq >1, ]$Var1, ])
  # VAl_SPHS <- droplevels(VAl_SPHS[!VAl_SPHS$HHID %in% plsp[plsp$Freq >1, ]$Var1, ])
  # VAl_SPHS_dupPlDate <- getLatestPlDate(VAl_SPHS_dupPlDate)
  # VAl_SPHS <- rbind(VAl_SPHS, VAl_SPHS_dupPlDate)
  
  dataVAl_SPHS <- read.csv("data/dataVAL_SPHS.csv")
  pointssphsdat <- unique(subset(dataVAl_SPHS, select=c(HHID, EAID, SubmissionDate, event, plantingDate, gappingDate,
                                                        dateWeeding1, dateWeeding2, dateWeeding3, effHarvestDate_CON, effHarvestDate_REC)))
  colnames(pointssphsdat) <- c("HHID", "EAID", "subdate", "p_event", "actualplantingdate", "gapping_date", "weeding1", "weeding2", "weeding3", "cassavahvstdateCON", "cassavahvstdateREC" )
  
  pointssphsdat2 <- droplevels(unique(merge(pointssphsdat, dsEA, by=c("EAID"))))
  
  library(lubridate)	
  # ADDING column for DST run
  # #fix projected harvest dates for CON and REC
  # fls <- paste(wd, "/data/", list.files(path=paste(wd, "/data", sep=""), pattern="VAL_SPHS_"), sep="")
  # fls_TZ <- fls[grep("TZ", fls)]
  # fls_NG <- fls[-grep("TZ", fls)]
  # dst_TZ <- dropGroupNames(do.call(rbind, lapply(fls_TZ, function(x) read.csv(x))))
  # dst_NG <- dropGroupNames(do.call(rbind, lapply(fls_NG, function(x) read.csv(x))))
  # dst_TZ$season <- 'NA'
  # dst_TZ <- dst_TZ[, colnames(dst_NG)]
  # dst <- rbind(dst_TZ, dst_NG)
  VAl_SPHS_KW <- read.csv("data/VAL_SPHS_KW.csv",stringsAsFactors=FALSE)
  VAl_SPHS_OG <- read.csv("data/VAL_SPHS_OG.csv",stringsAsFactors=FALSE)
  VAl_SPHS_ON <- read.csv("data/VAL_SPHS_ON.csv",stringsAsFactors=FALSE)
  VAl_SPHS_OY <- read.csv("data/VAL_SPHS_OY.csv",stringsAsFactors=FALSE)
  
  VAl_SPHS_TZEZ <- read.csv("data/VAL_SPHS_TZEZ.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZLZE <- read.csv("data/VAL_SPHS_TZLZE.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZLZW <- read.csv("data/VAL_SPHS_TZLZW.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZSZ <- read.csv("data/VAL_SPHS_TZSZ.csv",stringsAsFactors=FALSE)
  
  
  VAl_SPHS_NG <- rbind(VAl_SPHS_KW, VAl_SPHS_OG, VAl_SPHS_ON, VAl_SPHS_OY)
  VAl_SPHS_TZ <- rbind(VAl_SPHS_TZEZ, VAl_SPHS_TZLZE, VAl_SPHS_TZLZW, VAl_SPHS_TZSZ)
  VAl_SPHS_NG <- VAl_SPHS_NG[, colnames(VAl_SPHS_TZ)]
  VAl_SPHS <- rbind(VAl_SPHS_NG, VAl_SPHS_TZ)
  dsSPHS <- VAl_SPHS
 
  
  VAl_SPpts <- subset(dsSPHSp, select = c(HHID, SubmissionDate))
  VAl_SPpts$SubmissionDate <- mdy_hms(VAl_SPpts$SubmissionDate)
  VAl_SPpts$SubmissionDate <- as.Date(as.POSIXct(VAl_SPpts$SubmissionDate, format = "%m/%d/%Y"))
  head(VAl_SPpts)
  pts_valSP <- merge(pointssphsdat2, VAl_SPpts, by=c("HHID") )
  
  
  SP_runs <- NULL
  for(hhids in unique(pts_valSP$HHID)){
    hd <- pts_valSP[pts_valSP$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$p_event <- "DST run"
    SP_runs <- rbind(SP_runs, hd, hd1)
  }
  
  
  head(SP_runs)
  
  pointssphs <-SP_runs
  
  pointssphs$EA_Name<- ifelse(pointssphs$EA_Name %in% "Wargaret Asuo", "Margaret Asuo", as.character(pointssphs$EA_Name))
  pointssphs$subdate <-mdy_hms(pointssphs$subdate)
  pointssphs$subdate <- as.Date(as.POSIXct(pointssphs$subdate, format = "%m/%d/%Y"))
  
  pointssphs$actualmn <- lubridate::month(ymd(pointssphs$subdate))
  
  pointssphs$month  <- month.abb[pointssphs$actualmn]
  pointssphs$actuald <- lubridate::day(ymd(pointssphs$subdate))
  pointssphs$actualday <- lubridate::wday(ymd(pointssphs$subdate))
  pointssphs$actualYR <- lubridate::year(ymd(pointssphs$subdate))
  
  pointssphs$submn <- lubridate::month(ymd(pointssphs$SubmissionDate))
  pointssphs$submonth  <- month.abb[pointssphs$submn]
  pointssphs$subd <- lubridate::day(ymd(pointssphs$SubmissionDate))
  pointssphs$subday <- lubridate::wday(ymd(pointssphs$SubmissionDate))
  pointssphs$subYR <- lubridate::year(ymd(pointssphs$SubmissionDate))
  head(pointssphs)
  pointssphs <- unique(pointssphs)
  
  #Omit data for events not done
  pointssphs$event1 <-as.numeric(ifelse ((!is.na(pointssphs$subdate)) &  (pointssphs$p_event == "event0"), "2", "0"))
  pointssphs$event1 <-as.numeric(ifelse ((!is.na(pointssphs$subdate)) &  (pointssphs$p_event == "event1"), "2", "0"))
  pointssphs$event2 <- as.numeric(ifelse ((!is.na(pointssphs$subdate)) &  (pointssphs$p_event == "event2"), "2", "0"))
  pointssphs$event3 <- as.numeric(ifelse ((!is.na(pointssphs$subdate)) &  (pointssphs$p_event == "event3"), "2", "0"))
  pointssphs$event4 <- as.numeric(ifelse ((!is.na(pointssphs$subdate)) &  (pointssphs$p_event == "event4"), "2", "0"))
  pointssphs$event5 <- as.numeric(ifelse ((!is.na(pointssphs$subdate)) &  (pointssphs$p_event == "event5"), "2", "0"))
  pointssphs$event6 <- as.numeric(ifelse ((!is.na(pointssphs$subdate)) &  (pointssphs$p_event == "event6"), "5", "0"))
  pointssphs$event7 <- as.numeric(ifelse ((!is.na(pointssphs$subdate)) &  (pointssphs$p_event == "event7"), "5", "0"))
  pointssphs$DST_run <- as.numeric(ifelse ((!is.na(pointssphs$SubmissionDate)) &  (pointssphs$p_event == "DST run"), "2", "0"))
  
  SPHSpts <- subset(pointssphs, select = c(HHID, EAID, EA_Name, event1, event2, event3, event4, event5, event6, event7, DST_run))
  head(SPHSpts)
  
  
  library(dplyr)
  SPHSpoints <- SPHSpts %>% group_by(HHID, EAID, EA_Name) %>% summarise_at(1:8, sum)
  SPHSpoints <- data.table::setDT(SPHSpoints)[HHID!="ACHHNG123456" & HHID!="ACHHNG000000"]
  SPHSpoints = unique(SPHSpoints)
  
  SPHSpoints$Totalpoints <- SPHSpoints$event1+SPHSpoints$event2+SPHSpoints$event3+ SPHSpoints$event4+SPHSpoints$event5+SPHSpoints$event6 +SPHSpoints$event7 + SPHSpoints$DST_run
  head(SPHSpoints)
  dsSPHSpnts1 <- subset(pointssphs, select=c(EAID, actualYR, subYR, month, submonth, event1, event2, event3, event4, event5, event6, event7, DST_run))
  dsSPHSpnts2 <- dsSPHSpnts1 %>% gather(events, points,event1, event2, event3, event4, event5, event6, event7, DST_run)
  
  dsSPHSpnts2$month[dsSPHSpnts2$events == "DST_run"] <- dsSPHSpnts2$submonth
  dsSPHSpnts2$actualYR[dsSPHSpnts2$events == "DST_run"] <- dsSPHSpnts2$subYR
  
  head(dsSPHSpnts2)	
  
  dsSPHSpnts2 <- subset(dsSPHSpnts2, select=c(EAID, events, actualYR, month, points))
  colnames (dsSPHSpnts2) <- c("EAID", "Events", "Year", "month", "points")
  
  dsSPHSpnts2 <- droplevels(dsSPHSpnts2[!is.na(dsSPHSpnts2$month), ])
  
  dsSPHSpnts3 <- dsSPHSpnts2 %>% dplyr::mutate(i = row_number()) %>% tidyr::spread(month, points)
  
  dsSPHSpnts3 <- subset(dsSPHSpnts3, select = -c(i))
  
  head(dsSPHSpnts3)
  
  #fix all calendar months not available in data

  Notavailmonths <- month.abb[!month.abb %in% colnames(dsSPHSpnts3)]
  x <- suppressWarnings({as.data.frame(matrix(ncol=length(Notavailmonths), nrow=nrow(dsSPHSpnts3), data=as.numeric('NA')))})
  colnames(x)<-  Notavailmonths 
  dsSPHSpnts4 <- cbind(dsSPHSpnts3, x)
  head(dsSPHSpnts4)
  
  #summarize points
  SPHSpointsdt <- ddply(dsSPHSpnts4, .(EAID, Events, Year), summarize,
                        Jan = sum(Jan, na.rm = TRUE),
                        Feb = sum(Feb, na.rm = TRUE),
                        Mar = sum(Mar, na.rm = TRUE),
                        Apr = sum(Apr, na.rm = TRUE),
                        May = sum(May, na.rm = TRUE),
                        Jun = sum(Jun, na.rm = TRUE),
                        Jul = sum(Jul, na.rm = TRUE),
                        Aug = sum(Aug, na.rm = TRUE),
                        Sep = sum(Sep, na.rm = TRUE),
                        Oct = sum(Oct, na.rm = TRUE),
                        Nov = sum(Nov, na.rm = TRUE),
                        Dec = sum(Dec, na.rm = TRUE))
  
  SPHSpointsdt2 <- ddply(SPHSpointsdt, .(EAID,Events, Year,  Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
                         summarize, Total = sum(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec))
  
  SPHSpointsdt3 <- ddply(SPHSpointsdt, .(EAID, Year),
                         summarize, Jan = sum(Jan), Feb=sum(Feb), Mar=sum(Mar), Apr=sum(Apr), May=sum(May), Jun=sum(Jun), Jul=sum(Jul), Aug=sum(Aug), 
                         Sep=sum(Sep), Oct=sum(Oct), Nov=sum(Nov), Dec=sum(Dec))
  SPHSpointsdt3$Events <- "Total"
  SPHSpointsdt3$Total <- ""
  SPHSpointsdt3 <- SPHSpointsdt3[, colnames(SPHSpointsdt2)]
  
  SPHSpointsdt2 <- SPHSpointsdt2[SPHSpointsdt2$Total >0, ]
  SPHSpointsdt2 <- rbind(SPHSpointsdt2, SPHSpointsdt3)
  
  head(SPHSpointsdt2)
  
  #########################################################################################################################################
  
  ########################################################################################
  # SPNG data
  ########################################################################################
  #uncomment when running
  #dat_SPNG <- read.csv("data/dataVAl_SPHS.csv")
  
  
  # dsSPNGactual <- unique(subset(dataVAl_SPNG, select=c(HHID,  plantingDetails.plantingDate, gappingDetails.gappingDate,weedingDetails.dateWeeding1, weedingDetails.dateWeeding2, weedingDetails.dateWeeding3, harvestDate.intHarvestDate_CON, harvestDate.intHarvestDate_REC, effHarvestDate_REC,effHarvestDate_CON )))
  # 
  dsSPNGH <- unique(subset(dat_SPNG, select=c(HHID,  seasonSelect, plantingDate, intHarvestDate_CON, intHarvestDate_REC,
                                              effHarvestDate_CON_Tri,effHarvestDate_REC_Tri
  )))
  #find latest dates for planting 
  pldateissue_VAl_SPNG <-  unique(dsSPNGH[, c("HHID", "plantingDate")])
  plSPNG <- as.data.frame(table(pldateissue_VAl_SPNG$HHID))
  plSPNG[plSPNG$Freq >1, ]
  VAl_SPNG_dupPlDate <- droplevels(dsSPNGH[dsSPNGH$HHID %in% plSPNG[plSPNG$Freq >1, ]$Var1, ])
  dsSPNGH <- droplevels(dsSPNGH[!dsSPNGH$HHID %in% plSPNG[plSPNG$Freq >1, ]$Var1, ])
  VAl_SPNG_dupPlDate <- getLatestPlDate(VAl_SPNG_dupPlDate)
  dsSPNGH <- rbind(dsSPNGH, VAl_SPNG_dupPlDate)
  
  #drop season
  dsSPNGH2 <- droplevels(dsSPNGH[!is.na(dsSPNGH$seasonSelect), ])
  dsSPNGH3 <- droplevels(dsSPNGH2[dsSPNGH2$seasonSelect ==3, ])
  dsSPNGH4 <- unique(subset(dsSPNGH, select=c(HHID, seasonSelect, plantingDate)))
  
  
  #dsSPH4 <- unique(subset(dsSPH3, select=c(HHID, seasonSelect, monitored, rapidYield, fieldID, reject, plantingDate )))
  # 
  #dsICheatmap <- droplevels(unique(merge(dsICplanned, dsICactual, by=c("HHID"))))
  #SHOULD WE INCLUDE THE EFF HARVEST DATES?
  
  # dsSPHSactual <- unique(subset(dsSPH3, select=c(HHID, intHarvestDate_CON, intHarvestDate_REC, effHarvestDate_CON_Tri,effHarvestDate_REC_Tri)))
  colnames(dsSPNGH3) <- c("HHID", "seasonSelect", "actualplantingdate", "intHarvestDate_CON", "intHarvestDate_REC",
                          "effHarvestDate_CON_Tri","effHarvestDate_REC_Tri")
  head(dsSPNGH3)
  
  
  # dfSPNG = as.data.frame(dsSPNGheatmap %>%
  #                         dplyr::group_by(HHID) %>%
  #                         fill(everything(), .direction = "down") %>%
  #                         fill(everything(), .direction = "up") %>%
  #                         slice(1))
  
  dsSPNGheatmap <- EventLatestDate(usecasedata=dsSPNGH3, usecase="SP")
  dsSPNGheatmap <- unique(merge(dsSPNGH4, dsSPNGheatmap, by=c("HHID" )))
  dsSPNGheatmap <- subset(dsSPNGheatmap,select=-c(seasonSelect.y))
  head(dsSPNGheatmap)
  colnames(dsSPNGheatmap) <- c("HHID", "seasonSelect", "plantingDate", "actualplantingdate", "intHarvestDate_CON", "intHarvestDate_REC",
                                 "effHarvestDate_CON_Tri","effHarvestDate_REC_Tri")
  #removed section on planned and actual planting dates 1978-2038
  
  #solve dates into days of the year
  
  plantng <- solvedates(colNr = 4, usecasedata = dsSPNGheatmap)
  
  head(plantng)
  
  inthvstcon <- solvedates(colNr = 5, usecasedata=dsSPNGheatmap)
  head(inthvstcon)
  inthvstrec <- solvedates(colNr = 6, usecasedata=dsSPNGheatmap)
  head(inthvstrec)
  
  effhvstdateCON <- solvedates(colNr = 7, usecasedata = dsSPNGheatmap)
  
  colnames(effhvstdateCON) <- c("HHID", "effharvestdateCON")
  head(effhvstdateCON)
  
  effhvstdateREC <- solvedates(colNr = 8, usecasedata = dsSPNGheatmap)
  colnames(effhvstdateREC) <- c("HHID", "effharvestdateREC")
  head(effhvstdateREC)
  
  seasoninfo <- unique(subset(dsSPNGheatmap, select = c("HHID", "seasonSelect")))
  
  #merge solved days of the year
  SPNG <- NULL
  SPNG1 <- unique(merge(plantng, inthvstcon, by="HHID", all=TRUE))
  SPNG2 <- unique(merge(SPNG1, inthvstrec, by="HHID", all=TRUE))
  SPNG3 <- unique(merge(SPNG2, effhvstdateCON, by="HHID", all=TRUE))
  SPNG4 <- unique(merge(SPNG3, effhvstdateREC, by="HHID", all=TRUE))
  SPNG5 <- unique(merge(SPNG4, seasoninfo, by = "HHID", all=TRUE))
  dsSPNGheatmapdates <- unique(SPNG5)
  
  head(dsSPNGheatmapdates)
  colnames (dsSPNGheatmapdates) <- c("HHID", "plantingDate", "intHarvestDate_CON", "intHarvestDate_REC",
                                       "effharvestdateCON", "effharvestdateREC",  "season")
  
  #deduct solved days of the year against actual planting dates
  dsSPNGheatmapdates$planting <-  dsSPNGheatmapdates$plantingDate - dsSPNGheatmapdates$plantingDate
  dsSPNGheatmapdates$effharvestREC <- dsSPNGheatmapdates$effharvestdateREC - dsSPNGheatmapdates$plantingDate
  dsSPNGheatmapdates$effharvestCON <- dsSPNGheatmapdates$effharvestdateCON - dsSPNGheatmapdates$plantingDate
  dsSPNGheatmapdates$intharvestCON <- dsSPNGheatmapdates$intHarvestDate_CON - dsSPNGheatmapdates$plantingDate
  dsSPNGheatmapdates$intharvestREC <- dsSPNGheatmapdates$intHarvestDate_REC - dsSPNGheatmapdates$plantingDate
  
  head(dsSPNGheatmapdates)
  
  #get EA names from dsEAHH
  dsEAHH <- unique(dsEAHH)
  
  dsSPNGheatmap2 <- droplevels(unique(merge(dsSPNGheatmapdates, dsEAHH, by=c("HHID"))))
  head(dsSPNGheatmap2)
  drops <- c("EAID.y","EA_PhoneNr", "EA_Partner", "HH_Name", "HH_PhoneNr", "useCase", "Latitude", "Longitude", "Country", "region_state")
  dsSPNGheatmap3 <- (dsSPNGheatmap2[ , !(names(dsSPNGheatmap2) %in% drops)])
  
  head(dsSPNGheatmap3)
  dsSPNGheatmap3$actualplantingdate <- dsSPNGheatmap3$plantingDate
  dsSPNGheatmap3$EA_Name <- ifelse(dsSPNGheatmap3$EA_Name == "GAFARAKINWUMI OMOTOSHO", "Gafar Omotosho", as.character(dsSPNGheatmap3$EA_Name))
  
  dsSPNGH5 <- unique(subset(dat_SPNG, select=c(HHID, monitored, reject )))
  
  #find latest dates for planting 
  pldateissue_VAl_SPNG <-  unique(dsSPNGH5[, c("HHID",  "reject")])
  plSPNG <- as.data.frame(table(pldateissue_VAl_SPNG$HHID))
  plSPNG[plSPNG$Freq >1, ]
  VAl_SPNG_dupPlDate <- droplevels(dsSPNGH5[dsSPNGH5$HHID %in% plSPNG[plSPNG$Freq >1, ]$Var1, ])
  dsSPNGH5 <- droplevels(dsSPNGH5[!dsSPNGH5$HHID %in% plSPNG[plSPNG$Freq >1, ]$Var1, ])
  VAl_SPNG_dupPlDate <- droplevels(VAl_SPNG_dupPlDate[VAl_SPNG_dupPlDate$reject == 'FALSE', ])
  dsSPNGH5 <- rbind(dsSPNGH5, VAl_SPNG_dupPlDate)
  dsSPNGH5 <- droplevels(dsSPNGH5[!is.na(dsSPNGH5$HHID), ])
  dsSPNGss <- unique(merge(dsSPNGheatmap3,dsSPNGH5))
  head(dsSPNGss)
  
  #reshape actual dates
  dsSPNGss1 <- subset(dsSPNGss, select=c(HHID, season, plantingDate,  actualplantingdate, intHarvestDate_CON, intHarvestDate_REC,effharvestdateCON, effharvestdateREC))
  
  colnames(dsSPNGss1) <- c("HHID", "season", "plantingdate",  "actualplantingdate", "intharvestdateCON", "intharvestdateREC", "effharvestdateCON", "effharvestdateREC")
  
  dsSPNGss2 <- subset(dsSPNGss, select=c(HHID, EAID, EA_Name,  season, planting, effharvestREC, effharvestCON, intharvestCON, intharvestREC ))
  
  dsSPNGeshapess1 <- suppressWarnings({dsSPNGss1 %>% tidyr::gather(actualEvents, actualDates,  plantingdate, intharvestdateCON, intharvestdateREC, effharvestdateCON, effharvestdateREC)})
  
  dsSPNGeshapess2 <- suppressWarnings({dsSPNGss2 %>% tidyr::gather(events, dates,  planting, effharvestREC, effharvestCON, intharvestCON, intharvestREC )})
  #dsSPNGeshapess2$dates <- as.numeric(dsSPNGeshapess2$dates)
  
  #merge actual dates and solved dates
  dsSPNGeshapess1$events <- gsub("actual", "", dsSPNGeshapess1$actualEvents)
  
  dsSPNGeshapess1$events <- gsub("date", "", dsSPNGeshapess1$events)
  dsSPNGeshape <- merge(dsSPNGeshapess1, dsSPNGeshapess2)
  dim(dsSPNGeshape)
  
  dsSPNGeshape <- unique(dsSPNGeshape)
  unique(dsSPNGeshape$events)
  dsSPNGeshape$dates <- as.numeric(dsSPNGeshape$dates)
  head(dsSPNGeshape)
  
  
  
  #dsSPNGeshape$diffdate <- today()-anytime::anydate(as.character(dsSPNGeshape$actualDates))
  names(dsSPNGeshape) <- c('HHID', 'season', 'events', "plannedplantingdate", 'actualEvents', 'actualDates', 'EAID', 
                             'EA_Name',  'DatesBnPlantingEvent')
  
  
  dsSPNGeshape1 <- droplevels(dsSPNGeshape[!dsSPNGeshape$actualEvents == "monitoreddate", ])
  SPNGdata <- droplevels(dsSPNGeshape1[!dsSPNGeshape1$actualEvents == "rejectdate", ])
  SPNGdata2 <- droplevels(dsSPNGeshape[dsSPNGeshape$actualEvents == "monitoreddate" | dsSPNGeshape$actualEvents == "rejectdate", ])
  
  
  #get status of events
  getSPNGstatus<- NULL
  for(h in unique(SPNGdata$HHID)){
    #maxDate <- max(SPNGdata$Dates, na.rm=TRUE)
    SPNGdatass <- subset(SPNGdata, HHID == h)
    if(!all(is.na(SPNGdatass$DatesBnPlantingEvent))){
      #sphsdatass <- subset(sphsdata, HHID == "ACHHNG000151")
      #sphsdatass <- replace(sphsdatass, is.na(sphsdatass), -100)
      
      SPNGdatass$status <-    
        ifelse(SPNGdatass$events %in% c("planting")  & SPNGdatass$DatesBnPlantingEvent  == 0, "On-time",
               ifelse(SPNGdatass$events %in% c("effharvestCON") & SPNGdatass$DatesBnPlantingEvent > 0, "On-time", 
                      ifelse(SPNGdatass$events %in% c("effharvestCON")  & SPNGdatass$DatesBnPlantingEvent == -100, "To be done",
                             
                             ifelse(SPNGdatass$events %in% c("effharvestREC") & SPNGdatass$DatesBnPlantingEvent > 0, "On-time", 
                                    ifelse(SPNGdatass$events %in% c("effharvestREC")  & SPNGdatass$DatesBnPlantingEvent == -100, "To be done",
                                           
                                           ifelse(SPNGdatass$events %in% c("intharvestCON") & SPNGdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                  ifelse(SPNGdatass$events %in% c("intharvestCON")  & SPNGdatass$DatesBnPlantingEvent == -100, "To be done",
                                                         
                                                         ifelse(SPNGdatass$events %in% c("intharvestREC") & SPNGdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                ifelse(SPNGdatass$events %in% c("intharvestREC")  & SPNGdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                       
                                                                       as.character(SPNGdatass$status))))))))))          
      
      
      
      SPNGdatass <- replace(SPNGdatass, SPNGdatass == -100, NA)
      suppressWarnings({getSPNGstatus <- rbind(getSPNGstatus, SPNGdatass)})
    }
  } 
  
  
  SPNGdata <- getSPNGstatus
  head(SPNGdata)
  
  SPNGdata$status <- ifelse(is.na(SPNGdata$DatesBnPlantingEvent), "To be done", as.character(SPNGdata$status))
  SPNGdata$status <- ifelse(is.na(SPNGdata$status), "To be done", as.character(SPNGdata$status))
  head(SPNGdata)
  
  SPNGdata$actualmn <- lubridate::month(ymd(SPNGdata$actualDates))
  
  SPNGdata$month  <- month.abb[SPNGdata$actualmn]
  SPNGdata$actuald <- lubridate::day(ymd(SPNGdata$actualDates))
  SPNGdata$actualday <- lubridate::wday(ymd(SPNGdata$actualDates))
  SPNGdata$actualYR <- lubridate::year(ymd(SPNGdata$actualDates))
  
  head(SPNGdata)
  
  SPNGdata$datetbd <- ifelse(!is.na(SPNGdata$actuald), paste(SPNGdata$actualYR, SPNGdata$actualmn, SPNGdata$actuald, sep = "/"), NA)
  
  SPNGdata$datetbd2 <- as.Date(SPNGdata$datetbd)
  
  DsSPNGdue <- SPNGdata
  
  names(DsSPNGdue) <- c('HHID', 'season', 'Events', 'plannedplantingdate', 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 
                          'DatesBnPlantingEvent','status',  'actualmn', 'month', 'actuald', 'actualday', 'actualYR',  
                          'datetbd',   'datetbd2')
  
  DsSPNGdue <- unique(DsSPNGdue)
  head(DsSPNGdue)
  DsSPNGdue$DatesBnPlantingEvent <- as.numeric(DsSPNGdue$DatesBnPlantingEvent)
  
  DsSPNGdue$status <- ifelse(DsSPNGdue$Events=='intharvestCON',  'Control', as.character(DsSPNGdue$status))
  DsSPNGdue$status <- ifelse(DsSPNGdue$Events=='intharvestREC',  'Recommended', as.character(DsSPNGdue$status))
  
  DsSPNGdue$Events <- ifelse(DsSPNGdue$Events=='intharvestREC',  'int. harvest red plot', as.character(DsSPNGdue$Events))
  DsSPNGdue$Events <- ifelse(DsSPNGdue$Events=='effharvestREC',  'eff. harvest red plot', as.character(DsSPNGdue$Events))
  DsSPNGdue$Events <- ifelse(DsSPNGdue$Events=='intharvestCON',  'int. harvest blue plot', as.character(DsSPNGdue$Events))
  DsSPNGdue$Events <- ifelse(DsSPNGdue$Events=='effharvestCON',  'eff. harvest blue plot', as.character(DsSPNGdue$Events))
  DsSPNGdue$Events <- ifelse(DsSPNGdue$Events=='Reject',  'ok field', as.character(DsSPNGdue$Events))
  
  DsSPNGdue$status <- ifelse(DsSPNGdue$status == "To be done"& DsSPNGdue$datetbd2 < Sys.Date(), "Overdue", as.character(DsSPNGdue$status))
  DsSPNGdue$season <- ifelse(DsSPNGdue$season == "NA", 1, as.character(DsSPNGdue$season))
  SPNG_dstchecked <- droplevels(DsSPNGdue[!is.na(DsSPNGdue$HHID), ] )
  
  
  SPNG_dstchecked[SPNG_dstchecked$status == "Recommended", ]
  SPNG_dstchecked <- droplevels(SPNG_dstchecked[!is.na(SPNG_dstchecked$HHID), ])
  SPNG_dstchecked <- unique(SPNG_dstchecked)
  
  head(SPNG_dstchecked)
  SPNG_dstchecked$datetbd2 <- as.character( SPNG_dstchecked$datetbd2)
  dsSPNGH4 <- unique(subset(dat_SPNG, select=c(HHID, monitored, rapidYield, reject )))
  hhidSPNG_TF <- droplevels(dsSPNGH4[dsSPNGH4$reject %in% c("FALSE"), ])
  hhidSPNG_TT <- droplevels(dsSPNGH4[dsSPNGH4$reject %in% c("TRUE"), ])
  
  hhidSPNG_monY <- droplevels(dsSPNGH4[dsSPNGH4$monitored %in% c("yes"), ])
  hhidSPNG_monN <- droplevels(dsSPNGH4[dsSPNGH4$monitored %in% c("no"), ])
  
  hhidSPNG_rapY <- droplevels(dsSPNGH4[dsSPNGH4$rapidYield %in% c("yes"), ])
  hhidSPNG_rapN <- droplevels(dsSPNGH4[dsSPNGH4$rapidYield %in% c("no"), ])
  
  
  hhidSPNG_TT2 <- droplevels(SPNG_dstchecked[SPNG_dstchecked$HHID %in% hhidSPNG_TT$HHID, ])
  hhidSPNG_TF2 <- droplevels(SPNG_dstchecked[SPNG_dstchecked$HHID %in% hhidSPNG_TF$HHID, ])
  
  hhidSPNG_monY2 <- droplevels(SPNG_dstchecked[SPNG_dstchecked$HHID %in% hhidSPNG_monY$HHID, ])
  hhidSPNG_monN2 <- droplevels(SPNG_dstchecked[SPNG_dstchecked$HHID %in% hhidSPNG_monN$HHID, ])
  
  hhidSPNG_rapY2 <- droplevels(SPNG_dstchecked[SPNG_dstchecked$HHID %in% hhidSPNG_rapY$HHID, ])
  hhidSPNG_rapN2 <- droplevels(SPNG_dstchecked[SPNG_dstchecked$HHID %in% hhidSPNG_rapN$HHID, ])
  
  SPNG_rapY <- NULL
  for(hhids in unique(hhidSPNG_rapY2$HHID)){
    hd <- hhidSPNG_rapY2[hhidSPNG_rapY2$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "yes"
    hd1$Events <- "rapid yield"
    hd1$status <- "To be done"
    SPNG_rapY <- rbind(SPNG_rapY, hd, hd1)
  }
  
  SPNG_rapN <- NULL
  for(hhids in unique(hhidSPNG_rapN2$HHID)){
    hd <- hhidSPNG_rapN2[hhidSPNG_rapN2$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "no"
    hd1$Events <- "rapid yield"
    hd1$status <- "To be done"
    SPNG_rapN <- rbind(SPNG_rapN, hd, hd1)
  }
  
  
  
  SPNG_monY <- NULL
  for(hhids in unique(hhidSPNG_monY2$HHID)){
    hd <- hhidSPNG_monY2[hhidSPNG_monY2$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "yes"
    hd1$Events <- "monitored"
    hd1$status <- "To be done"
    SPNG_monY <- rbind(SPNG_monY, hd, hd1)
  }
  
  SPNG_monN <- NULL
  for(hhids in unique(hhidSPNG_monN2$HHID)){
    hd <- hhidSPNG_monN2[hhidSPNG_monN2$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "no"
    hd1$Events <- "monitored"
    hd1$status <- "To be done"
    SPNG_monN <- rbind(SPNG_monN, hd, hd1)
  }
  
  
  SPNG_TT <- NULL
  for(hhids in unique(hhidSPNG_TT2$HHID)){
    hd <- hhidSPNG_TT2[hhidSPNG_TT2$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "TRUE"
    hd1$Events <- "reject"
    hd1$status <- "To be done"
    SPNG_TT <- rbind(SPNG_TT, hd, hd1)
  }
  
  SPNG_TF <- NULL
  for(hhids in unique(hhidSPNG_TF2$HHID)){
    hd <- hhidSPNG_TF2[hhidSPNG_TF2$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "FALSE"
    hd1$Events <- "reject"
    hd1$status <- "To be done"
    SPNG_TF <- rbind(SPNG_TF, hd, hd1)
  }
  
  
  SPNG_dstchecked <- rbind(SPNG_monN, SPNG_monY, SPNG_rapN, SPNG_rapY, SPNG_TT, SPNG_TF)
  SPNG_dstchecked$Events <- ifelse(SPNG_dstchecked$Events=='reject',  'ok field', as.character(SPNG_dstchecked$Events))
  SPNG_dstchecked[SPNG_dstchecked$HHID == "ACHHNG006284", ]
  unique(SPNG_dstchecked$EAID)
  # dsSPNGactual <- unique(subset(dataVAl_SPNG, select=c(HHID,  plantingDetails.plantingDate, gappingDetails.gappingDate,weedingDetails.dateWeeding1, weedingDetails.dateWeeding2, weedingDetails.dateWeeding3, harvestDate.intHarvestDate_CON, harvestDate.intHarvestDate_REC, effHarvestDate_REC,effHarvestDate_CON )))
  # 
  #dsSPNGH4 <- unique(subset(dat_SPNG, select=c(HHID, monitored, rapidYield, reject
  #)))
  #################################################################################################################################################
  #SPNG weeks
  # head(SPNG_dstchecked)
  # SPNG_dstchecked$datetbd2 <- as.Date(SPNG_dstchecked$datetbd2)
  dsSPNGwks2 <- DsSPNGdue
  
  #get days/weeks/months/year from actual dates 
  dsSPNGwks2$planting1wk <- lubridate::week(ymd(dsSPNGwks2$plannedplantingdate))
  dsSPNGwks2$eventwk <- lubridate::week(ymd(dsSPNGwks2$datetbd2))
  dsSPNGwks2$planting1yr <- lubridate::year(ymd(dsSPNGwks2$plannedplantingdate))
  
  SPNGwkhh <- dsSPNGwks2
  
  ##deduct weeks of the year from planting week
  SPNGwkhh$status2 <- ifelse(SPNGwkhh$eventwk == -999, -999, SPNGwkhh$eventwk - SPNGwkhh$planting1wk)
  head(SPNGwkhh)
  
  drops <- c( "DatesBnPlantingEvent") 
  SPNGwkhh <- (SPNGwkhh[ , !(names(SPNGwkhh) %in% drops)])
  head(SPNGwkhh)
  
  colnames(SPNGwkhh) <- c('HHID', 'season', 'events', 'plannedplanting', 'actualEvents', "actualDates", "EAID","EA_Name", 
                            "status", "actualmn","month","actuald", "actualday", "actualYR","datetbd", "datetbd2", 
                            "planting1wk", "weeks", "planting1yr", "status2")
  
  
  SPNGwksdata <- SPNGwkhh
  
  SPNGwksdata$dueyear <- lubridate::year(SPNGwksdata$datetbd2)
  # icwksdata$diffsys <- SPNGwksdata$datetbd2 > Sys.Date() & SPNGwksdata$dueyear ==  lubridate::year(Sys.Date())
  # SPNGwksdata$status <- ifelse(SPNGwksdata$diffsys == 'TRUE', "Due soon", as.character(SPNGwksdata$status))
  SPNGwksdata$index <- 1:nrow(SPNGwksdata)
  
  toBeDue <- droplevels(SPNGwksdata[SPNGwksdata$datetbd2 > Sys.Date() & SPNGwksdata$dueyear ==  lubridate::year(Sys.Date()), ] )
  
  
  if (nrow(toBeDue) > 0) {
    toBeDue$todayweek <-  as.numeric(lubridate::week(Sys.Date()))
    toBeDue$t2 <- as.numeric(as.character(toBeDue$weeks)) - toBeDue$todayweek
    toBeDue <- toBeDue[toBeDue$t2 <= 2, ]
    #toBeDone <- toBeDue[toBeDue$t2 > 2, ]
    if (nrow(toBeDue) > 0) {
      toBeDue$status <- 'Due soon'
      toBeDue$month <- as.factor(month.abb[lubridate::month(ymd(toBeDue$datetbd2))]) 
      SPNGwksdata <- SPNGwksdata[!SPNGwksdata$index %in% toBeDue$index, ]
      SPNGwksdata <- rbind(SPNGwksdata, toBeDue[, colnames(SPNGwksdata)]) 
    }
    # if (nrow(toBeDone) > 0) {
    #   toBeDone$status <- 'To be done'
    #   toBeDone$month <- as.factor(month.abb[lubridate::month(ymd(toBeDone$datetbd2))]) 
    #   icwksdata <- icwksdata[!icwksdata$index %in% toBeDone$index, ]
    #   icwksdata <- rbind(icwksdata, toBeDone[, colnames(icwksdata)]) 
    # }
    
  }
  
  
  SPNGwksdata <- droplevels(SPNGwksdata[!is.na(SPNGwksdata$HHID), ])
  SPNGwksdata <- droplevels(SPNGwksdata[!is.na(SPNGwksdata$HHID), ])
  SPNGwksdata$status <- ifelse(SPNGwksdata$status == "To be done" & Sys.Date() > SPNGwksdata$datetbd2, "Not done", as.character(SPNGwksdata$status))
  SPNGwksdata$status <- ifelse(is.na(SPNGwksdata$status), "To be done", as.character(SPNGwksdata$status))
  # head(icwksdata)
  # 
  # ppwksdata <- droplevels(ppwksdata[!is.na(ppwksdata$month) , ])
  # 
  # ppwksdata$diffsys <- ppwksdata$datetbd2 > Sys.Date() & ppwksdata$actualYR ==  lubridate::year(Sys.Date())
  # ppwksdata$status <- ifelse(ppwksdata$diffsys == 'TRUE', "Due soon", as.character(ppwksdata$status))
  
  head(SPNGwksdata)
  
  ###################################################
  #SPNG points
  ###################################################
  
  # pldateissue_SPNG <-  unique(VAl_SPNG[, c("HHID", "plantingDate")])
  # plSPNG <- as.data.frame(table(pldateissue_SPNG$HHID))
  # VAl_SPNG_dupPlDate <- droplevels(VAl_SPNG[VAl_SPNG$HHID %in% plSPNG[plSPNG$Freq >1, ]$Var1, ])
  # VAl_SPNG <- droplevels(VAl_SPNG[!VAl_SPNG$HHID %in% plSPNG[plSPNG$Freq >1, ]$Var1, ])
  # VAl_SPNG_dupPlDate <- getLatestPlDate(VAl_SPNG_dupPlDate)
  # VAl_SPNG <- rbind(VAl_SPNG, VAl_SPNG_dupPlDate)
  #setwd("C:/Users/Turry/Documents/ACAI/EA Tools/ValActivityTool")
  dataVAl_SPNG <- read.csv("data/dataVAL_SPHS.csv")
  
  
  pointsSPNGdat <- unique(subset(dataVAl_SPNG, select=c(HHID, EAID,  fieldID, seasonSelect, event,SubmissionDate, 
                                                            reject, plantingDate, 
                                                            effHarvestDate_CON_Tri,effHarvestDate_REC_Tri
  )))
  
  #drop season
  pointsSPNGdat2 <- droplevels(pointsSPNGdat[!is.na(pointsSPNGdat$seasonSelect), ])
  pointsSPNGdat3 <- droplevels(pointsSPNGdat2[pointsSPNGdat2$seasonSelect ==3, ])
  
  colnames(pointsSPNGdat3) <- c("HHID", "EAID",  "fieldID", "seasonSelect", "p_event", "subdate",
                                  "reject", "actualplantingdate",  "effHarvestDate_CON_Tri",
                                  "effHarvestDate_REC_Tri" )
  head(pointsSPNGdat3)
  
  
  pointsSPNGdat4 <- droplevels(unique(merge(pointsSPNGdat3, dsEA, by=c("EAID"))))
  pointsSPNG <- unique(pointsSPNGdat4)
  pointsSPNG$subdate <- as.factor(mdy_hms(pointsSPNG$subdate))
  pointsSPNG$subdate <- as.Date(pointsSPNG$subdate, format = "%Y-%m-%d")
  
  
  #convert dates back again to factor
  
  pointsSPNG$subdate <-  as.factor(pointsSPNG$subdate)
  pointsSPNG$actualmn <- lubridate::month(ymd(pointsSPNG$subdate))
  pointsSPNG$month  <- month.abb[pointsSPNG$actualmn]
  pointsSPNG$actuald <- lubridate::day(ymd(pointsSPNG$subdate))
  pointsSPNG$actualYR <- lubridate::year(ymd(pointsSPNG$subdate))
  pointsSPNG$f0sep <- paste( pointsSPNG$actuald, "," ,  sep = "")
  
  pointsSPNG$f0ymd <- ifelse(is.na(pointsSPNG$month), NA, paste(pointsSPNG$month,  pointsSPNG$f0sep, pointsSPNG$actualYR, sep = " "))
  pointsSPNG$f0ymd <- as.factor(pointsSPNG$f0ymd)
  
  pointsSPNG <- unique(pointsSPNG)
  pointsSPNG$EA_Name<- ifelse(pointsSPNG$EA_Name %in% "Wargaret Asuo", "Margaret Asuo", as.character(pointsSPNG$EA_Name))
  #pointsSPNG <- subset(pointsSPNG,select=-c(subdate))
  # pointsSPNG$subdate <-mdy_hms(pointsSPNG$subdate)
  # pointsSPNG$subdate <- as.Date(as.POSIXct(pointsSPNG$subdate, format = "%m/%d/%Y"))
  
  
  
  pointsSPNG$HHID_event <- paste(pointsSPNG$HHID, pointsSPNG$p_event, sep ="-")
  takeLatest2 <-  unique(pointsSPNG[, c("HHID_event", "HHID", "f0ymd", "p_event")])
  
  pointsSPNG2 <- droplevels(takeLatest2[takeLatest2$p_event == "event7", ])
  plSPNG <- as.data.frame(table(pointsSPNG2$HHID_event))
  plSPNG[plSPNG$Freq >1, ]
  
  pointsSPNG_dupPlDate <- droplevels(pointsSPNG[pointsSPNG$HHID_event %in% plSPNG[plSPNG$Freq >1, ]$Var1, ])
  pointsSPNGtake <- droplevels(pointsSPNG[!pointsSPNG$p_event == "event7", ])
  pointsSPNG <- droplevels(pointsSPNG[!pointsSPNG$HHID_event %in% plSPNG[plSPNG$Freq >1 , ]$Var1, ])
  
  pointsSPNG_dupPlDate <- getLatestsubDate(pointsSPNG_dupPlDate)
  pointsSPNG <- rbind(pointsSPNG, pointsSPNG_dupPlDate)
  
  head(pointsSPNG)
  
  #Omit data for events not done
  
  pointsSPNG$suitability <- as.numeric(ifelse ((!is.na(pointsSPNG$subdate)) &  !is.na(pointsSPNG$reject) & (!pointsSPNG$reject == TRUE), "2", "0"))
  pointsSPNG$harvest_blue <- as.numeric(ifelse ((!is.na(pointsSPNG$subdate)) &  (pointsSPNG$p_event == "event6"), "5", "0"))
  pointsSPNG$harvest_red <- as.numeric(ifelse ((!is.na(pointsSPNG$subdate)) &  (pointsSPNG$p_event == "event7"), "5", "0"))
  
  SPNGpoints <- subset(pointsSPNG, select = c(HHID, EAID, EA_Name, fieldID,suitability,harvest_blue, harvest_red))
  head(SPNGpoints)
  
  #COLLAPSE
  
  # dfSPNG = as.data.frame(SPNG_yield_var1 %>%
  #                        dplyr::group_by(HHID) %>%
  #                        fill(everything(), .direction = "down") %>%
  #                        fill(everything(), .direction = "up") %>%
  #                        slice(1))
  
  SPNGpoints <- SPNGpoints %>% group_by(HHID, EAID, EA_Name, fieldID) %>% summarise_at(1:3, sum)
  
  SPNGpoints <- data.table::setDT(SPNGpoints)[HHID!="ACHHNG123456" & HHID!="ACHHNG000000"]
  SPNGpoints = unique(SPNGpoints)
  
  SPNGpoints$Totalpoints <- SPNGpoints$suitability+SPNGpoints$harvest_blue+SPNGpoints$harvest_red
  head(SPNGpoints)
  dsSPNGpoints1 <- subset(pointsSPNG, select=c(EAID, actualYR, month, suitability, harvest_blue, harvest_red))
  dsSPNGpoints2 <- dsSPNGpoints1 %>% tidyr::gather(events, points,suitability,harvest_blue, harvest_red)
  
  head(dsSPNGpoints2)	
  
  dsSPNGpoints2 <- subset(dsSPNGpoints2, select=c(EAID, events, actualYR, month, points))
  colnames (dsSPNGpoints2) <- c("EAID", "Events", "Year", "month", "points")
  
  dsSPNGpoints2 <- droplevels(dsSPNGpoints2[!is.na(dsSPNGpoints2$month), ])
  
  dsSPNGpoints3 <- dsSPNGpoints2 %>% dplyr::mutate(i = row_number()) %>% tidyr::spread(month, points)
  
  dsSPNGpoints3 <- subset(dsSPNGpoints3, select = -c(i))
  
  head(dsSPNGpoints3)
  
  #fix all calendar months not available in data
  
  Notavailmonths <- month.abb[!month.abb %in% colnames(dsSPNGpoints3)]
  x <- suppressWarnings({as.data.frame(matrix(ncol=length(Notavailmonths), nrow=nrow(dsSPNGpoints3), data=as.numeric('NA')))})
  colnames(x)<-  Notavailmonths 
  dsSPNGpoints4 <- cbind(dsSPNGpoints3, x)
  head(dsSPNGpoints4)
  
  #summarize points
  
  library(plyr)
  
  SPNGpointsdt <- plyr::ddply(dsSPNGpoints4, .(EAID, Events, Year), summarize,
                                  Jan = sum(Jan, na.rm = TRUE),
                                  Feb = sum(Feb, na.rm = TRUE),
                                  Mar = sum(Mar, na.rm = TRUE),
                                  Apr = sum(Apr, na.rm = TRUE),
                                  May = sum(May, na.rm = TRUE),
                                  Jun = sum(Jun, na.rm = TRUE),
                                  Jul = sum(Jul, na.rm = TRUE),
                                  Aug = sum(Aug, na.rm = TRUE),
                                  Sep = sum(Sep, na.rm = TRUE),
                                  Oct = sum(Oct, na.rm = TRUE),
                                  Nov = sum(Nov, na.rm = TRUE),
                                  Dec = sum(Dec, na.rm = TRUE))
  
  SPNGpointsdt2 <- plyr::ddply(SPNGpointsdt, .(EAID,Events, Year,  Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
                                   summarize, Total = sum(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec))
  
  SPNGpointsdt3 <- plyr::ddply(SPNGpointsdt, .(EAID, Year),
                                   summarize, Jan = sum(Jan), Feb=sum(Feb), Mar=sum(Mar), Apr=sum(Apr), May=sum(May), Jun=sum(Jun), Jul=sum(Jul), Aug=sum(Aug), 
                                   Sep=sum(Sep), Oct=sum(Oct), Nov=sum(Nov), Dec=sum(Dec))
  SPNGpointsdt3$Events <- "Total"
  SPNGpointsdt3$Total <- ""
  SPNGpointsdt3 <- SPNGpointsdt3[, colnames(SPNGpointsdt2)]
  
  SPNGpointsdt2 <- SPNGpointsdt2[SPNGpointsdt2$Total >0, ]
  SPNGpointsdt2 <- rbind(SPNGpointsdt2, SPNGpointsdt3)
  
  head(SPNGpointsdt2)
  
  ########################################################################################
  
  
  
  ######################################################################################
  # CIS data
  ########################################################################################
  
  useCase = "IC"
  
  dataVAl_CIS <- read.csv("data/dataVAL_CIS.csv")
  data_CIS <- dataVAl_CIS
  VAl_dataCIS <- VAl_CIS
  VAl_dataCIS$season <- NA
  data_CIS$season <- NA

  data_CIS[is.na(data_CIS$season) , ]$season <- 1
  VAl_dataCIS[is.na(VAl_dataCIS$season) , ]$season <- 1
  
  names(data_CIS)[grepl('\\.', names(data_CIS))] <- sub('.*\\.', '', names(data_CIS)[grepl('\\.', names(data_CIS))])
  
  #data_CIS <- filterSingleSubmission(data_CIS, ID="HHID",recent=TRUE)
  cleanVALCIS(useCase = "IC", wd, recent=TRUE)
  
  head(data_CIS)
  CISreg_HH <- data_CIS[!data_CIS$HHID %in% dsEAHH$HHID, ]#HHs not registered
  CISreg_EA <- droplevels(data_CIS[!data_CIS$EAID %in% dsEAHH$EAID, ])#EAs not registered
  #HHNOTLINK <- droplevels(CISreg_HH[!CISreg_HH$HHID %in% CISreg_EA$HHID, ])
  #EAG <- droplevels(CISreg_HH[!CISreg_HH$HHID %in% dsEAHH$HHID, ])
  
  pldateissue_CIS <-  unique(VAl_dataCIS[, c("HHID", "plantingDate")])
  plCIS <- as.data.frame(table(pldateissue_CIS$HHID))
  VAl_CIS_dupPlDate <- droplevels(VAl_dataCIS[VAl_dataCIS$HHID %in% plCIS[plCIS$Freq >1, ]$Var1, ])
  VAl_dataCIS <- droplevels(VAl_dataCIS[!VAl_dataCIS$HHID %in% plCIS[plCIS$Freq >1, ]$Var1, ])
  #VAl_CIS_dupPlDate <- getLatestPlDate(VAl_CIS_dupPlDate)
  VAl_dataCIS <- rbind(VAl_dataCIS, VAl_CIS_dupPlDate)
  
  dsCISactual <- unique(subset(data_CIS, select=c(HHID,  plantingDate, plantingDateCS, plantingDateSP, cassavaGappingDate, sweetPotatoReplantingDate,
                                                  dateFertilizer0, dateFertilizer1, dateWeeding1, dateWeeding2, dateWeeding3, dateCassavaHarvest, 
                                                  dateSweetPotatoHarvest)))
  colnames(dsCISactual) <- c("HHID",  "actualplantingdate", "plantingDateCS", "plantingDateSP", "gapping_date", "sweetPotatoReplantingDate",
                             "fertlizerapp0", "fertlizerapp1", "weeding1", "weeding2", "weeding3", "cassavahvstdate", "sweetpotatohvstdate")
  
  
  
  dsCISactual <- EventLatestDate(usecasedata=dsCISactual, usecase="IC")
  head(dsCISactual)
  colnames(dsCISactual) <- c("HHID",  "actualplantingdate", "plantingDateCS", "plantingDateSP", "gappingdate", "sweetPotatoReplantingDate", 
                             "fertlizerapp0", "fertlizerapp1", 
                             "weedingdate1", "weedingdate2", "weedingdate3", "cassharvestdate", "sweetpotatohvstdate")
  
  
  dsCISplant <- unique(subset(VAl_dataCIS, select=c(HHID, season, plantingDate)))
  colnames(dsCISplant) <- c("HHID", "season", "Plannedplantingdate")
  
  #convert factor to date to enable ordering
  dsCISplant$f0m <- lubridate::month(mdy(dsCISplant$Plannedplantingdate))
  dsCISplant$f0d <- lubridate::day(mdy(dsCISplant$Plannedplantingdate))
  dsCISplant$f0y <- lubridate::year(mdy(dsCISplant$Plannedplantingdate))
  dsCISplant$f0dmy <- ifelse(is.na(dsCISplant$f0m), NA, paste(dsCISplant$f0y, dsCISplant$f0m,  dsCISplant$f0d,    sep = "/"))
  dsCISplant$f0dmy <-  as.Date(dsCISplant$f0dmy)
  
  dsCISplant <- dsCISplant[, c(1,2,7)]
  colnames(dsCISplant) <- c("HHID", "season", "Plannedplantingdate")
  head(dsCISplant)
  
  #pick latest date
  dsCISplant2 <- aggregate(dsCISplant$Plannedplantingdate, list(dsCISplant$HHID, dsCISplant$season), max) 
  
  #dsCISplant2 <- dsCISplant[dsCISplant$Plannedplantingdate %in% latest$x,]
  
  dsCISplanned <- dsCISplant2
  colnames(dsCISplanned) <- c("HHID", "season", "Plannedplantingdate")
  
  #convert dates back again to factor
  dsCISplanned$Plannedplantingdate <-  as.factor(dsCISplanned$Plannedplantingdate)
  dsCISplanned$f0mn <- lubridate::month(ymd(dsCISplanned$Plannedplantingdate))
  
  dsCISplanned$f0m <- month.abb[dsCISplanned$f0mn]
  dsCISplanned$f0d <- lubridate::day(ymd(dsCISplanned$Plannedplantingdate))
  dsCISplanned$f0y <- lubridate::year(ymd(dsCISplanned$Plannedplantingdate))
  dsCISplanned$f0sep <- paste( dsCISplanned$f0d, "," ,  sep = "")
  
  dsCISplanned$f0ymd <- ifelse(is.na(dsCISplanned$f0m), NA, paste(dsCISplanned$f0m,  dsCISplanned$f0sep, dsCISplanned$f0y, sep = " "))
  
  
  dsCISplanned <- dsCISplanned[, c(1,2,9)]
  colnames(dsCISplanned) <- c("HHID", "season", "Plannedplantingdate")
  
  dsCISheatmap <- droplevels(unique(merge(dsCISplanned, dsCISactual, by=c("HHID"))))
  
  dsCISheatmap$Plannedplantingdate <- as.factor(dsCISheatmap$Plannedplantingdate)
  
  
  head(dsCISheatmap)
  #replace missing actual planting dates with planned planting date
  dsCISheatmaphid1 <- NULL
  for( hids in unique(dsCISheatmap$HHID)){
    dsCISheatmaphid <- droplevels(dsCISheatmap[dsCISheatmap$HHID == hids, ])
    if(all(unique(dsCISheatmaphid$actualplantingdate) == '')){
      dsCISheatmaphid$actualplantingdate <- ifelse(dsCISheatmaphid$actualplantingdate == '', as.character(dsCISheatmaphid$Plannedplantingdate), as.character(dsCISheatmaphid$actualplantingdate))
    }else{
      ap <- unique(dsCISheatmaphid[dsCISheatmaphid$actualplantingdate != "", ]$actualplantingdate)
      dsCISheatmaphid$actualplantingdate <- as.character(ap)
    }
    dsCISheatmaphid1 <- rbind(dsCISheatmaphid1, dsCISheatmaphid)
  }
  dsCISheatmap <- dsCISheatmaphid1
  
  #ACHHTZ000006
  #solve dates into days of the year
  head(dsCISheatmap)
  
  season <- dplyr::select(dsCISheatmap, HHID, season)
  
  actual <- solvedates(colNr = 4, usecasedata = dsCISheatmap)
  # colnames(actualplantingdate) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'actualplantingdate', 'actualOrigdate')
  # actual<- subset(actualplantingdate, select = c(HHID,actualplantingdate,actualOrigdate ))
  head(actual)
  
  planned <- solvedates(colNr = 3, usecasedata = dsCISheatmap)
  # colnames(Plannedplantingdate) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'plantingdate', 'plndOrigdate')
  # planned<- subset(Plannedplantingdate, select = c(HHID,plantingdate,plndOrigdate ))
  head(planned)
  
  plantCS <- solvedates(colNr = 5, usecasedata = dsCISheatmap)
  
  if(!is.null(plantCS)){
    plantCS<- subset(plantCS, select = c(HHID,plantingDateCS ))
  }else{
    plantCS <- as.data.frame(matrix(ncol=1, nrow=nrow(dsCISheatmap)), data="NA")
    colnames(plantCS) <- c("plantingCS" )
    plantCS$HHID <- dsCISheatmap$HHID
  }
  
  head(plantCS)
  
  plantSP <- solvedates(colNr = 6, usecasedata = dsCISheatmap)
  
  if(!is.null(plantSP)){
    plantSP<- subset(plantSP, select = c(HHID,plantingDateSP ))
  }else{
    plantSP <- as.data.frame(matrix(ncol=1, nrow=nrow(dsCISheatmap)), data="NA")
    colnames(plantSP) <- c("plantingSP" )
    plantSP$HHID <- dsCISheatmap$HHID
  }
  
  head(plantSP)
  
  gap <- solvedates(colNr = 7, usecasedata = dsCISheatmap)
  
  # if(!is.null(gap)){
  #   gap<- subset(gap, select = c(HHID,gappingdate ))
  # }else{
  #   gap <- as.data.frame(matrix(ncol=1, nrow=nrow(dsCISheatmap)), data="NA")
  #   colnames(gap) <- c("gapdate" )
  #   gap$HHID <- dsCISheatmap$HHID
  # }
  
  head(gap)
  
  replant <- solvedates(colNr = 8, usecasedata = dsCISheatmap)
  # if(!is.null(replant)){
  #   replant<- subset(replant, select = c(HHID,replantdate ))
  # }else{
  #   replant <- as.data.frame(matrix(ncol=1, nrow=nrow(dsCISheatmap)), data="NA")
  #   colnames(replant) <- c("replantdate" )
  #   replant$HHID <- dsCISheatmap$HHID
  # }
  head(replant)
  
  fert0 <- solvedates(colNr = 9, usecasedata = dsCISheatmap)
  # if(!is.null(fert0)){
  #   fert0<- subset(fert0, select = c(HHID,fertlizerapp0 ))
  # }else{
  #   fert0 <- as.data.frame(matrix(ncol=1, nrow=nrow(dsCISheatmap)), data="NA")
  #   colnames(fert0) <- c("fertlizerapp0" )
  #   fert0$HHID <- dsCISheatmap$HHID
  # }
  head(fert0)
  
  
  fert1 <- solvedates(colNr = 10, usecasedata = dsCISheatmap)
  # if(!is.null(fert1)){
  #   fert1<- subset(fert1, select = c(HHID,fertlizerapp1 ))
  # }else{
  #   fert1 <- as.data.frame(matrix(ncol=1, nrow=nrow(dsCISheatmap)), data="NA")
  #   colnames(fert1) <- c("fertlizerapp1" )
  #   fert1$HHID <- dsCISheatmap$HHID
  # }
  head(fert1)
  
  weed1 <- solvedates(colNr = 11, usecasedata = dsCISheatmap)
  # if(!is.null(weed1)){
  #   weed1<- subset(weed1, select = c(HHID,weedingdate1 ))
  # }else{
  #   weed1 <- as.data.frame(matrix(ncol=1, nrow=nrow(dsCISheatmap)), data="NA")
  #   colnames(weed1) <- c("weed1" )
  #   weed1$HHID <- dsCISheatmap$HHID
  # }
  head(weed1)
  
  weed2 <- solvedates(colNr = 12, usecasedata = dsCISheatmap)
  # if(!is.null(weed2)){
  #   weed2<- subset(weed2, select = c(HHID,weedingdate2 ))
  # }else{
  #   weed2 <- as.data.frame(matrix(ncol=1, nrow=nrow(dsCISheatmap)), data="NA")
  #   colnames(weed2) <- c("weed2" )
  #   weed2$HHID <- dsCISheatmap$HHID
  # }
  head(weed2)
  
  weed3 <- solvedates(colNr = 13, usecasedata = dsCISheatmap)
  if(!is.null(weed3)){
    weed3<- subset(weed3, select = c(HHID,weedingdate3 ))
  }else{
    weed3 <- as.data.frame(matrix(ncol=1, nrow=nrow(dsCISheatmap)), data="NA")
    colnames(weed3) <- c("weed3" )
    weed3$HHID <- dsCISheatmap$HHID
  }
  head(weed3)
  
  harvestdateCS <- solvedates(colNr = 14, usecasedata = dsCISheatmap)
  # if(!is.null(harvestdateCS)){
  #   harvestdateCS<- subset(harvestdateCS, select = c(HHID,harvestdateCS ))
  # }else{
  #   harvestdateCS <- as.data.frame(matrix(ncol=1, nrow=nrow(dsCISheatmap)), data="NA")
  #   colnames(harvestdateCS) <- c("CSharvestdate" )
  #   harvestdateCS$HHID <- dsCISheatmap$HHID
  # }
  
  harvestdateSP <- solvedates(colNr = 15, usecasedata = dsCISheatmap)
  # if(!is.null(harvestdateSP)){
  #   harvestdateSP<- subset(harvestdateSP, select = c(HHID,harvestdateSP ))
  # }else{
  #   harvestdateSP <- as.data.frame(matrix(ncol=1, nrow=nrow(dsCISheatmap)), data="NA")
  #   colnames(harvestdateSP) <- c("SPharvestdate" )
  #   harvestdateSP$HHID <- dsCISheatmap$HHID
  # }
  head(harvestdateSP)
  #merge solved days of the year
  CIS0 <- unique(merge(season, actual, by="HHID"), all=TRUE)
  CIS <- unique(merge(CIS0, planned, by="HHID"), all=TRUE)
  CIS1 <- unique(merge(CIS, gap, by="HHID", all=TRUE))
  CIS2 <- unique(merge(CIS1, replant, by='HHID', all=TRUE))
  CIS3 <- unique(merge(CIS2, fert0, by='HHID', all=TRUE))
  CIS4 <- unique(merge(CIS3, fert1, by='HHID', all=TRUE))
  CIS5 <- unique(merge(CIS4, weed1, by='HHID', all=TRUE))
  CIS6 <- unique(merge(CIS5, weed2, by='HHID', all=TRUE))
  CIS7 <- unique(merge(CIS6, weed3, by='HHID', all=TRUE))
  CIS8 <- unique(merge(CIS7, harvestdateCS, by='HHID', all=TRUE))
  CIS9 <- unique(merge(CIS8, plantCS, by='HHID', all=TRUE))
  CIS10 <- unique(merge(CIS9, plantSP, by='HHID', all=TRUE))
  CIS11 <- unique(merge(CIS10, harvestdateSP, by='HHID', all=TRUE))
  
  # if(!is.null(harvestdater)){
  #   dsCISheatmapdates <- unique(merge(fr7, harvestdater, by='HHID', all=TRUE))
  # }else{
  #   dsFRheatmapdates <- fr7
  # }
  
  dsCISheatmapdates <- CIS11
  head(dsCISheatmapdates)
  
  colnames(dsCISheatmapdates) <- c("HHID", "season", "actualplantingdate", "Plannedplantingdate", "gappingdate", "replantdate", "fertlizerapp0", "fertlizerapp1",
                                   "weedingdate1", "weedingdate2","weed3", "CSharvestdate", "plantingCS","plantingSP", "SPharvestdate"  )
  
  #deduct solved days of the year against actual planting dates
  dsCISheatmapdates$planting <- as.numeric(dsCISheatmapdates$actualplantingdate) - as.numeric(dsCISheatmapdates$Plannedplantingdate)   
  dsCISheatmapdates$gappin <- as.numeric(dsCISheatmapdates$gappingdate) - as.numeric(dsCISheatmapdates$actualplantingdate)
  dsCISheatmapdates$replant <- as.numeric(dsCISheatmapdates$replantdate) - as.numeric(dsCISheatmapdates$actualplantingdate)
  dsCISheatmapdates$fertilizr0 <- as.numeric(dsCISheatmapdates$fertlizerapp0) - as.numeric(dsCISheatmapdates$actualplantingdate)
  dsCISheatmapdates$fertilizr1 <- as.numeric(dsCISheatmapdates$fertlizerapp1) - as.numeric(dsCISheatmapdates$actualplantingdate)
  dsCISheatmapdates$weedin1 <- as.numeric(dsCISheatmapdates$weedingdate1) - as.numeric(dsCISheatmapdates$actualplantingdate)
  dsCISheatmapdates$weedin2 <- as.numeric(dsCISheatmapdates$weedingdate2) - as.numeric(dsCISheatmapdates$actualplantingdate)
  dsCISheatmapdates$weedin3 <- as.numeric(dsCISheatmapdates$weed3) - as.numeric(dsCISheatmapdates$actualplantingdate)
  dsCISheatmapdates$harvestCS <- as.numeric(dsCISheatmapdates$CSharvestdate) - as.numeric(dsCISheatmapdates$actualplantingdate)
  dsCISheatmapdates$PlntSP <- as.numeric(dsCISheatmapdates$plantingSP) - as.numeric(dsCISheatmapdates$actualplantingdate)
  dsCISheatmapdates$Plntcs <- as.numeric(dsCISheatmapdates$plantingCS) - as.numeric(dsCISheatmapdates$actualplantingdate)
  dsCISheatmapdates$harvestSP <- as.numeric(dsCISheatmapdates$SPharvestdate) - as.numeric(dsCISheatmapdates$actualplantingdate)
  head(dsCISheatmapdates)
  
  
  colnames (dsCISheatmapdates)<- c("HHID", "season","actualplantingdate", "plannedplantingdate",  "actualgappingdate", "actualreplantdate", "fertilizerdate0","fertilizerdate1", "weedingdate1",
                                   "weedingdate2", "weedingdate3","harvestdateCS", "plantingCSdate", "plantingSPdate", "harvestdateSP", "planting", "gappin", "replant", "fertilizr0", "fertilizr1", "weedin1", "weedin2", "weedin3", "harvestCS", "PlantingCS", "PlantingSP", "harvestSP")
  #get EA names from dsEAHH
  dsCISheatmap2 <- droplevels(unique(merge(dsCISheatmapdates, dsEAHH, by=c("HHID"))))
  head(dsCISheatmap2)
  drops <- c("EAID.y","EA_PhoneNr", "EA_Partner", "HH_Name", "HH_PhoneNr", "useCase", "Latitude", "Longitude", "Country", "region_state")
  dsCIS1 <- (dsCISheatmap2[ , !(names(dsCISheatmap2) %in% drops)])
  
  dsCIS <- dsCIS1
  
  #identify activities not done
  dsCIS$gapping <- ifelse(is.na(dsCIS$gappin) & !is.na(dsCIS$fertilizr0), -999, dsCIS$gappin)
  dsCIS$fertilizer0 <- ifelse(is.na(dsCIS$fertilizr0) & !is.na(dsCIS$fertilizr1), -999, dsCIS$fertilizr0)
  dsCIS$fertilizer1 <-  ifelse(is.na(dsCIS$fertilizr1) & !is.na(dsCIS$weedin1), -999, dsCIS$fertilizr1)
  dsCIS$weeding1 <-  ifelse(is.na(dsCIS$weedin1) & !is.na(dsCIS$weedin2), -999, dsCIS$weedin1)
  dsCIS$weeding2 <-  ifelse(is.na(dsCIS$weedin2) & !is.na(dsCIS$weedin3), -999, dsCIS$weedin2)
  dsCIS$weeding3 <-  ifelse(is.na(dsCIS$weedin3) & !is.na(dsCIS$harvestCS), -999, dsCIS$weedin3)
  head(dsCIS)
  
  drops <- c("gappin", "fertilizr0", "fertilizr1", "weedin1", "weedin2", "weedin3")
  dsCIS2 <- (dsCIS[ , !(names(dsCIS) %in% drops)])
  head(dsCIS2)
  
  dsCIS3 <- subset(dsCIS2, select=c("HHID","season", "EAID", "EA_Name", "PlantingCS", "PlantingSP", "planting", "replant", "gapping", "fertilizer0", "fertilizer1", "weeding1", "weeding2", "weeding3", "harvestCS", "harvestSP"))
  
  #reshape actual dates
  dsCISss1 <- subset(dsCIS2, select=c(HHID, season, plannedplantingdate, actualplantingdate, actualreplantdate, actualgappingdate, fertilizerdate0, fertilizerdate1, weedingdate1, weedingdate2, weedingdate3, harvestdateCS, plantingCSdate, plantingSPdate, harvestdateSP))
  
  #colnames(dsCISss1) <- c("HHID", "plannedplantingdate", "actualplantingdate", "actualgappingdate", "fertilizerdate1", "fertilizerdate2", "weedingdate1", "weedingdate2", "weedingdate3", 'harvestdate')
  
  dsCISss2 <- subset(dsCIS2, select=c(HHID, season, planting, replant, harvestCS, EAID,  EA_Name, gapping, fertilizer0, fertilizer1, weeding1, weeding2, weeding3, PlantingCS, PlantingSP, harvestSP))
  dsCISeshapess1 <- suppressWarnings({dsCISss1 %>% gather(actualEvents, actualDates, actualplantingdate, actualreplantdate, actualgappingdate, fertilizerdate0, fertilizerdate1, weedingdate1, weedingdate2, weedingdate3, harvestdateCS, plantingCSdate, plantingSPdate, harvestdateSP)})
  
  dsCISeshapess2 <- suppressWarnings({dsCISss2 %>% gather(events, dates,planting, replant, harvestCS, gapping, fertilizer0, fertilizer1, weeding1, weeding2, weeding3, PlantingCS, PlantingSP, harvestSP)})
  dsCISeshapess2$dates <- as.numeric(dsCISeshapess2$dates)
  
  #hhdata <- dsCISeshapess2
  head(dsCISeshapess1)
  
  #dsFReshapess1$actualDates <- as.Date(dsFReshapess1$actualDates, origin = lubridate::origin)
  #as_date(x, origin = lubridate::origin)
  #merge actual dates and solved dates
  dsCISeshapess1$events <- gsub("actual", "", dsCISeshapess1$actualEvents)
  
  dsCISeshapess1$events <- gsub("date", "", dsCISeshapess1$events)
  dsCISeshape <- merge(dsCISeshapess1, dsCISeshapess2)
  dim(dsCISeshape)
  
  dsCISeshape <- unique(dsCISeshape)
  head(dsCISeshape)
  
  #dsFReshape$diffdate <- today()-anytime::anydate(as.character(dsCISeshape$actualDates))
  names(dsCISeshape) <- c('HHID', "season",'events', "plannedplantingdate", 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 'DatesBnPlantingEvent')
  chhdata <- dsCISeshape
  chhdata$actualDates <- as.Date((chhdata$actualDates), origin = "1970-01-01")
  #names(hhdata) <- c('HHID', 'EAID', 'EA_Name', 'Events', 'DatesBnPlantingEvent')
  #hhdata <- replace(hhdata, is.na(hhdatass), -100)
  #get status of events
  getstatus<- NULL
  for(h in unique(chhdata$HHID)){
    #maxDate <- max(chhdata$Dates, na.rm=TRUE)
    chhdatass <- subset(chhdata, HHID == h)
    if(!all(is.na(chhdatass$DatesBnPlantingEvent))){
      
      
      chhdatass$status <- ifelse(chhdatass$events %in% c("planting")  & chhdatass$DatesBnPlantingEvent  == 0, "On-time",
                                 ifelse(chhdatass$events %in% c("planting")  & (chhdatass$DatesBnPlantingEvent  < 0 & chhdatass$DatesBnPlantingEvent > -50), "Earlier",
                                        ifelse(chhdatass$events %in% c("planting")  & chhdatass$DatesBnPlantingEvent  > 0, "Done late",
                                               ifelse(chhdatass$events %in% c("planting")  & chhdatass$DatesBnPlantingEvent  == -999, "Not done", 
                                                      
                                                      ifelse(chhdatass$events %in% c("fertilizer0")  & chhdatass$DatesBnPlantingEvent  == 0, "On-time",
                                                             ifelse(chhdatass$events %in% c("fertilizer0")  & (chhdatass$DatesBnPlantingEvent  < 0 & chhdatass$DatesBnPlantingEvent > -50), "Earlier",
                                                                    ifelse(chhdatass$events %in% c("fertilizer0")  & chhdatass$DatesBnPlantingEvent  > 0, "Done late",
                                                                           ifelse(chhdatass$events %in% c("fertilizer0")  & chhdatass$DatesBnPlantingEvent  == -999, "Not done", 
                                                                                  
                                                                                  ifelse(chhdatass$events %in% c("gapping") & (chhdatass$DatesBnPlantingEvent < 28 & chhdatass$DatesBnPlantingEvent > -50), "Earlier", 
                                                                                         ifelse(chhdatass$events %in% c("gapping") & chhdatass$DatesBnPlantingEvent  == 28 , "On-time", 
                                                                                                ifelse(chhdatass$events %in% c("gapping") & chhdatass$DatesBnPlantingEvent > 28 , "Done late",
                                                                                                       ifelse(chhdatass$events %in% c("gapping")  & chhdatass$DatesBnPlantingEvent  == -999, "Not done", 
                                                                                                              ifelse(chhdatass$events %in% c("gapping")  & chhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                     
                                                                                                                     ifelse(chhdatass$events %in% c("fertilizer1") & (chhdatass$DatesBnPlantingEvent < 21  & chhdatass$DatesBnPlantingEvent > -50), "Earlier", 
                                                                                                                            ifelse(chhdatass$events %in% c("fertilizer1") & chhdatass$DatesBnPlantingEvent  == 28 , "On-time",  
                                                                                                                                   ifelse(chhdatass$events %in% c("fertilizer1") & chhdatass$DatesBnPlantingEvent > 28 , "Done late",
                                                                                                                                          
                                                                                                                                          ifelse(chhdatass$events %in% c("fertilizer1")  & chhdatass$DatesBnPlantingEvent  == -999, "Not done", 
                                                                                                                                                 ifelse(chhdatass$events %in% c("fertilizer1")  & chhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                        
                                                                                                                                                        
                                                                                                                                                        ifelse(chhdatass$events %in% c("weeding1") & chhdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                                                                               ifelse(chhdatass$events %in% c("weeding1")  & chhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                      ifelse(chhdatass$events %in% c("weeding1") & chhdatass$DatesBnPlantingEvent == -999 , "Not done",                                                                                                                                                   
                                                                                                                                                                             
                                                                                                                                                                             ifelse(chhdatass$events %in% c("weeding2") & chhdatass$DatesBnPlantingEvent > 0, "On-time",
                                                                                                                                                                                    ifelse(chhdatass$events %in% c("weeding2")  & chhdatass$Dates == -100, "To be done",
                                                                                                                                                                                           ifelse(chhdatass$events %in% c("weeding2") & chhdatass$DatesBnPlantingEvent == -999 , "Not done",                               
                                                                                                                                                                                                  
                                                                                                                                                                                                  ifelse(chhdatass$events %in% c("weeding3") & chhdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                                                                                                                         ifelse(chhdatass$events %in% c("weeding3")  & chhdatass$Dates == -100, "To be done",
                                                                                                                                                                                                                ifelse(chhdatass$events %in% c("weeding3") & chhdatass$DatesBnPlantingEvent == -999 , "Not done",                                                                                                                                                                            
                                                                                                                                                                                                                       
                                                                                                                                                                                                                       ifelse(chhdatass$events %in% c("harvest") & chhdatass$DatesBnPlantingEvent > 0, "On-time", 
                                                                                                                                                                                                                              ifelse(chhdatass$events %in% c("harvest")  & chhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                     as.character(chhdatass$status))))))))))))))))))))))))))))))
      
      chhdatass <- replace(chhdatass, chhdatass == -100, NA)
      suppressWarnings({getstatus <- rbind(getstatus, chhdatass)})
    }
  } 
  
  
  chhdata <- getstatus
  
  
  chhdata$status <- ifelse(is.na(chhdata$DatesBnPlantingEvent), "To be done", as.character(chhdata$status))
  
  chhdata$status <- ifelse(chhdata$DatesBnPlantingEvent == "", "To be done", as.character(chhdata$status))
  chhdata$status <- ifelse(is.na(chhdata$status), "To be done", as.character(chhdata$status))
  
  head(chhdata)
  
  chhdata$actualmn <- lubridate::month(ymd(chhdata$actualDates))
  
  chhdata$month  <- month.abb[chhdata$actualmn]
  chhdata$actuald <- lubridate::day(ymd(chhdata$actualDates))
  chhdata$actualday <- lubridate::wday(ymd(chhdata$actualDates))
  chhdata$actualYR <- lubridate::year(ymd(chhdata$actualDates))
  head(chhdata)
  
  chhdata$datetbd <- ifelse(!is.na(chhdata$actuald), paste(chhdata$actualYR, chhdata$actualmn, chhdata$actuald, sep = "/"), NA)
  
  chhdata$datetbd2 <- as.Date(chhdata$datetbd) 
  
  
  #projected dates
  DsCISdue <- NULL
  for(hids in unique(chhdata$HHID)){
    hd <- chhdata[chhdata$HHID == hids, ]
    hdPl <- hd[hd$events == 'planting', ]
    
    hdG <- hd[hd$events == 'gapping', ]
    if(is.na(hdG$datetbd2)){
      hdG$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 28
    }
    
    hdF0 <- hd[hd$events == 'fertilizer0', ]
    if(is.na(hdF0$datetbd2)){
      hdF0$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 28
    }
    hdF1 <- hd[hd$events == 'fertilizer1', ]
    if(is.na(hdF1$datetbd2)){
      hdF1$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 70
      
    }
    hdW1 <- hd[hd$events == 'weeding1', ]
    if(is.na(hdW1$datetbd2)){
      hdW1$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 84
    }
    
    hdW2 <- hd[hd$events == 'weeding2', ]
    if(is.na(hdW2$datetbd2)){
      hdW2$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 168
    }
    
    hdW3 <- hd[hd$events == 'weeding3', ]
    if(is.na(hdW3$datetbd2)){
      hdW3$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 252
    }
    
    hdRp <- hd[hd$events == 'replant', ]
    
    hdh <- hd[hd$events == 'harvestCS', ]
    
    hdhp <- hd[hd$events == 'harvestSP', ]
    
    DsCISdue <- suppressWarnings({rbind(DsCISdue, hdPl, hdG, hdRp, hdF0, hdF1, hdW1,hdW2,hdW3, hdh, hdhp)})
  }
  
  #DsCISdue$HHID <- unique(DsCISdue$HHID)
  
  names(DsCISdue) <- c('HHID', "season",'Events', 'plannedplantinddates', 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 'DatesBnPlantingEvent', 'status', 
                       'actualmn','month', 'actuald', 'actualday', 'actualYR', 'datetbd', 'datetbd2')
  
  head(DsCISdue)
  
  DsCISdue <- droplevels(DsCISdue[!is.na(DsCISdue$HHID), ] )
  DsCISdue$status <- ifelse(is.na(DsCISdue$DatesBnPlantingEvent), "To be done", as.character(DsCISdue$status))
  DsCISdue$status <- ifelse(DsCISdue$status == "To be done"& DsCISdue$datetbd2 < Sys.Date(), "Overdue", as.character(DsCISdue$status))
  DsCISdue$status <- ifelse(is.na(DsCISdue$DatesBnPlantingEvent), "To be done", as.character(DsCISdue$status))
  DsCISdue$status <- ifelse(DsCISdue$Events == "replant" & !is.na(DsCISdue$DatesBnPlantingEvent) , "On-time", as.character(DsCISdue$status))
  DsCISdue$status <- ifelse(DsCISdue$Events == "harvestCS" & !is.na(DsCISdue$DatesBnPlantingEvent) , "On-time", as.character(DsCISdue$status))
  DsCISdue$status <- ifelse(DsCISdue$Events == "harvestSP" & !is.na(DsCISdue$DatesBnPlantingEvent) , "On-time", as.character(DsCISdue$status))
  # ADDING column for DST run
  DsCISdue$datetbd2 <- as.character(DsCISdue$datetbd2)
  
  hhidCIS_TT <- droplevels(DsCISdue[DsCISdue$HHID %in% VAl_CIS$HHID, ])
  hhidCIS_TF <- droplevels(VAl_CIS[!VAl_CIS$HHID %in% DsCISdue$HHID, ])## dst is run but no data is submitted
  hhidCIS_FT <- droplevels(DsCISdue[!DsCISdue$HHID %in% VAl_CIS$HHID, ])## dst is not run but there is data
  
  CIS_TT <- NULL
  for(hhids in unique(hhidCIS_TT$HHID)){
    hd <- hhidCIS_TT[hhidCIS_TT$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "True"
    hd1$Events <- "DST run"
    hd1$status <- "To be done"
    CIS_TT <- rbind(CIS_TT, hd, hd1)
  }
  
  
  # eaids <- "ACEANG000123"
  CIS_TF <- NULL
  for(eaids in unique(hhidCIS_TF$EAID)){
    edata <- DsCISdue[DsCISdue$EAID == eaids, ]
    hh_nodata <- unique(hhidCIS_TF[hhidCIS_TF$EAID == eaids, ]$HHID)
    if(length(hh_nodata) > 0){
      asas <- edata[1:length(hh_nodata),]
      asas$HHID <- hh_nodata
      asas$Events <- "DST run"
      asas$actualEvents <- "DST run"
      asas$status <- "To be done"
      asas$datetbd2 <- "True"
      CIS_TF <- rbind(CIS_TF, edata, asas)
    }
  }
  
  CIS_dstchecked <- rbind(CIS_TT, CIS_TF)
  
  head(CIS_dstchecked)
  unique(CIS_dstchecked$EAID)
  
  
  #METRICS
  # length(unique(CIS_dstchecked$HHID))
  # length(unique(CIS_dstchecked$EAID))
  # CIS_dstchecked$COUNTRY <- ifelse(grepl("ACHHTZ", CIS_dstchecked$HHID), "TZ", "NG")
  # TZCIS <- CIS_dstchecked[CIS_dstchecked$COUNTRY == "TZ", ]
  # length(unique(TZCIS$HHID))
  # length(unique(TZCIS$EAID))
  ##################################################################################################################################################################################
  #CIS calendar
  
  dsCISwks2 <- DsCISdue
  
  #get days/weeks/months/year CISom actual dates
  dsCISwks2$planting1wk <- lubridate::week(ymd(dsCISwks2$plannedplantinddates))
  dsCISwks2$eventwk <- lubridate::week(ymd(dsCISwks2$datetbd2))
  
  dsCISwks2$planting1yr <- lubridate::year(ymd(dsCISwks2$plannedplantinddates))
  
  CISwkhh <- dsCISwks2
  
  #deduct weeks of the year CISom planting week
  CISwkhh$status2 <- ifelse(CISwkhh$eventwk == -999, -999, CISwkhh$eventwk - CISwkhh$planting1wk)
  head(CISwkhh)
  
  drops <- c( "DatesBnPlantingEvent", "DiffToday") 
  CISwkhh <- (CISwkhh[ , !(names(CISwkhh) %in% drops)])
  head(CISwkhh)
  
  colnames(CISwkhh) <- c('HHID', 'season', 'events', 'plannedplanting', 'actualEvents', "actualDates", "EAID","EA_Name", "status", "actualmn","month","actuald", "actualday", "actualYR",   
                         "datetbd",   "datetbd2", "planting1wk", "weeks", "planting1yr", "status2")
  head(CISwkhh)
  
  
  wksCISdtplot <- CISwkhh
  wksCISdtplot$dueyear <- lubridate::year(wksCISdtplot$datetbd2)
  
  
  wksCISdtplot$index <- 1:nrow(wksCISdtplot)
  
  toBeDue <- droplevels(wksCISdtplot[wksCISdtplot$datetbd2 > Sys.Date() & wksCISdtplot$dueyear ==  lubridate::year(Sys.Date()), ] )
  
  
  if (nrow(toBeDue) > 0) {
    toBeDue$todayweek <-  as.numeric(lubridate::week(Sys.Date()))
    toBeDue$t2 <- as.numeric(as.character(toBeDue$weeks)) - toBeDue$todayweek
    toBeDue <- toBeDue[toBeDue$t2 <= 2, ]
    #toBeDone <- toBeDue[toBeDue$t2 > 2, ]
    if (nrow(toBeDue) > 0) {
      toBeDue$status <- 'Due soon'
      toBeDue$month <- as.factor(month.abb[lubridate::month(ymd(toBeDue$datetbd2))])
      wksCISdtplot <- wksCISdtplot[!wksCISdtplot$index %in% toBeDue$index, ]
      wksCISdtplot <- rbind(wksCISdtplot, toBeDue[, colnames(wksCISdtplot)])
    }
    # if (nrow(toBeDone) > 0) {
    #   toBeDone$status <- 'To be done'
    #   toBeDone$month <- as.factor(month.abb[lubridate::month(ymd(toBeDone$datetbd2))])
    #   wksdtplot <- wksdtplot[!wksdtplot$index %in% toBeDone$index, ]
    #   wksdtplot <- rbind(wksdtplot, toBeDone[, colnames(wksdtplot)])
    # }
    
  }
  
  
  wksCISdtplot <- droplevels(wksCISdtplot[!is.na(wksCISdtplot$HHID), ])
  
  wksCISdtplot$status <- ifelse(wksCISdtplot$status == "To be done" & Sys.Date() > wksCISdtplot$datetbd2, "Not done", as.character(wksCISdtplot$status))
  wksCISdtplot$status <- ifelse(is.na(wksCISdtplot$status), "To be done", as.character(wksCISdtplot$status))
  head(wksCISdtplot)
  
  #######################################################################################################################################################################
  #CIS points data
  ######################################################################################
  
  # 
  # pldateissue_CIS <-  unique(VAl_CIS[, c("HHID", "plantingDate")])
  # plCIS <- as.data.frame(table(pldateissue_CIS$HHID))
  # VAl_CIS_dupPlDate <- droplevels(VAl_CIS[VAl_CIS$HHID %in% plCIS[plCIS$Freq >1, ]$Var1, ])
  # VAl_CIS <- droplevels(VAl_CIS[!VAl_CIS$HHID %in% plCIS[plCIS$Freq >1, ]$Var1, ])
  # VAl_CIS_dupPlDate <- getLatestPlDate(VAl_CIS_dupPlDate)
  # VAl_CIS <- rbind(VAl_CIS, VAl_CIS_dupPlDate)
  
  dataVAl_CIS <- read.csv("data/dataVAL_CIS.csv")
  
  pointscisdat <- unique(subset(dataVAl_CIS, select=c(HHID, EAID, SubmissionDate, event, plantingDate, plantingDateCS, plantingDateSP, cassavaGappingDate, sweetPotatoReplantingDate, 
                                                      dateFertilizer0, dateFertilizer1, dateWeeding1, dateWeeding2, dateWeeding3, dateCassavaHarvest, dateSweetPotatoHarvest)))
  colnames(pointscisdat) <- c("HHID", "EAID", "subdate", "p_event", "actualplantingdate", "plantingDateCS", "plantingDateSP", "gapping_date", "sweetPotatoReplantingDate", "fertlizerapp0", "fertlizerapp1", "weeding1", "weeding2", "weeding3", "cassavahvstdate", "sweetpotatohvstdate")
  
  pointscisdat <- droplevels(unique(merge(pointscisdat, dsEA, by=c("EAID"))))
  head(pointscisdat)
  
  ########
  #CIS
  #######
  
  library(lubridate)	
  # ADDING column for DST run
  VAl_CISpts <- subset(VAl_CIS, select = c(HHID, SubmissionDate))
  VAl_CISpts$SubmissionDate <- mdy_hms(VAl_CISpts$SubmissionDate)
  VAl_CISpts$SubmissionDate <- as.Date(as.POSIXct(VAl_CISpts$SubmissionDate, format = "%m/%d/%Y"))
  head(VAl_CISpts)
  pts_valCIS <- merge(pointscisdat, VAl_CISpts, by=c("HHID") )
  
  
  CIS_runs <- NULL
  for(hhids in unique(pts_valCIS$HHID)){
    hd <- pts_valCIS[pts_valCIS$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$p_event <- "DST run"
    CIS_runs <- rbind(CIS_runs, hd, hd1)
  }
  
  
  head(CIS_runs)
  
  pointsCIS <-CIS_runs
  
  library(lubridate)
  pointsCIS$EA_Name<- ifelse(pointsCIS$EA_Name %in% "Wargaret Asuo", "Margaret Asuo", as.character(pointsCIS$EA_Name))
  pointsCIS$subdate <-mdy_hms(pointsCIS$subdate)
  pointsCIS$subdate <- as.Date(as.POSIXct(pointsCIS$subdate, format = "%m/%d/%Y"))
  
  pointsCIS$actualmn <- lubridate::month(ymd(pointsCIS$subdate))
  
  pointsCIS$month  <- month.abb[pointsCIS$actualmn]
  pointsCIS$actuald <- lubridate::day(ymd(pointsCIS$subdate))
  pointsCIS$actualday <- lubridate::wday(ymd(pointsCIS$subdate))
  pointsCIS$actualYR <- lubridate::year(ymd(pointsCIS$subdate))
  
  
  pointsCIS$submn <- lubridate::month(ymd(pointsCIS$SubmissionDate))
  pointsCIS$submonth  <- month.abb[pointsCIS$submn]
  pointsCIS$subd <- lubridate::day(ymd(pointsCIS$SubmissionDate))
  pointsCIS$subday <- lubridate::wday(ymd(pointsCIS$SubmissionDate))
  pointsCIS$subYR <- lubridate::year(ymd(pointsCIS$SubmissionDate))
  head(pointsCIS)
  
  head(pointsCIS)
  
  pointsCIS <- unique(pointsCIS)
  
  
  pointsCIS$event1 <-as.numeric(ifelse ((!is.na(pointsCIS$subdate)) &  (pointsCIS$p_event == "event1"), "10", "0"))
  
  pointsCIS$event2 <- as.numeric(ifelse ((!is.na(pointsCIS$subdate)) &  (pointsCIS$p_event == "event2"), "5", "0"))
  pointsCIS$event3 <- as.numeric(ifelse ((!is.na(pointsCIS$subdate)) &  (pointsCIS$p_event == "event3"), "5", "0"))
  pointsCIS$event4 <- as.numeric(ifelse ((!is.na(pointsCIS$subdate)) &  (pointsCIS$p_event == "event4"), "10", "0"))
  pointsCIS$event5 <- as.numeric(ifelse ((!is.na(pointsCIS$subdate)) &  (pointsCIS$p_event == "event5"), "25", "0"))
  pointsCIS$event6 <- as.numeric(ifelse ((!is.na(pointsCIS$subdate)) &  (pointsCIS$p_event == "event6"), "10", "0"))
  pointsCIS$event7 <- as.numeric(ifelse ((!is.na(pointsCIS$subdate)) &  (pointsCIS$p_event == "event7"), "10", "0"))
  pointsCIS$event8 <- as.numeric(ifelse ((!is.na(pointsCIS$subdate)) &  (pointsCIS$p_event == "event8"), "25", "0"))
  pointsCIS$DST_run <- as.numeric(ifelse ((!is.na(pointsCIS$SubmissionDate)) &  (pointsCIS$p_event == "DST run"), "2", "0"))
  #pointsCIS$DSTrun <- as.numeric
  
  pointsdtCIS <- subset(pointsCIS, select = c(HHID, EAID, EA_Name, event1, event2, event3, event4, event5, event6, event7, event8, DST_run ))
  
  
  library(dplyr)
  library(tidyr)
  
  CISpoints <- pointsdtCIS %>% group_by(HHID, EAID, EA_Name) %>% summarise_at(1:9, sum)
  CISpoints <- data.table::setDT(CISpoints)[HHID!="ACHHNG123456" & HHID!="ACHHNG000000"]
  
  CISpoints = unique(CISpoints)
  
  #Sum points
  CISpoints$Totalpoints <- CISpoints$event1 + CISpoints$event2 + CISpoints$event3 + CISpoints$event4 + CISpoints$event5 + CISpoints$event6 + CISpoints$event7 + CISpoints$event8 + CISpoints$DST_run
  head(CISpoints) 
  dsCISpnts1 <- subset(pointsCIS, select=c(EAID, actualYR, month, event1, event2, event3, event4, event5, event6, event7, event8, DST_run))
  dsCISpnts2 <- dsCISpnts1 %>% gather(events, points,event1, event2, event3, event4, event5, event6, event7, event8, DST_run)
  
  head(dsCISpnts2)
  dsCISpnts2 <- subset(dsCISpnts2, select=c(EAID, events, actualYR, month, points))
  colnames (dsCISpnts2) <- c("EAID", "Events", "Year", "month", "points")
  
  dsCISpnts2 <- droplevels(dsCISpnts2[!is.na(dsCISpnts2$month), ])
  
  dsCISpnts3 <- dsCISpnts2 %>% dplyr::mutate(i = row_number()) %>% spread(month, points)
  dsCISpnts3 <- subset(dsCISpnts3, select = -c(i))
  
  head(dsCISpnts3)
  #fix all calendar months not available in data
  Notavailmonths <- month.abb[!month.abb %in% colnames(dsCISpnts3)]
  x <-  suppressWarnings({as.data.frame(matrix(ncol=length(Notavailmonths), nrow=nrow(dsCISpnts3), data=as.numeric('NA')))})
  colnames(x)<-  Notavailmonths 
  dsCISpnts4 <- cbind(dsCISpnts3, x)
  
  #summarize points
  CISpointsdt <- ddply(dsCISpnts4, .(EAID, Events, Year), summarize,
                       Jan = sum(Jan, na.rm = TRUE),
                       Feb = sum(Feb, na.rm = TRUE),
                       Mar = sum(Mar, na.rm = TRUE),
                       Apr = sum(Apr, na.rm = TRUE),
                       May = sum(May, na.rm = TRUE),
                       Jun = sum(Jun, na.rm = TRUE),
                       Jul = sum(Jul, na.rm = TRUE),
                       Aug = sum(Aug, na.rm = TRUE),
                       Sep = sum(Sep, na.rm = TRUE),
                       Oct = sum(Oct, na.rm = TRUE),
                       Nov = sum(Nov, na.rm = TRUE),
                       Dec = sum(Dec, na.rm = TRUE))
  
  
  CISpointsdt2 <- ddply(CISpointsdt, .(EAID,Events, Year,  Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
                        summarize, Total = sum(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec))
  
  CISpointsdt3 <- ddply(CISpointsdt, .(EAID, Year),
                        summarize, Jan = sum(Jan), Feb=sum(Feb), Mar=sum(Mar), Apr=sum(Apr), May=sum(May), Jun=sum(Jun), Jul=sum(Jul), Aug=sum(Aug), 
                        Sep=sum(Sep), Oct=sum(Oct), Nov=sum(Nov), Dec=sum(Dec))
  CISpointsdt3$Events <- "Total"
  CISpointsdt3$Total <- ""
  CISpointsdt3 <- CISpointsdt3[, colnames(CISpointsdt2)]
  
  CISpointsdt2 <- CISpointsdt2[CISpointsdt2$Total >0, ]
  CISpointsdt2 <- rbind(CISpointsdt2, CISpointsdt3)
  head(CISpointsdt2)
  
  ########################################################################################################################################################
  ##########################################################################################################################################
  #WEED (PP) DATA
  ##########################################################################################################################################
  
  useCase = "PP"
  
  #cleanVAL(useCase = "PP", wd, recent=TRUE)	
  
  dataVAL_PP_TZ <- read.csv("data/dataVAL_PP_TZ.csv")
  
  VAl_PP_TZ <- read.csv("data/VAL_PP_TZ.csv")
 
  #DELETE HHID ACHHTZ002811 AND ACHHTZ000054 ENTERED IN OCT AND NOV 2019. THIS IS TEST DATA
  #dataVAL_PP_TZ <- droplevels(dataVAL_PP_TZ[!dataVAL_PP_TZ$EAID == "ACEATZ000751", ])
  #dataVAL_PP_TZ <- droplevels(dataVAL_PP_TZ[!dataVAL_PP_TZ$EAID == "ACEATZ000538", ])
                            
  
  unique(dataVAL_PP_TZ$EAID)
  dat_PP_TZ <- dataVAL_PP_TZ
  dst_PP_TZ <- VAl_PP_TZ
  names(dat_PP_TZ)[grepl('\\.', names(dat_PP_TZ))] <- sub('.*\\.', '', names(dat_PP_TZ)[grepl('\\.', names(dat_PP_TZ))])
  #dat_PP_TZ <- filterSingleSubmission(dat_PP_TZ, ID="HHID",recent=TRUE)
  
  cleanVALPPTV(useCase = "PP", wd, recent=TRUE)	
  
  head(dat_PP_TZ)
  PPreg_HH <- droplevels(dat_PP_TZ[!dat_PP_TZ$HHID %in% dsHH$HHID, ])#HHs not registered
  PPreg_EA <- droplevels(dat_PP_TZ[!dat_PP_TZ$EAID %in% dsEA$EAID, ])#EAs not registered
  
  library(lubridate)
  
  # pldateissue_PP <-  unique(dst_PP_TZ[, c("HHID", "plantingDate")])
  # plpp <- as.data.frame(table(pldateissue_PP$HHID))
  # VAl_PP_dupPlDate <- droplevels(dst_PP_TZ[dst_PP_TZ$HHID %in% plpp[plpp$Freq >1, ]$Var1, ])
  # dst_PP_TZ_TZ <- droplevels(dst_PP_TZ[!dst_PP_TZ$HHID %in% plpp[plpp$Freq >1, ]$Var1, ])
  # VAl_PP_dupPlDate <- getLatestPlDate(VAl_PP_dupPlDate)
  # dst_PP_TZ <- rbind(dst_PP_TZ, VAl_PP_dupPlDate)
  # # 
  
  #PP data add season variable
  
  #VAl_PP <- VAl_PP[,-which(names(VAl_PP)=="harrow_plot3")[1]] #dropping accidentally column with duplicated variable name
  #dst_PP_TZ[is.na(dst_PP_TZ$season),]$season <- 1 #replace NA values by 1 (season variable did not exist in season 1)
  # VAl_PP <- filterSingleSubmission(VAl_PP, ID=c("HHID", "season"), recent=recent) #retains only the most recent DST submission per season
  # VAl_PP <- merge(VAl_PP, subset(dsEA, select=c(EAID, firstNameEA, surNameEA, phoneNrEA)), all.x=TRUE) #add EA details
  # VAl_PP <- merge(VAl_PP, subset(dsHH, select=c(HHID, firstNameHH, surNameHH, phoneNrHH)), all.x=TRUE) #add HH details
  
  
  dsPPTZactual <- unique(subset(dat_PP_TZ, select=c(HHID,  plantingDate, gappingDate,
                                                    dateWeeding1, dateWeeding2, dateWeeding3, harvestDate)))
  colnames(dsPPTZactual) <- c("HHID", "actualplantingdate", "gapping_date", "weeding1", "weeding2", "weeding3", "cassavahvstdate")
  dsPPTZactual <- EventLatestDate(usecasedata=dsPPTZactual, usecase="PP")
  colnames(dsPPTZactual) <- c("HHID",  "actualplantingdate", "gappingdate", "weedingdate1", "weedingdate2", "weedingdate3", "harvestdate")
  
  dsPPTZplant <- unique(subset(dst_PP_TZ, select=c(HHID, season, plantingDate)))
  colnames(dsPPTZplant) <- c("HHID", "season", "Plannedplantingdate")
  
  #convert factor to date to enable ordering
  dsPPTZplant$f0m <- lubridate::month(mdy(dsPPTZplant$Plannedplantingdate))
  dsPPTZplant$f0d <- lubridate::day(mdy(dsPPTZplant$Plannedplantingdate))
  dsPPTZplant$f0y <- lubridate::year(mdy(dsPPTZplant$Plannedplantingdate))
  dsPPTZplant$f0dmy <- ifelse(is.na(dsPPTZplant$f0m), NA, paste(dsPPTZplant$f0y, dsPPTZplant$f0m,  dsPPTZplant$f0d,    sep = "/"))
  dsPPTZplant$f0dmy <-  as.Date(dsPPTZplant$f0dmy)
  
  dsPPTZplant <- dsPPTZplant[, c(1,2,7)]
  colnames(dsPPTZplant) <- c("HHID", "season", "Plannedplantingdate")
  head(dsPPTZplant)
  
  #pick latest date
  dsPPTZplant2 <- aggregate(dsPPTZplant$Plannedplantingdate, list(dsPPTZplant$HHID, dsPPTZplant$season), max) 
  
  #dsPPTZplant2 <- dsPPTZplant[dsPPTZplant$Plannedplantingdate %in% latest$x,]
  
  dsPPTZplanned <- dsPPTZplant2
  colnames(dsPPTZplanned) <- c("HHID", "season","Plannedplantingdate")
  
  #convert dates back again to factor
  dsPPTZplanned$Plannedplantingdate <-  as.factor(dsPPTZplanned$Plannedplantingdate)
  dsPPTZplanned$f0mn <- lubridate::month(ymd(dsPPTZplanned$Plannedplantingdate))
  
  dsPPTZplanned$f0m <- month.abb[dsPPTZplanned$f0mn]
  dsPPTZplanned$f0d <- lubridate::day(ymd(dsPPTZplanned$Plannedplantingdate))
  dsPPTZplanned$f0y <- lubridate::year(ymd(dsPPTZplanned$Plannedplantingdate))
  dsPPTZplanned$f0sep <- paste( dsPPTZplanned$f0d, "," ,  sep = "")
  
  dsPPTZplanned$f0ymd <- ifelse(is.na(dsPPTZplanned$f0m), NA, paste(dsPPTZplanned$f0m,   dsPPTZplanned$f0sep, dsPPTZplanned$f0y, sep = " "))
  
  dsPPTZplanned <- dsPPTZplanned[, c(1,2,9)]
  colnames(dsPPTZplanned) <- c("HHID","season", "Plannedplantingdate")
  
  dsPPTZheatmap <- droplevels(unique(merge(dsPPTZplanned, dsPPTZactual, by=c("HHID"))))
  
  dsPPTZheatmap$Plannedplantingdate <- as.factor(dsPPTZheatmap$Plannedplantingdate)
  
  dsPPTZheatmap <- droplevels(unique(merge(dsPPTZplanned, dsPPTZactual, by=c("HHID"))))
  head(dsPPTZheatmap)
  
  #replace missing actual planting dates with planned planting date
  dsPPTZheatmaphid1 <- NULL
  for( hids in unique(dsPPTZheatmap$HHID)){
    dsPPTZheatmaphid <- droplevels(dsPPTZheatmap[dsPPTZheatmap$HHID == hids, ])
    if(all(unique(dsPPTZheatmaphid$actualplantingdate) == '')){
      dsPPTZheatmaphid$actualplantingdate <- ifelse(dsPPTZheatmaphid$actualplantingdate == '', as.character(dsPPTZheatmaphid$Plannedplantingdate), as.character(dsPPTZheatmaphid$actualplantingdate))
    }else{
      aap <- unique(dsPPTZheatmaphid[dsPPTZheatmaphid$actualplantingdate != "", ]$actualplantingdate)
      dsPPTZheatmaphid$actualplantingdate <- as.character(aap)
    }
    dsPPTZheatmaphid1 <- rbind(dsPPTZheatmaphid1, dsPPTZheatmaphid)
  }
  dsPPTZheatmap <- dsPPTZheatmaphid1
  head(dsPPTZheatmap)
  #solve dates into days of the year
  plannedPPTZ <- solvedates(colNr = 3, usecasedata=dsPPTZheatmap)
  # colnames(Plannedplantingdate) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'Plannedplantingdate', 'plndOrigdate')
  # plannedPP<- subset(Plannedplantingdate, select = c(HHID,Plannedplantingdate,plndOrigdate ))
  head(plannedPPTZ)
  
  actualPPTZ <- solvedates(colNr = 4, usecasedata=dsPPTZheatmap)
  # colnames(actualplantingdatePP) <- c('HHID', 'month', 'date', 'year', 'leapyear',  'actualplantingdate', 'actualOrigdate')
  # actualPP<- subset(actualplantingdatePP, select = c(HHID,actualplantingdate,actualOrigdate ))
  head(actualPPTZ)
  
  gapPPTZ <- solvedates(colNr = 5, usecasedata=dsPPTZheatmap)
 
  head(gapPPTZ)
  
  weedPPTZ1 <- solvedates(colNr = 6, usecasedata=dsPPTZheatmap)
  # if(!is.null(weedPPTZ1) ){
  #   #colnames(harvestpp) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'harvestdatePP', 'Origharvestdate')
  #   weedPPTZ1 <- subset(weedPPTZ1, select = c(HHID, weedPPTZ1))
  #   head(weedPPTZ1)
  # }else{
  #   weedPPTZ1 <- as.data.frame(matrix(ncol=2, nrow=nrow(dsPPTZheatmap)), data="NA")
  #   colnames(weedPPTZ1) <- c("HHID", "weedingdate1")
  #   weedPPTZ1$HHID <- dsPPTZheatmap$HHID
  # }
  head(weedPPTZ1)
  
  weedPPTZ2 <- solvedates(colNr = 7, usecasedata=dsPPTZheatmap)
  # if(!is.null(weedPPTZ2) ){
  #   #colnames(harvestpp) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'harvestdatePP', 'Origharvestdate')
  #   weedPPTZ2 <- subset(weedPPTZ2, select = c(HHID, weedPPTZ2))
  #   head(weedPPTZ2)
  # }else{
  #   weedPPTZ2 <- as.data.frame(matrix(ncol=2, nrow=nrow(dsPPTZheatmap)), data="NA")
  #   colnames(weedPPTZ2) <- c("HHID", "weedingdate2")
  #   weedPPTZ2$HHID <- dsPPTZheatmap$HHID
  # }
  head(weedPPTZ2)
  
  weedPPTZ3 <- solvedates(colNr = 8, usecasedata=dsPPTZheatmap)
  # if(!is.null(weedPPTZ3) ){
  #   #colnames(harvestpp) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'harvestdatePP', 'Origharvestdate')
  #   weedPPTZ3 <- subset(weedPPTZ3, select = c(HHID, weedPPTZ3))
  #   head(weedPPTZ3)
  # }else{
  #   weedPPTZ3 <- as.data.frame(matrix(ncol=2, nrow=nrow(dsPPTZheatmap)), data="NA")
  #   colnames(weedPPTZ3) <- c("HHID", "weedingdate3")
  #   weedPPTZ3$HHID <- dsPPTZheatmap$HHID
  # }
  head(weedPPTZ3)
  
  harvestPPTZ <- solvedates(colNr = 9, usecasedata=dsPPTZheatmap)
  # if(!is.null(harvestPPTZ) ){
  #   #colnames(harvestpp) <- c('HHID', 'month', 'date', 'year', 'leapyear', 'harvestdatePP', 'Origharvestdate')
  #   harvestPPTZ <- subset(harvestPPTZ, select = c(HHID, HarvestPPTZ))
  #   head(harvestPPTZ)
  # }else{
  #   harvestPPTZ <- as.data.frame(matrix(ncol=2, nrow=nrow(dsPPTZheatmap)), data="NA")
  #   colnames(harvestPPTZ) <- c("HHID", "harvestdate")
  #   harvestPPTZ$HHID <- dsPPTZheatmap$HHID
  # }
  head(harvestPPTZ)
  
  #merge solved days of the year
  season <- select(dsPPTZheatmap, HHID, season)
  pptz0 <- unique(merge(season, actualPPTZ, by="HHID", all=TRUE))
  pptz1 <- unique(merge(pptz0, plannedPPTZ, by="HHID", all=TRUE))
  pptz2 <- unique(merge(pptz1, gapPPTZ, by="HHID", all=TRUE))
  pptz3 <- unique(merge(pptz2, weedPPTZ1, by="HHID", all=TRUE))
  pptz4 <- unique(merge(pptz3, weedPPTZ2, by="HHID", all=TRUE))
  pptz5 <- unique(merge(pptz4, weedPPTZ3, by="HHID", all=TRUE))
  pptz6 <- unique(merge(pptz5, harvestPPTZ, by= "HHID", all=TRUE))
  dsPPTZheatmapdates <- pptz6
  
  
  dsPPTZheatmapdates$planting <- as.numeric(dsPPTZheatmapdates$actualplantingdate) - as.numeric(dsPPTZheatmapdates$Plannedplantingdate) 
  dsPPTZheatmapdates$gappingPP <- as.numeric(dsPPTZheatmapdates$gappingdate) - as.numeric(dsPPTZheatmapdates$actualplantingdate)
  dsPPTZheatmapdates$weeding1PP <- as.numeric(dsPPTZheatmapdates$weedingdate1) - as.numeric(dsPPTZheatmapdates$actualplantingdate)
  dsPPTZheatmapdates$weeding2PP <- as.numeric(dsPPTZheatmapdates$weedingdate2) - as.numeric(dsPPTZheatmapdates$actualplantingdate)
  dsPPTZheatmapdates$weeding3PP <- as.numeric(dsPPTZheatmapdates$weedingdate3) - as.numeric(dsPPTZheatmapdates$actualplantingdate)
  dsPPTZheatmapdates$harvestdate <- as.numeric(dsPPTZheatmapdates$harvestdate) - as.numeric(dsPPTZheatmapdates$actualplantingdate)
  
  head(dsPPTZheatmapdates)
  
  heatmapPPTZ <-  dsPPTZheatmapdates
  dsPPTZheatmap2 <- droplevels(unique(merge(heatmapPPTZ, dsEAHH, by=c("HHID"))))
  head(dsPPTZheatmap2)
  drops <- c("EAID.y","EA_PhoneNr", "EA_Partner", "HH_Name", "HH_PhoneNr", "useCase", "Latitude", "Longitude", "Country", "region_state")
  dsPPTZ <- (dsPPTZheatmap2[ , !(names(dsPPTZheatmap2) %in% drops)])
  
  #identify activities not done
  dsPPTZ$gapping <-  ifelse(is.na(dsPPTZ$gappingPP) & (!is.na(dsPPTZ$weeding1PP) | !is.na(dsPPTZ$weeding2PP)), -999, dsPPTZ$gappingPP)
  dsPPTZ$weeding1 <-  ifelse(is.na(dsPPTZ$weeding1PP) & (!is.na(dsPPTZ$weeding2PP) | !is.na(dsPPTZ$weeding3PP)), -999, dsPPTZ$weeding1PP)
  dsPPTZ$weeding2 <-  ifelse(is.na(dsPPTZ$weeding2PP) & (!is.na(dsPPTZ$weeding3PP) | !is.na(dsPPTZ$harvest)), -999, dsPPTZ$weeding2PP)
  dsPPTZ$weeding3 <-  ifelse(is.na(dsPPTZ$weeding3PP) & !is.na(dsPPTZ$harvestdate), -999, dsPPTZ$weeding3PP)
  
  drops <- c("plantingPP", "gappingPP", "weeding1PP", "weeding2PP", "weeding3PP")
  dsPPTZ2 <- (dsPPTZ[ , !(names(dsPPTZ) %in% drops)])
  head(dsPPTZ2)
  #reshape actual dates
  dsPPTZss1 <- subset(dsPPTZ2, select=c(HHID, season, actualplantingdate, Plannedplantingdate, gappingdate, weedingdate1, weedingdate2, weedingdate3, harvestdate))
  colnames(dsPPTZss1) <- c('HHID', 'season', 'actualplantingdate', ' plannedplanting','actualgapping', 'actualweeding1', 'actualweeding2','actualweeding3', 'actualharvest')
  dsPPTZss2 <- subset(dsPPTZ2, select=c(HHID, season, planting, EAID,  EA_Name, gapping, weeding1, weeding2, weeding3, harvestdate ))
  colnames(dsPPTZss2) <- c('HHID', 'season', 'planting', ' EAID','EA_Name', "gapping", 'weeding1', 'weeding2','weeding3', 'harvest')
  
  dsPPTZRss1 <- suppressWarnings({dsPPTZss1 %>% gather(actualEvents, actualDates, actualplantingdate, actualgapping, actualweeding1, actualweeding2, actualweeding3, actualharvest)})
  dsPPTZRss2 <- suppressWarnings({dsPPTZss2 %>% gather(events, dates, planting, gapping, weeding1, weeding2, weeding3, harvest)})
  dsPPTZRss2$dates <- as.numeric(dsPPTZRss2$dates)
  #merge actual dates and solved dates
  dsPPTZRss1$events <- gsub("actual", "", dsPPTZRss1$actualEvents)
  
  dsPPTZRss1$events <- gsub("date", "", dsPPTZRss1$events)
  dsPPTZreshape <- merge(dsPPTZRss1, dsPPTZRss2)
  dim(dsPPTZreshape)
  
  dsPPTZreshape <- unique(dsPPTZreshape)
  head(dsPPTZreshape)
  
  #dsPPTZreshape$diffdate <- today()-anytime::anydate(as.character(dsPPTZreshape$actualDates))
  names(dsPPTZreshape) <- c('HHID', 'season', 'events', 'plannedplanting', 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 'DatesBnPlantingEvent')
  PPTZhhdata <- dsPPTZreshape
  PPTZhhdata$actualDates <- as.Date((PPTZhhdata$actualDates), origin = "1970-01-01")
  #get status for events 
  
  getstatus<- NULL
  for(h in unique(PPTZhhdata$HHID)){
    PPTZhhdatass <- subset(PPTZhhdata, HHID == h)
    if(!all(is.na(PPTZhhdatass$DatesBnPlantingEvent))){
      #hhdatass <- subset(hhdata, HHID == "ACHHNG000151")
      #PPhhdatass <- replace(PPhhdatass, is.na(PPhhdatass), -100)
      
      PPTZhhdatass$status <- ifelse(PPTZhhdatass$events %in% c("planting")  & PPTZhhdatass$DatesBnPlantingEvent  == 0, "On-time",
                                    ifelse(PPTZhhdatass$events %in% c("planting")  & PPTZhhdatass$DatesBnPlantingEvent  < 0 & !PPTZhhdatass$DatesBnPlantingEvent == -999, "Earlier",
                                           ifelse(PPTZhhdatass$events %in% c("planting")  & PPTZhhdatass$DatesBnPlantingEvent  > 0, "Done later",
                                                  ifelse(PPTZhhdatass$events %in% c("planting")  & PPTZhhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                         
                                                         ifelse(PPTZhhdatass$events %in% c("gapping")   & PPTZhhdatass$DatesBnPlantingEvent < 28  & !PPTZhhdatass$DatesBnPlantingEvent == -999, "Earlier",
                                                                ifelse(PPTZhhdatass$events %in% c("gapping")   & PPTZhhdatass$DatesBnPlantingEvent  == 28 , "On-time",
                                                                       ifelse(PPTZhhdatass$events %in% c("gapping")   & PPTZhhdatass$DatesBnPlantingEvent > 28 , "Done later",
                                                                              ifelse(PPTZhhdatass$events %in% c("gapping")   & PPTZhhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                                                     ifelse(PPTZhhdatass$events %in% c("gapping")   & PPTZhhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                            
                                                                                            ifelse(PPTZhhdatass$events %in% c("weeding1")  & PPTZhhdatass$DatesBnPlantingEvent < 56  & !PPTZhhdatass$DatesBnPlantingEvent == -999, "Earlier",
                                                                                                   ifelse(PPTZhhdatass$events %in% c("weeding1")  & PPTZhhdatass$DatesBnPlantingEvent  == 56, "On-time",
                                                                                                          ifelse(PPTZhhdatass$events %in% c("weeding1")  & PPTZhhdatass$DatesBnPlantingEvent > 56 , "Done later",
                                                                                                                 ifelse(PPTZhhdatass$events %in% c("weeding1")  & PPTZhhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                                                                                        ifelse(PPTZhhdatass$events %in% c("weeding1")  & PPTZhhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                               
                                                                                                                               ifelse(PPTZhhdatass$events %in% c("weeding2")  & PPTZhhdatass$DatesBnPlantingEvent > 0, "On-time",
                                                                                                                                      ifelse(PPTZhhdatass$events %in% c("weeding2")  & PPTZhhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                             ifelse(PPTZhhdatass$events %in% c("weeding2")  & PPTZhhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                                                                                                                    ifelse(PPTZhhdatass$events %in% c("weeding3")  & PPTZhhdatass$DatesBnPlantingEvent > 0, "On-time",
                                                                                                                                                           ifelse(PPTZhhdatass$events %in% c("weeding3")  & PPTZhhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                  ifelse(PPTZhhdatass$events %in% c("weeding3")  & PPTZhhdatass$DatesBnPlantingEvent  == -999, "Not done",
                                                                                                                                                                         
                                                                                                                                                                         ifelse(PPTZhhdatass$events %in% c("harvest")   & PPTZhhdatass$DatesBnPlantingEvent > 0, "On-time",
                                                                                                                                                                                ifelse(PPTZhhdatass$events %in% c("harvest")   & PPTZhhdatass$DatesBnPlantingEvent == -100, "To be done",
                                                                                                                                                                                       
                                                                                                                                                                                       as.character(PPTZhhdatass$status)))))))))))))))))))))))
      
      PPTZhhdatass <- replace(PPTZhhdatass, PPTZhhdatass == -100, NA)
      getstatus <- rbind(getstatus, PPTZhhdatass)
    }
  }
  
  
  PPTZhhdata <- getstatus
  head(PPTZhhdata)
  #dsPPTZR <- PPTZhhdata
  
  #calculating due dates for PPTZ
  #PPTZhhdata$actualDates <- as.Date(PPTZhhdata$actualDates) 
  PPTZhhdata$status <- ifelse(is.na(PPTZhhdata$DatesBnPlantingEvent), "To be done", as.character(PPTZhhdata$status))
  PPTZhhdata$status <- ifelse(PPTZhhdata$DatesBnPlantingEvent == "", "To be done", as.character(PPTZhhdata$status))
  PPTZhhdata$status <- ifelse(is.na(PPTZhhdata$status), "To be done", as.character(PPTZhhdata$status))
  
  PPTZhhdata$actualmn <- lubridate::month(ymd(PPTZhhdata$actualDates))
  
  PPTZhhdata$month  <- month.abb[PPTZhhdata$actualmn]
  PPTZhhdata$actuald <- lubridate::day(ymd(PPTZhhdata$actualDates))
  PPTZhhdata$actualday <- lubridate::wday(ymd(PPTZhhdata$actualDates))
  PPTZhhdata$actualYR <- lubridate::year(ymd(PPTZhhdata$actualDates))
  head(PPTZhhdata)
  
  PPTZhhdata$datetbd <- ifelse(!is.na(PPTZhhdata$actuald), paste(PPTZhhdata$actualYR, PPTZhhdata$actualmn, PPTZhhdata$actuald, sep = "/"), NA)
  
  PPTZhhdata$datetbd2 <- as.Date(PPTZhhdata$datetbd) 
  
  
  DsPPTZdue <- NULL
  for(hids in unique(PPTZhhdata$HHID)){
    hd <- PPTZhhdata[PPTZhhdata$HHID == hids, ]
    hdpl <- hd[hd$events == 'planting', ]
    
    hdG <- hd[hd$events == 'gapping', ]
    if(is.na(hdG$datetbd2)){
      hdG$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 28
    }
    
    hdW1 <- hd[hd$events == 'weeding1', ]
    if(is.na(hdW1$datetbd2)){
      hdW1$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 84
    }
    
    hdW2 <- hd[hd$events == 'weeding2', ]
    if(is.na(hdW2$datetbd2)){
      hdW2$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 168
    }
    
    hdW3 <- hd[hd$events == 'weeding3', ]
    if(is.na(hdW3$datetbd2)){
      hdW3$datetbd2 <- as.Date(hd[hd$events == "planting",]$datetbd2) + 168
    }
    
    hdv <- hd[hd$events == 'harvest', ]
    
    DsPPTZdue <- suppressWarnings({rbind(DsPPTZdue,hdpl, hdG, hdW1,hdW2,hdW3,hdv)})
  }
  
  names(DsPPTZdue) <- c('HHID', 'season', 'Events', 'plannedplantingdate', 'actualEvents', 'actualDates', 'EAID', 'EA_Name', 'DatesBnPlantingEvent','status', 
                        'actualmn', 'month', 'actuald', 'actualday', 'actualYR',   'datetbd',   'datetbd2')
  head(DsPPTZdue)
  
  
  DsPPTZdue$status <- ifelse(DsPPTZdue$status == "To be done"& DsPPTZdue$datetbd2 < Sys.Date(), "Overdue", as.character(DsPPTZdue$status))
  DsPPTZdue <- droplevels(DsPPTZdue[!is.na(DsPPTZdue$HHID), ] )
  
  DsPPTZdue$status <- ifelse(is.na(DsPPTZdue$DatesBnPlantingEvent), "To be done", as.character(DsPPTZdue$status))
  
  # ADDING column for DST run
  DsPPTZdue$datetbd2 <- as.character(DsPPTZdue$datetbd2)
  
  hhid_TT <- droplevels(DsPPTZdue[DsPPTZdue$HHID %in% dst_PP_TZ$HHID, ])
  hhid_TF <- droplevels(dst_PP_TZ[!dst_PP_TZ$HHID %in% DsPPTZdue$HHID, ])## dst is run but no data is submitted
  hhid_FT <- droplevels(DsPPTZdue[!DsPPTZdue$HHID %in% dst_PP_TZ$HHID, ])## dst is not run but there is data
  
  PPTZ_TT <- NULL
  for(hhids in unique(hhid_TT$HHID)){
    hd <- hhid_TT[hhid_TT$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$datetbd2 <- "True"
    hd1$Events <- "DST run"
    hd1$status <- "To be done"
    PPTZ_TT <- rbind(PPTZ_TT, hd, hd1)
  }
  
  
  # eaids <- "ACEANG000123"
  PPTZ_TF <- NULL
  for(eaids in unique(hhid_TF$EAID)){
    edata <- DsPPTZdue[DsPPTZdue$EAID == eaids, ]
    hh_nodata <- unique(hhid_TF[hhid_TF$EAID == eaids, ]$HHID)
    if(length(hh_nodata) > 0){
      asas <- edata[1:length(hh_nodata),]
      asas$HHID <- hh_nodata
      asas$Events <- "DST run"
      asas$actualEvents <- "DST run"
      asas$status <- "To be done"
      asas$datetbd2 <- "True"
      PPTZ_TF <- rbind(PPTZ_TF, edata, asas)
    }
  }
  
  PPTZ_dstchecked <- rbind(PPTZ_TT, PPTZ_TF)
  head(PPTZ_dstchecked)
  
  # length(unique(PPTZ_dstchecked$HHID))
  # length(unique(PPTZ_dstchecked$EAID))
  # PPTZ_dstchecked$COUNTRY <- ifelse(grepl("ACHHTZ", PPTZ_dstchecked$HHID), "TZ", "NG")
  # TZPPT <- PPTZ_dstchecked[PPTZ_dstchecked$COUNTRY == "TZ", ]
  # length(unique(TZPPT$HHID))
  # length(unique(TZPPT$EAID))
  
  ###################################################
  #PPTZpoints
  ###################################################
  dataVAl_PPTZ <- read.csv("data/dataVAL_PP_TZ.csv")
  
  pointsPPTZdat <- unique(subset(dataVAl_PPTZ, select=c(HHID, EAID, SubmissionDate,event,plantingDate, gappingDate,
                                                        dateWeeding1, dateWeeding2, dateWeeding3, harvestDate)))
  colnames(pointsPPTZdat) <- c("HHID", "EAID", "subdate", "p_event", "actualplantingdate", "gapping_date", "weeding1", "weeding2", "weeding3", "cassavahvstdate")
  pointsPPTZdat <- droplevels(unique(merge(pointsPPTZdat, dsEA, by=c("EAID"))))
  
  library(lubridate)	
  # ADDING column for DST run
  VAl_PPTZpts <- subset(VAl_PP_TZ, select = c(HHID, SubmissionDate))
  VAl_PPTZpts$SubmissionDate <- mdy_hms(VAl_PPTZpts$SubmissionDate)
  VAl_PPTZpts$SubmissionDate <- as.Date(as.POSIXct(VAl_PPTZpts$SubmissionDate, format = "%m/%d/%Y"))
  head(VAl_PPTZpts)
  pts_valPPTZ <- merge(pointsPPTZdat, VAl_PPTZpts, by=c("HHID") )
  
  
  PPTZ_runs <- NULL
  for(hhids in unique(pts_valPPTZ$HHID)){
    hd <- pts_valPPTZ[pts_valPPTZ$HHID == hhids, ]
    hd1 <- hd[1,]
    hd1$p_event <- "DST run"
    PPTZ_runs <- rbind(PPTZ_runs, hd, hd1)
  }
  
  
  head(PPTZ_runs)
  
  pointsPPTZ <-PPTZ_runs
  pointsPPTZ$EA_Name<- ifelse(pointsPPTZ$EA_Name %in% "Wargaret Asuo", "Margaret Asuo", as.character(pointsPPTZ$EA_Name))
  
  #fixing dates
  pointsPPTZ$subdate <-mdy_hms(pointsPPTZ$subdate)
  pointsPPTZ$subdate <- as.Date(as.POSIXct(pointsPPTZ$subdate, format = "%m/%d/%Y"))
  pointsPPTZ$actualmn <- lubridate::month(ymd(pointsPPTZ$subdate))
  
  pointsPPTZ$month  <- month.abb[pointsPPTZ$actualmn]
  pointsPPTZ$actuald <- lubridate::day(ymd(pointsPPTZ$subdate))
  pointsPPTZ$actualday <- lubridate::wday(ymd(pointsPPTZ$subdate))
  pointsPPTZ$actualYR <- lubridate::year(ymd(pointsPPTZ$subdate))
  
  pointsPPTZ$submn <- lubridate::month(ymd(pointsPPTZ$SubmissionDate))
  
  pointsPPTZ$submonth  <- month.abb[pointsPPTZ$submn]
  pointsPPTZ$subd <- lubridate::day(ymd(pointsPPTZ$SubmissionDate))
  pointsPPTZ$subday <- lubridate::wday(ymd(pointsPPTZ$SubmissionDate))
  pointsPPTZ$subYR <- lubridate::year(ymd(pointsPPTZ$SubmissionDate))
  head(pointsPPTZ)
  
  pointsPPTZ <- unique(pointsPPTZ)
  
  #Omit data for events not done
  #
  pointsPPTZ$event1 <-as.numeric(ifelse ((!is.na(pointsPPTZ$subdate)) &  (pointsPPTZ$p_event == "event1"), "15", "0"))
  #
  pointsPPTZ$event2 <- as.numeric(ifelse ((!is.na(pointsPPTZ$subdate)) &  (pointsPPTZ$p_event == "event2"), "7", "0"))
  pointsPPTZ$event3 <- as.numeric(ifelse ((!is.na(pointsPPTZ$subdate)) &  (pointsPPTZ$p_event == "event3"), "7", "0"))
  pointsPPTZ$event4 <- as.numeric(ifelse ((!is.na(pointsPPTZ$subdate)) &  (pointsPPTZ$p_event == "event4"), "7", "0"))
  pointsPPTZ$event5 <- as.numeric(ifelse ((!is.na(pointsPPTZ$subdate)) &  (pointsPPTZ$p_event == "event5"), "7", "0"))
  pointsPPTZ$event6 <- as.numeric(ifelse ((!is.na(pointsPPTZ$subdate)) &  (pointsPPTZ$p_event == "event6"), "7", "0"))
  pointsPPTZ$event7 <- as.numeric(ifelse ((!is.na(pointsPPTZ$subdate)) &  (pointsPPTZ$p_event == "event7"), "40", "0"))
  pointsPPTZ$DST_run <- as.numeric(ifelse ((!is.na(pointsPPTZ$SubmissionDate)) &  (pointsPPTZ$p_event == "DST run"), "10", "0"))
  
  
  ptsPPTZ <- subset(pointsPPTZ, select = c(HHID, EAID, EA_Name, event1, event2, event3, event4, event5, event6, event7, DST_run))
  head(ptsPPTZ)
  
  library(dplyr)
  PPTZpoints <- ptsPPTZ %>% group_by(HHID, EAID, EA_Name) %>% summarise_at(1:8, sum)
  PPTZpoints <- data.table::setDT(PPTZpoints)[HHID!="ACHHNG123456" & HHID!="ACHHNG000000"]
  PPTZpoints = unique(PPTZpoints)
  
  PPTZpoints$Totalpoints <- PPTZpoints$event1+PPTZpoints$event2+PPTZpoints$event3+ PPTZpoints$event4+PPTZpoints$event5+PPTZpoints$event6 +PPTZpoints$event7 + PPTZpoints$DST_run 
  head(PPTZpoints)
  library(tidyr)
  dsPPTZpnts1 <- subset(pointsPPTZ, select=c(EAID, actualYR, subYR, month, submonth, event1, event2, event3, event4, event5, event6, event7, DST_run))
  dsPPTZpnts2 <- dsPPTZpnts1 %>% gather(events, points,event1, event2, event3, event4, event5, event6, event7, DST_run)
  dsPPTZpnts2$month[dsPPTZpnts2$events == "DST_run"] <- dsPPTZpnts2$submonth
  dsPPTZpnts2$actualYR[dsPPTZpnts2$events == "DST_run"] <- dsPPTZpnts2$subYR
  
  head(dsPPTZpnts2)
  
  dsPPTZpnts2 <- subset(dsPPTZpnts2, select=c(EAID, events, actualYR, month, points))
  colnames (dsPPTZpnts2) <- c("EAID", "Events", "Year", "month", "points")
  
  
  dsPPTZpnts2 <- droplevels(dsPPTZpnts2[!is.na(dsPPTZpnts2$month), ])
  
  dsPPTZpnts3 <- dsPPTZpnts2 %>% dplyr::mutate(i = row_number()) %>% spread(month, points)
  dsPPTZpnts3 <- subset(dsPPTZpnts3, select = -c(i))
  
  head(dsPPTZpnts3)
  
  
  #fix all calendar months not available in data
  
  Notavailmonths <- month.abb[!month.abb %in% colnames(dsPPTZpnts3)]
  x <- suppressWarnings({as.data.frame(matrix(ncol=length(Notavailmonths), nrow=nrow(dsPPTZpnts3), data=as.numeric('NA')))})
  colnames(x)<-  Notavailmonths 
  dsPPTZpnts4 <- cbind(dsPPTZpnts3, x)
  head(dsPPTZpnts4)
  
  #summarize points
  PPTZpointsdt <- ddply(dsPPTZpnts4, .(EAID, Events, Year), summarize,
                        Jan = sum(Jan, na.rm = TRUE),
                        Feb = sum(Feb, na.rm = TRUE),
                        Mar = sum(Mar, na.rm = TRUE),
                        Apr = sum(Apr, na.rm = TRUE),
                        May = sum(May, na.rm = TRUE),
                        Jun = sum(Jun, na.rm = TRUE),
                        Jul = sum(Jul, na.rm = TRUE),
                        Aug = sum(Aug, na.rm = TRUE),
                        Sep = sum(Sep, na.rm = TRUE),
                        Oct = sum(Oct, na.rm = TRUE),
                        Nov = sum(Nov, na.rm = TRUE),
                        Dec = sum(Dec, na.rm = TRUE))
  
  PPTZpointsdt2 <- ddply(PPTZpointsdt, .(EAID,Events, Year,  Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
                         summarize, Total = sum(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec))
  
  PPTZpointsdt3 <- ddply(PPTZpointsdt, .(EAID, Year),
                         summarize, Jan = sum(Jan), Feb=sum(Feb), Mar=sum(Mar), Apr=sum(Apr), May=sum(May), Jun=sum(Jun), Jul=sum(Jul), Aug=sum(Aug), 
                         Sep=sum(Sep), Oct=sum(Oct), Nov=sum(Nov), Dec=sum(Dec))
  PPTZpointsdt3$Events <- "Total"
  PPTZpointsdt3$Total <- ""
  PPTZpointsdt3 <- PPTZpointsdt3[, colnames(PPTZpointsdt2)]
  
  PPTZpointsdt2 <- PPTZpointsdt2[PPTZpointsdt2$Total >0, ]
  PPTZpointsdt2 <- rbind(PPTZpointsdt2, PPTZpointsdt3)
  
  head(PPTZpointsdt2)
  
  #################################################################################################################################################
  #PPTZ weeks
  head(DsPPTZdue)
  dsPPTZwks2 <- DsPPTZdue
  #get days/weeks/months/year from actual dates 
  dsPPTZwks2$planting1wk <- lubridate::week(ymd(dsPPTZwks2$plannedplanting))
  dsPPTZwks2$eventwk <- lubridate::week(ymd(dsPPTZwks2$datetbd2))
  
  dsPPTZwks2$planting1yr <- lubridate::year(ymd(dsPPTZwks2$plannedplanting))
  
  PPTZwkhh <- dsPPTZwks2
  
  ##deduct weeks of the year from planting week
  PPTZwkhh$status2 <- ifelse(PPTZwkhh$eventwk == -999, -999, PPTZwkhh$eventwk - PPTZwkhh$planting1wk)
  head(PPTZwkhh)
  
  drops <- c( "DatesBnPlantingEvent", "DiffToday") 
  PPTZwkhh <- (PPTZwkhh[ , !(names(PPTZwkhh) %in% drops)])
  head(PPTZwkhh)
  
  colnames(PPTZwkhh) <- c('HHID', 'season', 'events', 'plannedplanting', 'actualEvents', "actualDates", "EAID","EA_Name", "status", "actualmn","month","actuald", "actualday", "actualYR",   
                          "datetbd",   "datetbd2", "planting1wk", "weeks", "planting1yr", "status2")
  
  
  PPTZwksdata <- PPTZwkhh
  
  PPTZwksdata$dueyear <- lubridate::year(PPTZwksdata$datetbd2)
  # icwksdata$diffsys <- ppwksdata$datetbd2 > Sys.Date() & ppwksdata$dueyear ==  lubridate::year(Sys.Date())
  # ppwksdata$status <- ifelse(ppwksdata$diffsys == 'TRUE', "Due soon", as.character(ppwksdata$status))
  PPTZwksdata$index <- 1:nrow(PPTZwksdata)
  
  toBeDue <- droplevels(PPTZwksdata[PPTZwksdata$datetbd2 > Sys.Date() & PPTZwksdata$dueyear ==  lubridate::year(Sys.Date()), ] )
  
  
  if (nrow(toBeDue) > 0) {
    toBeDue$todayweek <-  as.numeric(lubridate::week(Sys.Date()))
    toBeDue$t2 <- as.numeric(as.character(toBeDue$weeks)) - toBeDue$todayweek
    toBeDue <- toBeDue[toBeDue$t2 <= 2, ]
    #toBeDone <- toBeDue[toBeDue$t2 > 2, ]
    if (nrow(toBeDue) > 0) {
      toBeDue$status <- 'Due soon'
      toBeDue$month <- as.factor(month.abb[lubridate::month(ymd(toBeDue$datetbd2))]) 
      PPTZwksdata <- PPTZwksdata[!PPTZwksdata$index %in% toBeDue$index, ]
      PPTZwksdata <- rbind(PPTZwksdata, toBeDue[, colnames(PPTZwksdata)]) 
    }
    # if (nrow(toBeDone) > 0) {
    #   toBeDone$status <- 'To be done'
    #   toBeDone$month <- as.factor(month.abb[lubridate::month(ymd(toBeDone$datetbd2))]) 
    #   icwksdata <- icwksdata[!icwksdata$index %in% toBeDone$index, ]
    #   icwksdata <- rbind(icwksdata, toBeDone[, colnames(icwksdata)]) 
    # }
    
  }
  
  
  PPTZwksdata <- droplevels(PPTZwksdata[!is.na(PPTZwksdata$HHID), ])
  PPTZwksdata$status <- ifelse(PPTZwksdata$status == "To be done" & Sys.Date() > PPTZwksdata$datetbd2, "Not done", as.character(PPTZwksdata$status))
  PPTZwksdata$status <- ifelse(is.na(PPTZwksdata$status), "To be done", as.character(PPTZwksdata$status))
  
  head(PPTZwksdata)
  
  ########################################################################################
  
  #Recommendation table
  
  dsFR <- read.csv("data/VAL_FR.csv")
  dsPP <- read.csv("data/VAL_PP.csv")
  dsPPTZ <- read.csv("data/VAL_PP_TZ.csv")
  dsIC <- read.csv("data/VAL_IC.csv")
  dsIC$HHID <- paste(dsIC$HHID, ifelse(dsIC$maizeVariety=="Ikom_White", "_Ikom_White", ""), sep="")
  dsCIS <- read.csv("data/VAL_CIS.csv")
  
  VAl_FR <- read.csv("data/VAL_FR.csv")
  VAl_IC <- read.csv("data/VAL_IC.csv")
  VAl_PP <- read.csv("data/VAL_PP.csv")
  VAl_PP_TZ <- read.csv("data/VAL_PP_TZ.csv")
  VAl_CIS <- read.csv("data/VAL_CIS.csv")
  
  VAl_SPHS_KW <- read.csv("data/VAL_SPHS_KW.csv",stringsAsFactors=FALSE)
  VAl_SPHS_OG <- read.csv("data/VAL_SPHS_OG.csv",stringsAsFactors=FALSE)
  VAl_SPHS_ON <- read.csv("data/VAL_SPHS_ON.csv",stringsAsFactors=FALSE)
  VAl_SPHS_OY <- read.csv("data/VAL_SPHS_OY.csv",stringsAsFactors=FALSE)
  
  VAl_SPHS_TZEZ <- read.csv("data/VAL_SPHS_TZEZ.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZLZE <- read.csv("data/VAL_SPHS_TZLZE.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZLZW <- read.csv("data/VAL_SPHS_TZLZW.csv",stringsAsFactors=FALSE)
  VAl_SPHS_TZSZ <- read.csv("data/VAL_SPHS_TZSZ.csv",stringsAsFactors=FALSE)
  
  VAl_SPHS_NG <- rbind(VAl_SPHS_KW, VAl_SPHS_OG, VAl_SPHS_ON, VAl_SPHS_OY)
  VAl_SPHS_TZ <- rbind(VAl_SPHS_TZEZ, VAl_SPHS_TZLZE, VAl_SPHS_TZLZW, VAl_SPHS_TZSZ)
  VAl_SPHS_NG <- VAl_SPHS_NG[, colnames(VAl_SPHS_TZ)]
  VAl_SPHS <- rbind(VAl_SPHS_NG, VAl_SPHS_TZ)
  dsSPHS <- VAl_SPHS
  
  names(dsFR)[grepl('\\.', names(dsFR))] <- sub('.*\\.', '', names(dsFR)[grepl('\\.', names(dsFR))])
  names(dsPP)[grepl('\\.', names(dsPP))] <- sub('.*\\.', '', names(dsPP)[grepl('\\.', names(dsPP))])
  names(dsIC)[grepl('\\.', names(dsIC))] <- sub('.*\\.', '', names(dsIC)[grepl('\\.', names(dsIC))])
  names(dsCIS)[grepl('\\.', names(dsCIS))] <- sub('.*\\.', '', names(dsCIS)[grepl('\\.', names(dsCIS))])
  names(dsSPHS)[grepl('\\.', names(dsSPHS))] <- sub('.*\\.', '', names(dsSPHS)[grepl('\\.', names(dsSPHS))])
  names(dsPPTZ)[grepl('\\.', names(dsPPTZ))] <- sub('.*\\.', '', names(dsPPTZ)[grepl('\\.', names(dsPPTZ))])
  
  #SHORT DEF:   Function to extract short version of the recommendations for the FR use case.
  #RETURNS:     Vector of short recommendations. 
  #DESCRIPTION: Runs a for loop across each line of the DST output file and reconstructs the recommendations using the ifelse rules in the ODK form.
  #INPUT:       dsIC: dataframe containing the DST output on ONA, downloaded using BriefCase and after removing group names
  getFRrec <- function(dsFR){
    
    result <- NULL
    
    for (i in 1:nrow(dsFR)){
      
      #composing FR recommendation
      s0a <- "This field requires following fertilizer rates: "
      s0b <- "We recommend to not apply any fertilizer. We do not expect that fertilizer application will increase the quantity of cassava roots produced in this field. We wish to confirm this. The yellow plot therefore evaluates the response to a moderate fertilizer application.\n"
      s0c <- "We are unable to provide you with a recommendation. Your location is either outside of the area for which recommendations have been developed, or the planting time is outside of the recommended planting window."
      s1  <- paste0(dsFR$rateUrea_AB[i], "kg of urea, ") 
      s2  <- ifelse(dsFR$country[i]=="NG", paste0(dsFR$rateTSP_AB[i], "kg of TSP, and "), "")
      s3  <- ifelse(dsFR$country[i]=="TZ", paste0(dsFR$rateNafaka_AB[i], "kg of Minjingu Nafaka Plus, and "), "")
      s4  <- paste0(dsFR$rateMOP_AB[i], "kg of MOP per ", dsFR$areaBasis[i], ".\n")
      s5  <- "Between 2 and 6 weeks after planting, following quantities of fertilizer must be applied to the yellow plot: "
      s6  <- paste0(dsFR$quantity1Urea[i], "g of urea, ")
      s7  <- ifelse(dsFR$country[i]=="NG", paste0(dsFR$quantity1TSP[i], "g of TSP, and "), "")
      s8  <- ifelse(dsFR$country[i]=="TZ", paste0(dsFR$quantity1Nafaka[i], "g of Minjingu Nafaka Plus, and "), "")
      s9  <- paste0(dsFR$quantity1MOP[i], "g of MOP.\n")
      s10 <- "Between 8 and 12 weeks after planting, following quantities of fertilizer must be applied to the yellow plot: "
      s11 <- paste0(dsFR$quantity2Urea[i], "g of urea, ")
      s12 <- ifelse(dsFR$country[i]=="NG", paste0(dsFR$quantity2TSP[i], "g of TSP, and "), "")
      s13 <- ifelse(dsFR$country[i]=="TZ", paste0(dsFR$quantity2Nafaka[i], "g of Minjingu Nafaka Plus, and "), "")
      s14 <- paste0(dsFR$quantity2MOP[i], "g of MOP.")
      
      #printing relevant recommendation based on outcome of 'rec' variable
      if(dsFR$rec[i]=="SSR"){
        recFR <- paste0(s0a,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14)
      }else{
        if(dsFR$rec[i]=="HR"){
          recFR <- paste0(s0b,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14)
        }else{
          recFR <- s0c
        }
      }
      result <- c(result, recFR)
    }
    
    return(result)
    
  }
  
  #SHORT DEF:   Function to extract short version of the recommendations for the SPHS use case.
  #RETURNS:     Vector of short recommendations. 
  #DESCRIPTION: Runs a for loop across each line of the DST output file and reconstructs the recommendations using the ifelse rules in the ODK form.
  #INPUT:       dsSPHS: dataframe containing the DST output on ONA, downloaded using BriefCase and after removing group names
  getSPHSrec <- function(dsSPHS){
    
    result <- NULL
    
    for (i in 1:nrow(dsSPHS)){
      
      #extracting the intended and recommended harvest date
      HD <- dsSPHS$harvestDate[i]
      recHD <- format(as.Date(HD, format="%b %d, %Y") + ifelse(dsSPHS$optHarvest[i]==0, -30, dsSPHS$optHarvest[i]*15), "%b %d, %Y")
      
      #composing SPHS recommendation
      s0  <- paste0("Your intended harvest date is ", HD, ". The blue plot must be harvested on this date.\n")
      s1a <- "You have selected the optimal harvest date for your cassava crop. We wish to confirm this, and compare with a harvest 1 month earlier. "
      s1b <- paste0("We recommend that you harvest ", dsSPHS$optHarvestText[i], " than the proposed harvest date. ")
      s1c <- "We are unable to provide you with a recommendation. Your location is either outside of the area for which recommendations have been developed, or the planting time is outside of the recommended planting window."
      s2  <- paste0("The red plot must be harvested on ", recHD, ".")
      
      #printing relevant recommendation based on outcome of 'optHarvest' and 'GRdif' variables
      if(is.na(dsSPHS$optHarvest[i])){
        recSPHS <- s1c
      }else{
        if(dsSPHS$optHarvest[i]==0){
          recSPHS <- paste0(s0,s1a,s2)
        }else{
          recSPHS <- paste0(s0,s1b,s2)
        }
      }
      result <- c(result, recSPHS)
    }
    
    return(result)
    
  }
  
  
  #SHORT DEF:   Function to extract short version of the recommendations for the SPHS use case.
  #RETURNS:     Vector of short recommendations. 
  #DESCRIPTION: Runs a for loop across each line of the DST output file and reconstructs the recommendations using the ifelse rules in the ODK form.
  #INPUT:       dsSPHS: dataframe containing the DST output on ONA, downloaded using BriefCase and after removing group names
  # getSPHSNGrec <- function(dsSPHSNG){
  #   
  #   result <- NULL
  #   
  #   for (i in 1:nrow(dsSPHSNG)){
  #     
  #     #extracting the intended and recommended harvest date
  #     HD <- dsSPHSNG$harvestDate[i]
  #     recHD <- format(as.Date(HD, format="%d-%b-%Y") + ifelse(dsSPHSNG$optHarvest[i]==0, -30, dsSPHSNG$optHarvest[i]*15), "%d-%b-%Y")
  #     
  #     #composing SPHS recommendation
  #     s0  <- paste0("Your intended harvest date is ", HD, ". The blue plot must be harvested on this date.\n")
  #     s1a <- "You have selected the optimal harvest date for your cassava crop. We wish to confirm this, and compare with a harvest 1 month earlier. "
  #     s1b <- paste0("We recommend that you harvest ", dsSPHSNG$optHarvestText[i], " than the proposed harvest date. ")
  #     s1c <- "We are unable to provide you with a recommendation. Your location is either outside of the area for which recommendations have been developed, or the planting time is outside of the recommended planting window."
  #     s2  <- paste0("The red plot must be harvested on ", recHD, ".")
  #     
  #     #printing relevant recommendation based on outcome of 'optHarvest' and 'GRdif' variables
  #     if(is.na(dsSPHSNG$optHarvest[i])){
  #       recSPHSNG <- s1c
  #     }else{
  #       if(dsSPHSNG$optHarvest[i]==0){
  #         dsSPHSNG <- paste0(s0,s1a,s2)
  #       }else{
  #         dsSPHSNG <- paste0(s0,s1b,s2)
  #       }
  #     }
  #     result <- c(result, dsSPHSNG)
  #   }
  #   
  #   return(result)
  #   
  # }
  #SHORT DEF:   Function to extract short version of the recommendations for the PP use case.
  #RETURNS:     Vector of short recommendations. 
  #DESCRIPTION: Runs a for loop across each line of the DST output file and reconstructs the recommendations using the ifelse rules in the ODK form.
  #INPUT:       dsPP: dataframe containing the DST output on ONA, downloaded using BriefCase and after removing group names
  getPPrec <- function(dsPP){
    
    #revalueing basicTill and ridging variables to text for plot 3
    dsPP$basicTill_plot3 <- revalue(dsPP$basicTill_plot3, c("T0" = "zero-tillage (no ploughing operation)",
                                                            "T1" = "a single ploughing operation",
                                                            "T2" = "a double ploughing operation"))
    dsPP$ridging_plot3 <- revalue(dsPP$ridging_plot3, c("FL" = "planting on flat",
                                                        "RG" = "ridging"))
    
    result <- NULL
    
    for (i in 1:nrow(dsPP)){
      
      #composing PP recommendation
      s0  <- paste0("Your current practice (blue plot) is ", dsPP$CPtext[i], ".\n")
      s1a <- paste0("The recommended practice (yellow plot) is ", dsPP$recText[i], ".")
      s1b <- paste0("The yellow plot is planted on flat instead of ridges. Primary tillage remained unchanged (", dsPP$basicTillRec[i], ").")
      s1c <- "In the yellow plot, a single ploughing operation is conducted instead of double ploughing. Secondary tillage remained unchanged (planting on flat)."
      s1d <- "In the yellow plot, zero-tillage (no ploughing operation) is practiced instead of a single ploughing operation. Secondary tillage remained unchanged (planting on flat)."
      s1e <- "In the yellow plot a single ploughing operation was conducted instead of zero-tillage (no ploughing operation). Secondary tillage remained unchanged (planting on flat)."
      s2a <- paste0("In the red plot, land preparation was done following your current practice but without the", dsPP$harrowText2[i], ".")
      s2b <- paste0("In the red plot, ", dsPP$basicTill_plot3[i], " and ", dsPP$ridging_plot3[i], " was practiced.")
      s2  <- paste0("\n", ifelse(dsPP$harrow[i] != "H0", s2a, ifelse(dsPP$plot3[i]=="yes", s2b, "No red plot was established.")))
      
      #printing relevant recommendation
      if(dsPP$netValueDifMax[i]>0){
        recPP <- paste0(s0,s1a,s2)
      }else{
        if(dsPP$ridging[i]=="RG"){
          recPP <- paste0(s0,s1b,s2)
        }else{
          if(dsPP$basicTill[i]=="T2"){
            recPP <- paste0(s0,s1c,s2)
          }else{
            if(dsPP$basicTill[i]=="T1"){
              recPP <- paste0(s0,s1d,s2)
            }else{
              recPP <- paste0(s0,s1e,s2)
            }
          }
        }
      }
      result <- c(result, recPP)
    }
    
    return(result)
    
  }
  
  
  #SHORT DEF:   Function to extract short version of the recommendations for the PP use case.
  #RETURNS:     Vector of short recommendations. 
  #DESCRIPTION: Runs a for loop across each line of the DST output file and reconstructs the recommendations using the ifelse rules in the ODK form.
  #INPUT:       dsPP: dataframe containing the DST output on ONA, downloaded using BriefCase and after removing group names
  getPPTZrec <- function(dsPPTZ){
    
    #revalueing basicTill and ridging variables to text for plot 3
    dsPPTZ$basicTill <- revalue(dsPPTZ$basicTill, c("T0" = "zero-tillage (no ploughing operation)",
                                                            "T1" = "a single ploughing operation"))
                                                            #"T2" = "a double ploughing operation")
    dsPPTZ$ridging <- revalue(dsPPTZ$ridging, c("FL" = "planting on flat",
                                                        "RG" = "ridging"))
    
    result <- NULL
    
    for (i in 1:nrow(dsPPTZ)){
      
      #composing PP recommendation
      s0  <- paste0("Your current practice (blue plot) is ", dsPPTZ$CPtext[i], ".\n")
      s1a <- paste0("The recommended practice (yellow plot) is ", dsPPTZ$recText[i], ".")
      s1b <- paste0("The yellow plot is planted on flat instead of ridges. Primary tillage remained unchanged (", dsPPTZ$basicTillRec[i], ").")
      s1c <- "In the yellow plot, a single ploughing operation is conducted instead of double ploughing. Secondary tillage remained unchanged (planting on flat)."
      s1d <- "In the yellow plot, zero-tillage (no ploughing operation) is practiced instead of a single ploughing operation. Secondary tillage remained unchanged (planting on flat)."
      s1e <- "In the yellow plot a single ploughing operation was conducted instead of zero-tillage (no ploughing operation). Secondary tillage remained unchanged (planting on flat)."
      s2a <- paste0("In the red plot, land preparation was done following your current practice but without the", dsPPTZ$harrowText2[i], ".")
      s2b <- paste0("In the red plot, ", dsPPTZ$basicTill[i], " and ", dsPPTZ$ridgingMethod[i], " was practiced.")
      s2  <- paste0("\n", ifelse(dsPPTZ$harrow[i] != "H0", s2a, ifelse(dsPPTZ$plot[i]=="yes", s2b, "No red plot was established.")))
      
      #printing relevant recommendation
      if(dsPPTZ$netValueDifMax[i]>0){
        recPP <- paste0(s0,s1a,s2)
      }else{
        if(dsPPTZ$ridging[i]=="RG"){
          recPP <- paste0(s0,s1b,s2)
        }else{
          if(dsPPTZ$basicTill[i]=="T2"){
            recPP <- paste0(s0,s1c,s2)
          }else{
            if(dsPPTZ$basicTill[i]=="T1"){
              recPP <- paste0(s0,s1d,s2)
            }else{
              recPP <- paste0(s0,s1e,s2)
            }
          }
        }
      }
      result <- c(result, recPP)
    }
    
    return(result)
    
  }
  
  
  #SHORT DEF:   Function to extract short version of the recommendations for the IC use case.
  #RETURNS:     Vector of short recommendations. 
  #DESCRIPTION: Runs a for loop across each line of the DST output file and reconstructs the recommendations using the ifelse rules in the ODK form.
  #INPUT:       dsIC: dataframe containing the DST output on ONA, downloaded using BriefCase and after removing group names
  getICrec <- function(dsIC){
    
    result <- NULL
    
    for (i in 1:nrow(dsIC)){
      # if(dsIC$season[i]==2){
      # dsIC$heightMaize <- ifelse(dsIC$heightMaize %in% "low", "1",
      #                ifelse(dsIC$heightMaize %in% "medium", "3",
      #                       ifelse(dsIC$heightMaize %in% "tall", "5", as.character((dsIC$heightMaize)))))
      if(dsIC$heightMaize[i]==1 | (dsIC$heightMaize[i] != 5 & dsIC$profitExtra[i]<dsIC$riskRatio2[i])){
        recIC <- "The recommended practice is low density maize without fertilizer application (blue plot)."
      }else{
        if(dsIC$heightMaize[i]==5){
          recIC <- "The recommended practice is high density maize without fertilizer application (green plot)."
        }else{
          recIC <- "The recommended practice is high density maize with fertilizer application (yellow plot)."
          
          
          if(dsIC$heightMaize[i] != 'medium' | dsIC$profitExtra[i]<dsIC$riskRatio2[i]){
            recIC <- "The recommended practice is high density maize without fertilizer application (green plot)."
          }else{
            recIC <- "The recommended practice is high density maize with fertilizer application (yellow plot)."
          }
        }
      }
      result <- c(result, recIC)
    }
    
    return(result)
    
  }
  
  # SHORT DEF:   Function to extract short version of the recommendations for the CIS use case.
  # RETURNS:     Vector of short recommendations.
  # DESCRIPTION: Runs a for loop across each line of the DST output file and reconstructs the recommendations using the ifelse rules in the ODK form.
  # INPUT:       dsCIS: dataframe containing the DST output on ONA, downloaded using BriefCase and after removing group names
  getCISrec <- function(dsCIS){
    
    result <- NULL
    
    for (i in 1:nrow(dsCIS)){
      if(dsCIS$rec[i]=="MC"){
        recCIS <- "We recommend not to intercrop your cassava with sweet potato (blue plot)."
      }else{
        if(dsCIS$rec[i]=="IC"){
          recCIS <- "We recommend to intercrop your cassava with sweet potato, without fertilizer application."
        }else{
          recCIS <- paste0("We recommend to intercrop your cassava with sweet potato, and to apply additional fertilizer at a rate of ", dsCIS$rateFertilizerAB_Text[i], ".")
        }
      }
      result <- c(result, recCIS)
    }
    
    return(result)
    
  }
  

  
  #SHORT DEF:   Wrapper function cobmining the extraction of short versions of the recommendations across use cases.
  #RETURNS:     Vector of short recommendations. 
  #DESCRIPTION: Calls for the specific function to extract short recommendations.
  #INPUT:       ds: dataframe containing the DST output on ONA, downloaded using BriefCase and after removing group names
  #             useCase: string indicating the specific use case for which to extract short recommendations. One of either "FR", "IC", "SPHS", "CIS", "PP".
  getRec <- function(ds, country = c("NG", "TZ"), useCase = c("FR", "IC", "SPHS", "PP")){
    if (useCase=="FR") result <- getFRrec(dsFR=ds)
    if (useCase=="IC" & country == "NG") result <- getICrec(dsIC=ds) else if (useCase=="IC" & country == "TZ")result <- getCISrec(dsCIS=ds)
 
  
    if (useCase=="SPHS")  result <- getSPHSrec(dsSPHS=ds)
    if (useCase=="PP" & country == "TZ")  result <- getPPTZrec(dsPPTZ=ds) else if (useCase=="PP" & country == "NG") result <- getPPrec(dsPP=ds)
    return(result)
  }
  
  dsICcommend <- droplevels(unique(merge(dsIC, dsHH, by=c("HHID", "EAID"))))
  dsICcommend$IC_Recommendation <- getRec(ds=dsICcommend, useCase="IC")
  dsicrecom <- unique(subset(dsICcommend, select=c(HHID, HH_Name, EAID, IC_Recommendation)))
  
  
  dsFRcommend <- droplevels(unique(merge(dsFR, dsHH, by=c("HHID", "EAID"))))
  dsFRcommend$FR_Recommendation <- getRec(ds=dsFRcommend, useCase="FR")
  dsfrecom <- unique(subset(dsFRcommend, select=c(HHID, HH_Name, EAID, FR_Recommendation)))
  
  
  drops <- c("basicTillMethod2_plot3", "harrowSelect_plot3", "harrow_plot3", "ridgingMethodSelect_plot3", "ridgingMethod_plot3", "call", "confirmVAL", "hostBPP3")
  dsPP1 <- (dsPP[ , !(names(dsPP) %in% drops)])
  dsPPcommend <- droplevels(unique(merge(dsHH, dsPP1, by=c("HHID", "EAID"))))
  dsPPcommend$PP_Recommendation <- getRec(ds=dsPPcommend, useCase="PP")
  dspprecom <- unique(subset(dsPPcommend, select=c(HHID, HH_Name,EAID, PP_Recommendation)))
  
  drops <- c("basicTillMethod2_plot3", "harrowSelect_plot3", "harrow_plot3", "ridgingMethodSelect_plot3", "ridgingMethod_plot3", "call", "confirmVAL", "hostBPP3")
  dsPPTZ1 <- (dsPPTZ[ , !(names(dsPPTZ) %in% drops)])
  dsPPTZcommend <- droplevels(unique(merge(dsHH, dsPPTZ1, by=c("HHID", "EAID"))))
  dsPPTZcommend$PP_Recommendation <- getRec(ds=dsPPTZcommend, useCase="PP")
  dsPPTZrecom <- unique(subset(dsPPTZcommend, select=c(HHID, HH_Name,EAID, PP_Recommendation)))
  
  dsSPHScommend <- droplevels(unique(merge(dsSPHS, dsHH, by=c("HHID", "EAID"))))
  dsSPHScommend$SPHS_Recommendation <- getRec(ds=dsSPHScommend, useCase="SPHS")
  dssphsrecom <- unique(subset(dsSPHScommend, select=c(HHID, HH_Name, EAID, SPHS_Recommendation)))
  
  dsCIScommend <- droplevels(unique(merge(dsCIS, dsHH, by=c("HHID", "EAID"))))
  dsCIScommend$CIS_Recommendation <- getCISrec(ds=dsCIScommend)
  dsCISrecom <- unique(subset(dsCIScommend, select=c(HHID, HH_Name, EAID, CIS_Recommendation)))
  
  #Dates data issue
  errordatesfr <- unique(hhdata[hhdata$actualDates > Sys.Date(), ])
  
  HHFRerrordates <- droplevels(dsEAHH[dsEAHH$HHID %in% errordatesfr$HHID, ])
  HHFRerrordates$data_issue <- "unrealistic dates"
  HHFRerrordates <- select(HHFRerrordates, -c(EAHHUC))
  
  errordatesic <- unique(ichhdata[ichhdata$actualDates > Sys.Date(), ])
  HHICerrordates <- droplevels(dsEAHH[dsEAHH$HHID %in% errordatesic$HHID, ])
  HHICerrordates$data_issue <- "unrealistic dates"
  HHICerrordates <- select(HHICerrordates, -c(EAHHUC))
  
  errordatesCIS <- unique(chhdata[chhdata$actualDates > Sys.Date(), ])
  HHCISerrordates <- droplevels(dsEAHH[dsEAHH$HHID %in% errordatesCIS$HHID, ])
  HHCISerrordates$data_issue <- "unrealistic dates"
  HHCISerrordates <- select(HHCISerrordates, -c(EAHHUC))
  
  errordatespp <- unique(PPhhdata[PPhhdata$actualDates > Sys.Date(), ])
  HHPPerrordates <- droplevels(dsEAHH[dsEAHH$HHID %in% errordatespp$HHID, ])
  HHPPerrordates$data_issue <- "unrealistic dates"
  HHPPerrordates <- select(HHPPerrordates, -c(EAHHUC))
  
  errordatessp <- unique(sphsdata[sphsdata$actualDates > Sys.Date(), ])
  HHSPerrordates <- droplevels(dsEAHH[dsEAHH$HHID %in% errordatessp$HHID, ])
  HHSPerrordates$data_issue <- "unrealistic dates"
  HHSPerrordates <- select(HHSPerrordates, -c(EAHHUC))
  
  
  dsEAHH_DI <- rbind(multipleHHName, multipleHHID, multipleEAName, oneEADupHH, HHFRerrordates, HHICerrordates,HHPPerrordates, HHSPerrordates)
  dsEAHH_DI$Latitude <- round(dsEAHH_DI$Latitude, digits=3)
  dsEAHH_DI$Longitude <- round(dsEAHH_DI$Longitude, digits=3)
  dsEAHH_DI <- unique(dsEAHH_DI)
  dsEAHH_DI$Status <- "Keep"
  
  #setwd("C:/Users/Turry/Documents/ACAI/EA Tools/ValActivityTool/sourceData")
  #setwd("D:/ACAI_EA_Monitoring/sourceData")
  saveRDS(dsHH, "dsHH.RDS")
  saveRDS(dsEA, "dsEA.RDS")
  saveRDS(dataVAl_FR, "dataVAl_FR.RDS")
  saveRDS(dataVAl_IC, "dataVAl_IC.RDS")
  saveRDS(dataVAl_PP, "dataVAl_PP.RDS")
  saveRDS(dataVAl_CIS, "dataVAl_CIS.RDS")
  saveRDS(dataVAl_SPHS, "dataVAl_SPHS.RDS")
  saveRDS(dataVAL_PP_TZ, "dataVAl_PP_TZ.RDS")
  saveRDS(VAl_FR, "VAl_FR.RDS")
  saveRDS(VAl_IC, "VAl_IC.RDS")
  saveRDS(VAl_dataIC, "VAl_dataIC.RDS")
  saveRDS(VAl_PP, "VAl_PP.RDS")
  saveRDS(VAl_PP_TZ, "VAl_PP_TZ.RDS")
  saveRDS(VAl_CIS, "VAl_CIS.RDS")
  saveRDS(VAl_SPHS, "VAl_SPHS.RDS")
  saveRDS(PRA_SPHS, "PRA_SPHS.RDS")
  saveRDS(data_FR, "data_FR.RDS")
  saveRDS(dat_PP, "dat_PP.RDS")
  saveRDS(dat_PP_TZ, "dat_PP_TZ.RDS")
  saveRDS(dat_SP, "dat_SP.RDS")
  saveRDS(data_CIS, "data_CIS.RDS")
  saveRDS(dsICactual, "dsICactual.RDS")
  saveRDS(dsFRactual, "dsFRactual.RDS")
  saveRDS(dsPPactual, "dsPPactual.RDS")
  saveRDS(dsPPTZactual, "dsPPTZactual.RDS")
  saveRDS(dsCISactual, "dsCISactual.RDS")
  saveRDS(dsEAHH, "dsEAHH.RDS")
  saveRDS(multipleHHName, "multipleHHName.RDS")
  saveRDS(multipleHHID, "multipleHHID.RDS")
  #saveRDS(multipleEAName, "multipleEAName.RDS")
  saveRDS(oneEADupHH, "oneEADupHHRDS")
  saveRDS(DsFRdue, "DsFRdue.RDS")
  saveRDS(FR_dstchecked, "FR_dstchecked.RDS")
  saveRDS(wksdtplot, "wksdtplot.RDS")
  saveRDS(FRpoints, "FRpoints.RDS")
  saveRDS(dsfrecom, "dsfrecom.RDS")
  saveRDS(FRpointsdt2,'FRpointsdt2.RDS')
  saveRDS(DsCISdue, "DsCISdue.RDS")
  saveRDS(wksCISdtplot, "wksCISdtplot.RDS")
  saveRDS(DsICdue, "DsICdue.RDS")
  saveRDS(IC_dstchecked, "IC_dstchecked.RDS")
  saveRDS(icwksdata, "icwksdata.RDS")
  saveRDS(ICpoints2, "ICpoints2.RDS")
  saveRDS(dsicrecom, "dsicrecom.RDS")
  saveRDS(ICpointsdt2, "ICpointsdt2.RDS")
  saveRDS(DsPPdue, "DsPPdue.RDS")
  saveRDS(PP_dstchecked, "PP_dstchecked.RDS")
  saveRDS(ppwksdata, "ppwksdata.RDS")
  saveRDS(PPpoints, "PPpoints.RDS")
  saveRDS(PPpointsdt2, "PPpointsdt2.RDS")
  saveRDS(DsPPTZdue, "DsPPTZdue.RDS")
  saveRDS(PPTZ_dstchecked, "PPTZ_dstchecked.RDS")
  saveRDS(PPTZwksdata, "PPTZwksdata.RDS")
  saveRDS(PPTZpoints, "PPTZpoints.RDS")
  saveRDS(PPTZpointsdt2, "PPTZpointsdt2.RDS")
  saveRDS(dsPPTZrecom, "dsPPTZrecom.RDS")
  saveRDS(CISpoints, "CISpoints.RDS")
  saveRDS(dspprecom, "dspprecom.RDS")
  #saveRDS(dsEAHH_DI, "dsEAHH_DI.RDS")
  saveRDS(DsSPHSdue, "DsSPHSdue.RDS")
  saveRDS(SP_dstchecked, "SP_dstchecked.RDS")
  saveRDS(SPHSwksdata, "SPHSwksdata.RDS")
  saveRDS(SPHSpoints, "SPHSpoints.RDS")
  saveRDS(SPHSpointsdt2, "SPHSpointsdt2.RDS")
  saveRDS(dssphsrecom, "dssphsrecom.RDS")
  saveRDS(DsSPNGdue, "DsSPNGdue.RDS")
  saveRDS(SPNG_dstchecked, "SPNG_dstchecked.RDS")
  saveRDS(SPNGwksdata, "SPNGwksdata.RDS")
  saveRDS(SPNGpoints, "SPNGpoints.RDS")
  saveRDS(SPNGpointsdt2, "SPNGpointsdt2.RDS")
  saveRDS(dssphsrecom, "dssphsrecom.RDS")
  saveRDS(dsCISrecom, "dsCISrecom.RDS")
  saveRDS(CIS_dstchecked, "CIS_dstchecked.RDS")
  saveRDS(CISpointsdt2, "CISpointsdt2.RDS")
  
}

dataProcessing ()
