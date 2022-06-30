require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggExtra)

# source("D://workspace//ACAI//ODK_functions.R")
# wd <- "D://workspace//ODK_briefcase_dld"
# setwd(wd)

addProjectZone <- function(ds){
  #function to add variable "projectZone" to a dataset
  #ds: dataset, must contain variables Latitude, Longitude and country
  
  ds$projectZone <- as.character(cut(ds$Latitude, c(-15, -9, -6.5, -4, 0, 15), labels=c("SZ", "EZ", "ZZ", "LZ", "NG")))
  ds[ds$country %in% c("Nigeria", "NG") & ds$Longitude<5, ]$projectZone <- "SW"
  ds[ds$country %in% c("Nigeria", "NG") & ds$Longitude>=5,]$projectZone <- "SE"
  ds$projectZone <- paste(ds$country, "-", ds$projectZone)
  
  return(ds)
  
}

dropGroupNames <- function(ds){
  names(ds)[grepl('\\.', names(ds))] <- sub('.*\\.', '', names(ds)[grepl('\\.', names(ds))])
  return(ds)
}

filterSingleSubmission <- function(ds, ID, recent=TRUE) {
  #ds: dataset to filter (must contain the end variable)
  #ID: vector of ID variables for which a unique combination must be retained
  #recent: if TRUE, the most recent submission is retained, else the first submission is retained
  if(is.character(ds$end) | is.factor(ds$end)) {ds$end <- as.POSIXlt(as.character(ds$end), format="%b %d, %Y %I:%M:%S %p", tz="GMT")}
  tmp <- subset(ds, select=c(ID, "end", "KEY"))
  tmp$end <- as.numeric(julian(tmp$end))
  res <- ds[ds$KEY %in% (tmp %>% group_by_at(ID) %>% filter(end == ifelse(recent,max(end), min(end))))$KEY,]
  return(res)
}

cleanVAL <- function(useCase = c("FR", "IC", "PP", "SPHS", "CIS"), wd, recent=TRUE, link=TRUE){
  #useCase: character indicating the use case for which to retrieve and clean up the DST run and data submissions
  #wd: directory where the raw ODK briefcase downloads are stored
  #recent: if TRUE, the most recent submission is retained, else the first submission is retained
  #link: if TRUE, only data submissions for HHs for which a DST run is available are retained, else data submissions without DST runs are retained as well.
  

  #read in the EA registration data
  dsEA <- dropGroupNames(read.csv(paste(wd, "Register_EA.csv", sep="/")))
  dsEA <- filterSingleSubmission(dsEA, ID="EAID", recent=recent) #drops all duplicate EA registration submissions, retaining only the most recent submission
  
  
  #read in the HH registration data
  dsHH <- dropGroupNames(read.csv(paste(wd, "Register_HH.csv", sep="/")))
  dsHH <- filterSingleSubmission(dsHH, ID="HHID", recent=recent) #same for HH registration submissions
  
  
  #read in the dst submissions (SPHS requires merging multiple files as dst submissions are split by country/region)
  
  if(useCase=="SPHS"){
    fls <- paste(wd, list.files(path=wd, pattern=paste("^", "VAL_SPHS_", sep="")), sep="/")
    dst <- dropGroupNames(do.call(rbind, lapply(fls, function(x) read.csv(x))))
  }else{
    dst <- dropGroupNames(read.csv(paste0(wd, "/VAL_", useCase, ".csv")))
  }
  

  
  #OR
  # if(useCase=="SPHS"){
  #   #fls <- paste(wd, list.files(path=wd, pattern=paste("/", "data/VAL_SPHS_", sep="")), sep="/")
  #   #dst <- dropGroupNames(do.call(rbind, lapply(fls, function(x) read.csv(x))))
  #   fls <- paste(wd, "/data/", list.files(path=paste(wd, "/data", sep=""), pattern="VAL_SPHS_"), sep="")
  #   fls_TZ <- fls[grep("TZ", fls)]
  #   fls_NG <- fls[-grep("TZ", fls)]
  #   dst_TZ <- dropGroupNames(do.call(rbind, lapply(fls_TZ, function(x) read.csv(x))))
  #   dst_NG <- dropGroupNames(do.call(rbind, lapply(fls_NG, function(x) read.csv(x))))
  #   dst_TZ$season <- 'NA'
  #   dst_TZ <- dst_TZ[, colnames(dst_NG)]
  #   dst <- rbind(dst_TZ, dst_NG)
  #   
  # }else{
  #   dst <- dropGroupNames(read.csv(paste0(wd, "/data/VAL_", useCase, ".csv")))
  # }
  
  if(useCase == "PP") dst <- dst[,-which(names(dst)=="harrow_plot3")[1]] #dropping accidentally column with duplicated variable name
  if(!"season" %in% names(dst)) {dst$season <- NA}
  if(sum(is.na(dst$season))>0) {dst[is.na(dst$season),]$season <- 1} #replace NA values by 1 (season variable did not exist in season 1)
  dst <- filterSingleSubmission(dst, ID=c("HHID", "season"), recent=recent) #retains only the most recent DST submission per season
  dst <- merge(dst, subset(dsEA, select=c(EAID, firstNameEA, surNameEA, phoneNrEA)), all.x=TRUE) #add EA details
  dst <- merge(dst, subset(dsHH, select=c(HHID, firstNameHH, surNameHH, phoneNrHH)), all.x=TRUE) #add HH details
  if(useCase == "IC") dst$HHID <- paste(dst$HHID, ifelse(dst$maizeVariety=="Ikom_White", "_Ikom_White", ""), sep="")
  
  
  #read in the dataVAL submissions
  dat <- dropGroupNames(read.csv(paste0(wd, "dataVAL_", useCase, ".csv")))
  if(link) {dat <- dat[dat$HHID %in% dst$HHID,]}  #only data linked to a dst submission is retained.
  dat <- merge(dat, subset(dsEA, select=c(EAID, firstNameEA, surNameEA, phoneNrEA)), all.x=TRUE) #add EA details
  dat <- merge(dat, subset(dsHH, select=c(HHID, firstNameHH, surNameHH, phoneNrHH)), all.x=TRUE) #add HH details
  dat$end <- as.POSIXlt(dat$end, format="%b %d, %Y %I:%M:%S %p", tz="GMT")
  if(useCase == "IC") dat$HHID <- paste(dat$HHID, ifelse(dat$maizeVarietySelect=="Ikom_White", "_Ikom_White", ""), sep="")
  
  repeated_HHs <- as.character(unique(dst[duplicated(dst$HHID),]$HHID)) #HHIDs that run a first and second season trial
  dat1 <- dat[!dat$HHID %in% repeated_HHs,] #subset for HHIDs that have only run a single season of trials
  dat1 <- merge(dat1, subset(dst, select=c("HHID", "season"))) #add the season variable
  
  dat2 <- dat[dat$HHID %in% repeated_HHs,]
  if(nrow(dat2)>0){
    
    ff <- rbind(data.frame(useCase = "PP",   event = paste0("event", 1:7), WAP = c(0,4,8,12,24,36,48)),
                data.frame(useCase = "SPHS", event = paste0("event", 0:7), WAP = c(-2,0,4,12,24,36,48,48)),
                data.frame(useCase = "IC",   event = paste0("event", 1:8), WAP = c(0,3:5,12,24,36,48)),
                data.frame(useCase = "CIS",  event = paste0("event", 1:8), WAP = c(0,3,4,8,12,24,36,48)),
                data.frame(useCase = "FR",   event = paste0("event", 1:8), WAP = c(0,4,4,10,12,24,36,48)))
    
    res <- NULL
    for (i in repeated_HHs){
      
      PD <- subset(dst[dst$HHID==i,], select=c(HHID, season, plantingDate))
      PD$plantingDate <- as.Date(PD$plantingDate, format="%b %d, %Y")
      PD$useCase <- useCase
      PD <- merge(PD, ff)
      #set min and max dates for data collection: not earlier than 30 days before and not later than 180 days after the target data collection date
      PD$minDate <- as.numeric(julian(PD$plantingDate - 30  + 7*PD$WAP)) #earliest date at which a data collection event can be considered.
      PD$maxDate <- as.numeric(julian(PD$plantingDate + 180 + 7*PD$WAP)) #latest date at which a data collection event can be considered
      
      ss <- subset(dat2[dat2$HHID==i,], select=c(HHID, event, end, KEY))
      ss$end <- as.numeric(julian(ss$end))
      tt <- merge(ss, PD) %>% 
        filter(end >= minDate, end <= maxDate) %>%
        select(KEY, season) #merge and filter based on dates between min and max dates permitted for data collection
      res <- rbind(res, tt)
      
    }
    
    dat2 <- merge(dat2, res) #only data collection within the min-max date range per event are retained.
    
  }
  
  dat <- rbind(dat1, dat2)
  dat <- filterSingleSubmission(dat, c("HHID", "event", "season"), recent=recent) #retain most recent submission per HHID and event and season

  return(list(dst, dat))
  
  
}


add_monValQCpars <- function(dat, wd, pars){
  #dat: dataset containing the dataVAL output after running the cleaning script; must contain HHID concatenated with Ikom_White for CIM trials with Ikom_White
  #wd: directory where the raw ODK output for monitorVAL form is stored
  #pars: variables to add from the monitorVAL form to the dat dataset
  
  monVal <- read.csv(paste0(wd, "/data/monitorVAL.csv"))  
  monVal <- subset(monVal, select = c("HHID", "maizeVarietySelect", pars))
  monVal$HHID <- paste0(monVal$HHID, ifelse(monVal$maizeVarietySelect=="Ikom_White", "_Ikom_White", ""))
  monVal <- subset(monVal, select = -maizeVarietySelect)
  
  dat <- merge(dat, monVal, all.x=TRUE) #no data will be removed, trials not monitored will have NA values for pars
  
  return(dat)

}




# #downloading the data...
# forms <- c("VAL_CIS", "VAL_FR", "VAL_IC", "VAL_PP", "VAL_PP_TZ", "VAL_SPHS_TZSZ", "VAL_SPHS_TZLZE", "VAL_SPHS_TZLZW", "VAL_SPHS_TZEZ", "VAL_SPHS_OY", "VAL_SPHS_OG", "VAL_SPHS_ON", "VAL_SPHS_OS", "VAL_SPHS_KW",
#            "dataVAL_FR", "dataVAL_IC", "dataVAL_PP", "dataVAL_PP_TZ", "dataVAL_CIS", "dataVAL_SPHS",
#            "Register_EA", "Register_HH")
# briefCaseDwnld(forms=forms, target=wd, source="ONA")


#cleaning up to have unique submissions...
#PP <- cleanVAL("PP", wd)
#dstPP <- PP[[1]]
#datPP <- PP[[2]]
#
#IC <- cleanVAL("IC", wd)
#dstIC <- IC[[1]]
#datIC <- IC[[2]]
#
#CIS <- cleanVAL("CIS", wd)
#dstCIS <- CIS[[1]]
#datCIS <- CIS[[2]]
#
#FR <- cleanVAL("FR", wd)
#dstFR <- FR[[1]]
#datFR <- FR[[2]]
#
#SPHS <- cleanVAL("SPHS", wd)
#dstSPHS <- SPHS[[1]]
#datSPHS <- SPHS[[2]]
#
#

######
#dst_rates <- subset(dstFR, select=c("HHID", names(dstFR)[grepl("quantity", names(dstFR)) & !grepl("Note", names(dstFR)) & (grepl("1", names(dstFR)) | grepl("2", names(dstFR)))]))
#dat_rates <- merge(subset(datFR[datFR$event=="event3",], select=c("HHID", "urea1", "TSP1", "MOP1")),
#                   subset(datFR[datFR$event=="event4",], select=c("HHID", "urea2", "TSP2", "MOP2")))
#
#rates <- merge(dst_rates, dat_rates)
#rates[is.na(rates)] <- 0
#rates$urea <- ifelse(rates$quantity1Urea + rates$quantity2Urea - rates$urea1 - rates$urea2 < 50, TRUE, FALSE)
#rates$TSP  <- ifelse(rates$quantity1TSP  + rates$quantity2TSP  - rates$TSP1  - rates$TSP2  < 50, TRUE, FALSE)
#rates$MOP  <- ifelse(rates$quantity1MOP  + rates$quantity2MOP  - rates$MOP1  - rates$MOP2  < 50, TRUE, FALSE)
#rates$country <- substr(rates$HHID, 5, 6)

#table(rates$country, rates$urea)
#table(rates$country, rates$TSP)
#table(rates$country, rates$MOP)


#calculating EA rewards... here you need the most recent submission
#PP <- cleanVAL("PP", wd, recent=FALSE)
#dstPP <- PP[[1]]
#datPP <- PP[[2]]
#
#EA_reward <- subset(dstPP, select=c(EAID, HHID, SubmissionDate, season))
#EA_reward$event <- "DST_run"
#EA_reward <- rbind(EA_reward, subset(datPP, select=c(EAID, HHID, SubmissionDate, season, event)))
#EA_reward$SubmissionDate <- format(as.Date(EA_reward$SubmissionDate, format="%d-%b-%Y"), "%Y-%m")
#EA_reward <- droplevels(EA_reward)

#head(with(EA_reward, ftable(EAID, season, event, SubmissionDate)))





