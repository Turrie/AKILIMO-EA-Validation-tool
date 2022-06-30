require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggExtra)

#wd: directory where the raw ODK briefcase/ONA downloads are stored
#wd <- "C:/Users/User/Documents/ACAI/EA Tools/ValActivityTool/data"

# setwd(wd)
source("clean_validation_trialData.R")

#use applicable code/the one below to download your data. Ensure output is stored in wd as above

# #downloading the data...
# forms <- c("VAL_CIS", "VAL_FR", "VAL_IC", "VAL_PP", "VAL_PP_TZ", "VAL_SPHS_TZSZ", "VAL_SPHS_TZLZE", "VAL_SPHS_TZLZW", "VAL_SPHS_TZEZ", "VAL_SPHS_OY", "VAL_SPHS_OG", "VAL_SPHS_ON", "VAL_SPHS_OS", "VAL_SPHS_KW",
#            "dataVAL_FR", "dataVAL_IC", "dataVAL_PP", "dataVAL_PP_TZ", "dataVAL_CIS", "dataVAL_SPHS",
#            "Register_EA", "Register_HH")
# briefCaseDwnld(forms=forms, target=wd, source="ONA")



#HOW TO APPLY THE DATA CLEANING FUNCTIONS
#Each function has specific arguments and these are specified in the script. e.g. for dropGroupNames:
#ds: dataset, must contain variables Latitude, Longitude and country

#with IC as an example #the only argument here is ds.

ds_VAL_IC <- dropGroupNames(read.csv(paste(wd, "VAL_IC.csv", sep="/")))

#If we want to use the output from above into another function;
ds_VAL_IC2 <- addProjectZone(ds_VAL_IC)

#Some functions have multiple arguments but these are all specified in the script e.g. for filterSingleSubmission:####

#ds: dataset to filter (must contain the end variable)
#ID: vector of ID variables for which a unique combination must be retained
#recent: if TRUE, the most recent submission is retained, else the first submission is retained

dsEA <- filterSingleSubmission(dsEA, ID="EAID", recent=TRUE) #drops all duplicate EA registration submissions, retaining only the most recent submission

#CLEANING THE DATA
#This function requires the following arguments:

#useCase: character indicating the use case for which to retrieve and clean up the DST run and data submissions
#wd: directory where the raw ODK briefcase downloads are stored
#recent: if TRUE, the most recent submission is retained, else the first submission is retained
#link: if TRUE, only data submissions for HHs for which a DST run is available are retained, else data submissions without DST runs are retained as well.

#the following line produces two lists dat, dst.# dat is the dataval output, while dst is the val output
IC <- cleanVAL("IC", wd,recent=TRUE, link = TRUE)

#to get val output
dstIC <- IC[[1]]

#to get dataval output
datIC <- IC[[2]]

###for monitorVAL these are the arguments################################################################
#dat: dataset containing the dataVAL output after running the cleaning script; must contain HHID concatenated with Ikom_White for CIM trials with Ikom_White
#wd: directory where the raw ODK output for monitorVAL form is stored
#pars: variables to add from the monitorVAL form to the dat dataset

pars <- c("trialValid", "trialQualityScore")#etc as required
ds_monitorval <- add_monValQCpars(datIC, wd, pars)#dat is from output above






