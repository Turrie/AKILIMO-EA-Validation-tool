## EA monitoring tool
## Meklit Chernet and Turry Ouma IITA
# last updated November 2020

require(plyr)
library(shiny)
library(xtable)
require(tidyr)
library(plotly)
library(leaflet)
library(reshape2)
library(shinydashboard)
library(extrafont)
library(ggplot2)
library(lubridate)
library(viridis)
library(gridExtra)
library(ggExtra)
library(dplyr)
library(zoo)
library(googlesheets)
library(RCurl)
library(gsheet)
library(shinyBS)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(formattable)
library(shinyjs)
library(shinyalert)
library(googlesheets)
library(gsheet)
library(ggExtra)

#setwd("C:/Users/User/Documents/ACAI/EA Tools/ValActivityTool - server/getEAandHHdata")

#run 
source("getEAandHH_shiny.R")

#password list
userslist <-  c("Idris","Chris","Nor","Ekok","Femi","Samson","Gbenga","Odion","Emmanuel","Bashir","Taiwo","Stephen","Demola",
                "Abubakari", "Dada", "Frederick", "Ally", "Thompson","Saburi","Stefan","Christine","Guillaume", "Pieter", "Vera", 
                "Turry", "Meklit","Yemi", "Mark", "Busari","Florence", "Rebecca")

shinyServer(function(input, output) {
  
  #####################################################################
  ### Render UI functions
  values <- reactiveValues()
  values$tokenid <- NULL
  values$userName <- NULL
  values$EAplot <- NULL
  values$eaidsfr <- NULL
  values$eaidsphs_ng <- NULL
  values$eaidsdata <- NULL
  values$recom <- NULL
  values$icrecom <- NULL
  values$pprecom <- NULL
  #values$pptzrecom <- NULL
  values$sphsrecom <- NULL
  values$CISrecom <- NULL
  values$hhdata2 <- NULL
  values$EAplotData <- NULL
  values$ICplotData  <- NULL
  values$PPplotData  <- NULL
  values$EASPNGplot  <- NULL
  values$EASPHSplot  <- NULL
  values$EACISplot  <- NULL
  values$FRpoints1 <- NULL
  values$FRpoints <- NULL
  values$ICpoints1 <- NULL
  values$ICpoints2 <- NULL
  values$PPpoints1 <- NULL
  values$PPpoints2 <- NULL
  values$PPTZpoints1 <- NULL
  values$PPTZpoints2 <- NULL
  values$SPHSpoints1 <- NULL
  values$SPHSpoints2 <- NULL
  values$SPNGpoints1 <- NULL
  values$SPNGpoints2 <- NULL
  values$CISpoints1 <- NULL
  values$CISpoints2 <- NULL
  values$hhdata2 <- NULL
  values$SPHSplotData <- NULL
  values$SPNGplotData <- NULL
  values$season <- NULL
  values$PPTZplotData <- NULL 
  values$PPplotData <- NULL 
  # output$tokenKey <- renderUI({
  #     textInput("tokenkeyid", label="Please enter the password:")
  # })
  
  output$tokenKey <- renderUI({
    if(!is.null(values$userName)){
      if(values$userName %in% userslist){
        passwordInput("tokenkeyid", "Password:")
      }
    }
  })
  
  
  output$username <- renderUI({
    textInput("userName", label="User Name (first letter in Capital):")
  })
  
  # output$season <- renderUI({
  #   textInput("season", label="Season:")
  # })
  
  observeEvent(input$tokenkeyid, {
    values$tokenid <- input$tokenkeyid
  })
  
  observeEvent(input$userName, {
    values$userName <- input$userName
  })
  
  
  output$seasonfr <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Seasonfr", label="Season:", choices = c("1", "2", "3"),selected="1")
      }
    }
  })
  
  output$seasonic <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Seasonic", label="Season:", choices = c("1", "2", "3"),selected="1")
      }
    }
  })
  
  output$seasonpp <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Seasonpp", label="Season:", choices = c("1", "2", "3"),selected="1")
      }
    }
  })
  
  output$seasonsphs_tz <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Seasonsphs_tz", label="Season:", choices = c("1", "2", "3"),selected="1")
      }
    }
  })
  
  output$seasonsphs_ng <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Seasonsphs_ng", label="Season:", choices = c("3"),selected="3")
      }
    }
  })
  
  output$seasonCIS <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("SeasonCIS", label="Season:", choices = c("1", "2"),selected="1")
      }
    }
  })
  
  output$seasonpptz <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Seasonpptz", label="Season:", choices = c("1", "2", "3"),selected="1")
      }
    }
  })
  
  output$country <- renderUI({
    usid <- NULL
    if(!is.null(values$tokenid) & !is.null(values$userName)){
      usid <- droplevels(userspwd[userspwd$Name == values$userName, ])
      if(usid$pwd == values$tokenid & !is.null(usid)){
        selectInput("Country", label="Country:", choices = c("Nigeria", "Tanzania"), selected="Nigeria")
      }
    }
  }) 
  
  
  output$countryfr <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Countryfr", label="Country:", choices = c("Nigeria", "Tanzania"), selected="Nigeria")
      }
    }
  }) 
  
  
  output$countryic <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Countryic", label="Country:", choices = c("Nigeria", "Tanzania"), selected="Nigeria")
      }
    }
  })
  
  
  output$countrypp <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Countrypp", label="Country:", choices = c("Nigeria"), selected="Nigeria")
      }
    }
  })
  
  output$countrypptz <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Countrypptz", label="Country:", choices = c("Tanzania"), selected="Tanzania")
      }
    }
  })
  
  output$countrysphs_tz <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Countrysphs_tz", label="Country:", choices = c("Nigeria", "Tanzania"), selected="Tanzania")
      }
    }
  })
  
  output$countrysphs_ng <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Countrysphs_ng", label="Country:", choices = c("Nigeria"), selected="Nigeria")
      }
    }
  })

  output$countryCIS <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("CountryCIS", label="Country:", choices = c("Nigeria", "Tanzania"), selected="Tanzania")
      }
    }
  })
  
  
  output$countrydi <- renderUI({
    if(!is.null(values$tokenid) & !is.null(input$userName)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        selectInput("Countrydi", label="Country:", choices = c("Nigeria", "Tanzania"), selected="Nigeria")
      }
    }
  })
  
  
  output$region <- renderUI({
    if(!is.null(input$Country) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        ds<-droplevels(dsEAHH[dsEAHH$Country==input$Country, ])
        regs <- as.character(unique(ds$region_state))
        if(input$Country == "Nigeria"){
          selectInput("Regions", label="State:", choices = c("All", as.character(regs)), selected="OY")
        }else{
          selectInput("Regions", label="Region:", choices = c("All",  as.character(regs)), selected="MW")
        }
      }
    }
  })
  
  
  output$regionfr <- renderUI({
    if(!is.null(input$Countryfr) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHfrr <- dsEAHH[dsEAHH$EAID %in% FR_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        ds<-droplevels(dsEAHHfrr[dsEAHHfrr$Country==input$Countryfr & dsEAHHfrr$useCase == "FR", ])
        regs <- as.character(unique(ds$region_state))
        
        if(input$Countryfr == "Nigeria"){
          selectInput("Regionsfr", label="State:", choices = c("All", as.character(regs)), selected="OY")
        }else{
          selectInput("Regionsfr", label="Region:", choices = c("All",  as.character(regs)), selected="MW")
        }
      }
    }
  })
  
  
  output$regionic <- renderUI({
    if(!is.null(input$Countryic) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHicr <- dsEAHH[dsEAHH$EAID %in% IC_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        ds<-droplevels(dsEAHHicr[dsEAHHicr$Country==input$Countryic & dsEAHHicr$useCase == "IC", ])
        regs <- as.character(unique(ds$region_state))
        if(input$Countryic == "Nigeria"){
          selectInput("Regionsic", label="State:", choices = c("All", as.character(regs)), selected="OY")
        }else{
          selectInput("Regionsic", label="Region:", choices = c("All",  as.character(regs)), selected="MW")
        }
      }
    }
  })
  
  
  output$regionpp <- renderUI({
    if(!is.null(input$Countrypp) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHppr <- dsEAHH[dsEAHH$EAID %in% PP_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        ds<-droplevels(dsEAHHppr[dsEAHHppr$Country==input$Countrypp & dsEAHHppr$useCase == "PP", ])
        regs <- as.character(unique(ds$region_state))
        if(input$Countrypp == "Nigeria"){
          selectInput("Regionspp", label="State:", choices = c("All", as.character(regs)), selected="OY")
        }else{
          selectInput("Regionspp", label="Region:", choices = c("All",  as.character(regs)), selected="MW")
        }
      }
    }
  })
  
  
  output$regionpptz <- renderUI({
    if(!is.null(input$Countrypptz) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHptz <- dsEAHH[dsEAHH$EAID %in% PPTZ_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        ds<-droplevels(dsEAHHptz[dsEAHHptz$Country==input$Countrypptz & dsEAHHptz$useCase == "PP", ])
        regs <- as.character(unique(ds$region_state))
        if(input$Countrypptz == "Nigeria"){
          selectInput("Regionspptz", label="State:", choices = c("All", as.character(regs)), selected="OY")
        }else{
          selectInput("Regionspptz", label="Region:", choices = c("All",  as.character(regs)), selected="MW")
        }
      }
    }
  })
  
  
  output$regionsphs_ng <- renderUI({
    if(!is.null(input$Countrysphs_ng) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHspr <- droplevels(dsEAHH[dsEAHH$EAID %in% SPNG_dstchecked$EAID, ])
      if(usid$pwd == values$tokenid){
        ds<-droplevels(dsEAHHspr[dsEAHHspr$Country==input$Countrysphs_ng & dsEAHHspr$useCase == "SP", ])
        regs <- as.character(unique(ds$region_state))
        if(input$Countrysphs_ng == "Nigeria"){
          selectInput("Regionssphs_ng", label="State:", choices = c("All", as.character(regs)), selected="OY")
        }else{
          selectInput("Regionssphs_ng", label="Region:", choices = c("All",  as.character(regs)), selected="MW")
        }
      }
    }
  })
  
  

  output$regionsphs_tz <- renderUI({
    if(!is.null(input$Countrysphs_tz) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHspt <- droplevels(dsEAHH[dsEAHH$EAID %in% SP_dstchecked$EAID, ])
      if(usid$pwd == values$tokenid){
        ds<-droplevels(dsEAHHspt[dsEAHHspt$Country==input$Countrysphs_tz & dsEAHHspt$useCase == "SP", ])
        regs <- as.character(unique(ds$region_state))
        if(input$Countrysphs_tz == "Nigeria"){
          selectInput("Regionssphs_tz", label="State:", choices = c("All", as.character(regs)), selected="OY")
        }else{
          selectInput("Regionssphs_tz", label="Region:", choices = c("All",  as.character(regs)), selected="MW")
        }
      }
    }
  })
  
  
  output$regionCIS <- renderUI({
    if(!is.null(input$CountryCIS) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHspr <- droplevels(dsEAHH[dsEAHH$EAID %in% CIS_dstchecked$EAID, ])
      if(usid$pwd == values$tokenid){
        ds<-droplevels(dsEAHHspr[dsEAHHspr$Country==input$CountryCIS & dsEAHHspr$useCase == "IC", ])
        regs <- as.character(unique(ds$region_state))
        if(input$CountryCIS == "Nigeria"){
          selectInput("RegionsCIS", label="State:", choices = c("All", as.character(regs)), selected="OY")
        }else{
          selectInput("RegionsCIS", label="Region:", choices = c("All",  as.character(regs)), selected="MW")
        }
      }
    }
  })
  
  
  output$partner <- renderUI({
    if(!is.null(input$Country) & !is.null(input$Regions) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        if(input$Regions != "All"){
          eadata <- droplevels(dsEAHH[dsEAHH$Country == input$Country & dsEAHH$region_state == input$Regions, ])
        }else{
          eadata <- droplevels(dsEAHH[dsEAHH$Country == input$Country, ])
        }
        pats <- as.character(unique(eadata$EA_Partner))
        selectInput("Partner", label="Partner:", choices = c("All", as.character(pats)), selected = "Notore")
      }
    }
  })
  
  
  output$partnerfr <- renderUI({
    if(!is.null(input$Countryfr) & !is.null(input$Regionsfr) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHfrp <- dsEAHH[dsEAHH$EAID %in% FR_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionsfr != "All"){
          eadata <- droplevels(dsEAHHfrp[dsEAHHfrp$Country == input$Countryfr & dsEAHHfrp$region_state == input$Regionsfr & dsEAHHfrp$useCase == "FR", ])
        }else{
          eadata <- droplevels(dsEAHHfrp[dsEAHHfrp$Country == input$Countryfr & dsEAHHfrp$useCase == "FR", ])
        }
        pats <- as.character(unique(eadata$EA_Partner))
        selectInput("Partnerfr", label="Partner:", choices = c("All", as.character(pats)), selected = "Notore")
      }
    }
  })
  
  
  output$partneric <- renderUI({
    if(!is.null(input$Countryic) & !is.null(input$Regionsic) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHicp <- dsEAHH[dsEAHH$EAID %in% IC_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionsic != "All"){
          eadata <- droplevels(dsEAHHicp[dsEAHHicp$Country == input$Countryic & dsEAHHicp$region_state == input$Regionsic & dsEAHHicp$useCase == "IC", ])
        }else{
          eadata <- droplevels(dsEAHHicp[dsEAHHicp$Country == input$Countryic & dsEAHHicp$useCase == "IC", ])
        }
        pats <- as.character(unique(eadata$EA_Partner))
        selectInput("Partneric", label="Partner:", choices = c("All", as.character(pats)), selected = "Notore")
      }
    }
  })
  
  
  output$partnerpp <- renderUI({
    if(!is.null(input$Countrypp) & !is.null(input$Regionspp) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHppp <- dsEAHH[dsEAHH$EAID %in% PP_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionspp != "All"){
          eadata <- droplevels(dsEAHHppp[dsEAHHppp$Country == input$Countrypp & dsEAHHppp$region_state == input$Regionspp & dsEAHHppp$useCase == "PP", ])
        }else{
          eadata <- droplevels(dsEAHHppp[dsEAHHppp$Country == input$Countrypp & dsEAHHppp$useCase == "PP", ])
        }
        pats <- as.character(unique(eadata$EA_Partner))
        selectInput("Partnerpp", label="Partner:", choices = c("All", as.character(pats)), selected = "Notore")
      }
    }
  })
  
#CHANGE FR TO PP WITH TZ DATA
  output$partnerpptz <- renderUI({
    if(!is.null(input$Countrypptz) & !is.null(input$Regionspptz) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHpptz <- dsEAHH[dsEAHH$EAID %in% PPTZ_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionspptz != "All"){
          eadata <- droplevels(dsEAHHpptz[dsEAHHpptz$Country == input$Countrypptz & dsEAHHpptz$region_state == input$Regionspptz & dsEAHHpptz$useCase == "PP", ])
        }else{
          eadata <- droplevels(dsEAHHpptz[dsEAHHpptz$Country == input$Countrypptz & dsEAHHpptz$useCase == "PP", ])
        }
        pats <- as.character(unique(eadata$EA_Partner))
        selectInput("Partnerpptz", label="Partner:", choices = c("All", as.character(pats)), selected = "Notore")
      }
    }
  })
  
  output$partnersphs_tz <- renderUI({
    if(!is.null(input$Countrysphs_tz) & !is.null(input$Regionssphs_tz) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHspt <- dsEAHH[dsEAHH$EAID %in% SP_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionssphs_tz != "All"){
          eadata <- droplevels(dsEAHHspt[dsEAHHspt$Country == input$Countrysphs_tz & 
                                           dsEAHHspt$region_state == input$Regionssphs_tz & dsEAHHspt$useCase == "SP", ])
        }else{
          eadata <- droplevels(dsEAHHspt[dsEAHHspt$Country == input$Countrysphs_tz & dsEAHHspt$useCase == "SP", ])
        }
        pats <- as.character(unique(eadata$EA_Partner))
        selectInput("Partnersphs_tz", label="Partner:", choices = c("All", as.character(pats)), selected = "Notore")
      }
    }
  })
  
  output$partnersphs_ng <- renderUI({
    if(!is.null(input$Countrysphs_ng) & !is.null(input$Regionssphs_ng) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHspp <- dsEAHH[dsEAHH$EAID %in% SPNG_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionssphs_ng != "All"){
          eadata <- droplevels(dsEAHHspp[dsEAHHspp$Country == input$Countrysphs_ng & dsEAHHspp$region_state == input$Regionssphs_ng & dsEAHHspp$useCase == "SP", ])
        }else{
          eadata <- droplevels(dsEAHHspp[dsEAHHspp$Country == input$Countrysphs_ng & dsEAHHspp$useCase == "SP", ])
        }
        pats <- as.character(unique(eadata$EA_Partner))
        selectInput("Partnersphs_ng", label="Partner:", choices = c("All", as.character(pats)), selected = "Psaltry")
      }
    }
  })
  
  output$partnerCIS <- renderUI({
    if(!is.null(input$CountryCIS) & !is.null(input$RegionsCIS) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHcis <- dsEAHH[dsEAHH$EAID %in% CIS_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$RegionsCIS != "All"){
          eadata <- droplevels(dsEAHHcis[dsEAHHcis$Country == input$CountryCIS & dsEAHHcis$region_state == input$RegionsCIS & 
                                           dsEAHHcis$useCase == "IC", ])
        }else{
          eadata <- droplevels(dsEAHHcis[dsEAHHcis$Country == input$CountryCIS & dsEAHHcis$useCase == "IC", ])
        }
        pats <- as.character(unique(eadata$EA_Partner))
        selectInput("PartnerCIS", label="Partner:", choices = c("All", as.character(pats)), selected = "Notore")
      }
    }
  })
  
  output$partnerdi <- renderUI({
    if(!is.null(input$Countrydi) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
        pats <- as.character(unique(eadata$EA_Partner))
        selectInput("Partnerdi", label="Partner:", choices = c("All", as.character(pats)), selected = "All")
      }
    }
  })
  
  output$eaid <- renderUI({
    if(!is.null(input$Country) & !is.null(input$Partner) & !is.null(input$Regions) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        if(input$Regions != "All"){
          eadata <- droplevels(dsEAHH[dsEAHH$Country == input$Country & dsEAHH$region_state == input$Regions, ])
        }else{
          eadata <- droplevels(dsEAHH[dsEAHH$Country == input$Country, ])
        }
        if(input$Partner != "All"){
          eadata <- droplevels(eadata[eadata$EA_Partner == input$Partner,])
        }
        eaids <- as.character(unique(eadata$EAID))
        # selectInput("eaids", label="EA-id:", choices = c("All", eaids), selected="All")
        selectInput("eaids", label="EAID:", choices = c("All",eaids), selected = "All")
      }
    }
  })
  
  
  output$eaidfr <- renderUI({
    if(!is.null(input$Countryfr) & !is.null(input$Partnerfr) & !is.null(input$Regionsfr)  & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHfid <- dsEAHH[dsEAHH$EAID %in% FR_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionsfr != "All"){
          eadata <- droplevels(dsEAHHfid[dsEAHHfid$Country == input$Countryfr & dsEAHHfid$region_state == input$Regionsfr & dsEAHHfid$useCase == "FR", ])
        }else{
          eadata <- droplevels(dsEAHHfid[dsEAHHfid$Country == input$Countryfr & dsEAHHfid$useCase == "FR", ])
        }
        if(input$Partnerfr != "All"){
          eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerfr,])
        }
        eaids <- as.character(unique(eadata$EAID))
        selectInput("eaidsfr", label="EAID:", choices = c("All",eaids), selected = "All")
      }
    }
  })
  
  #####################IC###################
  output$eaidic <- renderUI({
    if(!is.null(input$Countryic) & !is.null(input$Partneric) & !is.null(input$Regionsic) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHicid <- dsEAHH[dsEAHH$EAID %in% IC_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionsic != "All"){
          eadata <- droplevels(dsEAHHicid[dsEAHHicid$Country == input$Countryic & dsEAHHicid$region_state == input$Regionsic & dsEAHHicid$useCase == "IC", ])
        }else{
          eadata <- droplevels(dsEAHHicid[dsEAHHicid$Country == input$Countryic & dsEAHHicid$useCase == "IC", ])
        }
        if(input$Partneric != "All"){
          eadata <- droplevels(eadata[eadata$EA_Partner == input$Partneric,])
        }
        eaids <- as.character(unique(eadata$EAID))
        selectInput("eaidsic", label="EAID:", choices = c("All",eaids), selected = "All")
      }
    }
  })
  
  
  
  output$eaidpp <- renderUI({
    if(!is.null(input$Countrypp) & !is.null(input$Partnerpp) & !is.null(input$Regionspp) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHppid <- dsEAHH[dsEAHH$EAID %in% PP_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionspp != "All"){
          eadata <- droplevels(dsEAHHppid[dsEAHHppid$Country == input$Countrypp & dsEAHHppid$region_state == input$Regionspp & dsEAHHppid$useCase == "PP", ])
        }else{
          eadata <- droplevels(dsEAHHppid[dsEAHHppid$Country == input$Countrypp & dsEAHHppid$useCase == "PP", ])
        }
        if(input$Partnerpp != "All"){
          eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerpp,])
        }
        eaids <- as.character(unique(eadata$EAID))
        selectInput("eaidspp", label="EAID:", choices = c("All",eaids), selected = "All")
      }
    }
  })
  
  
  output$eaidpptz <- renderUI({
    if(!is.null(input$Countrypptz) & !is.null(input$Partnerpptz) & !is.null(input$Regionspptz) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHpptzid <- dsEAHH[dsEAHH$EAID %in% PPTZ_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionspptz != "All"){
          eadata <- droplevels(dsEAHHpptzid[dsEAHHpptzid$Country == input$Countrypptz & dsEAHHpptzid$region_state == input$Regionspptz & dsEAHHpptzid$useCase == "PP", ])
        }else{
          eadata <- droplevels(dsEAHHpptzid[dsEAHHpptzid$Country == input$Countrypptz & dsEAHHpptzid$useCase == "PP", ])
        }
        if(input$Partnerpptz != "All"){
          eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerpptz,])
        }
        eaids <- as.character(unique(eadata$EAID))
        selectInput("eaidspptz", label="EAID:", choices = c("All",eaids), selected = "All")
      }
    }
  })
  
  
  
  output$eaidsphs_tz <- renderUI({
    if(!is.null(input$Countrysphs_tz) & !is.null(input$Partnersphs_tz) & !is.null(input$Regionssphs_tz) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHspid <- dsEAHH[dsEAHH$EAID %in% SP_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionssphs_tz != "All"){
          eadata <- droplevels(dsEAHHspid[dsEAHHspid$Country == input$Countrysphs_tz & dsEAHHspid$region_state == input$Regionssphs_tz & dsEAHHspid$useCase == "SP", ])
        }else{
          eadata <- droplevels(dsEAHHspid[dsEAHHspid$Country == input$Countrysphs_tz & dsEAHHspid$useCase == "SP", ])
        }
        if(input$Partnersphs_tz != "All"){
          eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnersphs_tz,])
        }
        eaids <- as.character(unique(eadata$EAID))
        selectInput("eaidssphs_tz", label="EAID:", choices = c("All",eaids), selected = "All")
      }
    }
  })
  
  
  output$eaidsphs_ng <- renderUI({
    if(!is.null(input$Countrysphs_ng) & !is.null(input$Partnersphs_ng) & !is.null(input$Regionssphs_ng) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHspnid <- dsEAHH[dsEAHH$EAID %in% SPNG_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$Regionssphs_ng != "All"){
          eadata <- droplevels(dsEAHHspnid[dsEAHHspnid$Country == input$Countrysphs_ng & dsEAHHspnid$region_state == input$Regionssphs_ng & dsEAHHspnid$useCase == "SP", ])
        }else{
          eadata <- droplevels(dsEAHHspnid[dsEAHHspnid$Country == input$Countrysphs_ng & dsEAHHspnid$useCase == "SP", ])
        }
        if(input$Partnersphs_ng != "All"){
          eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnersphs_ng,])
        }
        eaids <- as.character(unique(eadata$EAID))
        selectInput("eaidssphs_ng", label="EAID:", choices = c("All",eaids), selected = "All")
      }
    }
  })
 
  
  output$eaidCIS <- renderUI({
    if(!is.null(input$CountryCIS) & !is.null(input$PartnerCIS) & !is.null(input$RegionsCIS) & !is.null(values$tokenid)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      dsEAHHspid <- dsEAHH[dsEAHH$EAID %in% CIS_dstchecked$EAID, ]
      if(usid$pwd == values$tokenid){
        if(input$RegionsCIS != "All"){
          eadata <- droplevels(dsEAHHspid[dsEAHHspid$Country == input$CountryCIS & dsEAHHspid$region_state == input$RegionsCIS & dsEAHHspid$useCase == "IC", ])
        }else{
          eadata <- droplevels(dsEAHHspid[dsEAHHspid$Country == input$CountryCIS & dsEAHHspid$useCase == "IC", ])
        }
        if(input$PartnerCIS != "All"){
          eadata <- droplevels(eadata[eadata$EA_Partner == input$PartnerCIS,])
        }
        eaids <- as.character(unique(eadata$EAID))
        selectInput("eaidsCIS", label="EAID:", choices = c("All",eaids), selected = "All")
      }
    }
  })
  
  output$dataissue <- renderUI({
    usid <- NULL
    if(!is.null(values$tokenid) & !is.null(values$userName) & !is.null(input$Countrydi) & !is.null(input$Partnerdi)){
      usid <- droplevels(userspwd[userspwd$Name == values$userName, ])
      if(usid$pwd == values$tokenid & !is.null(usid)){
        didata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
        if(input$Partnerdi != "All"){
          didata <- droplevels(didata[didata$EA_Partner == input$Partnerdi,])
        }
        selectInput("di", label="Data issues:", choices = c("All", unique(didata$data_issue)), selected="All")
      }
    }
  }) 
  
  output$eaiddi <- renderUI({
    if(!is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(values$tokenid) & !is.null(input$di)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
        if(input$Partnerdi != "All"){
          eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
        }
        if(input$di != "All"){
          eadata <- droplevels(eadata[eadata$data_issue == input$di,])
        }
        eaids <- as.character(unique(eadata$EAID))
        selectInput("eaidsdi", label="EAID:", choices = c("All",eaids), selected = "All")
      }
    }
  })
  
  
  output$hhiddi <- renderUI({
    if(!is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(values$tokenid) & !is.null(input$eaidsdi) & !is.null(input$di)){
      usid <- droplevels(userspwd[userspwd$Name == input$userName, ])
      if(usid$pwd == values$tokenid){
        eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
        if(input$Partnerdi != "All"){
          eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
        }
        if(input$di != "All"){
          eadata <- droplevels(eadata[eadata$data_issue == input$di,])
        }
        if(input$eaidsdi != "All"){
          eadata <- droplevels(eadata[eadata$EAID == input$eaidsdi,])
        }
        hhids <- as.character(unique(eadata$HHID))
        selectInput("hhidsdi", label="HHID:", choices = c("All",hhids), selected = "All")
      }
    }
  })
  
  
  ##################################################################### 
  output$EASummary <- renderDataTable({
    if(!is.null(input$Country) & !is.null(input$Partner) & !is.null(input$Regions) & !is.null(input$eaids)){
      if(input$Country == "Nigeria"){
        eadata <- droplevels(dsEAHH[dsEAHH$Country == "Nigeria", ])
        if(input$Regions != "All"){
          eadata <- droplevels(eadata[eadata$region_state == input$Regions,])
        }
        colnames(eadata) <- c( "EAID","EA_Name","EA_PhoneNr", "EA_Partner","HHID","HH_Name", "HH_PhoneNr","useCase","Latitude","Longitude", "Country", "State")
      }else{
        eadata <- droplevels(dsEAHH[dsEAHH$Country == "Tanzania", ])
        if(input$Regions != "All"){
          eadata <- droplevels(eadata[eadata$region_state == input$Regions,])
        }
        colnames(eadata) <- c( "EAID","EA_Name","EA_PhoneNr", "EA_Partner","HHID","HH_Name", "HH_PhoneNr","useCase","Latitude","Longitude", "Country", "Region")
      }
      
      
      if(input$Partner != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partner,])
      }
      
      if(input$eaids == "All" ) {
        eaidsdata <- eadata
      }else{
        eaidsdata <- droplevels(eadata[eadata$EAID == input$eaids,])
      }
      
      eaidsdata$Latitude <- round(eaidsdata$Latitude, digits=3)
      eaidsdata$Longitude <- round(eaidsdata$Longitude, digits=3)
      values$eaidsdata <- eaidsdata
      
      return(eaidsdata)
      
    }
    
  }, options =list(pagingType = "simple"))
  
  
  output$downloadr <- downloadHandler(
    filename = function(){"Register.csv"},
    content = function(file){ 
      if(!is.null(values$eaidsdata)){
        write.csv(values$eaidsdata, file, row.names = FALSE)
      }
    }
  )
  
  
  
  output$mapEA <- renderLeaflet({
    if(!is.null(input$eaids) & !is.null(values$eaidsdata)){
      if(input$eaids != "All"){
        eadata <- values$eaidsdata
        eaidsdata <- droplevels(eadata[eadata$EAID == input$eaids,])
        varpoints <- droplevels(eaidsdata[, c("Longitude", "Latitude", "HHID")])
        colnames(varpoints) <- c("Long", "Lat", "Name")
        varpoints <- varpoints[complete.cases(varpoints), ]
        varpoints <- varpoints[, c("Name", "Lat", "Long")]
        
        data =varpoints[1,]
        minLong = data$Long - 0.05
        maxLong = data$Long + 0.05
        minLat = data$Lat - 0.05
        maxLat = data$Lat + 0.05
        library(leaflet)
        m <- leaflet(data =varpoints, options = leafletOptions(minZoom = 6, maxZoom = 14)) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addCircleMarkers(~Long, ~Lat, color = "red", radius= 7, fillOpacity = 0.5 ) %>%
          # addCircleMarkers(~Long, ~Lat, color = "red", radius= 7, fillOpacity = 0.5, popup = ~htmlEscape(Name) ) %>%
          addScaleBar()
        return(m)
        
      }
    }
  })
  
  output$EAplot <- renderPlot({
    if(!is.null(input$eaidsfr)){
      if(input$eaidsfr != "All"){
        values$eaidsfr <- input$eaidsfr
        h <- input$eaidsfr
        
        EALs.h <- FR_dstchecked[FR_dstchecked$EAID == h, ] # extract by EA_Name
        EALs.h <- droplevels(EALs.h[!is.na(EALs.h$HHID), ])
        if(!all(is.na(EALs.h$DatesBnPlantingEvent))){
          
          cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done late"="yellowgreen", 
                    "Overdue"="violetred2", "Not done"="orangered2", "To be done"="grey", "Due soon"="purple")
          
          Events <- c("DST run", "planting", "gapping", "fertilizer1", "fertilizer2", "weeding1", "weeding2", "weeding3", "harvest")
          values$EAplotData <- EALs.h
          EAheatplot <- ggplot(EALs.h, aes(x=Events, y=HHID, fill= status)) +
            scale_x_discrete(limits = Events) +
            geom_tile(color="white", size=0.1) + 
            theme(legend.text=element_text(size=10)) +
            ggtitle(paste("FR validation exercise schedule for EAID = ",h, unique(EALs.h$EA_Name),sep = " " )) +
            geom_text(aes(label=datetbd2))+
            xlab(" ")+
            theme(axis.title.y=element_blank()) +
            theme(plot.title = element_text(size = 20)) +
            scale_fill_manual("Event timing", values=cols) +
            theme(plot.title=element_text(hjust=0), axis.ticks=element_blank(), axis.text.y=element_text(size=14),
                  axis.title=element_text(size=20),axis.text.x = element_text(angle = 25, size=14, vjust = 1, hjust=1) ) +
            theme(legend.position="right") +
            coord_fixed(ratio = 0.25)
          print(EAheatplot)
          
        }else{
          NULL
        }
      }
    }
  })
  
  output$downloadPlotFR <- downloadHandler(
    filename = function(){"FRplot.pdf"},
    content = function(file){
      if(!is.null(values$EAplotData) & !is.null(values$FRpoints1) & !is.null(values$FRpointsdt2)){
        hhdata2 <- values$EAplotData
        FRpoints1 <- values$FRpoints1
        FRpointsdt2 <- values$FRpointsdt2
        plotable(hhdata2, FRpoints1, FRpointsdt2)
        file.copy("FRplot.pdf", file, overwrite = TRUE)
      }
    }
  )
  
  
  output$FRwkheatplot <- renderPlot({
    if(!is.null(input$eaidsfr)& !is.null(input$Seasonfr)){
      if(input$eaidsfr != "All"){
        h <- input$eaidsfr
        EALs.h <- wksdtplot[wksdtplot$EAID == h, ] # extract by EA_Name
        
        head( EALs.h)
        EALs.h$eventCode <- ifelse( EALs.h$events == "planting", "P",
                                    ifelse( EALs.h$events == "gapping", "G",
                                            ifelse( EALs.h$events == "fertilizer1","F1",
                                                    ifelse( EALs.h$events == "fertilizer2", "F2",
                                                            ifelse( EALs.h$events == "weeding1", "W1",
                                                                    ifelse(EALs.h$events == "weeding2", "W2",
                                                                           ifelse(EALs.h$events == "weeding3", "W3", "H")))))))
        dayToday <- lubridate::week(ymd(Sys.Date()))
        mindate <- min( EALs.h[ EALs.h$events == "planting", ]$weeks, na.rm = TRUE)
        statscolor <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done late"="yellowgreen",
                        "Overdue"="violetred2", "Not done"="orangered2", "To be done"="grey", "Mixed events"="cyan", "Due soon" = "purple")
        
        EALs.h <- EALs.h[rowSums(is.na(EALs.h)) != ncol(EALs.h), ]
        #EALs.h <- droplevels(EALs.h[!is.na(EALs.h$day), ])
        EALs.h$hhweeks <- paste(EALs.h$HHID, EALs.h$weeks, sep="_")
        
        EAdata <- NULL
        for(hw in unique(EALs.h$hhweeks)){
          hd <- EALs.h[EALs.h$hhweeks == hw, ]
          if(nrow(hd) >1){
            hd$eventCode2 <- paste(unique(hd$eventCode), collapse="/")
            if(length(unique(hd$status)) > 1){
              hd$status <- 'Mixed events'
            }
            
            hd <- head(hd,1)
            
          }else{
            hd$eventCode2 <- hd$eventCode
          }
          
          EAdata <- rbind(EAdata, hd)
        }
        
        label=c("FR validation calendar for EA \n P=planting, G=gapping, F1=fertilizer 1, F2=fertilizer 2, W1=weeding1, W2=weeding2, W3=weeding3, H=Harvest")
        monthorder <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        
        EAdata$month <- factor(EAdata$month, levels = monthorder)
        #EAdata$yeartbd <- lubridate::year(ymd(EAdata$datetbd2))
        EAdata$yeartbd <- as.yearmon(EAdata$datetbd2)
        
        #EAdata <- droplevels(EAdata[!is.na(EAdata$month), ])
        
        EAdata$weeks <- as.factor(EAdata$weeks)
        
        if(nrow(subset(EAdata, season == input$Seasonfr))>0){
          plotdatafr <- subset(EAdata, season == input$Seasonfr)
        }else{
          plotdatafr <-  EAdata
        }
        
        FRcalendar <- ggplot(plotdatafr, aes(x=weeks, y=HHID, fill = factor(status))) +
          geom_tile(size=0.1) +
          scale_fill_manual(values=statscolor) +
          ggtitle(label) +
          geom_text(aes(label = eventCode2), size=3)+
          
          #facet_grid(.~ yeartbd, space = 'free', scales = 'free', switch = 'x') +
          #geom_vline(data=toBeDue, aes(xintercept = weeks)) + 
          labs(x = "") + labs(y = "") +
          guides(fill=guide_legend("Timing of activities:"))+
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5, size=16), axis.ticks=element_blank(), axis.text=element_text(size=12),
                panel.grid.minor = element_blank(), axis.title = element_text(size=14),strip.text.x = element_text(size=10),
                legend.key.size = unit(2.5,"line"), legend.position="right")
        FRcalendar
      }else{
        NULL
      }
    }
  })
  
  
  output$FRtable <- DT::renderDataTable({
    if(!is.null(input$eaidsfr)){
      if(input$eaidsfr != "All"){
        h <- input$eaidsfr
        p<- FRpoints[FRpoints$EAID == h, ]
        p
        values$FRpoints1 <- p
      }else{
        NULL
      }
    }
    
  })
  
  
  output$tpointout <- renderUI({
    if(!is.null(input$eaidsfr)){
      if(input$eaidsfr != "All"){
        h <- input$eaidsfr
        p<- FRpoints[FRpoints$EAID == h, ]
      }
    } 
    
    x = colSums(p[,12:13], na.rm = FALSE)
    totalpoints = x[2]
    paste("Total EA points earned:" , totalpoints, 
          "\n *For each activity, points are earned only once even if event done on different dates*")
  })
  
  output$FRecom <- DT::renderDataTable({
    if(!is.null(input$eaidsfr)){
      if(input$eaidsfr != "All"){
        h <- input$eaidsfr
        p<- dsfrecom[dsfrecom$EAID == h, ]
        p
      }else{
        p <- dsfrecom
        p
      }
      values$recom <- p
    }
    
  })	
  
  output$downloadrecomFR <- downloadHandler(
    filename = function(){"Recommendations.csv"},
    content = function(file){ 
      if(!is.null(values$recom)){
        write.csv(values$recom , file, row.names = FALSE)
      }
    }
  )
  
  
  output$FRptsummary <- DT::renderDataTable({
    
    if(!is.null(input$eaidsfr)){
      if(input$eaidsfr != "All"){
        h <- input$eaidsfr
        p<- FRpointsdt2[FRpointsdt2$EAID == h, ]
        options = list(
          "pageLength" = 20)
        p
        values$FRpoints2 <- p
      }else{
        NULL
      }
    }
    
  })
  
  ############# IC ##################
  output$EAICplot <- renderPlot({
    if(!is.null(input$eaidsic)){
      if(input$eaidsic != "All"){
        h <- input$eaidsic
        
        
        EAIC.h <- IC_dstchecked[IC_dstchecked$EAID == h, ] # extract by EA_Name
        EAIC.h <- droplevels(EAIC.h[!is.na(EAIC.h$HHID), ])
        if(!all(is.na(EAIC.h$DatesBnPlantingEvent))){
          
          
          cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done later"="yellowgreen",
                    "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Due soon"="purple")
          
          Events <- c("DST run", "planting", "reseeding", "thinning", "gapping", "fertilizer0", "weeding1", "weeding2", "weeding3", "maizeharvest", "cassharvest")
          values$ICplotData <- EAIC.h
          EAICheatplot <- ggplot(EAIC.h, aes(x=Events, y=HHID, fill= status)) +
            scale_x_discrete(limits = Events) +
            geom_tile(color="white", size=0.1) +
            theme(legend.text=element_text(size=10)) +
            ggtitle(paste("IC validation exercise schedule for EAID = ",h, unique(EAIC.h$EA_Name),sep = " " )) +
            geom_text(aes(label=datetbd2))+
            theme(axis.title.y=element_blank()) +
            theme(plot.title = element_text(size = 20)) +
            scale_fill_manual("Event timing", values=cols) +
            theme(plot.title=element_text(hjust=0), axis.ticks=element_blank(), axis.text.y=element_text(size=14),
                  axis.title=element_text(size=20),axis.text.x = element_text(angle = 25, size=14, vjust = 1, hjust=1) ) +
            theme(legend.position="right") +
            coord_fixed(ratio = 0.25)
          EAICheatplot
        }else{
          NULL
        }
      }
    }
  })
  
  
  
  output$ICwkheatplot <- renderPlot({
    if(!is.null(input$eaidsic) & !is.null(input$Seasonic)){
      if(input$eaidsic != "All"){
        h <- input$eaidsic
        
        EALs.h <- icwksdata[icwksdata$EAID == h, ] # extract by EAID
        
        head( EALs.h)
        EALs.h$eventCode <- ifelse( EALs.h$events == "planting", "P",
                                    ifelse( EALs.h$events == "gapping", "G",
                                            ifelse( EALs.h$events == "fertilizer0","F0",
                                                    ifelse( EALs.h$events == "weeding1", "W1",
                                                            ifelse( EALs.h$events == "weeding2", "W2",
                                                                    ifelse(EALs.h$events == "weeding2", "W2",
                                                                           ifelse(EALs.h$events == "weeding3", "W3",
                                                                                  ifelse(EALs.h$events == "thinning", "Th",
                                                                                         ifelse(EALs.h$events == "reseeding", "R",
                                                                                                ifelse(EALs.h$events == "maizeharvest", "Hm",
                                                                                                       ifelse(EALs.h$events == "cassharvest", "Hc","NA")))))))))))
        dayToday <- lubridate::week(ymd(Sys.Date()))
        mindate <- min( EALs.h[ EALs.h$events == "planting", ]$weeks, na.rm = TRUE)
        statscolor <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done later"="yellowgreen",
                        "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Mixed events"="cyan", "Due soon" = 'purple')
        
        EALs.h <- EALs.h[rowSums(is.na(EALs.h)) != ncol(EALs.h), ]
        EALs.h <- EALs.h[!is.na(EALs.h$status2), ]
        EALs.h <- EALs.h[EALs.h$weeks != -999, ]
        
        # EALs.h <- droplevels(EALs.h[!is.na(EALs.h$day), ])
        EALs.h$hhweeks <- paste(EALs.h$HHID, EALs.h$weeks, EALs.h$dueyear, sep="_")
        
        EAdata <- NULL
        for(hw in unique(EALs.h$hhweeks)){
          hd <- EALs.h[EALs.h$hhweeks == hw, ]
          if(nrow(hd) >1){
            hd$eventCode2 <- paste(unique(hd$eventCode), collapse="/")
            if(length(unique(hd$status)) > 1){
              hd$status <- 'Mixed events'
            }
            
            hd <- head(hd,1)
            
          }else{
            hd$eventCode2 <- hd$eventCode
          }
          
          EAdata <- rbind(EAdata, hd)
        }
        
        label=c("IC validation calendar for EA \n P=planting, R=Reseeding, Thinning=T, G=Gapping, F1=fertilizer 1, F2=fertilizer 2,
                W1=weeding1, W2=weeding2, W3=weeding3, maizeharvest=Hm, cassharvest=Hc")
        monthorder <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        EAdata$month <- factor(EAdata$month, levels = monthorder)
        EAdata$yeartbd <- as.yearmon(EAdata$datetbd2)
        
        #EAdata <- droplevels(EAdata[!is.na(EAdata$month), ])
        
        EAdata$weeks <- as.factor(EAdata$weeks)
        if(nrow(subset(EAdata, season == input$Seasonic))>0){
          plotdata <- subset(EAdata, season == input$Seasonic)
        }else{
          plotdata <-  EAdata
        }
        
        
        ICwkheatplot <- ggplot(plotdata, aes(x=weeks, y=HHID, fill = factor(status))) +
          
          
          geom_tile(size=0.1) +
          scale_fill_manual(values=statscolor) +
          ggtitle(label) +
          geom_text(aes(label = eventCode2), size=3)+
          
          #facet_grid(.~ yeartbd, space = 'free', scales = 'free', switch = 'x') +
          #geom_vline(data=toBeDue, aes(xintercept = weeks)) + 
          labs(x = "") + labs(y = "") +
          guides(fill=guide_legend("Timing of activities:"))+
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5, size=16), axis.ticks=element_blank(), axis.text=element_text(size=12),
                panel.grid.minor = element_blank(), axis.title = element_text(size=14),strip.text.x = element_text(size=10, angle = 20),
                legend.key.size = unit(2.5,"line"), legend.position="right")
        
        ICwkheatplot 
        
      }else{
        NULL
      }
    }
    
  })
  
  
  
  output$ICtable <- DT::renderDataTable({
    if(!is.null(input$eaidsic)){
      if(input$eaidsic != "All"){
        h <- input$eaidsic
        p<- ICpoints2[ICpoints2$EAID == h, ]
        p
        values$ICpoints1 <- p
        
      }else{
        NULL
      }
    }
  })
  
  
  output$ICpointout <- renderUI({
    if(!is.null(input$eaidsic)){
      if(input$eaidsic != "All"){
        h <- input$eaidsic
        p<- ICpoints2[ICpoints2$EAID == h, ]
      }
    }
    
    x = colSums(p[,12:13], na.rm = FALSE)
    totalpoints = x[2]
    paste("Total EA points" , totalpoints,
          "\n *For each activity, points are earned only once even if activity done on different dates*")
  })
  
  
  
  output$ICrecom <- DT::renderDataTable({
    if(!is.null(input$eaidsic)){
      if(input$eaidsic != "All"){
        h <- input$eaidsic
        p<- dsicrecom[dsicrecom$EAID == h, ]
        p
      }else{
        p <- dsicrecom
        p
      }
      values$icrecom <- p
    }
  })
  
  
  output$downloadrecomIC <- downloadHandler(
    filename = function(){"Recommendations.csv"},
    content = function(file){
      if(!is.null(values$icrecom)){
        write.csv(values$icrecom , file, row.names = FALSE)
      }
    }
  )
  
  
  
  output$ICptsummary <- DT::renderDataTable({
    if(!is.null(input$eaidsic)){
      if(input$eaidsic != "All"){
        h <- input$eaidsic
        p<- ICpointsdt2[ICpointsdt2$EAID == h, ]
        p
        values$ICpoints2 <- p
      }else{
        NULL
      }
    }
    
  })
  
  
  output$downloadPlotIC <- downloadHandler(
    filename = function(){"ICplot.pdf"},
    content = function(file){
      if(!is.null(values$ICplotData) & !is.null(values$ICpoints1) & !is.null(values$ICpoints2)){
        ICplotData <- values$ICplotData
        ICpoints1 <- values$ICpoints1
        ICpoints2 <- values$ICpoints2
        plotableIC(ICplotData, ICpoints1, ICpoints2)
        file.copy("ICplot.pdf", file, overwrite = TRUE)
      }
    }
  )
  
  
  
  output$EAPPplot <- renderPlot({
    if(!is.null(input$eaidspp)){
      if(input$eaidspp != "All"){
        h <- input$eaidspp
        
        EAPP.h <- PP_dstchecked[PP_dstchecked$EAID == h, ] # extract by EAID
        EAPP.h <- droplevels(EAPP.h[!is.na(EAPP.h$HHID), ])
        if(!all(is.na(EAPP.h$DatesBnPlantingEvent))){
          
          cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done later"="yellowgreen",
                    "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Due soon"="purple")
          
          Events <- c("DST run", "planting", "gapping", "weeding1", "weeding2","weeding3", "harvest")
          
          values$PPplotData <- EAPP.h
          
          EAPPplot <- ggplot(EAPP.h, aes(x=Events, y=HHID, fill= status)) +
            scale_x_discrete(limits = Events) +
            geom_tile(color="white", size=0.1) +
            theme(legend.text=element_text(size=10)) +
            ggtitle(paste("PP validation exercise schedule for EAID = ",h, unique(EAPP.h$EA_Name),sep = " " )) +
            geom_text(aes(label=datetbd2))+
            theme(axis.title.y=element_blank()) +
            theme(plot.title = element_text(size = 20, face = "bold")) +
            scale_fill_manual("Event timing", values=cols) +
            theme(plot.title=element_text(hjust=0), axis.ticks=element_blank(), axis.text.y=element_text(size=14),
                  axis.title=element_text(size=20),axis.text.x = element_text(angle = 25, size=14, vjust = 1, hjust=1) ) +
            theme(legend.position="right") +
            coord_fixed(ratio = 0.25)
          EAPPplot
          
        }else{
          NULL
        }
      }
    }
  })
  
  output$PPwkheatplot <- renderPlot({
    if(!is.null(input$eaidspp)& !is.null(input$Seasonpp)){
      if(input$eaidspp != "All"){
        h <- input$eaidspp
        EALs.h <- ppwksdata[ppwksdata$EAID == h, ] # extract by EA_Name
        EALs.h$eventCode <- ifelse(EALs.h$events == "planting", "P",
                                   ifelse(EALs.h$events == "gapping", "G",
                                          ifelse(EALs.h$events == "weeding1", "W1",
                                                 ifelse(EALs.h$events == "weeding2", "W2",
                                                        ifelse(EALs.h$events == "weeding3", "W3", "H")))))
        
        dayToday <- lubridate::week(ymd(Sys.Date()))
        mindate <- min( EALs.h[ EALs.h$events == "planting", ]$weeks, na.rm = TRUE)
        statscolor <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done later"="yellowgreen",
                        "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Mixed events"="cyan")
        
        EALs.h <- EALs.h[rowSums(is.na(EALs.h)) != ncol(EALs.h), ]
        EALs.h$hhweeks <- paste(EALs.h$HHID, EALs.h$weeks, sep="_")
        
        EAdata <- NULL
        for(hw in unique(EALs.h$hhweeks)){
          hd <- EALs.h[EALs.h$hhweeks == hw, ]
          if(nrow(hd) >1){
            hd$eventCode2 <- paste(unique(hd$eventCode), collapse=", ")
            if(length(unique(hd$status)) > 1){
              hd$status <- 'Mixed events'
            }
            hd <- head(hd,1)
          }else{
            hd$eventCode2 <- hd$eventCode
          }
          EAdata <- rbind(EAdata, hd)
        }
        
        label=c("PP validation calendar for EA \n P=planting, G=Gapping, 
                W1=weeding1, W2=weeding2, W3=weeding3, harvest=H")
        monthorder <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        EAdata$month <- factor(EAdata$month, levels = monthorder)
        EAdata$yeartbd <- as.yearmon(EAdata$datetbd2)
        
        #EAdata <- droplevels(EAdata[!is.na(EAdata$month), ])
        
        
        EAdata$weeks <- as.factor(EAdata$weeks)
        if(nrow(subset(EAdata, season == input$Seasonpp))>0){
          plotdatapp <- subset(EAdata, season == input$Seasonpp)
        }else{
          plotdatapp <-  EAdata
        }
        
        
        PPwkheatplot <- ggplot(plotdatapp, aes(x=weeks, y=HHID, fill = factor(status))) +
          geom_tile(size=0.1) +
          scale_fill_manual(values=statscolor) +
          ggtitle(label) +
          geom_text(aes(label = eventCode2), size=3)+
          
          #facet_grid(.~ yeartbd, space = 'free', scales = 'free', switch = 'x') +
          #geom_vline(data=toBeDue, aes(xintercept = weeks)) + 
          labs(x = "") + labs(y = "") +
          guides(fill=guide_legend("Timing of activities:"))+
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5, size=16), axis.ticks=element_blank(), axis.text=element_text(size=12),
                panel.grid.minor = element_blank(), axis.title = element_text(size=14),strip.text.x = element_text(size=10),
                legend.key.size = unit(2.5,"line"), legend.position="right")
        
        PPwkheatplot
      }else{
        NULL
      }
    }
  })
  
  output$PPpointout <- renderUI({
    if(!is.null(input$eaidspp)){
      if(input$eaidspp != "All"){
        h <- input$eaidspp
        p <- PPpoints[PPpoints$EAID == h, ]
        
      }
    }
    x = colSums(p[,11:12], na.rm = FALSE)
    totalpoints = x[2]
    paste("Total EA points" , totalpoints,
          "\n *For each activity, points are earned only once even if activity done on different dates*")
  })
  
  
  
  output$PPtable <- DT::renderDataTable({
    if(!is.null(input$eaidspp)){
      if(input$eaidspp != "All"){
        h <- input$eaidspp
        p<- PPpoints[PPpoints$EAID == h, ]
        p
        values$PPpoints1 <- p
      }else{
        NULL
      }
    }
  })
  
  output$PPptsummary <- DT::renderDataTable({
    if(!is.null(input$eaidspp)){
      if(input$eaidspp != "All"){
        h <- input$eaidspp
        p<- PPpointsdt2[PPpointsdt2$EAID == h, ]
        p
        values$PPpoints2 <- p
      }else{
        NULL
      }
    }
  })
  
  
  output$PPrecom <- DT::renderDataTable({
    if(!is.null(input$eaidspp)){
      if(input$eaidspp != "All"){
        h <- input$eaidspp
        p<- dspprecom[dspprecom$EAID == h, ]
        p
      }else{
        p <- dspprecom
        p
      }
      values$pprecom <- p
    }
  })
  
  
  
  output$downloadPlotPP <- downloadHandler(
    filename = function(){"PPplot.pdf"},
    content = function(file){
      if(!is.null(values$PPplotData) & !is.null(values$PPpoints1) & !is.null(values$PPpoints2)){
        PPplotData <- values$PPplotData
        PPpoints1 <- values$PPpoints1
        PPpoints2 <- values$PPpoints2
        plotablePP(PPplotData, PPpoints1, PPpoints2)
        file.copy("PPplot.pdf", file, overwrite = TRUE)
      }
    }
  )
  
  
  
  output$downloadrecomPP <- downloadHandler(
    filename = function(){"PP_Recommendations.csv"},
    content = function(file){
      if(!is.null(values$pprecom)){
        write.csv(values$pprecom , file, row.names = FALSE)
      }
    }
  )
  
  
  output$EAPPTZplot <- renderPlot({
    if(!is.null(input$eaidspptz)){
      if(input$eaidspptz != "All"){
        h <- input$eaidspptz
        
        EAPPTZ.h <- PPTZ_dstchecked[PPTZ_dstchecked$EAID == h, ] # extract by EAID
        EAPPTZ.h <- droplevels(EAPPTZ.h[!is.na(EAPPTZ.h$HHID), ])
        if(!all(is.na(EAPPTZ.h$DatesBnPlantingEvent))){
          
          cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done later"="yellowgreen",
                    "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Due soon"="purple")
          
          Events <- c("DST run", "planting", "gapping", "weeding1", "weeding2","weeding3", "harvest")
          
          values$PPTZplotData <- EAPPTZ.h
          
          EAPPTZplot <- ggplot(EAPPTZ.h, aes(x=Events, y=HHID, fill= status)) +
            scale_x_discrete(limits = Events) +
            geom_tile(color="white", size=0.1) +
            theme(legend.text=element_text(size=10)) +
            ggtitle(paste("Weed management & PP validation exercise schedule for EAID = ",h, unique(EAPPTZ.h$EA_Name),sep = " " )) +
            geom_text(aes(label=datetbd2))+
            theme(axis.title.y=element_blank()) +
            theme(plot.title = element_text(size = 20, face = "bold")) +
            scale_fill_manual("Event timing", values=cols) +
            theme(plot.title=element_text(hjust=0), axis.ticks=element_blank(), axis.text.y=element_text(size=14),
                  axis.title=element_text(size=20),axis.text.x = element_text(angle = 25, size=14, vjust = 1, hjust=1) ) +
            theme(legend.position="right") +
            coord_fixed(ratio = 0.25)
          EAPPTZplot
          
        }else{
          NULL
        }
      }
    }
  })
  
  
  
  output$PPTZwkheatplot <- renderPlot({
    if(!is.null(input$eaidspptz)& !is.null(input$Seasonpptz)){
      if(input$eaidspptz != "All"){
        h <- input$eaidspptz
        EALs.h <- PPTZwksdata[PPTZwksdata$EAID == h, ] # extract by EA_Name
        EALs.h$eventCode <- ifelse(EALs.h$events == "planting", "P",
                                   ifelse(EALs.h$events == "gapping", "G",
                                          ifelse(EALs.h$events == "weeding1", "W1",
                                                 ifelse(EALs.h$events == "weeding2", "W2",
                                                        ifelse(EALs.h$events == "weeding3", "W3", "H")))))
        
        dayToday <- lubridate::week(ymd(Sys.Date()))
        mindate <- min( EALs.h[ EALs.h$events == "planting", ]$weeks, na.rm = TRUE)
        statscolor <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done later"="yellowgreen",
                        "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Mixed events"="cyan")
        
        EALs.h <- EALs.h[rowSums(is.na(EALs.h)) != ncol(EALs.h), ]
        EALs.h$hhweeks <- paste(EALs.h$HHID, EALs.h$weeks, sep="_")
        
        EAdata <- NULL
        for(hw in unique(EALs.h$hhweeks)){
          hd <- EALs.h[EALs.h$hhweeks == hw, ]
          if(nrow(hd) >1){
            hd$eventCode2 <- paste(unique(hd$eventCode), collapse=", ")
            if(length(unique(hd$status)) > 1){
              hd$status <- 'Mixed events'
            }
            hd <- head(hd,1)
          }else{
            hd$eventCode2 <- hd$eventCode
          }
          EAdata <- rbind(EAdata, hd)
        }
        
        label=c("Weed management & PP validation calendar for EA \n P=planting, G=Gapping, 
                W1=weeding1, W2=weeding2, W3=weeding3, harvest=H")
        monthorder <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        EAdata$month <- factor(EAdata$month, levels = monthorder)
        EAdata$yeartbd <- as.yearmon(EAdata$datetbd2)
        
        #EAdata <- droplevels(EAdata[!is.na(EAdata$month), ])
        
        EAdata$weeks <- as.factor(EAdata$weeks)
        
        
        if(nrow(subset(EAdata, season == input$Seasonpptz))>0){
          plotdatapptz <- subset(EAdata, season == input$Seasonpptz)
        }else{
          plotdatapptz <-  EAdata
        }
        
        
        
        PPTZwkheatplot <- ggplot(plotdatapptz,aes(x=weeks, y=HHID, fill = factor(status))) +
          geom_tile(size=0.1) +
          scale_fill_manual(values=statscolor) +
          ggtitle(label) +
          geom_text(aes(label = eventCode2), size=3)+
          
          #facet_grid(.~ yeartbd, space = 'free', scales = 'free', switch = 'x') +
          #geom_vline(data=toBeDue, aes(xintercept = weeks)) + 
          labs(x = "") + labs(y = "") +
          guides(fill=guide_legend("Timing of activities:"))+
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5, size=16), axis.ticks=element_blank(), axis.text=element_text(size=12),
                panel.grid.minor = element_blank(), axis.title = element_text(size=14),strip.text.x = element_text(size=10),
                legend.key.size = unit(2.5,"line"), legend.position="right")
        
        PPTZwkheatplot
        #ADD SEASON DATA 
      }else{
        NULL
      }
    }
  })
  
  output$PPTZpointout <- renderUI({
    if(!is.null(input$eaidspptz)){
      if(input$eaidspptz != "All"){
        h <- input$eaidspptz
        p <- PPTZpoints[PPTZpoints$EAID == h, ]
        
      }
    }
    x = colSums(p[,11:12], na.rm = FALSE)
    totalpoints = x[2]
    paste("Total EA points" , totalpoints,
          "\n *For each activity, points are earned only once even if activity done on different dates*")
  })
  
  
  
  output$PPTZtable <- DT::renderDataTable({
    if(!is.null(input$eaidspptz)){
      if(input$eaidspptz != "All"){
        h <- input$eaidspptz
        p<- PPTZpoints[PPTZpoints$EAID == h, ]
        p
        values$PPTZpoints1 <- p
      }else{
        NULL
      }
    }
  })
  
  output$PPTZptsummary <- DT::renderDataTable({
    if(!is.null(input$eaidspptz)){
      if(input$eaidspptz != "All"){
        h <- input$eaidspptz
        p<- PPTZpointsdt2[PPTZpointsdt2$EAID == h, ]
        p
        values$PPTZpoints2 <- p
      }else{
        NULL
      }
    }
  })
  
  
  output$PPTZrecom <- DT::renderDataTable({
    if(!is.null(input$eaidspptz)){
      if(input$eaidspptz != "All"){
        h <- input$eaidspptz
        p<- dsPPTZrecom[dsPPTZrecom$EAID == h, ]
        p
      }else{
        p <- dsPPTZrecom
        p
      }
      values$pptzrecom <- p
    }
  })
  
  
  
  output$downloadPlotPPTZ <- downloadHandler(
    filename = function(){"PPTZplot.pdf"},
    content = function(file){
      if(!is.null(values$PPTZplotData) & !is.null(values$PPTZpoints1) & !is.null(values$PPTZpoints2)){
        PPTZplotData <- values$PPTZplotData
        PPTZpoints1 <- values$PPTZpoints1
        PPTZpoints2 <- values$PPTZpoints2
        plotablePPTZ(PPTZplotData, PPTZpoints1, PPTZpoints2)
        file.copy("PPTZplot.pdf", file, overwrite = TRUE)
      }
    }
  )
  
  
  
  output$downloadrecomPPTZ <- downloadHandler(
    filename = function(){"PP_TZRecommendations.csv"},
    content = function(file){
      if(!is.null(values$pptzrecom)){
        write.csv(values$pptzrecom , file, row.names = FALSE)
      }
    }
  )
  
  
  ############# SPHS_TZ ##################
  output$EASPHSplot <- renderPlot({
    if(!is.null(input$eaidssphs_tz)){
      if(input$eaidssphs_tz != "All"){
        h <- input$eaidssphs_tz
        
        
        EASPHS.h <- SP_dstchecked[SP_dstchecked$EAID == h, ] # extract by EA_Name
        EASPHS.h <- droplevels(EASPHS.h[!is.na(EASPHS.h$HHID), ])
        
        if(!all(is.na(EASPHS.h$DatesBnPlantingEvent))){
          
          cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done late"="yellowgreen",
                    "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Due soon"="purple", "Control"="dodgerblue4", "Recommended"="red3")
          
          Events <- c("DST run", "planting",  "gapping", "weeding1", "weeding2", "weeding3", "intharvestCON", "effharvestCON","intharvestREC","effharvestREC")
          values$SPHSplotData <- EASPHS.h
          EASPHSplot <- ggplot(EASPHS.h, aes(x=Events, y=HHID, fill= status)) +
            scale_x_discrete(limits = Events) +
            geom_tile(color="white", size=0.1) +
            theme(legend.text=element_text(size=10)) +
            ggtitle(paste("SP validation exercise schedule for EAID = ",h, unique(EASPHS.h$EA_Name),sep = " " )) +
            geom_text(aes(label=datetbd2))+
            theme(axis.title.y=element_blank()) +
            theme(plot.title = element_text(size = 20)) +
            scale_fill_manual("Event timing", values=cols) +
            theme(plot.title=element_text(hjust=0), axis.ticks=element_blank(), axis.text.y=element_text(size=14),
                  axis.title=element_text(size=20),axis.text.x = element_text(angle = 25, size=14, vjust = 1, hjust=1) ) +
            theme(legend.position="right") +
            coord_fixed(ratio = 0.25)
          EASPHSplot
        }else{
          NULL
        }
      }
    }
  })
  
  
  
  output$SPHSwkheatplot <- renderPlot({
    if(!is.null(input$eaidssphs_tz)& !is.null(input$Seasonsphs_tz)){
      if(input$eaidssphs_tz != "All"){
        h <- input$eaidssphs_tz
        
        EALs.h <- SPHSwksdata[SPHSwksdata$EAID == h, ] # extract by EA_Name
        
        head( EALs.h)
        EALs.h$eventCode <- ifelse( EALs.h$events == "planting", "P",
                                    ifelse( EALs.h$events == "gapping", "G",
                                            ifelse( EALs.h$events == "weeding1", "W1",
                                                    ifelse( EALs.h$events == "weeding2", "W2",
                                                            ifelse(EALs.h$events == "weeding3", "W3", 
                                                                   ifelse(EALs.h$events == "effharvestCON", "HC",
                                                                          ifelse(EALs.h$events == "effharvestREC", "HR","NA")))))))
        dayToday <- lubridate::week(ymd(Sys.Date()))
        mindate <- min( EALs.h[ EALs.h$events == "planting", ]$weeks, na.rm = TRUE)
        statscolor <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done late"="yellowgreen",
                        "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Mixed events"="cyan", 
                        "Due soon" = "purple", "Control"="dodgerblue4", "Recommended"="red3")
        
        EALs.h <- EALs.h[rowSums(is.na(EALs.h)) != ncol(EALs.h), ]
        #EALs.h <- droplevels(EALs.h[!is.na(EALs.h$day), ])
        EALs.h$hhweeks <- paste(EALs.h$HHID, EALs.h$weeks, sep="_")
        
        EAdata <- NULL
        for(hw in unique(EALs.h$hhweeks)){
          hd <- EALs.h[EALs.h$hhweeks == hw, ]
          if(nrow(hd) >1){
            hd$eventCode2 <- paste(unique(hd$eventCode), collapse="/")
            if(length(unique(hd$status)) > 1){
              hd$status <- 'Mixed events'
            }
            
            hd <- head(hd,1)
            
          }else{
            hd$eventCode2 <- hd$eventCode
          }
          
          EAdata <- rbind(EAdata, hd)
        }
        
        label=c("SP validation calendar for EA \n P=planting, G=gapping, W1=weeding1, W2=weeding2, W3=weeding3, harvestCON=HC, harvestREC=HR")
        monthorder <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        
        EAdata$month <- factor(EAdata$month, levels = monthorder)
        #EAdata$yeartbd <- lubridate::year(ymd(EAdata$datetbd2))
        EAdata$yeartbd <- as.yearmon(EAdata$datetbd2)
        
        #EAdata <- droplevels(EAdata[!is.na(EAdata$month), ])
        
        EAdata$weeks <- as.factor(EAdata$weeks)
        
        
        SPHScalendar <- ggplot(subset(EAdata, season == input$Seasonsphs_tz), aes(x=weeks, y=HHID, fill = factor(status))) +
          geom_tile(size=0.1) +
          scale_fill_manual(values=statscolor) +
          ggtitle(label) +
          geom_text(aes(label = eventCode2), size=3)+
          
          #facet_grid(.~ yeartbd, space = 'free', scales = 'free', switch = 'x') +
          #geom_vline(data=toBeDue, aes(xintercept = weeks)) + 
          labs(x = "") + labs(y = "") +
          guides(fill=guide_legend("Timing of activities:"))+
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5, size=16), axis.ticks=element_blank(), axis.text=element_text(size=12),
                panel.grid.minor = element_blank(), axis.title = element_text(size=14),strip.text.x = element_text(size=10),
                legend.key.size = unit(2.5,"line"), legend.position="right")
        SPHScalendar
      }else{
        NULL
      }
    }
  })
  
  
  
  output$SPHStable <- DT::renderDataTable({
    if(!is.null(input$eaidssphs_tz)){
      if(input$eaidssphs_tz != "All"){
        h <- input$eaidssphs_tz
        p<- SPHSpoints[SPHSpoints$EAID == h, ]
        p
        values$SPHSpoints1 <- p
        
      }else{
        NULL
      }
    }
  })
  
  
  output$SPHSpointout <- renderUI({
    if(!is.null(input$eaidssphs_tz)){
      if(input$eaidssphs_tz != "All"){
        h <- input$eaidssphs_tz
        p<- SPHSpoints[SPHSpoints$EAID == h, ]
      }
    }
    
    x = colSums(p[,11:12], na.rm = FALSE)
    totalpoints = x[2]
    paste("Total EA points" , totalpoints,
          "\n *For each activity, points are earned only once even if activity done on different dates*")
  })
  
  
  
  output$SPHSrecom <- DT::renderDataTable({
    if(!is.null(input$eaidssphs_tz)){
      if(input$eaidssphs_tz != "All"){
        h <- input$eaidssphs_tz
        p<- dssphsrecom[dssphsrecom$EAID == h, ]
        p
      }else{
        p <- dssphsrecom
        p
      }
      values$sphsrecom <- p
    }
  })
  
  
  output$downloadrecomSPHS <- downloadHandler(
    filename = function(){"Recommendations.csv"},
    content = function(file){
      if(!is.null(values$sphsrecom)){
        write.csv(values$sphsrecom , file, row.names = FALSE)
      }
    }
  )
  
  
  
  output$SPHSptsummary <- DT::renderDataTable({
    if(!is.null(input$eaidssphs_tz)){
      if(input$eaidssphs_tz != "All"){
        h <- input$eaidssphs_tz
        p<- SPHSpointsdt2[SPHSpointsdt2$EAID == h, ]
        p
        values$SPHSpoints2 <- p
      }else{
        NULL
      }
    }
    
  })
  
 
  output$downloadPlotSPHS <- downloadHandler(
    filename = function(){"SPHSplot.pdf"},
    content = function(file){
      if(!is.null(values$SPHSplotData) & !is.null(values$SPHSpoints1) & !is.null(values$SPHSpoints2)){
        SPHSplotData <- values$SPHSplotData
        SPHSpoints1 <- values$SPHSpoints1
        SPHSpoints2 <- values$SPHSpoints2
        plotableSPHS(SPHSplotData, SPHSpoints1, SPHSpoints2)
        file.copy("SPplot.pdf", file, overwrite = TRUE)
      }
    }
  )
  
  
  ##########################################################################
  
  ############# SPHS_NG ##################
  output$EASPNGplot <- renderPlot({
    if(!is.null(input$eaidssphs_ng)){
      if(input$eaidssphs_ng != "All"){
        h <- input$eaidssphs_ng
        
        EASPNG.h <- SPNG_dstchecked[SPNG_dstchecked$EAID == h, ] # extract by EA_Name
        EASPNG.h <- unique(EASPNG.h)
        EASPNG.h <- droplevels(EASPNG.h[!is.na(EASPNG.h$HHID), ])
        
        if(!all(is.na(EASPNG.h$DatesBnPlantingEvent))){
          
          cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done late"="yellowgreen",
                    "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Due soon"="purple", "Control"="dodgerblue4", "Recommended"="red3")
          
          Events <- c( "monitored", "rapid yield", "ok field", "planting","int. harvest blue plot", "eff. harvest blue plot", "int. harvest red plot", "eff. Harvest red plot"  )
          values$SPNGplotData <- EASPNG.h
          EASPNGplot <- ggplot(EASPNG.h, aes(x=Events, y=HHID, fill= status)) +
            scale_x_discrete(limits = Events) +
            geom_tile(color="white", size=0.1) +
            theme(legend.text=element_text(size=10)) +
            ggtitle(paste("SP validation exercise schedule for EAID = ",h, unique(EASPNG.h$EA_Name),sep = " " )) +
            geom_text(aes(label=datetbd2))+
            theme(axis.title.y=element_blank()) +
            theme(plot.title = element_text(size = 20)) +
            scale_fill_manual("Event timing", values=cols) +
            theme(plot.title=element_text(hjust=0), axis.ticks=element_blank(), axis.text.y=element_text(size=14),
                  axis.title=element_text(size=20),axis.text.x = element_text(angle = 25, size=14, vjust = 1, hjust=1) ) +
            theme(legend.position="right") +
            coord_fixed(ratio = 0.25)
          EASPNGplot
        }else{
          NULL
        }
      }
    }
  })
  

  
  output$SPNGwkheatplot <- renderPlot({
    if(!is.null(input$eaidssphs_ng)& !is.null(input$Seasonsphs_ng)){
      if(input$eaidssphs_ng != "All"){
        h <- input$eaidssphs_ng
        
        EALs.h <- SPNGwksdata[SPNGwksdata$EAID == h, ] # extract by EA_Name
        
        head( EALs.h)
        EALs.h$eventCode <- ifelse( EALs.h$events == "planting", "P",
                                                         ifelse(EALs.h$events == "intharvestCON", "IHC",
                                                                          ifelse(EALs.h$events == "intharvestREC", "IHR",
                                                                                 ifelse(EALs.h$events == "effharvestCON", "EHC",
                                                                                        ifelse(EALs.h$events == "effharvestREC", "EHR", "NA")))))
        dayToday <- lubridate::week(ymd(Sys.Date()))
        mindate <- min( EALs.h[ EALs.h$events == "planting", ]$weeks, na.rm = TRUE)
        statscolor <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done late"="yellowgreen",
                        "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Mixed events"="cyan", 
                        "Due soon" = "purple", "Control"="dodgerblue4", "Recommended"="red3")
        
        EALs.h <- EALs.h[rowSums(is.na(EALs.h)) != ncol(EALs.h), ]
        #EALs.h <- droplevels(EALs.h[!is.na(EALs.h$day), ])
        EALs.h$hhweeks <- paste(EALs.h$HHID, EALs.h$weeks, sep="_")
        
        EAdata <- NULL
        for(hw in unique(EALs.h$hhweeks)){
          hd <- EALs.h[EALs.h$hhweeks == hw, ]
          if(nrow(hd) >1){
            hd$eventCode2 <- paste(unique(hd$eventCode), collapse="/")
            if(length(unique(hd$status)) > 1){
              hd$status <- 'Mixed events'
            }
            
            hd <- head(hd,1)
            
          }else{
            hd$eventCode2 <- hd$eventCode
          }
          
          EAdata <- rbind(EAdata, hd)
        }
        
        label=c("SP validation calendar for EA \n P=planting, G=gapping, W1=weeding1, W2=weeding2, W3=weeding3, intharvestdateCON=IHC, \n intharvestdateREC=IHR, effharvestdateCON=EHC, effharvestdateREC=EHR")
        monthorder <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        
        EAdata$month <- factor(EAdata$month, levels = monthorder)
        #EAdata$yeartbd <- lubridate::year(ymd(EAdata$datetbd2))
        EAdata$yeartbd <- as.yearmon(EAdata$datetbd2)
        
        #EAdata <- droplevels(EAdata[!is.na(EAdata$month), ])
        #
        
        EAdata$weeks <- as.factor(EAdata$weeks)
        
        if(nrow(subset(EAdata, season == input$Seasonsphs_ng))>0){
          plotdataSPNG <- subset(EAdata, season == input$Seasonsphs_ng)
        }else{
          plotdataSPNG <-  EAdata
        }
        
        SPNGcalendar <- ggplot(plotdataSPNG, aes(x=weeks, y=HHID, fill = factor(status))) +
          geom_tile(size=0.1) +
          scale_fill_manual(values=statscolor) +
          ggtitle(label) +
          geom_text(aes(label = eventCode2), size=3)+
          
          #facet_grid(.~ yeartbd, space = 'free', scales = 'free', switch = 'x') +
          #geom_vline(data=toBeDue, aes(xintercept = weeks)) + 
          labs(x = "") + labs(y = "") +
          guides(fill=guide_legend("Timing of activities:"))+
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5, size=16), axis.ticks=element_blank(), axis.text=element_text(size=12),
                panel.grid.minor = element_blank(), axis.title = element_text(size=14),strip.text.x = element_text(size=10),
                legend.key.size = unit(2.5,"line"), legend.position="right")
        SPNGcalendar
      }else{
        NULL
      }
    }
  })
  
  
  
  output$SPNGtable <- DT::renderDataTable({
    if(!is.null(input$eaidssphs_ng)){
      if(input$eaidssphs_ng != "All"){
        h <- input$eaidssphs_ng
        p<- SPNGpoints[SPNGpoints$EAID == h, ]
        p
        values$SPNGpoints1 <- p
        
      }else{
        NULL
      }
    }
  })
  
  
  output$SPNGpointout <- renderUI({
    if(!is.null(input$eaidssphs_ng)){
      if(input$eaidssphs_ng != "All"){
        h <- input$eaidssphs_ng
        p<- SPNGpoints[SPNGpoints$EAID == h, ]
      }
    }
    
    x = colSums(p[,7:8], na.rm = FALSE)
    totalpoints = x[2]
    paste("Total EA points" , totalpoints,
          "\n *For each activity, points are earned only once even if same activity done on different dates*")
  })
  
  
  
  output$downloadPlotSPNG <- downloadHandler(
    filename = function(){"SPHSNGplot.pdf"},
    content = function(file){
      if(!is.null(values$SPNGplotData) & !is.null(values$SPNGpoints1) & !is.null(values$SPNGpoints2)){
        SPNGplotData <- values$SPNGplotData
        SPNGpoints1 <- values$SPNGpoints1
        SPNGpoints2 <- values$SPNGpoints2
        plotableSPNG(SPNGplotData, SPNGpoints1, SPNGpoints2)
        file.copy("SPNGplot.pdf", file, overwrite = TRUE)
      }
    }
  )
  
  output$SPNGptsummary <- DT::renderDataTable({
    if(!is.null(input$eaidssphs_ng)){
      if(input$eaidssphs_ng != "All"){
        h <- input$eaidssphs_ng
        p<- SPNGpointsdt2[SPNGpointsdt2$EAID == h, ]
        p
        values$SPNGpoints2 <- p
      }else{
        NULL
      }
    }
    
  })

 ######################
  #CIS
  ####################
  
  output$EACISplot <- renderPlot({
    if(!is.null(input$eaidsCIS)){
      if(input$eaidsCIS != "All"){
        values$eaidsCIS <- input$eaidsCIS
        h <- input$eaidsCIS
        
        EALs.h <- CIS_dstchecked[CIS_dstchecked$EAID == h, ] # extract by EA_Name
        EALs.h <- droplevels(EALs.h[!is.na(EALs.h$HHID), ])
        
        if(!all(is.na(EALs.h$DatesBnPlantingEvent))){
          
          cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done late"="yellowgreen", 
                    "Overdue"="violetred2", "Not done"="orangered2", "To be done"="grey", "Due soon"="purple")
          
          Events <- c("DST run", "planting", "replant", "gapping", "fertilizer0", "fertilizer1", "weeding1", "weeding2", "weeding3", "harvestCS", "harvestSP")
          values$EACISplotData <- EALs.h
          EACISheatplot <- ggplot(EALs.h, aes(x=Events, y=HHID, fill= status)) +
            scale_x_discrete(limits = Events) +
            geom_tile(color="white", size=0.1) + 
            theme(legend.text=element_text(size=10)) +
            ggtitle(paste("CIS validation exercise schedule for EAID = ",h, unique(EALs.h$EA_Name),sep = " " )) +
            geom_text(aes(label=datetbd2))+
            xlab(" ")+
            theme(axis.title.y=element_blank()) +
            theme(plot.title = element_text(size = 20)) +
            scale_fill_manual("Event timing", values=cols) +
            theme(plot.title=element_text(hjust=0), axis.ticks=element_blank(), axis.text.y=element_text(size=14),
                  axis.title=element_text(size=20),axis.text.x = element_text(angle = 25, size=14, vjust = 1, hjust=1) ) +
            theme(legend.position="right") +
            coord_fixed(ratio = 0.25)
          print(EACISheatplot)
          
        }else{
          NULL
        }
      }
    }
  })
  
  
  
  output$downloadPlot <- downloadHandler(
    filename = function(){"CISplot.pdf"},
    content = function(file){ 
      if(!is.null(values$EACISplotData) & !is.null(values$CISpoints1) & !is.null(values$CISpoints2)){
        hhdata2 <- values$EACISplotData
        CISpoints1 <- values$CISpoints1
        CISpoints2 <- values$CISpoints2
        plotable(chhdata2, CISpoints1, CISpoints2)
        file.copy("CISplot.pdf", file, overwrite = TRUE)
      }
    }
  )
  
  
  output$CISwkheatplot <- renderPlot({
    if(!is.null(input$eaidsCIS)& !is.null(input$SeasonCIS)){
      if(input$eaidsCIS != "All"){
        h <- input$eaidsCIS
        
        EALs.h <- wksCISdtplot[wksCISdtplot$EAID == "ACEATZ000218", ] # extract by EA_Name
        
        head( EALs.h)
        EALs.h$eventCode <- ifelse( EALs.h$events == "planting", "P",
                                    ifelse( EALs.h$events == "gapping", "G",
                                            ifelse( EALs.h$events == "replant", "R", 
                                                    ifelse( EALs.h$events == "replant", "R",      
                                                            ifelse( EALs.h$events == "fertilizer0","F0",
                                                                    ifelse( EALs.h$events == "fertilizer1", "F1",
                                                                            ifelse( EALs.h$events == "weeding1", "W1",
                                                                                    ifelse(EALs.h$events == "weeding2", "W2",
                                                                                           ifelse(EALs.h$events == "weeding3","W3",
                                                                                                  ifelse(EALs.h$events == "harvestCS","HC",
                                                                                                         "HS"))))))))))
        dayToday <- lubridate::week(ymd(Sys.Date()))
        mindate <- min( EALs.h[ EALs.h$events == "planting", ]$weeks, na.rm = TRUE)
        statscolor <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done late"="yellowgreen",
                        "Overdue"="violetred2", "Not done"="orangered2", "To be done"="grey", "Mixed events"="cyan", "Due soon" = "purple")
        
        EALs.h <- EALs.h[rowSums(is.na(EALs.h)) != ncol(EALs.h), ]
        #EALs.h <- droplevels(EALs.h[!is.na(EALs.h$day), ])
        EALs.h$hhweeks <- paste(EALs.h$HHID, EALs.h$weeks, sep="_")
        
        EAdata <- NULL
        for(hw in unique(EALs.h$hhweeks)){
          hd <- EALs.h[EALs.h$hhweeks == hw, ]
          if(nrow(hd) >1){
            hd$eventCode2 <- paste(unique(hd$eventCode), collapse="/")
            if(length(unique(hd$status)) > 1){
              hd$status <- 'Mixed events'
            }
            
            hd <- head(hd,1)
            
          }else{
            hd$eventCode2 <- hd$eventCode
          }
          
          EAdata <- rbind(EAdata, hd)
        }
        
        label=c("CIS validation calendar for EA \n P=planting, R=Replant, G=gapping, F1=fertilizer 0, F1=fertilizer 1, W1=weeding1, W2=weeding2, W3=weeding3, HC=harvestCS, HP=harvestSP")
        monthorder <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        
        EAdata$month <- factor(EAdata$month, levels = monthorder)
        #EAdata$yeartbd <- lubridate::year(ymd(EAdata$datetbd2))
        EAdata$yeartbd <- as.yearmon(EAdata$datetbd2)
        
        #EAdata <- droplevels(EAdata[!is.na(EAdata$month), ])
        
        EAdata$weeks <- as.factor(EAdata$weeks)
        
        
        if(nrow(subset(EAdata, season == input$SeasonCIS))>0){
          plotdatacis <- subset(EAdata, season == input$SeasonCIS)
        }else{
          plotdatacis <-  EAdata
        }
        
    
        
        CIScalendar <- ggplot(EAdata, aes(x=weeks, y=HHID, fill = factor(status))) +
          geom_tile(size=0.1) +
          scale_fill_manual(values=statscolor) +
          ggtitle(label) +
          geom_text(aes(label = eventCode2), size=3)+
          
          #facet_grid(.~ yeartbd, space = 'free', scales = 'free', switch = 'x') +
          #geom_vline(data=toBeDue, aes(xintercept = weeks)) + 
          labs(x = "") + labs(y = "") +
          guides(fill=guide_legend("Timing of activities:"))+
          theme_bw()+
          theme(plot.title=element_text(hjust=0.5, size=16), axis.ticks=element_blank(), axis.text=element_text(size=12),
                panel.grid.minor = element_blank(), axis.title = element_text(size=14),strip.text.x = element_text(size=10),
                legend.key.size = unit(2.5,"line"), legend.position="right")
        CIScalendar
      }else{
        NULL
      }
    }
  })
  
  
  output$CIStable <- DT::renderDataTable({
    if(!is.null(input$eaidsCIS)){
      if(input$eaidsCIS != "All"){
        h <- input$eaidsCIS
        p<- CISpoints[CISpoints$EAID == h, ]
        p
        values$CISpoints1 <- p
      }else{
        NULL
      }
    }
    
  })
  
  
  output$CISpointout <- renderUI({
    if(!is.null(input$eaidsCIS)){
      if(input$eaidsCIS != "All"){
        h <- input$eaidsCIS
        p<- CISpoints[CISpoints$EAID == h, ]
      }
    } 
    
    x = colSums(p[,12:13], na.rm = FALSE)
    totalpoints = x[2]
    paste("Total EA points earned:" , totalpoints, 
          "\n *For each activity, points are earned only once even if event done on different dates*")
  })
  
  output$CISrecom <- DT::renderDataTable({
    if(!is.null(input$eaidsCIS)){
      if(input$eaidsCIS != "All"){
        h <- input$eaidsCIS
        p<- dsCISrecom[dsCISrecom$EAID == h, ]
        p
      }else{
        p <- dsCISrecom
        p
      }
      values$CISrecom <- p
    }
    
  })	
  
  output$downloadrecomCIS <- downloadHandler(
    filename = function(){"Recommendations.csv"},
    content = function(file){ 
      if(!is.null(values$recom)){
        write.csv(values$recom , file, row.names = FALSE)
      }
    }
  )
  
  
  output$CISptsummary <- DT::renderDataTable({
    if(!is.null(input$eaidsCIS)){
      if(input$eaidsCIS != "All"){
        h <- input$eaidsCIS
        p<- CISpointsdt2[CISpointsdt2$EAID == h, ]
        p
        values$CISpoints2 <- p
      }else{
        NULL
      }
    }
    
  })
  
  
  output$frditable <- DT::renderDataTable({
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      
      dsEAHH_DI$EAHHUC <- paste(dsEAHH_DI$EAID, dsEAHH_DI$EA_Name, dsEAHH_DI$HHID, dsEAHH_DI$HH_Name, dsEAHH_DI$useCase, sep="")
      EAmonitoringIssue$EAHHUC <- paste(EAmonitoringIssue$EAID, EAmonitoringIssue$EA_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$useCase, sep="")
      
      dsEAHH_DIRemain <- droplevels(dsEAHH_DI[!dsEAHH_DI$EAHHUC %in% EAmonitoringIssue$EAHHUC, ])
      dsEAHH_DI <- rbind(dsEAHH_DIRemain, EAmonitoringIssue)    
      dsEAHH_DI <- subset(dsEAHH_DI, select = -c(EAHHUC))
      
      
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      eadata$HH_Name <- gsub(" $", "", eadata$HH_Name)
      rownames(eadata) <- NULL
      
      eadata <- eadata[eadata$useCase %in% c("FR", "FR IC", "FR PP", "FR SP") ,]
      return(eadata)
      # 
      # tt <- formattable(eadata, list(
      #   data_issue = formatter("span",
      #                            style = x ~ style(color = ifelse(x == "multipleHHName", "darkgreen", 
      #                                                             ifelse(x == "multipleHHID", "purple", 
      #                                                                    ifelse(x == "multipleEAName", "brown", "orange"))))),
      #   Status = formatter("span",
      #                          style = x ~ style(color = ifelse(x == "Keep", "blue", "red")))
      #                            ))
      # as.datatable(tt, extensions = 'Buttons', options = list(pageLength = 10, dom = 'lBfrtip', buttons = c('copy', 'csv')))
    }
  })
  
  observeEvent(input$discard1,{
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("FR", "FR IC", "FR PP", "FR SP") ,]
      
      spdata <- data.frame(eadata[input$frditable_rows_selected, ])
      spdata$Status <- "Remove"
      
      spdata$index <- paste(spdata$EA_Name, spdata$EAID, spdata$HH_Name, spdata$HHID, spdata$useCase, sep="_")
      EAmonitoringIssue$index <- paste(EAmonitoringIssue$EA_Name, EAmonitoringIssue$EAID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$useCase, sep="_")
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[!EAmonitoringIssue$index %in% spdata$index, ])
      
      EAmonitoringIssue <- unique(rbind(EAmonitoringIssue, spdata))
      EAmonitoringIssue <- subset(EAmonitoringIssue, select=-c(index))
      write.csv(unique(EAmonitoringIssue), "EAmonitoringIssue.csv", row.names = FALSE)
      gs_upload("EAmonitoringIssue.csv", sheet_title = 'EAmonitoringIssue', overwrite = TRUE)
      
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Remove", ])
      EAmonitoringIssue$EAHHUC <- paste(EAmonitoringIssue$EAID, EAmonitoringIssue$EA_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$useCase, sep="")
      dsEAHH$EAHHUC <- paste(dsEAHH$EAID, dsEAHH$EA_Name, dsEAHH$HHID, dsEAHH$HH_Name, dsEAHH$useCase, sep="")
      dsEAHH <- droplevels(dsEAHH[!dsEAHH$EAHHUC %in% EAmonitoringIssue$EAHHUC, ])
      
    }
  })
  
  observeEvent(input$Restore1,{
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("FR", "FR IC", "FR PP", "FR SP") ,]
      
      spdata <- data.frame(eadata[input$frditable_rows_selected, ])
      spdata$Status <- "Keep"
      spdata$index <- paste(spdata$EA_Name, spdata$EAID, spdata$HH_Name, spdata$HHID, spdata$useCase, sep="_")
      EAmonitoringIssue$index <- paste(EAmonitoringIssue$EA_Name, EAmonitoringIssue$EAID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$useCase, sep="_")
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[!EAmonitoringIssue$index %in% spdata$index, ])
      
      EAmonitoringIssue <- unique(rbind(EAmonitoringIssue, spdata))
      EAmonitoringIssue <- subset(EAmonitoringIssue, select=-c(index))
      write.csv(unique(EAmonitoringIssue), "EAmonitoringIssue.csv", row.names = FALSE)
      gs_upload("EAmonitoringIssue.csv", sheet_title = 'EAmonitoringIssue', overwrite = TRUE)
      
      
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Keep", ])
      EAmonitoringIssue <- EAmonitoringIssue[, colnames(dsEAHH)]
      dsEAHH <- rbind(dsEAHH, EAmonitoringIssue)
    }
  })
  
  
  
  output$ppditable <- DT::renderDataTable({
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      
      dsEAHH_DI$EAHHUC <- paste(dsEAHH_DI$EAID, dsEAHH_DI$EA_Name, dsEAHH_DI$HHID, dsEAHH_DI$HH_Name, dsEAHH_DI$useCase, sep="")
      EAmonitoringIssue$EAHHUC <- paste(EAmonitoringIssue$EAID, EAmonitoringIssue$EA_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$useCase, sep="")
      
      dsEAHH_DIRemain <- droplevels(dsEAHH_DI[!dsEAHH_DI$EAHHUC %in% EAmonitoringIssue$EAHHUC, ])
      dsEAHH_DI <- rbind(dsEAHH_DIRemain, EAmonitoringIssue)    
      dsEAHH_DI <- subset(dsEAHH_DI, select = -c(EAHHUC))
      
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("PP", "FR PP"), ]
      return(eadata)
      # tt <- formattable(eadata, list(
      #   data_issue = formatter("span",
      #                          style = x ~ style(color = ifelse(x == "multipleHHName", "darkgreen", 
      #                                                           ifelse(x == "multipleHHID", "purple", 
      #                                                                  ifelse(x == "multipleEAName", "brown", "orange"))))),
      #   Status = formatter("span",
      #                      style = x ~ style(color = ifelse(x == "Keep", "blue", "red")))
      # ))
      # as.datatable(tt, extensions = 'Buttons', options = list(pageLength = 10, dom = 'lBfrtip', buttons = c('copy', 'csv')))
    }
  })
  
  observeEvent(input$discard2,{
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("PP", "FR PP") ,]
      
      spdata <- data.frame(eadata[input$ppditable_rows_selected, ])
      spdata$Status <- "Remove"
      
      spdata$index <- paste(spdata$EA_Name, spdata$EAID, spdata$HH_Name, spdata$HHID, spdata$useCase, sep="_")
      EAmonitoringIssue$index <- paste(EAmonitoringIssue$EA_Name, EAmonitoringIssue$EAID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$useCase, sep="_")
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[!EAmonitoringIssue$index %in% spdata$index, ])
      
      EAmonitoringIssue <- unique(rbind(EAmonitoringIssue, spdata))
      EAmonitoringIssue <- subset(EAmonitoringIssue, select=-c(index))
      
      write.csv(unique(EAmonitoringIssue), "EAmonitoringIssue.csv", row.names = FALSE)
      gs_upload("EAmonitoringIssue.csv", sheet_title = 'EAmonitoringIssue', overwrite = TRUE)
      
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Remove", ])
      EAmonitoringIssue$EAHHUC <- paste(EAmonitoringIssue$EAID, EAmonitoringIssue$EA_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$useCase, sep="")
      dsEAHH$EAHHUC <- paste(dsEAHH$EAID, dsEAHH$EA_Name, dsEAHH$HHID, dsEAHH$HH_Name, dsEAHH$useCase, sep="")
      dsEAHH <- droplevels(dsEAHH[!dsEAHH$EAHHUC %in% EAmonitoringIssue$EAHHUC, ])
      
    }
  })
  
  
  observeEvent(input$Restore2,{
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("PP", "FR PP") ,]
      
      spdata <- data.frame(eadata[input$ppditable_rows_selected, ])
      spdata$Status <- "Keep"
      spdata$index <- paste(spdata$EA_Name, spdata$EAID, spdata$HH_Name, spdata$HHID, spdata$useCase, sep="_")
      EAmonitoringIssue$index <- paste(EAmonitoringIssue$EA_Name, EAmonitoringIssue$EAID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$useCase, sep="_")
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[!EAmonitoringIssue$index %in% spdata$index, ])
      
      EAmonitoringIssue <- unique(rbind(EAmonitoringIssue, spdata))
      EAmonitoringIssue <- subset(EAmonitoringIssue, select=-c(index))
      write.csv(unique(EAmonitoringIssue), "EAmonitoringIssue.csv", row.names = FALSE)
      gs_upload("EAmonitoringIssue.csv", sheet_title = 'EAmonitoringIssue', overwrite = TRUE)
      
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Keep", ])
      EAmonitoringIssue <- EAmonitoringIssue[, colnames(dsEAHH)]
      dsEAHH <- rbind(dsEAHH, EAmonitoringIssue)
    }
  })
  
  
  output$icditable <- DT::renderDataTable({
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      
      
      dsEAHH_DI$EAHHUC <- paste(dsEAHH_DI$EAID, dsEAHH_DI$EA_Name, dsEAHH_DI$HHID, dsEAHH_DI$HH_Name, dsEAHH_DI$useCase, sep="")
      EAmonitoringIssue$EAHHUC <- paste(EAmonitoringIssue$EAID, EAmonitoringIssue$EA_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$useCase, sep="")
      
      dsEAHH_DIRemain <- droplevels(dsEAHH_DI[!dsEAHH_DI$EAHHUC %in% EAmonitoringIssue$EAHHUC, ])
      dsEAHH_DI <- rbind(dsEAHH_DIRemain, EAmonitoringIssue)    
      dsEAHH_DI <- subset(dsEAHH_DI, select = -c(EAHHUC))
      
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("IC", "FR IC"), ]
      return(eadata)
      
      
    }
  })
  
  observeEvent(input$discard3,{
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("IC", "FR IC") ,]
      
      spdata <- data.frame(eadata[input$icditable_rows_selected, ])
      spdata$Status <- "Remove"
      spdata$index <- paste(spdata$EA_Name, spdata$EAID, spdata$HH_Name, spdata$HHID, spdata$useCase, sep="_")
      EAmonitoringIssue$index <- paste(EAmonitoringIssue$EA_Name, EAmonitoringIssue$EAID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$useCase, sep="_")
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[!EAmonitoringIssue$index %in% spdata$index, ])
      
      EAmonitoringIssue <- unique(rbind(EAmonitoringIssue, spdata))
      EAmonitoringIssue <- subset(EAmonitoringIssue, select=-c(index))
      
      write.csv(unique(EAmonitoringIssue), "EAmonitoringIssue.csv", row.names = FALSE)
      gs_upload("EAmonitoringIssue.csv", sheet_title = 'EAmonitoringIssue', overwrite = TRUE)
      
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Remove", ])
      EAmonitoringIssue$EAHHUC <- paste(EAmonitoringIssue$EAID, EAmonitoringIssue$EA_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$useCase, sep="")
      dsEAHH$EAHHUC <- paste(dsEAHH$EAID, dsEAHH$EA_Name, dsEAHH$HHID, dsEAHH$HH_Name, dsEAHH$useCase, sep="")
      dsEAHH <- droplevels(dsEAHH[!dsEAHH$EAHHUC %in% EAmonitoringIssue$EAHHUC, ])
      
    }
  })
  
  observeEvent(input$Restore3,{
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("IC", "FR IC") ,]
      
      spdata <- data.frame(eadata[input$icditable_rows_selected, ])
      spdata$Status <- "Keep"
      spdata$index <- paste(spdata$EA_Name, spdata$EAID, spdata$HH_Name, spdata$HHID, spdata$useCase, sep="_")
      EAmonitoringIssue$index <- paste(EAmonitoringIssue$EA_Name, EAmonitoringIssue$EAID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$useCase, sep="_")
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[!EAmonitoringIssue$index %in% spdata$index, ])
      
      EAmonitoringIssue <- unique(rbind(EAmonitoringIssue, spdata))
      EAmonitoringIssue <- subset(EAmonitoringIssue, select=-c(index))
      write.csv(unique(EAmonitoringIssue), "EAmonitoringIssue.csv", row.names = FALSE)
      gs_upload("EAmonitoringIssue.csv", sheet_title = 'EAmonitoringIssue', overwrite = TRUE)
      
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Keep", ])
      EAmonitoringIssue <- EAmonitoringIssue[, colnames(dsEAHH)]
      dsEAHH <- rbind(dsEAHH, EAmonitoringIssue)
      
      
      
    }
  })
  
  
  
  
  output$sphsditable <- DT::renderDataTable({
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      
      dsEAHH_DI$EAHHUC <- paste(dsEAHH_DI$EAID, dsEAHH_DI$EA_Name, dsEAHH_DI$HHID, dsEAHH_DI$HH_Name, dsEAHH_DI$useCase, sep="")
      EAmonitoringIssue$EAHHUC <- paste(EAmonitoringIssue$EAID, EAmonitoringIssue$EA_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$useCase, sep="")
      
      dsEAHH_DIRemain <- droplevels(dsEAHH_DI[!dsEAHH_DI$EAHHUC %in% EAmonitoringIssue$EAHHUC, ])
      dsEAHH_DI <- rbind(dsEAHH_DIRemain, EAmonitoringIssue)    
      dsEAHH_DI <- subset(dsEAHH_DI, select = -c(EAHHUC))
      
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("SP"), ]
      return(eadata)
      # tt <- formattable(eadata, list(
      #   data_issue = formatter("span",
      #                          style = x ~ style(color = ifelse(x == "multipleHHName", "darkgreen", 
      #                                                           ifelse(x == "multipleHHID", "purple", 
      #                                                                  ifelse(x == "multipleEAName", "brown", "orange"))))),
      #   Status = formatter("span",
      #                      style = x ~ style(color = ifelse(x == "Keep", "blue", "red")))
      # ))
      # as.datatable(tt, extensions = 'Buttons', options = list(pageLength = 10, dom = 'lBfrtip', buttons = c('copy', 'csv')))
    }
  })
  
  observeEvent(input$discard4,{
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("SP") ,]
      
      spdata <- data.frame(eadata[input$sphsditable_rows_selected, ])
      spdata$Status <- "Remove"
      
      spdata$index <- paste(spdata$EA_Name, spdata$EAID, spdata$HH_Name, spdata$HHID, spdata$useCase, sep="_")
      EAmonitoringIssue$index <- paste(EAmonitoringIssue$EA_Name, EAmonitoringIssue$EAID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$useCase, sep="_")
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[!EAmonitoringIssue$index %in% spdata$index, ])
      
      EAmonitoringIssue <- unique(rbind(EAmonitoringIssue, spdata))
      EAmonitoringIssue <- subset(EAmonitoringIssue, select=-c(index))
      
      write.csv(unique(EAmonitoringIssue), "EAmonitoringIssue.csv", row.names = FALSE)
      gs_upload("EAmonitoringIssue.csv", sheet_title = 'EAmonitoringIssue', overwrite = TRUE)
      
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Remove", ])
      EAmonitoringIssue$EAHHUC <- paste(EAmonitoringIssue$EAID, EAmonitoringIssue$EA_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$useCase, sep="")
      dsEAHH$EAHHUC <- paste(dsEAHH$EAID, dsEAHH$EA_Name, dsEAHH$HHID, dsEAHH$HH_Name, dsEAHH$useCase, sep="")
      dsEAHH <- droplevels(dsEAHH[!dsEAHH$EAHHUC %in% EAmonitoringIssue$EAHHUC, ])
      
    }
  })
  
  
  observeEvent(input$Restore4,{
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("SP") ,]
      
      spdata <- data.frame(eadata[input$sphsditable_rows_selected, ])
      spdata$Status <- "Keep"
      spdata$index <- paste(spdata$EA_Name, spdata$EAID, spdata$HH_Name, spdata$HHID, spdata$useCase, sep="_")
      EAmonitoringIssue$index <- paste(EAmonitoringIssue$EA_Name, EAmonitoringIssue$EAID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$useCase, sep="_")
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[!EAmonitoringIssue$index %in% spdata$index, ])
      
      EAmonitoringIssue <- unique(rbind(EAmonitoringIssue, spdata))
      EAmonitoringIssue <- subset(EAmonitoringIssue, select=-c(index))
      write.csv(unique(EAmonitoringIssue), "EAmonitoringIssue.csv", row.names = FALSE)
      gs_upload("EAmonitoringIssue.csv", sheet_title = 'EAmonitoringIssue', overwrite = TRUE)
      
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Keep", ])
      EAmonitoringIssue <- EAmonitoringIssue[, colnames(dsEAHH)]
      dsEAHH <- rbind(dsEAHH, EAmonitoringIssue)
    }
  })
  
  output$CISditable <- DT::renderDataTable({
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      
      
      dsEAHH_DI$EAHHUC <- paste(dsEAHH_DI$EAID, dsEAHH_DI$EA_Name, dsEAHH_DI$HHID, dsEAHH_DI$HH_Name, dsEAHH_DI$useCase, sep="")
      EAmonitoringIssue$EAHHUC <- paste(EAmonitoringIssue$EAID, EAmonitoringIssue$EA_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$useCase, sep="")
      
      dsEAHH_DIRemain <- droplevels(dsEAHH_DI[!dsEAHH_DI$EAHHUC %in% EAmonitoringIssue$EAHHUC, ])
      dsEAHH_DI <- rbind(dsEAHH_DIRemain, EAmonitoringIssue)    
      dsEAHH_DI <- subset(dsEAHH_DI, select = -c(EAHHUC))
      
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("IC"), ]
      return(eadata)
      
      
    }
  })
  
  observeEvent(input$discard5,{
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("IC") ,]
      
      spdata <- data.frame(eadata[input$CISditable_rows_selected, ])
      spdata$Status <- "Remove"
      
      spdata$index <- paste(spdata$EA_Name, spdata$EAID, spdata$HH_Name, spdata$HHID, spdata$useCase, sep="_")
      EAmonitoringIssue$index <- paste(EAmonitoringIssue$EA_Name, EAmonitoringIssue$EAID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$useCase, sep="_")
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[!EAmonitoringIssue$index %in% spdata$index, ])
      
      EAmonitoringIssue <- unique(rbind(EAmonitoringIssue, spdata))
      EAmonitoringIssue <- subset(EAmonitoringIssue, select=-c(index))
      
      write.csv(unique(EAmonitoringIssue), "EAmonitoringIssue.csv", row.names = FALSE)
      gs_upload("EAmonitoringIssue.csv", sheet_title = 'EAmonitoringIssue', overwrite = TRUE)
      
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Remove", ])
      EAmonitoringIssue$EAHHUC <- paste(EAmonitoringIssue$EAID, EAmonitoringIssue$EA_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$useCase, sep="")
      dsEAHH$EAHHUC <- paste(dsEAHH$EAID, dsEAHH$EA_Name, dsEAHH$HHID, dsEAHH$HH_Name, dsEAHH$useCase, sep="")
      dsEAHH <- droplevels(dsEAHH[!dsEAHH$EAHHUC %in% EAmonitoringIssue$EAHHUC, ])
      
    }
  })
  
  
  observeEvent(input$Restore5,{
    if(!is.null(input$eaidsdi) & !is.null(input$Countrydi) & !is.null(input$Partnerdi) & !is.null(input$di) & !is.null(input$eaidsdi) & !is.null(input$hhidsdi)){
      EAmonitoringIssue <- getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRG5zzZudKHeUxzGNjw6jEVz6wCwGG5FeIz7dUfYUvhNmuoV1Xl0h2E__mfNsmHTkWq4DqIAV9Uqte0/pub?output=csv")
      EAmonitoringIssue <- read.csv(textConnection(EAmonitoringIssue))
      eadata <- droplevels(dsEAHH_DI[dsEAHH_DI$Country == input$Countrydi, ])
      if(input$Partnerdi != "All"){
        eadata <- droplevels(eadata[eadata$EA_Partner == input$Partnerdi,])
      }
      if(input$di != "All"){
        eadata <- droplevels(eadata[eadata$data_issue == input$di,])
      }
      if(input$eaidsdi != "All"){
        eadata <- eadata[eadata$EAID == input$eaidsdi, ]
      }
      if(input$hhidsdi != "All"){
        eadata <- eadata[eadata$HHID == input$hhidsdi, ]
      }
      
      eadata <- eadata[eadata$useCase %in% c("IC") ,]
      
      spdata <- data.frame(eadata[input$CISditable_rows_selected, ])
      spdata$Status <- "Keep"
      spdata$index <- paste(spdata$EA_Name, spdata$EAID, spdata$HH_Name, spdata$HHID, spdata$useCase, sep="_")
      EAmonitoringIssue$index <- paste(EAmonitoringIssue$EA_Name, EAmonitoringIssue$EAID, EAmonitoringIssue$HH_Name, EAmonitoringIssue$HHID, EAmonitoringIssue$useCase, sep="_")
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[!EAmonitoringIssue$index %in% spdata$index, ])
      
      EAmonitoringIssue <- unique(rbind(EAmonitoringIssue, spdata))
      EAmonitoringIssue <- subset(EAmonitoringIssue, select=-c(index))
      write.csv(unique(EAmonitoringIssue), "EAmonitoringIssue.csv", row.names = FALSE)
      gs_upload("EAmonitoringIssue.csv", sheet_title = 'EAmonitoringIssue', overwrite = TRUE)
      
      EAmonitoringIssue <- droplevels(EAmonitoringIssue[EAmonitoringIssue$Status == "Keep", ])
      EAmonitoringIssue <- EAmonitoringIssue[, colnames(dsEAHH)]
      dsEAHH <- rbind(dsEAHH, EAmonitoringIssue)
      
      
      
    }
  })
  
})


