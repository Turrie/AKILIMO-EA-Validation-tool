library(lubridate)

getLatestPlDate <- function(udata){
  onpldate <- NULL
  for(hids in unique(udata$HHID)){
    hdata <- udata[udata$HHID == hids, ]
    hdata$rtm <- lubridate::month(mdy(hdata$plantingDate))
    hdata$rtd <- lubridate::day(mdy(hdata$plantingDate))
    hdata$rty <- lubridate::year(mdy(hdata$plantingDate))
    hdata$rtdmy <- ifelse(is.na(hdata$rtm), NA, paste(hdata$rty, hdata$rtm, hdata$rtd, sep = "/"))
    hdata$rtdmy <-  as.Date(hdata$rtdmy)
    hdata <- hdata[order(hdata$rtdmy, decreasing = TRUE), ][1,]
    onpldate <- rbind(onpldate, hdata)
  }
  
  onpldate <- subset(onpldate, select = -c(rtm, rtd, rty, rtdmy))
  
  return(onpldate)
}


getLatestsubDate <- function(udata){
  onsubdate <- NULL
  for(hids in unique(udata$HHID)){
    hdata <- udata[udata$HHID == hids, ]
    hdata$rtm <- lubridate::month(mdy(hdata$f0ymd))
    hdata$rtd <- lubridate::day(mdy(hdata$f0ymd))
    hdata$rty <- lubridate::year(mdy(hdata$f0ymd))
    hdata$rtdmy <- ifelse(is.na(hdata$rtm), NA, paste(hdata$rty, hdata$rtm, hdata$rtd, sep = "/"))
    hdata$rtdmy <-  as.Date(hdata$rtdmy)
    hdata <- hdata[order(hdata$rtdmy, decreasing = TRUE), ][1,]
    onsubdate <- rbind(onsubdate, hdata)
  }
  
  onsubdate <- subset(onsubdate, select = -c(rtm, rtd, rty, rtdmy))
  
  return(onsubdate)
}



getLatestICDate <- function(udata){
  onICdate <- NULL
  for(hids in udata(udata$HHID)){
    hdata <- udata[udata$HHID == hids, ]
    
    hdata <- hdata[order(hdata$plannedplantingdate, decreasing = TRUE), ][1,]
    onICdate <- rbind(onICdate, hdata)
  }
  
  onICdate <- subset(onICdate, select = -c(rtm, rtd, rty, rtdmy))
  
  return(onICdate)
}

### taking latest dates per event
#'@param usecasedata: data FR, IC or PP
#'@param usecase is "FR', "IC" or "PP"
#'@return per HHID and per event it selects the most recent date in case of multiple dates being attached per event.
#usecasedata = dsCISactual
EventLatestDate <- function(usecasedata, usecase){
  takeLatest <- NULL
  for(hids in unique(usecasedata$HHID)){
    hrdates <- usecasedata[usecasedata$HHID == hids, ]
    if(nrow(hrdates[hrdates$actualplantingdate != "", ]) > 1){
      hrdates$plm <- lubridate::month(mdy(hrdates$actualplantingdate))
      hrdates$pld <- lubridate::day(mdy(hrdates$actualplantingdate))
      hrdates$ply <- lubridate::year(mdy(hrdates$actualplantingdate))
      hrdates$pldmy <- ifelse(is.na(hrdates$plm), NA, paste(hrdates$ply, hrdates$plm, hrdates$pld, sep = "/"))
      hrdates$pldmy <-  as.Date(hrdates$pldmy)
      hrdatespl <- hrdates[order(hrdates$pldmy, decreasing = TRUE), ][1, ]
      hrdatespl <- subset(hrdatespl, select = -c(plm, pld, ply, pldmy))
      hrdates <- subset(hrdates, select = -c(plm, pld, ply, pldmy))
    }else{
      hrdatespl <- hrdates[hrdates$actualplantingdate != "", ]
    }
    if(nrow(hrdatespl)>0){
      hhp <- as.data.frame(matrix(ncol=ncol(hrdatespl), nrow=1, data=''))
      names(hhp) <- names(hrdatespl)
      hhp$HHID <- hids
      hhp$actualplantingdate<- hrdatespl$actualplantingdate
      hrdatespl <- hhp
    }
    
    
    if(nrow(hrdates[hrdates$gapping_date != "", ]) > 1){
      hrdates$gm <- lubridate::month(mdy(hrdates$gapping_date))
      hrdates$gd <- lubridate::day(mdy(hrdates$gapping_date))
      hrdates$gy <- lubridate::year(mdy(hrdates$gapping_date))
      hrdates$gdmy <- ifelse(is.na(hrdates$gm), NA, paste(hrdates$gy, hrdates$gm, hrdates$gd, sep = "/"))
      hrdates$gdmy <-  as.Date(hrdates$gdmy)
      hrdatesgm <- hrdates[order(hrdates$gdmy, decreasing = TRUE), ][1, ]
      hrdatesgm <- subset(hrdatesgm, select =-c(gm, gd, gy, gdmy))
      hrdates <- subset(hrdates, select =-c(gm, gd, gy, gdmy))
    }else{
      hrdatesgm <- hrdates[hrdates$gapping_date != "", ]
    }
    if(nrow(hrdatesgm)>0){
      hhp <- as.data.frame(matrix(ncol=ncol(hrdatesgm), nrow=1, data=''))
      names(hhp) <- names(hrdatesgm)
      hhp$HHID <- hids
      hhp$gapping_date<- hrdatesgm$gapping_date
      hrdatesgm <- hhp
    }
    
    if(nrow(hrdates[hrdates$weeding1 != "", ]) > 1){
      hrdates$w1m <- lubridate::month(mdy(hrdates$weeding1))
      hrdates$w1d <- lubridate::day(mdy(hrdates$weeding1))
      hrdates$w1y <- lubridate::year(mdy(hrdates$weeding1))
      hrdates$w1dmy <- ifelse(is.na(hrdates$w1m), NA, paste(hrdates$w1y, hrdates$w1m, hrdates$w1d, sep = "/"))
      hrdates$w1dmy <-  as.Date(hrdates$w1dmy)
      hrdatesw1 <- hrdates[order(hrdates$w1dmy, decreasing = TRUE), ][1, ]
      hrdatesw1 <- subset(hrdatesw1, select =-c(w1m, w1d, w1y, w1dmy))
      hrdates <- subset(hrdates, select =-c(w1m, w1d, w1y, w1dmy))
    }else{
      hrdatesw1 <- hrdates[hrdates$weeding1 != "", ]
    }
    if(nrow(hrdatesw1)>0){
      hhp <- as.data.frame(matrix(ncol=ncol(hrdatesw1), nrow=1, data=''))
      names(hhp) <- names(hrdatesw1)
      hhp$HHID <- hids
      hhp$weeding1<- hrdatesw1$weeding1
      hrdatesw1 <- hhp
    }
    
    
    if(nrow(hrdates[hrdates$weeding2 != "", ]) > 1){
      hrdates$wm <- lubridate::month(mdy(hrdates$weeding2))
      hrdates$wd <- lubridate::day(mdy(hrdates$weeding2))
      hrdates$wy <- lubridate::year(mdy(hrdates$weeding2))
      hrdates$wdmy <- ifelse(is.na(hrdates$wm), NA, paste(hrdates$wy, hrdates$wm, hrdates$wd, sep = "/"))
      hrdates$wdmy <-  as.Date(hrdates$wdmy)
      hrdatesw <- hrdates[order(hrdates$wdmy, decreasing = TRUE), ][1,]
      hrdatesw <- subset(hrdatesw, select=-c(wm, wd, wy, wdmy))
      hrdates <- subset(hrdates, select=-c(wm, wd, wy, wdmy))
    }else{
      hrdatesw <- hrdates[hrdates$weeding2 != "", ]
    }
    if(nrow(hrdatesw)>0){
      hhp <- as.data.frame(matrix(ncol=ncol(hrdatesw), nrow=1, data=''))
      names(hhp) <- names(hrdatesw)
      hhp$HHID <- hids
      hhp$weeding2 <- hrdatesw$weeding2
      hrdatesw <- hhp
    }
    
    
    
    if(nrow(hrdates[hrdates$weeding3 != "", ]) > 1){
      hrdates$w3m <- lubridate::month(mdy(hrdates$weeding3))
      hrdates$w3d <- lubridate::day(mdy(hrdates$weeding3))
      hrdates$w3y <- lubridate::year(mdy(hrdates$weeding3))
      hrdates$w3dmy <- ifelse(is.na(hrdates$w3m), NA, paste(hrdates$w3y, hrdates$w3m, hrdates$w3d, sep = "/"))
      hrdates$w3dmy <-  as.Date(hrdates$w3dmy)
      hrdatesw3 <- hrdates[order(hrdates$w3dmy, decreasing = TRUE), ][1,]
      hrdatesw3 <- subset(hrdatesw3, select=-c(w3m, w3d, w3y, w3dmy))
      hrdates <- subset(hrdates, select=-c(w3m, w3d, w3y, w3dmy))
    }else{
      hrdatesw3 <- hrdates[hrdates$weeding3 != "", ]
    }
    if(nrow(hrdatesw3)>0){
      hhp <- as.data.frame(matrix(ncol=ncol(hrdatesw3), nrow=1, data=''))
      names(hhp) <- names(hrdatesw3)
      hhp$HHID <- hids
      hhp$weeding3 <- hrdatesw3$weeding3
      hrdatesw3 <- hhp
    }
    
    
    if(nrow(hrdates[hrdates$cassavahvstdate != "", ]) > 1){
      hrdates$hm <- lubridate::month(mdy(hrdates$cassavahvstdate))
      hrdates$hd <- lubridate::day(mdy(hrdates$cassavahvstdate))
      hrdates$hy <- lubridate::year(mdy(hrdates$cassavahvstdate))
      hrdates$hdmy <- ifelse(is.na(hrdates$hm), NA, paste(hrdates$hy, hrdates$hm, hrdates$hd, sep = "/"))
      hrdates$hdmy <-  as.Date(hrdates$hdmy)
      hrdatesh3 <- hrdates[order(hrdates$hdmy, decreasing = TRUE), ][1,]
      hrdatesh3 <- subset(hrdatesh3, select=-c(hm, hd, hy, hdmy))
      hrdates <- subset(hrdates, select=-c(hm, hd, hy, hdmy))
    }else{
      hrdatesh3 <- hrdates[hrdates$cassavahvstdate != "", ]
    }
    if(nrow(hrdatesh3) > 0){
      hhp <- as.data.frame(matrix(ncol=ncol(hrdatesh3), nrow=1, data=''))
      names(hhp) <- names(hrdatesh3)
      hhp$HHID <- hids
      hhp$cassavahvstdate <- hrdatesh3$cassavahvstdate
      hrdatesh3 <- hhp
    }

    if(usecase %in% c("FR", "IC")){
      if(nrow(hrdates[hrdates$fertlizerapp1 != "", ]) > 1){
        hrdates$fm <- lubridate::month(mdy(hrdates$fertlizerapp1))
        hrdates$fd <- lubridate::day(mdy(hrdates$fertlizerapp1))
        hrdates$fy <- lubridate::year(mdy(hrdates$fertlizerapp1))
        hrdates$fdmy <- ifelse(is.na(hrdates$fm), NA, paste(hrdates$fy, hrdates$fm, hrdates$fd, sep = "/"))
        hrdates$fdmy <-  as.Date(hrdates$fdmy)
        hrdatesf <- hrdates[order(hrdates$fdmy, decreasing = TRUE), ][1, ]
        hrdatesf <- subset(hrdatesf, select =-c(fm, fd, fy, fdmy))
        hrdates <- subset(hrdates, select =-c(fm, fd, fy, fdmy))
      }else{
        hrdatesf <- hrdates[hrdates$fertlizerapp1 != "", ]
      }
      if(nrow(hrdatesf)>0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdatesf), nrow=1, data=''))
        names(hhp) <- names(hrdatesf)
        hhp$HHID <- hids
        hhp$fertlizerapp1<- hrdatesf$fertlizerapp1
        hrdatesf <- hhp
      }
      
      
    }
    
    
    if(usecase == "FR"){
      if(nrow(hrdates[hrdates$fertlizerapp2 != "", ]) > 1){
        hrdates$f2m <- lubridate::month(mdy(hrdates$fertlizerapp2))
        hrdates$f2d <- lubridate::day(mdy(hrdates$fertlizerapp2))
        hrdates$f2y <- lubridate::year(mdy(hrdates$fertlizerapp2))
        hrdates$f2dmy <- ifelse(is.na(hrdates$f2m), NA, paste(hrdates$f2y, hrdates$f2m, hrdates$f2d, sep = "/"))
        hrdates$f2dmy <-  as.Date(hrdates$f2dmy)
        hrdatesf2 <- hrdates[order(hrdates$f2dmy, decreasing = TRUE), ][1, ]
        hrdatesf2 <- subset(hrdatesf2, select =-c(f2m, f2d, f2y, f2dmy))
        hrdates <- subset(hrdates, select =-c(f2m, f2d, f2y, f2dmy))
      }else{
        hrdatesf2 <- hrdates[hrdates$fertlizerapp2 != "", ]
      }
      if(nrow(hrdatesf2)>0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdatesf2), nrow=1, data=''))
        names(hhp) <- names(hrdatesf2)
        hhp$HHID <- hids
        hhp$fertlizerapp2<- hrdatesf2$fertlizerapp2
        hrdatesf2 <- hhp
      }
    }
    
    
    if(usecase == "FR"){
      if(nrow(hrdates[hrdates$harvestdate != "", ]) > 1){
        hrdates$hm <- lubridate::month(mdy(hrdates$harvestdate))
        hrdates$hd <- lubridate::day(mdy(hrdates$harvestdate))
        hrdates$hy <- lubridate::year(mdy(hrdates$harvestdate))
        hrdates$hdmy <- ifelse(is.na(hrdates$hm), NA, paste(hrdates$hy, hrdates$hm, hrdates$hd, sep = "/"))
        hrdates$hdmy <-  as.Date(hrdates$hdmy)
        hrdatesh2 <- hrdates[order(hrdates$hdmy, decreasing = TRUE), ][1, ]
        hrdatesh2 <- subset(hrdatesh2, select =-c(hm, hd, hy, hdmy))
        hrdates <- subset(hrdates, select =-c(hm, hd, hy, hdmy))
      }else{
        hrdatesh2 <- hrdates[hrdates$harvestdate != "", ]
      }
      if(nrow(hrdatesh2)>0){
        hhhz <- as.data.frame(matrix(ncol=ncol(hrdatesh2), nrow=1, data=''))
        names(hhhz) <- names(hrdatesh2)
        hhhz$HHID <- hids
        hhhz$harvestdate<- hrdatesh2$harvestdate
        hrdatesh2 <- hhhz
      }
    }
    
    if(usecase == c("IC")){
      if(nrow(hrdates[hrdates$fertlizerapp0 != "", ]) > 1){
        hrdates$f0m <- lubridate::month(mdy(hrdates$fertlizerapp0))
        hrdates$f0d <- lubridate::day(mdy(hrdates$fertlizerapp0))
        hrdates$f0y <- lubridate::year(mdy(hrdates$fertlizerapp0))
        hrdates$f0dmy <- ifelse(is.na(hrdates$f0m), NA, paste(hrdates$f0y, hrdates$f0m, hrdates$f0d, sep = "/"))
        hrdates$f0dmy <-  as.Date(hrdates$f0dmy)
        hrdatesf0 <- hrdates[order(hrdates$f0dmy, decreasing = TRUE), ][1, ]
        hrdatesf0 <- subset(hrdatesf0, select =-c(f0m, f0d, f0y, f0dmy))
        hrdates <- subset(hrdates, select =-c(f0m, f0d, f0y, f0dmy))
      }else{
        hrdatesf0 <- hrdates[hrdates$fertlizerapp0 != "", ]
      }
      if(nrow(hrdatesf0)>0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdatesf0), nrow=1, data=''))
        names(hhp) <- names(hrdatesf0)
        hhp$HHID <- hids
        hhp$fertlizerapp0<- hrdatesf0$fertlizerapp0
        hrdatesf0 <- hhp
      }
   
 
    
    }
    
    
    if(usecase == "IC"){
      if(nrow(hrdates[hrdates$maizehvstdate != "", ]) > 1){
        hrdates$hmm <- lubridate::month(mdy(hrdates$maizehvstdate))
        hrdates$hmd <- lubridate::day(mdy(hrdates$maizehvstdate))
        hrdates$hmy <- lubridate::year(mdy(hrdates$maizehvstdate))
        hrdates$hmdmy <- ifelse(is.na(hrdates$hmm), NA, paste(hrdates$hmy, hrdates$hmm, hrdates$hmd, sep = "/"))
        hrdates$hmdmy <-  as.Date(hrdates$hmdmy)
        hrdateshm <- hrdates[order(hrdates$hmdmy, decreasing = TRUE), ][1,]
        hrdateshm <- subset(hrdateshm, select=-c(hmm, hmd, hmy, hmdmy))
        hrdates <- subset(hrdates, select=-c(hmm, hmd, hmy, hmdmy))
      }else{
        hrdateshm <- hrdates[hrdates$maizehvstdate != "", ]
      }
      if(nrow(hrdateshm) > 0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdateshm), nrow=1, data=''))
        names(hhp) <- names(hrdateshm)
        hhp$HHID <- hids
        hhp$maizehvstdate <- hrdateshm$maizehvstdate
        hrdateshm <- hhp
      }
      
      
      if(nrow(hrdates[hrdates$reseedingdate != "", ]) > 1){
        hrdates$rm <- lubridate::month(mdy(hrdates$reseedingdate))
        hrdates$rd <- lubridate::day(mdy(hrdates$reseedingdate))
        hrdates$ry <- lubridate::year(mdy(hrdates$reseedingdate))
        hrdates$rdmy <- ifelse(is.na(hrdates$rm), NA, paste(hrdates$ry, hrdates$rm, hrdates$rd, sep = "/"))
        hrdates$rdmy <-  as.Date(hrdates$rdmy)
        hrdatesr <- hrdates[order(hrdates$rdmy, decreasing = TRUE), ][1,]
        hrdatesr <- subset(hrdatesr, select=-c(rm, rd, ry, rdmy))
        hrdates <- subset(hrdates, select=-c(rm, rd, ry, rdmy)) 
      }else{
        hrdatesr <- hrdates[hrdates$reseedingdate != "", ]
      }
      if(nrow(hrdatesr) > 0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdatesr), nrow=1, data=''))
        names(hhp) <- names(hrdatesr)
        hhp$HHID <- hids
        hhp$reseedingdate <- hrdatesr$reseedingdate
        hrdatesr <- hhp
      }
      
      
      if(nrow(hrdates[hrdates$thinningdate != "", ]) > 1){
        hrdates$rtm <- lubridate::month(mdy(hrdates$thinningdate))
        hrdates$rtd <- lubridate::day(mdy(hrdates$thinningdate))
        hrdates$rty <- lubridate::year(mdy(hrdates$thinningdate))
        hrdates$rtdmy <- ifelse(is.na(hrdates$rtm), NA, paste(hrdates$rty, hrdates$rtm, hrdates$rtd, sep = "/"))
        hrdates$rtdmy <-  as.Date(hrdates$rtdmy)
        hrdatesrt <- hrdates[order(hrdates$rtdmy, decreasing = TRUE), ][1,]
        hrdatesrt <- subset(hrdatesrt, select=-c(rtm, rtd, rty, rtdmy))
        hrdates <- subset(hrdates, select=-c(rtm, rtd, rty, rtdmy))
      }else{
        hrdatesrt <- hrdates[hrdates$thinningdate != "", ]
      }
      if(nrow(hrdatesrt) >0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdatesrt), nrow=1, data=''))
        names(hhp) <- names(hrdatesrt)
        hhp$HHID <- hids
        hhp$thinningdate <- hrdatesrt$thinningdate
        hrdatesrt <- hhp
      }
      
    }
    
    
    if(usecase == "IC"){
      if(nrow(hrdates[hrdates$sweetPotatoReplantingDate != "", ]) > 1){
        hrdates$rtm <- lubridate::month(mdy(hrdates$sweetPotatoReplantingDate))
        hrdates$rtd <- lubridate::day(mdy(hrdates$sweetPotatoReplantingDate))
        hrdates$rty <- lubridate::year(mdy(hrdates$sweetPotatoReplantingDate))
        hrdates$rtdmy <- ifelse(is.na(hrdates$rtm), NA, paste(hrdates$rty, hrdates$rtm, hrdates$rtd, sep = "/"))
        hrdates$rtdmy <-  as.Date(hrdates$rtdmy)
        hrdateswt <- hrdates[order(hrdates$rtdmy, decreasing = TRUE), ][1,]
        hrdateswt <- subset(hrdateswt, select=-c(rtm, rtd, rty, rtdmy))
        hrdates <- subset(hrdates, select=-c(rtm, rtd, rty, rtdmy))
      }else{
        hrdateswt <- hrdates[hrdates$sweetPotatoReplantingDate != "", ]
      }
      if(nrow(hrdateswt) >0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdateswt), nrow=1, data=''))
        names(hhp) <- names(hrdateswt)
        hhp$HHID <- hrdateswt$HHID
        hhp$sweetPotatoReplantingDate <- hrdateswt$sweetPotatoReplantingDate
        hrdateswt <- hhp
      }
      
      
      if(nrow(hrdates[hrdates$sweetpotatohvstdate != "", ]) > 1){
        hrdates$rtm <- lubridate::month(mdy(hrdates$sweetpotatohvstdate))
        hrdates$rtd <- lubridate::day(mdy(hrdates$sweetpotatohvstdate))
        hrdates$rty <- lubridate::year(mdy(hrdates$sweetpotatohvstdate))
        hrdates$rtdmy <- ifelse(is.na(hrdates$rtm), NA, paste(hrdates$rty, hrdates$rtm, hrdates$rtd, sep = "/"))
        hrdates$rtdmy <-  as.Date(hrdates$rtdmy)
        hrdateshv <- hrdates[order(hrdates$rtdmy, decreasing = TRUE), ][1,]
        hrdateshv <- subset(hrdateshv, select=-c(rtm, rtd, rty, rtdmy))
        hrdates <- subset(hrdates, select=-c(rtm, rtd, rty, rtdmy))
      }else{
        hrdateshv <- hrdates[hrdates$sweetpotatohvstdate != "", ]
      }
      if(nrow(hrdateshv) >0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdateshv), nrow=1, data=''))
        names(hhp) <- names(hrdateshv)
        hhp$HHID <- hrdateshv$HHID
        hhp$sweetpotatohvstdate <- hrdateshv$sweetpotatohvstdate
        hrdateshv <- hhp
      }
      
     
      
      if(nrow(hrdates[hrdates$plantingDateCS != "", ]) > 1){
        hrdates$rtm <- lubridate::month(mdy(hrdates$plantingDateCS))
        hrdates$rtd <- lubridate::day(mdy(hrdates$plantingDateCS))
        hrdates$rty <- lubridate::year(mdy(hrdates$plantingDateCS))
        hrdates$rtdmy <- ifelse(is.na(hrdates$rtm), NA, paste(hrdates$rty, hrdates$rtm, hrdates$rtd, sep = "/"))
        hrdates$rtdmy <-  as.Date(hrdates$rtdmy)
        hrdateCS <- hrdates[order(hrdates$rtdmy, decreasing = TRUE), ][1,]
        hrdateCS <- subset(hrdateCS, select=-c(rtm, rtd, rty, rtdmy))
        hrdates <- subset(hrdates, select=-c(rtm, rtd, rty, rtdmy))
      }else{
        hrdateCS <- hrdates[hrdates$plantingDateCS != "", ]
      }
      if(nrow(hrdateCS) >0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdateCS), nrow=1, data=''))
        names(hhp) <- names(hrdateCS)
        hhp$HHID <- hrdateCS$HHID
        hhp$plantingDateCS <- hrdateCS$plantingDateCS
        hrdateCS <- hhp
      }
      
     library(lubridate) 
      
      if(nrow(hrdates[hrdates$plantingDateSP != "", ]) > 1){
        hrdates$rtm <- lubridate::month(mdy(hrdates$plantingDateSP))
        hrdates$rtd <- lubridate::day(mdy(hrdates$plantingDateSP))
        hrdates$rty <- lubridate::year(mdy(hrdates$plantingDateSP))
        hrdates$rtdmy <- ifelse(is.na(hrdates$rtm), NA, paste(hrdates$rty, hrdates$rtm, hrdates$rtd, sep = "/"))
        hrdates$rtdmy <-  as.Date(hrdates$rtdmy)
        hrdateSP <- hrdates[order(hrdates$rtdmy, decreasing = TRUE), ][1,]
        hrdateSP <- subset(hrdateswt, select=-c(rtm, rtd, rty, rtdmy))
        hrdates <- subset(hrdates, select=-c(rtm, rtd, rty, rtdmy))
      }else{
        hrdateSP <- hrdates[hrdates$plantingDateSP != "", ]
      }
      if(nrow(hrdateSP) >0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdateSP), nrow=1, data=''))
        names(hhp) <- names(hrdateSP)
        hhp$HHID <- hrdateSP$HHID
        hhp$plantingDateSP <- hrdateSP$plantingDateSP
        hrdateSP <- hhp
      }
    }
    
      if(usecase == "SP"){
      if(nrow(hrdates[hrdates$intHarvestDate_CON != "", ]) > 1){
        hrdates$hmm <- lubridate::month(mdy(hrdates$intHarvestDate_CON))
        hrdates$hmd <- lubridate::day(mdy(hrdates$intHarvestDate_CON))
        hrdates$hmy <- lubridate::year(mdy(hrdates$intHarvestDate_CON))
        hrdates$hmdmy <- ifelse(is.na(hrdates$hmm), NA, paste(hrdates$hmy, hrdates$hmm, hrdates$hmd, sep = "/"))
        hrdates$hmdmy <-  as.Date(hrdates$hmdmy)
        hrdatespm <- hrdates[order(hrdates$hmdmy, decreasing = TRUE), ][1,]
        hrdatespm <- subset(hrdatespm, select=-c(hmm, hmd, hmy, hmdmy))
        hrdates <- subset(hrdates, select=-c(hmm, hmd, hmy, hmdmy))
      }else{
        hrdatespm <- hrdates[hrdates$intHarvestDate_CON != "", ]
      }
      if(nrow(hrdatespm) > 0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdatespm), nrow=1, data=''))
        names(hhp) <- names(hrdatespm)
        hhp$HHID <- hids
        hhp$intHarvestDate_CON <- hrdatespm$intHarvestDate_CON
        hrdatespm <- hhp
      }
      
      
    
      if(nrow(hrdates[hrdates$intHarvestDate_REC != "", ]) > 1){
        hrdates$rm <- lubridate::month(mdy(hrdates$intHarvestDate_REC))
        hrdates$rd <- lubridate::day(mdy(hrdates$intHarvestDate_REC))
        hrdates$ry <- lubridate::year(mdy(hrdates$intHarvestDate_REC))
        hrdates$rdmy <- ifelse(is.na(hrdates$rm), NA, paste(hrdates$ry, hrdates$rm, hrdates$rd, sep = "/"))
        hrdates$rdmy <-  as.Date(hrdates$rdmy)
        hrdatesrp <- hrdates[order(hrdates$rdmy, decreasing = TRUE), ][1,]
        hrdatesrp <- subset(hrdatesrp, select=-c(rm, rd, ry, rdmy))
        hrdates <- subset(hrdates, select=-c(rm, rd, ry, rdmy)) 
      }else{
        hrdatesrp <- hrdates[hrdates$intHarvestDate_REC != "", ]
      }
      if(nrow(hrdatesrp) > 0){
        hhp <- as.data.frame(matrix(ncol=ncol(hrdatesrp), nrow=1, data=''))
        names(hhp) <- names(hrdatesrp)
        hhp$HHID <- hids
        hhp$intHarvestDate_REC <- hrdatesrp$intHarvestDate_REC
        hrdatesrp <- hhp
      }
    
    
    if(nrow(hrdates[hrdates$effHarvestDate_CON_Tri != "", ]) > 1){
      hrdates$com <- lubridate::month(mdy(hrdates$effHarvestDate_CON_Tri))
      hrdates$cod <- lubridate::day(mdy(hrdates$effHarvestDate_CON_Tri))
      hrdates$coy <- lubridate::year(mdy(hrdates$effHarvestDate_CON_Tri))
      hrdates$codmy <- ifelse(is.na(hrdates$com), NA, paste(hrdates$coy, hrdates$com, hrdates$cod, sep = "/"))
      hrdates$codmy <-  as.Date(hrdates$codmy)
      hrdatescm <- hrdates[order(hrdates$codmy, decreasing = TRUE), ][1,]
      hrdatescm <- subset(hrdatescm, select=-c(com, cod, coy, codmy))
      hrdates <- subset(hrdates, select=-c(com, cod, coy, codmy))
    }else{
      hrdatescm <- hrdates[hrdates$effHarvestDate_CON_Tri != "", ]
    }
    if(nrow(hrdatescm) > 0){
      hhc <- as.data.frame(matrix(ncol=ncol(hrdatescm), nrow=1, data=''))
      names(hhc) <- names(hrdatescm)
      hhc$HHID <- hids
      hhc$effHarvestDate_CON_Tri <- hrdatescm$effHarvestDate_CON_Tri
      hrdatescm <- hhc
    }
    
    
    
    if(nrow(hrdates[hrdates$effHarvestDate_REC_Tri != "", ]) > 1){
      hrdates$cmr <- lubridate::month(mdy(hrdates$effHarvestDate_REC_Tri))
      hrdates$cdr <- lubridate::day(mdy(hrdates$effHarvestDate_REC_Tri))
      hrdates$cyr <- lubridate::year(mdy(hrdates$effHarvestDate_REC_Tri))
      hrdates$cdmyr <- ifelse(is.na(hrdates$cmr), NA, paste(hrdates$cyr, hrdates$cmr, hrdates$cdr, sep = "/"))
      hrdates$cdmyr <-  as.Date(hrdates$cdmyr)
      hrdatesrc <- hrdates[order(hrdates$cdmyr, decreasing = TRUE), ][1,]
      hrdatesrc <- subset(hrdatesrc, select=-c(cmr, cdr, cyr, cdmyr))
      hrdates <- subset(hrdates, select=-c(cmr, cdr, cyr, cdmyr)) 
    }else{
      hrdatesrc <- hrdates[hrdates$effHarvestDate_REC_Tri != "", ]
    }
    if(nrow(hrdatesrc) > 0){
      hhc <- as.data.frame(matrix(ncol=ncol(hrdatesrc), nrow=1, data=''))
      names(hhc) <- names(hrdatesrc)
      hhc$HHID <- hids
      hhc$effHarvestDate_REC_Tri <- hrdatesrc$effHarvestDate_REC_Tri
      hrdatesrc <- hhc
    }
        
        if(nrow(hrdates[hrdates$intharvestdateCON != "", ]) > 1){
          hrdates$hmm <- lubridate::month(mdy(hrdates$intharvestdateCON))
          hrdates$hmd <- lubridate::day(mdy(hrdates$intharvestdateCON))
          hrdates$hmy <- lubridate::year(mdy(hrdates$intharvestdateCON))
          hrdates$hmdmy <- ifelse(is.na(hrdates$hmm), NA, paste(hrdates$hmy, hrdates$hmm, hrdates$hmd, sep = "/"))
          hrdates$hmdmy <-  as.Date(hrdates$hmdmy)
          hrdatespmc <- hrdates[order(hrdates$hmdmy, decreasing = TRUE), ][1,]
          hrdatespmc <- subset(hrdatespmc, select=-c(hmm, hmd, hmy, hmdmy))
          hrdates <- subset(hrdates, select=-c(hmm, hmd, hmy, hmdmy))
        }else{
          hrdatespmc <- hrdates[hrdates$intharvestdateCON != "", ]
        }
        if(nrow(hrdatespmc) > 0){
          hhp <- as.data.frame(matrix(ncol=ncol(hrdatespmc), nrow=1, data=''))
          names(hhp) <- names(hrdatespmc)
          hhp$HHID <- hids
          hhp$intharvestdateCON <- hrdatespmc$intharvestdateCON
          hrdatespmc <- hhp
        }
        
        
        
        if(nrow(hrdates[hrdates$intharvestdateREC != "", ]) > 1){
          hrdates$rm <- lubridate::month(mdy(hrdates$intharvestdateREC))
          hrdates$rd <- lubridate::day(mdy(hrdates$intharvestdateREC))
          hrdates$ry <- lubridate::year(mdy(hrdates$intharvestdateREC))
          hrdates$rdmy <- ifelse(is.na(hrdates$rm), NA, paste(hrdates$ry, hrdates$rm, hrdates$rd, sep = "/"))
          hrdates$rdmy <-  as.Date(hrdates$rdmy)
          hrdatesrpr <- hrdates[order(hrdates$rdmy, decreasing = TRUE), ][1,]
          hrdatesrpr <- subset(hrdatesrpr, select=-c(rm, rd, ry, rdmy))
          hrdates <- subset(hrdates, select=-c(rm, rd, ry, rdmy)) 
        }else{
          hrdatesrpr <- hrdates[hrdates$intharvestdateREC != "", ]
        }
        if(nrow(hrdatesrpr) > 0){
          hhp <- as.data.frame(matrix(ncol=ncol(hrdatesrpr), nrow=1, data=''))
          names(hhp) <- names(hrdatesrpr)
          hhp$HHID <- hids
          hhp$intharvestdateREC <- hrdatesrpr$intharvestdateREC
          hrdatesrpr <- hhp
        }
        
        
        if(nrow(hrdates[hrdates$effharvestdateCON != "", ]) > 1){
          hrdates$cmm <- lubridate::month(mdy(hrdates$effharvestdateCON))
          hrdates$cmd <- lubridate::day(mdy(hrdates$effharvestdateCON))
          hrdates$cmy <- lubridate::year(mdy(hrdates$effharvestdateCON))
          hrdates$cmdmy <- ifelse(is.na(hrdates$cmm), NA, paste(hrdates$cmy, hrdates$cmm, hrdates$cmd, sep = "/"))
          hrdates$cmdmy <-  as.Date(hrdates$cmdmy)
          hrdatescmco <- hrdates[order(hrdates$cmdmy, decreasing = TRUE), ][1,]
          hrdatescmco <- subset(hrdatescmco, select=-c(cmm, cmd, cmy, cmdmy))
          hrdates <- subset(hrdates, select=-c(cmm, cmd, cmy, cmdmy))
        }else{
          hrdatescmco <- hrdates[hrdates$effharvestdateCON != "", ]
        }
        if(nrow(hrdatescmco) > 0){
          hhc <- as.data.frame(matrix(ncol=ncol(hrdatescmco), nrow=1, data=''))
          names(hhc) <- names(hrdatescmco)
          hhc$HHID <- hids
          hhc$effharvestdateCON <- hrdatescmco$effharvestdateCON
          hrdatescmco <- hhc
        }
        
        
        
        if(nrow(hrdates[hrdates$effharvestdateREC != "", ]) > 1){
          hrdates$cm <- lubridate::month(mdy(hrdates$effharvestdateREC))
          hrdates$cd <- lubridate::day(mdy(hrdates$effharvestdateREC))
          hrdates$cy <- lubridate::year(mdy(hrdates$effharvestdateREC))
          hrdates$cdmy <- ifelse(is.na(hrdates$cm), NA, paste(hrdates$cy, hrdates$cm, hrdates$cd, sep = "/"))
          hrdates$cdmy <-  as.Date(hrdates$cdmy)
          hrdatesrcre <- hrdates[order(hrdates$cdmy, decreasing = TRUE), ][1,]
          hrdatesrcre <- subset(hrdatesrcre, select=-c(cm, cd, cy, cdmy))
          hrdates <- subset(hrdates, select=-c(cm, cd, cy, cdmy)) 
        }else{
          hrdatesrcre <- hrdates[hrdates$effharvestdateREC != "", ]
        }
        if(nrow(hrdatesrcre) > 0){
          hhc <- as.data.frame(matrix(ncol=ncol(hrdatesrcre), nrow=1, data=''))
          names(hhc) <- names(hrdatesrcre)
          hhc$HHID <- hids
          hhc$effharvestdateREC <- hrdatesrcre$effharvestdateREC
          hrdatesrcre <- hhc
        }
  }
      
    if(usecase == "FR"){
      takeLatest <- rbind(takeLatest, hrdatespl, hrdatesgm, hrdatesf, hrdatesf2, hrdatesw1, hrdatesw, hrdatesw3, hrdatesh2)
     }else if (usecase == "IC"){
      takeLatest <- rbind(takeLatest, hrdatespl, hrdateshv, hrdatesr,hrdateSP, hrdateCS, hrdateswt, hrdatesf0,hrdatesf, hrdatesw, hrdatesw1,
                          hrdatesw3, hrdatesgm, hrdatesr, hrdatesh3, hrdateshm, hrdatesrt )
      } else if(usecase == "PP"){
      takeLatest <- rbind(takeLatest, hrdatespl, hrdatesgm, hrdatesw1, hrdatesw, hrdatesw3, hrdatesh3)
    }else if(usecase == "SP"){
      #takeLatest <- rbind(takeLatest, hrdatespl, hrdatespm, hrdatesrp, hrdatescm, hrdatesrc)
      takeLatest <- rbind(takeLatest, hrdatespl, hrdatesgm, hrdatesw1, hrdatesw, hrdatesw3, hrdatesh3, 
                          hrdatespm, hrdatesrp, hrdatescm, hrdatespmc, hrdatesrpr, hrdatesrc, hrdatesrcre, hrdatescmco)
    }
  
  }
  takeLatest <- droplevels(takeLatest[!is.na(takeLatest$HHID), ])
  return(unique(takeLatest))
}



datesIn365 <- function(dsFRdate){
  
  date <- dsFRdate$date
  
  month <- dsFRdate$month
  
  leapyear <- dsFRdate$leapyear
  
  
  
  if(leapyear==TRUE){
    
    if(month == "Jan"){
      
      dd <- date
      
    }else if(month == "Feb"){
      
      dd <- date + 31
      
    }else if(month == "Mar"){
      
      dd <- date + 60
      
    }else if(month == "Apr"){
      
      dd <- date + 91
      
    }else if(month == "May"){
      
      dd <- date + 121
      
    }else if(month == "Jun"){
      
      dd <- date + 152
      
    }else if(month == "Jul"){
      
      dd <- date + 182
      
    }else if(month == "Aug"){
      
      dd <- date + 213
      
    }else if(month == "Sep"){
      
      dd <- date + 244
      
    }else if(month == "Oct"){
      
      dd <- date + 274
      
    }else if(month == "Nov"){
      
      dd <- date + 305
      
    }else if(month == "Dec"){
      
      dd <- date + 335
      
    }
    
  }else{
    
    if(month == "Jan"){
      
      dd <- date
      
    }else if(month == "Feb"){
      
      dd <- date + 31
      
    }else if(month == "Mar"){
      
      dd <- date + 59
      
    }else if(month == "Apr"){
      
      dd <- date + 90
      
    }else if(month == "May"){
      
      dd <- date + 120
      
    }else if(month == "Jun"){
      
      dd <- date + 151
      
    }else if(month == "Jul"){
      
      dd <- date + 181
      
    }else if(month == "Aug"){
      
      dd <- date + 212
      
    }else if(month == "Sep"){
      
      dd <- date + 243
      
    }else if(month == "Oct"){
      
      dd <- date + 273
      
    }else if(month == "Nov"){
      
      dd <- date + 304
      
    }else if(month == "Dec"){
      
      dd <- date + 334
      
    }
    
  }
  
  dsFRdate$date365 <- dd
  
  return(dsFRdate)
  
}
usecasedata <- "IC"

## Loop IC

#' Title
#'
#' @param colNr 
#'
#' @return
#' @export
#'
#' @examples solvedatesIC(colNr=1)
#' 


solvedates <- function(colNr, usecasedata){
  md <- droplevels(usecasedata[, c(1, colNr)])
  md <- md[md[,2] !="", ]
  md <- md[!is.na(md[,2]), ]
  if(nrow(md) > 0){
    prodata <- NULL
    for(i in 1: nrow(md)){
      Rdata <- droplevels(md[i, ])
      dss <- as.character(Rdata[,2])
      dss<- gsub(",", "", dss)
      plm <- lubridate::month(mdy(dss))
      pld <- lubridate::day(mdy(dss))
      ply <- lubridate::year(mdy(dss))
      # dsICdate <- data.frame(HHID = Rdata$HHID, month = strsplit(dss, " ")[[1]][1], date =strsplit(dss, " ")[[1]][2], year=strsplit(dss, " ")[[1]][3])
      pldmy <-  as.Date(paste(ply, plm, pld, sep = "/"), origin = "1970-01-01")
      Rdata[,2] <- pldmy
      
      
      # if(dsICdate$year %in% c(2016, 2020, 2024, 2028, 2032)){
      #   dsICdate$leapyear <- TRUE
      # }else{
      #   dsICdate$leapyear <- FALSE
      # }
      # dsICdate$date <- as.numeric(as.character(dsICdate$date))
      # 
      # dd <- datesIn365(dsICdate)
      # colnames(dd) <- c('HHID', 'month', 'date', 'year', 'leapyear',  names(Rdata)[2])
      # dd$Orig_date <- Rdata[,2]
      prodata <- rbind(prodata, Rdata)
    }
    return(prodata)
  }else{
    return(NULL)
  }
  
}

solvedates <- function(colNr, usecasedata){
  md <- droplevels(usecasedata[, c(1, colNr)])
  md <- md[md[,2] !="", ]
  md <- md[!is.na(md[,2]), ]
  if(nrow(md) > 0){
    prodata <- NULL
    for(i in 1: nrow(md)){
      Rdata <- droplevels(md[i, ])
      dss <- as.character(Rdata[,2])
      dss<- gsub(",", "", dss)
      plm <- lubridate::month(mdy(dss))
      pld <- lubridate::day(mdy(dss))
      ply <- lubridate::year(mdy(dss))
      # dsICdate <- data.frame(HHID = Rdata$HHID, month = strsplit(dss, " ")[[1]][1], date =strsplit(dss, " ")[[1]][2], year=strsplit(dss, " ")[[1]][3])
      pldmy <-  as.Date(paste(ply, plm, pld, sep = "/"), origin = "1970-01-01")
      Rdata[,2] <- pldmy
      
      
      # if(dsICdate$year %in% c(2016, 2020, 2024, 2028, 2032)){
      #   dsICdate$leapyear <- TRUE
      # }else{
      #   dsICdate$leapyear <- FALSE
      # }
      # dsICdate$date <- as.numeric(as.character(dsICdate$date))
      # 
      # dd <- datesIn365(dsICdate)
      # colnames(dd) <- c('HHID', 'month', 'date', 'year', 'leapyear',  names(Rdata)[2])
      # dd$Orig_date <- Rdata[,2]
      prodata <- rbind(prodata, Rdata)
    }
    return(prodata)
  }else{
    return(NULL)
  }
  
}


plotable <- function(hhdata2, FRpoints1, FRpointsdt2){
  cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done late"="yellowgreen",
            "Overdue"="violetred2", "Not done"="orangered2", "To be done"="grey", "Due soon"="purple")

  Events <- c("DST run", "planting", "gapping", "fertilizer1", "fertilizer2", "weeding1", "weeding2", "weeding3", "harvest")
  EAheatplot <- ggplot(hhdata2, aes(x=Events, y=HHID, fill= status)) +
    scale_x_discrete(limits = Events) +
    geom_tile(color="white", size=0.1) +
    theme(legend.text=element_text(size=10)) +
    ggtitle(paste("FR validation exercise schedule for EAID = ",h, unique(hhdata2$EA_Name),sep = " " )) +
    geom_text(aes(label=datetbd2))+
    xlab(" ")+
    theme(axis.title.y=element_blank()) +
    theme(plot.title = element_text(size = 20)) +
    scale_fill_manual("Event timing", values=cols) +
    theme(plot.title=element_text(hjust=0), axis.ticks=element_blank(), axis.text.y=element_text(size=14),
          axis.title=element_text(size=20),axis.text.x = element_text(angle = 25, size=14, vjust = 1, hjust=1) ) +
    theme(legend.position="right") +
    coord_fixed(ratio = 0.25)
  
 # Set theme to allow for plotmath expressions
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(FRpoints1, rows=NULL, theme=tt)
  tbl2 <- tableGrob(FRpointsdt2, rows=NULL, theme=tt)

  # Plot chart and table into one object
  ggsave("FRplot.pdf", grid.arrange(EAheatplot, tbl, tbl2,
                                    nrow=3,
                                    as.table=TRUE,
                                    heights=c(3,3,3)), width = 18, height = 12
                                    )
}


plotableIC <- function(ICplotData, ICpoints1, ICpoints2){
  
  cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done later"="yellowgreen",
            "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Due soon"="purple")
  
  Events <- c("DST run", "planting", "reseeding", "thinning", "gapping", "fertilizer0", "weeding1", "weeding2", "weeding3", "maizeharvest", "cassharvest")
 
  
  EAICheatplot <- ggplot(ICplotData, aes(x=Events, y=HHID, fill= status)) +
    scale_x_discrete(limits = Events) +
    geom_tile(color="white", size=0.1) + 
    theme(legend.text=element_text(size=10)) +
    ggtitle(paste("IC validation exercise schedule for EAID = ",h, unique(ICplotData$EA_Name),sep = " " )) +
    geom_text(aes(label=datetbd2))+
    theme(axis.title.y=element_blank()) +
    theme(plot.title = element_text(size = 20)) +
    scale_fill_manual("Event timing", values=cols) +
    theme(plot.title=element_text(hjust=0), axis.ticks=element_blank(), axis.text.y=element_text(size=14),
          axis.title=element_text(size=20),axis.text.x = element_text(angle = 25, size=14, vjust = 1, hjust=1) ) +
    theme(legend.position="right") +
    coord_fixed(ratio = 0.25)
  # Set theme to allow for plotmath expressions
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(ICpoints1, rows=NULL, theme=tt)
  tbl2 <- tableGrob(ICpoints2, rows=NULL, theme=tt)
  
  # Plot chart and table into one object
  ggsave(filename = "ICplot.pdf", grid.arrange(EAICheatplot, tbl, tbl2,
                                        nrow=3,
                                        as.table=TRUE,
                                        heights=c(3,3,3)), width = 18, height = 12)
  
}



plotablePP <- function(EAPP.h, PPpoints1, PPpoints2){
  
  cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done later"="yellowgreen",
            "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Due soon"="purple")
  
  Events <- c("DST run", "planting", "gapping", "weeding1", "weeding2","weeding3", "harvest")

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
  
  # Set theme to allow for plotmath expressions
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(PPpoints1, rows=NULL, theme=tt)
  tbl2 <- tableGrob(PPpoints2, rows=NULL, theme=tt)
  
  # Plot chart and table into one object
  ggsave("PPplot.pdf", grid.arrange(EAPPplot, tbl, tbl2,
                                               nrow=3,
                                               as.table=TRUE,
                                               heights=c(3,3,3)), width = 18, height = 12)
  
}


plotableSPHS <- function(SPHSplotData, SPHSpoints1, SPHSpoints2){
  
  cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done late"="yellowgreen",
            "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Due soon"="purple", "Control"="dodgerblue4", "Recommended"="red3")

  Events <- c("DST run", "planting",  "gapping", "weeding1", "weeding2", "weeding3", "intharvestREC", "effharvestREC" ,"intharvestCON", "effharvestCON" )

  EASPHSplot <- ggplot(SPHSplotData, aes(x=Events, y=HHID, fill= status)) +
    scale_x_discrete(limits = Events) +
    geom_tile(color="white", size=0.1) +
    theme(legend.text=element_text(size=10)) +
    ggtitle(paste("SPHS validation exercise schedule for EAID = ",h, unique(SPHSplotData$EA_Name),sep = " " )) +
    geom_text(aes(label=datetbd2))+
    theme(axis.title.y=element_blank()) +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    scale_fill_manual("Event timing", values=cols) +
    theme(plot.title=element_text(hjust=0), axis.ticks=element_blank(), axis.text.y=element_text(size=14),
          axis.title=element_text(size=20),axis.text.x = element_text(angle = 25, size=14, vjust = 1, hjust=1) ) +
    theme(legend.position="right") +
    coord_fixed(ratio = 0.25)
  
  # Set theme to allow for plotmath expressions
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(SPHSpoints1, rows=NULL, theme=tt)
  tbl2 <- tableGrob(SPHSpoints2, rows=NULL, theme=tt)
  
  # Plot chart and table into one object
  ggsave("SPHSplot.pdf", grid.arrange(EASPHSplot, tbl, tbl2,
                                    nrow=3,
                                    as.table=TRUE,
                                    heights=c(3,3,3)), width = 18, height = 12)
  
}


plotablePPTZ <- function(PPTZplotData, PPTZpoints1, PPTZpoints2){
  
  cols <- c("Earlier"="lightgreen", "On-time"="seagreen3",  "Done late"="yellowgreen",
            "Overdue"="violetred2", "Not done"="darksalmon", "To be done"="grey", "Due soon"="purple", "Control"="dodgerblue4", "Recommended"="red3")
  
  Events <- c("DST run", "planting",  "gapping", "weeding1", "weeding2", "weeding3", "intharvestREC", "effharvestREC" ,"intharvestCON", "effharvestCON" )
  
  EAPPTZplot <- ggplot(PPTZplotData, aes(x=Events, y=HHID, fill= status)) +
    scale_x_discrete(limits = Events) +
    geom_tile(color="white", size=0.1) +
    theme(legend.text=element_text(size=10)) +
    ggtitle(paste("SPHS validation exercise schedule for EAID = ",h, unique(PPTZplotData$EA_Name),sep = " " )) +
    geom_text(aes(label=datetbd2))+
    theme(axis.title.y=element_blank()) +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    scale_fill_manual("Event timing", values=cols) +
    theme(plot.title=element_text(hjust=0), axis.ticks=element_blank(), axis.text.y=element_text(size=14),
          axis.title=element_text(size=20),axis.text.x = element_text(angle = 25, size=14, vjust = 1, hjust=1) ) +
    theme(legend.position="right") +
    coord_fixed(ratio = 0.25)
  
  # Set theme to allow for plotmath expressions
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(PPTZpoints1, rows=NULL, theme=tt)
  tbl2 <- tableGrob(PPTZpoints2, rows=NULL, theme=tt)
  
  # Plot chart and table into one object
  ggsave("PPTZplot.pdf", grid.arrange(EAPPTZplot, tbl, tbl2,
                                      nrow=3,
                                      as.table=TRUE,
                                      heights=c(3,3,3)), width = 18, height = 12)
  
}



dropGroupNames <- function(ds){
  names(ds)[grepl('\\.', names(ds))] <- sub('.*\\.', '', names(ds)[grepl('\\.', names(ds))])
  return(ds)
}

ID <- "HHID"
filterSingleSubmission <- function(ds, ID, recent=TRUE) {
  #ds: dataset to filter (must contain the end variable)
  #ID: vector of ID variables for which a unique combination must be retained
  #recent: if TRUE, the most recent submission is retained, else the first submission is retained
  ds$end <- as.POSIXlt(ds$end, format="%b %d, %Y %H:%M:%S", tz="GMT")
  tmp <- subset(ds, select=c(ID, "end", "KEY"))
  tmp$end <- as.numeric(julian(tmp$end))
  res <- ds[ds$KEY %in% (tmp %>% group_by_at(ID) %>% filter(end == ifelse(recent,max(end), min(end))))$KEY,]
  return(res)
}

cleanVAL <- function(useCase, wd, recent=TRUE){

  #read in the EA registration data
  dsEA <- dropGroupNames(read.csv(paste(wd, "data/Register_EA.csv", sep="/")))
  dsEA <- filterSingleSubmission(dsEA, ID="EAID", recent=recent) #drops all duplicate EA registration submissions, retaining only the most recent submission
  
  #read in the HH registration data
  dsHH <- dropGroupNames(read.csv(paste(wd, "data/Register_HH.csv", sep="/")))
  dsHH <- filterSingleSubmission(dsHH, ID="HHID", recent=recent) #same for HH registration submissions
  
  
  #read in the dst submissions (SPHS requires merging multiple files as dst submissions are split by country/region)
  if(useCase=="SPHS"){
      #fls <- paste(wd, list.files(path=wd, pattern=paste("/", "data/VAL_SPHS_", sep="")), sep="/")
      #dst <- dropGroupNames(do.call(rbind, lapply(fls, function(x) read.csv(x))))
    fls <- paste(wd, "/data/", list.files(path=paste(wd, "/data", sep=""), pattern="VAL_SPHS_"), sep="")
    fls_TZ <- fls[grep("TZ", fls)]
    fls_NG <- fls[-grep("TZ", fls)]
    dst_TZ <- dropGroupNames(do.call(rbind, lapply(fls_TZ, function(x) read.csv(x))))
    dst_NG <- dropGroupNames(do.call(rbind, lapply(fls_NG, function(x) read.csv(x))))
    dst_TZ$season <- 'NA'
    dst_TZ <- dst_TZ[, colnames(dst_NG)]
    dst <- rbind(dst_TZ, dst_NG)
    
  }else{
    dst <- dropGroupNames(read.csv(paste0(wd, "/data/VAL_", useCase, ".csv")))
  }
  
  if(useCase == "PP") dst <- dst[,-which(names(dst)=="harrow_plot3")[1]] #dropping accidentally column with duplicated variable name
  dst[is.na(dst$season),]$season <- 1 #replace NA values by 1 (season variable did not exist in season 1)
  dst <- filterSingleSubmission(dst, ID=c("HHID", "season"), recent=recent) #retains only the most recent DST submission per season
  dst <- merge(dst, subset(dsEA, select=c(EAID, firstNameEA, surNameEA, phoneNrEA)), all.x=TRUE) #add EA details
  dst <- merge(dst, subset(dsHH, select=c(HHID, firstNameHH, surNameHH, phoneNrHH)), all.x=TRUE) #add HH details
  if(useCase == "IC") dst$HHID <- paste(dst$HHID, ifelse(dst$maizeVariety=="Ikom_White", "_Ikom_White", ""), sep="")
  
  
  #read in the dataVAL submissions
  dat <- dropGroupNames(read.csv(paste0(wd, "/data/dataVAL_", useCase, ".csv")))
  dat <- dat[dat$HHID %in% dst$HHID,] #only data linked to a dst submission is retained.
  dat <- merge(dat, subset(dsEA, select=c(EAID, firstNameEA, surNameEA, phoneNrEA)), all.x=TRUE) #add EA details
  dat <- merge(dat, subset(dsHH, select=c(HHID, firstNameHH, surNameHH, phoneNrHH)), all.x=TRUE) #add HH details
  dat$end <- as.POSIXlt(dat$end, format="%d-%b-%Y %H:%M:%S", tz="GMT")
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
      PD$plantingDate <- as.Date(PD$plantingDate, format="%d-%b-%Y")
      PD$useCase <- useCase
      PD <- merge(PD, ff)
      #set min and max dates for data collection: not earlier than 30 days before and not later than 180 days after the target data collection date
      PD$minDate <- as.numeric(julian(PD$plantingDate - 30  + 7*PD$WAP)) #earliest date at which a data collection event can be considered.
      PD$maxDate <- as.numeric(julian(PD$plantingDate + 180 + 7*PD$WAP)) #latest date at which a data collection event can be considered
  
      ss <- subset(dat2[dat2$HHID==i,], select=c(HHID, event, end, KEY))
      ss$end <- as.numeric(julian(ss$end))
      tt <- merge(ss, PD) %>% filter(end >= minDate, end <= maxDate) %>% select(KEY, season) #merge and filter based on dates between min and max dates permitted for data collection
      res <- rbind(res, tt)
      
    }
    
    dat2 <- merge(dat2, res) #only data collection within the min-max date range per event are retained.
    
  }
  
  dat <- rbind(dat1, dat2)
  dat <- filterSingleSubmission(dat, c("HHID", "event", "season"), recent=recent) #retain most recent submission per HHID and event and season

  return(list(dst, dat))
  
}


cleanVALFR <- function(useCase = "FR",  wd, recent=TRUE){
  ID = "HHID"

  VAl_dataFR$season <- NA
  VAl_dataFR[is.na(VAl_dataFR$season),]$season <- 1
  #read in the dataVAL submissions

  data_FR <- data_FR[data_FR$HHID %in% VAl_dataFR$HHID,] #only data linked to a dst submission is retained.
  repeated_HHs <- as.character(unique(VAl_dataFR[duplicated(VAl_dataFR$HHID),]$HHID)) #HHIDs that run a first and second season trial
  data_FR1 <- data_FR[!data_FR$HHID %in% repeated_HHs,] #subset for HHIDs that have only run a single season of trials
  data_FR1 <- merge(data_FR1, subset(VAl_dataFR, select=c("HHID", "season"))) #add the season variable
  data_FR2 <- data_FR[data_FR$HHID %in% repeated_HHs,]


  if(nrow(data_FR2)>0){

    ff <- rbind(
      data.frame(useCase = "FR",   event = paste0("event", 1:8), WAP = c(0,4,4,10,12,24,36,48)))


    resFR <- NULL
    for (i in repeated_HHs){

      PD <- subset(VAl_dataFR[VAl_dataFR$HHID==i,], select=c(HHID, season, plantingDate))
      PD$plantingDate <- as.Date(PD$plantingDate, format="%b %d, %Y")
      PD$useCase <- useCase
      PD <- merge(PD, ff)
      #set min and max dates for data collection: not earlier than 30 days before and not later than 180 days after the target data collection date
      PD$minDate <- as.numeric(julian(PD$plantingDate - 30  + 7*PD$WAP)) #earliest date at which a data collection event can be considered.
      PD$maxDate <- as.numeric(julian(PD$plantingDate + 180 + 7*PD$WAP)) #latest date at which a data collection event can be considered
      ss <- subset(data_FR2[data_FR2$HHID==i,], select=c(HHID, event, end, KEY))
      ss$end <- as.POSIXlt(ss$end, format="%b %d, %Y %H:%M:%S", tz="GMT")
      ss$end <- as.numeric(julian(ss$end))
      tt <- merge(ss, PD) %>% filter(end >= minDate, end <= maxDate) %>% dplyr::select(KEY, season) #merge and filter based on dates between min and max dates permitted for data collection
      resFR <- rbind(resFR, tt)

    }

    data_FR2 <- merge(data_FR2, resFR) #only data collection within the min-max date range per event are retained.

  }

  data_FR <- rbind(data_FR1, data_FR2)
  data_FR <- filterSingleSubmission(data_FR, c("HHID", "event", "season"), recent=recent) #retain most recent submission per HHID and event and season

  return(list(VAl_dataFR, data_FR))

}



cleanVALIC <- function(useCase = "IC", wd, recent=TRUE){	
  ID = "HHID"
  
  #read in the dataVAL submissions
  
  data_IC <- data_IC[data_IC$HHID %in% VAl_dataIC$HHID,] #only data linked to a dst submission is retained.
  repeated_HHs <- as.character(unique(VAl_dataIC[duplicated(VAl_dataIC$HHID),]$HHID)) #HHIDs that run a first and second season trial
  data_IC1 <- data_IC[!data_IC$HHID %in% repeated_HHs,] #subset for HHIDs that have only run a single season of trials
  data_IC1 <- merge(data_IC1, subset(VAl_dataIC, select=c("HHID", "season"))) #add the season variable
  
  data_IC2 <- data_IC[data_IC$HHID %in% repeated_HHs,]
  
  
  if(nrow(data_IC2)>0){
    
    ff <- rbind(
      data.frame(useCase = "IC",   event = paste0("event", 1:8), WAP = c(0,3:5,12,24,36,48)))
    
    resIC <- NULL
    for (i in repeated_HHs){
      
      PD <- subset(VAl_dataIC[VAl_dataIC$HHID==i,], select=c(HHID, season, plantingDate))
      PD$plantingDate <- as.Date(PD$plantingDate, format="%b %d, %Y")
      PD$useCase <- useCase
      PD <- merge(PD, ff)
      #set min and max dates for data collection: not earlier than 30 days before and not later than 180 days after the target data collection date
      PD$minDate <- as.numeric(julian(PD$plantingDate - 30  + 7*PD$WAP)) #earliest date at which a data collection event can be considered.
      PD$maxDate <- as.numeric(julian(PD$plantingDate + 180 + 7*PD$WAP)) #latest date at which a data collection event can be considered
      
      ss <- subset(data_IC2[data_IC2$HHID==i,], select=c(HHID, event, end, KEY))
      ss$end <- as.POSIXlt(ss$end, format="%b %d, %Y %H:%M:%S", tz="GMT")
      ss$end <- as.numeric(julian(ss$end))
      tt <- merge(ss, PD) %>% filter(end >= minDate, end <= maxDate)  %>% dplyr::select(KEY, season) #merge and filter based on dates between min and max dates permitted for data collection
      resIC <- rbind(resIC, tt)
      
    }
    
    data_IC2 <- merge(data_IC2, resIC) #only data collection within the min-max date range per event are retained.
    
  }
  
  data_IC <- rbind(data_IC1, data_IC2)
  data_IC <- filterSingleSubmission(data_IC, c("HHID", "event", "season"), recent=recent) #retain most recent submission per HHID and event and season
  
  return(list(VAl_dataIC, data_IC))
  
}


cleanVALPP <- function(useCase = "PP", wd, recent=TRUE){	
  ID = "HHID"
  #VAl_PP[is.na(VAl_PP$season),]$season <- 1
  #read in the dataVAL submissions
  dat_PP <- dat_PP[dat_PP$HHID %in% dst_PP$HHID,] #only data linked to a dst submission is retained.
  repeated_HHs <- as.character(unique(dst_PP[duplicated(dst_PP$HHID),]$HHID)) #HHIDs that run a first and second season trial
  dat_PP1 <- dat_PP[!dat_PP$HHID %in% repeated_HHs,] #subset for HHIDs that have only run a single season of trials
  dat_PP1 <- merge(dat_PP1, subset(dst_PP, select=c("HHID", "season"))) #add the season variable
  dat_PP2 <- dat_PP[dat_PP$HHID %in% repeated_HHs,]
  
  if(nrow(dat_PP2)>0){
    
    ff <- rbind(
      data.frame(useCase = "PP",   paste0("event", 1:7), WAP = c(0,4,8,12,24,36,48)))
    
    resPP <- NULL
    for (i in repeated_HHs){
      
      PD <- subset(dst_PP[dst_PP$HHID==i,], select=c(HHID, season, plantingDate))
      PD$plantingDate <- as.Date(PD$plantingDate, format="%b %d, %Y")
      PD$useCase <- useCase
      PD <- merge(PD, ff)
      #set min and max dates for data collection: not earlier than 30 days before and not later than 180 days after the target data collection date
      PD$minDate <- as.numeric(julian(PD$plantingDate - 30  + 7*PD$WAP)) #earliest date at which a data collection event can be considered.
      PD$maxDate <- as.numeric(julian(PD$plantingDate + 180 + 7*PD$WAP)) #latest date at which a data collection event can be considered
      ss <- subset(dat_PP2[dat_PP2$HHID==i,], select=c(HHID, event, end, KEY))
      ss$end <- as.POSIXlt(ss$end, format="%b %d, %Y %H:%M:%S", tz="GMT")
      ss$end <- as.numeric(julian(ss$end))
      tt <- merge(ss, PD) %>% filter(end >= minDate, end <= maxDate) %>% dplyr::select(KEY, season) #merge and filter based on dates between min and max dates permitted for data collection
      resPP <- rbind(resPP, tt)
      
    }
    
    dat_PP2 <- merge(dat_PP2, resPP) #only data collection within the min-max date range per event are retained.
    
  }
  
  dat_PP <- rbind(dat_PP1, dat_PP2)
  dat_PP <- filterSingleSubmission(dat_PP, c("HHID", "event", "season"), recent=recent) #retain most recent submission per HHID and event and season
  
  return(list(dst_PP, dat_PP))
  
}


cleanVALSP <- function(useCase = "SPHS", wd, recent=TRUE){	
  
  dst_SP$season <- ""
  dst_SP[dst_SP$season == "",]$season <- 1
  #read in the dataVAL submissions
  dat_SP <- dat_SP[dat_SP$HHID %in% dst_SP$HHID,] #only data linked to a dst submission is retained.
  repeated_HHs <- as.character(unique(dst_SP[duplicated(dst_SP$HHID),]$HHID)) #HHIDs that run a first and second season trial
  dat_SP1 <- dat_SP[!dat_SP$HHID %in% repeated_HHs,] #subset for HHIDs that have only run a single season of trials
  dat_SP1 <- merge(dat_SP1, subset(dst_SP, select=c("HHID", "season"))) #add the season variable
  dat_SP2 <- dat_SP[dat_SP$HHID %in% repeated_HHs,]
  
  if(nrow(dat_SP2)>0){
    
    ff <- rbind(
      data.frame(useCase = "SPHS",   event = paste0("event", 0:7), WAP = c(-2,0,4,12,24,36,48,48)))
    
    resSP <- NULL
    for (i in repeated_HHs){
      
      PD <- subset(dst_SP[dst_SP$HHID==i,], select=c(HHID, season, plantingDate))
      PD$plantingDate <- as.Date(PD$plantingDate, format="%b %d, %Y")
      PD$useCase <- "SP"
      PD <- merge(PD, ff)
      #set min and max dates for data collection: not earlier than 30 days before and not later than 180 days after the target data collection date
      PD$minDate <- as.numeric(julian(PD$plantingDate - 30  + 7*PD$WAP)) #earliest date at which a data collection event can be considered.
      PD$maxDate <- as.numeric(julian(PD$plantingDate + 180 + 7*PD$WAP)) #latest date at which a data collection event can be considered
      ss <- subset(dat_SP2[dat_SP2$HHID==i,], select=c(HHID, event, end, KEY))
      ss$end <- as.POSIXlt(ss$end, format="%b %d, %Y %H:%M:%S", tz="GMT")
      ss$end <- as.numeric(julian(ss$end))
      tt <- merge(ss, PD) %>% filter(end >= minDate, end <= maxDate) %>% dplyr::select(KEY, season) #merge and filter based on dates between min and max dates permitted for data collection
      resSP <- rbind(resSP, tt)
      
    }
    
    dat_SP2 <- merge(dat_SP2, resSP) #only data collection within the min-max date range per event are retained.
    
  }
  
  dat_SP <- rbind(dat_SP1, dat_SP2)
  
  
  return(list(dst_SP, dat_SP))
  
}

cleanVALPPTV <- function(useCase = "PP", wd, recent=TRUE){	
  ID = "HHID"
  #dst_PP_TZ[is.na(dst_PP_TZ$season),]$season <- 1
  #read in the dataVAL submissions
  dat_PP_TZ <- dat_PP_TZ[dat_PP_TZ$HHID %in% dst_PP_TZ$HHID,] #only data linked to a dst submission is retained.
  repeated_HHs <- as.character(unique(dst_PP_TZ[duplicated(dst_PP_TZ$HHID),]$HHID)) #HHIDs that run a first and second season trial
  dat_PP_TZ1 <- dat_PP_TZ[!dat_PP_TZ$HHID %in% repeated_HHs,] #subset for HHIDs that have only run a single season of trials
  dat_PP_TZ1 <- merge(dat_PP_TZ1, subset(dst_PP_TZ, select=c("HHID", "season"))) #add the season variable
  dat_PP_TZ2 <- dat_PP_TZ[dat_PP_TZ$HHID %in% repeated_HHs,]
  
  if(nrow(dat_PP_TZ2)>0){
    
    ff <- rbind(
      data.frame(useCase = "PP",   paste0("event", 1:7), WAP = c(0,4,8,12,24,36,48)))
    
    resPP <- NULL
    for (i in repeated_HHs){
      
      PD <- subset(dst_PP_TZ[dst_PP_TZ$HHID==i,], select=c(HHID, season, plantingDate))
      PD$plantingDate <- as.Date(PD$plantingDate, format="%b %d, %Y")
      PD$useCase <- useCase
      PD <- merge(PD, ff)
      #set min and max dates for data collection: not earlier than 30 days before and not later than 180 days after the target data collection date
      PD$minDate <- as.numeric(julian(PD$plantingDate - 30  + 7*PD$WAP)) #earliest date at which a data collection event can be considered.
      PD$maxDate <- as.numeric(julian(PD$plantingDate + 180 + 7*PD$WAP)) #latest date at which a data collection event can be considered
      ss <- subset(dat_PP_TZ2[dat_PP_TZ2$HHID==i,], select=c(HHID, event, end, KEY))
      ss$end <- as.POSIXlt(ss$end, format="%b %d, %Y %H:%M:%S", tz="GMT")
      ss$end <- as.numeric(julian(ss$end))
      tt <- merge(ss, PD) %>% filter(end >= minDate, end <= maxDate) %>% dplyr::select(KEY, season) #merge and filter based on dates between min and max dates permitted for data collection
      resPP <- rbind(resPP, tt)
      
    }
    
    dat_PP_TZ2 <- merge(dat_PP_TZ2, resPP) #only data collection within the min-max date range per event are retained.
    
  }
  
  dat_PP_TZ <- rbind(dat_PP_TZ1, dat_PP_TZ2)
  dat_PP_TZ <- filterSingleSubmission(dat_PP_TZ, c("HHID", "event", "season"), recent=recent) #retain most recent submission per HHID and event and season
  
  return(list(dst_PP_TZ, dat_PP_TZ))
  
}

cleanVALCIS <- function(useCase = "IC", wd, recent=TRUE){	
  ID = "HHID"
  
  #read in the dataVAL submissions
  
  data_CIS <- data_CIS[data_CIS$HHID %in% VAl_dataCIS$HHID,] #only data linked to a dst submission is retained.
  repeated_HHs <- as.character(unique(VAl_dataCIS[duplicated(VAl_dataCIS$HHID),]$HHID)) #HHIDs that run a first and second season trial
  data_CIS1 <- data_CIS[!data_CIS$HHID %in% repeated_HHs,] #subset for HHIDs that have only run a single season of trials
  
  data_CIS1 <- merge(data_CIS1, subset(VAl_dataCIS, select=c("HHID", "season"))) #add the season variable
  
  data_CIS2 <- data_CIS[data_CIS$HHID %in% repeated_HHs,]
  
  
  if(nrow(data_CIS2)>0){
    
    ff <- rbind(
      data.frame(useCase = "IC",   event = paste0("event", 1:8), WAP = c(0,3,4,8,12,24,36,48)))
    
    resCIS <- NULL
    for (i in repeated_HHs){
      
      PD <- subset(VAl_dataCIS[VAl_dataCIS$HHID==i,], select=c(HHID, season, plantingDate))
      PD$plantingDate <- as.Date(PD$plantingDate, format="%b %d, %Y")
      PD$useCase <- useCase
      PD <- merge(PD, ff)
      #set min and max dates for data collection: not earlier than 30 days before and not later than 180 days after the target data collection date
      PD$minDate <- as.numeric(julian(PD$plantingDate - 30  + 7*PD$WAP)) #earliest date at which a data collection event can be considered.
      PD$maxDate <- as.numeric(julian(PD$plantingDate + 180 + 7*PD$WAP)) #latest date at which a data collection event can be considered
      
      ss <- subset(data_CIS2[data_CIS2$HHID==i,], select=c(HHID, event, end, KEY))
      ss$end <- as.POSIXlt(ss$end, format="%b %d, %Y %H:%M:%S", tz="GMT")
      ss$end <- as.numeric(julian(ss$end))
      tt <- merge(ss, PD) %>% filter(end >= minDate, end <= maxDate) %>% dplyr::select(KEY, season) #merge and filter based on dates between min and max dates permitted for data collection
      resCIS <- rbind(resCIS, tt)
      
    }
    
    data_CIS2 <- merge(data_CIS2, resCIS) #only data collection within the min-max date range per event are retained.
    
  }
  
  data_CIS <- rbind(data_CIS1, data_CIS2)
  data_CIS <- filterSingleSubmission(data_CIS, c("HHID", "event", "season"), recent=recent) #retain most recent submission per HHID and event and season
  
  return(list(VAl_dataCIS, data_CIS))
  
}

