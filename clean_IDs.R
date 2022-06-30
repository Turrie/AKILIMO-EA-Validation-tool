

# 
# 
Clean_EAHH_Ids <- function(dsEA, dsHH){
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG002071") & (dsHH$firstNameHH %in% c("Jacob"))),]
  
  dsHHs<-subset(dsHH[dsHH$EAID=="ACEANG000023" & dsHH$state=="NG.ED",])
  dsHHr<-dsHH[!(dsHH$EAID %in% c("ACEANG000023") & dsHH$state %in% c("NG.ED")),]
  dsHHs$state<-"NG.CR"
  dsHH<-rbind(dsHHs,dsHHr)
  
  dsHHs<-subset(dsHH[dsHH$HHID=="ACHHNG001721" & dsHH$phoneNrHH=="8160915393",])
  dsHHr<-dsHH[!(dsHH$HHID %in% c("ACHHNG001721") & dsHH$phoneNrHH %in% c("8100205133")),]
  dsHHs$phoneNrHH<-"8158117788"
  dsHH<-rbind(dsHHs,dsHHr)
  
  # duplicated HHs in CR one name registerd in field, one at the homestead, keep homestead
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001758") & dsHH$locHH %in% c("yes_homestead")),]
  
  # duplicated HH in OG both registerd in the field, keep the one of later submission assuming it was a correction
  #dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG002070") &
  # dsHH$SubmissionDate %in% c("Apr 12, 2018 3:05:15 PM")),]
  
  # removing duplicated HH ID; retaining entry in the cassava field
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001688") & (dsHH$locHH %in% c("yes_homestead"))),]
  # removing duplicated HH ID detected in June4 fertilizer
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001660") & (dsHH$firstNameHH %in% c("Gideon"))),]
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001651") & (dsHH$firstNameHH %in% c("970Aza"))),]
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG002170") & (dsHH$locHH %in% c("yes_homestead"))),]
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG002163") & (dsHH$firstNameHH %in% c("Moruff"))),]
  
  
  
  # removing duplicated HH IDs; NOT checking on details, general clean-up to 
  # have only one submission for data mergers for ag data 
  # for analysis of HH in relation to VALs need to analyse step by step
  # may also affect above subsettings...
  # dsHHd<-dsHH[duplicated(dsHH$HHID),]
  # dsHH<-dsHH[!duplicated(dsHH$HHID),]
  

  # removing training barcodes for EA and HH
  dsEA<-dsEA[!(dsEA$EAID=="ACEANG123456"),]
  dsHH<-dsHH[!(dsHH$HHID=="ACHHNG123456"),]
  dsHH<-dsHH[!(dsHH$EAID=="ACHEAG123456"),]
  # dsEA<-unique(dsEA)
  # 
  # removing replaced EA (coordinator Femi in OG)
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000066") & dsEA$surNameEA %in% c("YAKUBU")),]
  
  dsEA$start<-strptime(dsEA$start,format="%B %d,%Y %I:%M:%S %p")
  dsEA$start<-as.numeric(dsEA$start)
  
  dsEAd<-dsEA[duplicated(dsEA$HHID),]
  # dsEAdd<-dsEA[dsEA$HHID %in% c(""),]
  # dsEAdd<-subset(dsEAdd, select=c(HHID,start))
  # dsEAdd<-dsEAdd[order(dsEAdd$HHID),]
  # dsEAii<-dsEA[!(dsEA$HHID %in% c("",
  #                                "") &
  #                 (dsEA$start %in% c("", ""))),]
  
  dsEAeaD<-dsEA[duplicated(dsEA$EAID),]
  dsEAeaD<-subset(dsEAeaD, select=c(EAID))
  dsEAeaD<-as.data.frame(dsEAeaD[!duplicated(dsEAeaD),])
  
  
  # dsEAeaDd<-dsEA[dsEA$EAID %in% c("ACEANG000207",
  #                                 "ACEANG000500",
  #                                 "ACEANG000486",
  #                                 "ACEANG000483",
  #                                 "ACEANG000491",
  #                                 "ACEANG000114",
  #                                 "ACEANG000164",
  #                                 "ACEANG000036",
  #                                 "ACEANG000260",
  #                                 "ACEANG000124",
  #                                 "ACEANG000103",
  #                                 "ACEANG000039",
  #                                 "ACEANG000054",
  #                                 "ACEANG000230",
  #                                 "ACEANG000123",
  #                                 "ACEANG000016",
  #                                 "ACEANG000026",
  #                                 "ACEANG000326",
  #                                 "ACEANG000057",
  #                                 "ACEANG000325"),]
  # 
  # dsEAeaDd<-subset(dsEAeaDd, select=c(EAID, start))
  # dsEAeaDd<-dsEAeaDd[order(dsEAeaDd$EAID),]
  # 
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000016",
                                "ACEANG000026",
                                "ACEANG000036",
                                "ACEANG000039",
                                "ACEANG000054",
                                "ACEANG000057",
                                "ACEANG000103",
                                "ACEANG000114",
                                "ACEANG000123",
                                "ACEANG000124",
                                "ACEANG000164",
                                "ACEANG000207",
                                "ACEANG000230",
                                "ACEANG000260",
                                "ACEANG000325",
                                "ACEANG000326",
                                "ACEANG000483",
                                "ACEANG000486",
                                "ACEANG000491",
                                "ACEANG000500") &
                 (dsEA$start %in% c("1527849623",
                                    "1523447969",
                                    "1523522654",
                                    "1523467856",
                                    "1451676181",
                                    "1522066194",
                                    "1451878821",
                                    "1525263219",
                                    "1519643281",
                                    "1525368642",
                                    "1525263380",
                                    "1523455000",
                                    "1518703328",
                                    "1519048066",
                                    "1522119274",
                                    "1451673991",
                                    "1531118122",
                                    "1517076229",
                                    "1517034869",
                                    "1516979997",
                                    "1517307492",
                                    "1517729536"))),]
  
  # dsEAeaD2<-dsEA[duplicated(dsEA$EAID),]
  # dsEAeaD2s<-subset(dsEAeaD2, select=c(EAID))
  
  # dsEAeaDd2<-dsEA[dsEA$EAID %in% c("ACEANG000171"),]
  # dsEAeaDd2<-subset(dsEAeaDd2, select=c(EAID, start))
  # dsEAeaDd2<-dsEAeaDd2[order(dsEAeaDd2$EAID),]
  # 
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000171") & (dsEA$start %in% c("1530814904"))),]
  
  # dsEAeaD3<-dsEA[duplicated(dsEA$EAID),]
  # dsEAeaD3s<-subset(dsEAeaD3, select=c(EAID))
  # 
  # dsEAeaDd3<-dsEA[dsEA$EAID %in% c("ACEANG000043"),]
  # 
  # dsEAeaDd3<-subset(dsEAeaDd3, select=c(EAID, start))
  # dsEAeaDd3<-dsEAeaDd3[order(dsEAeaDd3$EAID),]
  
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000043") &
                 (dsEA$start %in% c("1525345451"))),]
  
  # dsEAeaD4<-dsEA[duplicated(dsEA$EAID),]
  # dsEAeaD4s<-subset(dsEAeaD4, select=c(EAID))
  # 
  # dsEAeaDd4<-dsEA[dsEA$EAID %in% c("ACEANG000218"),]
  # 
  # dsEAeaDd4<-subset(dsEAeaDd4, select=c(EAID, start))
  # dsEAeaDd4<-dsEAeaDd4[order(dsEAeaDd4$EAID),]
  
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000218") &
                 (dsEA$start %in% c("1524064695"))),]
  
  # dsEAeaD5<-dsEA[duplicated(dsEA$EAID),]
  # dsEAeaD5s<-subset(dsEAeaD5, select=c(EAID))
  # 
  # dsEAeaDd5<-dsEA[dsEA$EAID %in% c("ACEANG000254",
  #                                  "ACEANG000329"),]
  # 
  # dsEAeaDd5<-subset(dsEAeaDd5, select=c(EAID, start))
  # dsEAeaDd5<-dsEAeaDd5[order(dsEAeaDd5$EAID),]
  
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000254",
                                "ACEANG000329") &
                 (dsEA$start %in% c("1522237201",
                                    "1534531355"))),]
  
  # dsEAeaD6<-dsEA[duplicated(dsEA$EAID),]
  # dsEAeaD6s<-subset(dsEAeaD6, select=c(EAID))
  # 
  # dsEAeaDd6<-dsEA[dsEA$EAID %in% c("ACEANG000194"),]
  # 
  # dsEAeaDd6<-subset(dsEAeaDd6, select=c(EAID, start))
  # dsEAeaDd6<-dsEAeaDd6[order(dsEAeaDd6$EAID),]
  
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000194") &
                 (dsEA$SubmissionDate %in% c("Sep 15, 2018 8:01:19 PM"))),]
  
  # dsEAeaD7<-dsEA[duplicated(dsEA$EAID),]
  # dsEAeaD7s<-subset(dsEAeaD7, select=c(EAID))
  # 
  # dsEAeaDd7<-dsEA[dsEA$EAID %in% c("ACEANG000801"),]
  # 
  # dsEAeaDd7<-subset(dsEAeaDd7, select=c(EAID, start))
  # dsEAeaDd7<-dsEAeaDd7[order(dsEAeaDd7$EAID),]
  
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000801") & (dsEA$start %in% c("1537275377"))),]
  
  # dsEAeaD8<-dsEA[duplicated(dsEA$EAID),]
  # dsEAeaD8s<-subset(dsEAeaD8, select=c(EAID))
  # dsEAeaDd8<-dsEA[dsEA$EAID %in% c("ACEANG000105",
  #                                  "ACEANG000043"),]
  # dsEAeaDd8<-subset(dsEAeaDd8, select=c(EAID, start))
  # dsEAeaDd8<-dsEAeaDd8[order(dsEAeaDd8$EAID),]
  
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000043","ACEANG000105") & dsEA$start %in% c("1533027131", "1525263208")),]
  
  # dsEAeaD9<-dsEA[duplicated(dsEA$EAID),]
  # dsEAeaD9s<-subset(dsEAeaD9, select=c(EAID))
  
  # dsEAeaDd9<-dsEA[dsEA$EAID %in% c("ACEANG000270"),]
  # dsEAeaDd9<-subset(dsEAeaDd9, select=c(EAID, start))
  # dsEAeaDd9<-dsEAeaDd9[order(dsEAeaDd9$EAID),]
  
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000270") & (dsEA$start %in% c("1543840249"))),]
  
  # dsEAeaD10 <-dsEA[duplicated(dsEA$EAID),]
  # dsEAeaD10s<-subset(dsEAeaD10, select=c(EAID))
  # 
  # dsEAeaDd10<-dsEA[dsEA$EAID %in% c("ACEANG000104"),]
  # dsEAeaDd10<-subset(dsEAeaDd10, select=c(EAID, start))
  # dsEAeaDd10<-dsEAeaDd10[order(dsEAeaDd10$EAID),]
  
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000104") & (dsEA$start %in% c("1525263206"))),]
  
  
  # dsEAeaD11<-dsEA[duplicated(dsEA$EAID),]
  
  #dsEA <- dsEA[!(dsEA$EAID %in% c("ACEANG000485") & (dsEA$levelName %in% c("Orumba south"))),]
  
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000485") &
                 (dsEA$levelName %in% c("Orumba south"))),]
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000123") & 
                 (dsEA$levelName %in% c("IDO"))),]
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000088") & 
                 (dsEA$levelName %in% c("Ido Local government "))),]
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000133") & 
                 (dsEA$levelName %in% c("Afijio local Govt"))),]
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000176") & 
                 (dsEA$firstNameEA %in% c("Ndukwe"))),]
  dsEA<-dsEA[!(dsEA$EAID %in% c(",CEANG000028")),]
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000104") &
                 dsEA$levelName %in% c("Buruku Buruku")),]
  dsEA<-dsEA[!(dsEA$EAID %in% c("ACEANG000040") &
                 (dsEA$levelName %in% c("Benue"))),]
  
  #dsEA<-subset(dsEA, select=-c(SubmissionDate, start))
  
  
  dsHH<-subset(dsHH,select=-c(locHH))
  
  dsHH$start<-strptime(dsHH$start,format="%B %d,%Y %I:%M:%S %p")
  dsHH$start<-as.numeric(dsHH$start)
  ss<-subset(dsHH, select=c(HHID, start))
  ss<-unique(ss)
  
  ssd<-ss[duplicated(ss$HHID),]
  # dsHHdd<-dsHH[dsHH$HHID %in% c("ACHHNG001521",
  #                               "ACHHNG001521",
  #                               "ACHHNG001521", "ACHHNG002060", "ACHHNG001611",
  #                               "ACHHNG002334", "ACHHNG001612", "ACHHNG001623", "ACHHNG001639",
  #                               "ACHHNG001671",
  #                               "ACHHNG001616", "ACHHNG001617", "ACHHNG001612", "ACHHNG001613",
  #                               "ACHHNG001619",
  #                               "ACHHNG001618", "ACHHNG001616", "ACHHNG001614", "ACHHNG001625",
  #                               "ACHHNG001620",
  #                               "ACHHNG001623", "ACHHNG001622", "ACHHNG001627", "ACHHNG001630",
  #                               "ACHHNG001626",
  #                               "ACHHNG001629", "ACHHNG001709", "ACHHNG001693", "ACHHNG001697",
  #                               "ACHHNG001695",
  #                               "ACHHNG002129", "ACHHNG001615", "ACHHNG001611", "ACHHNG001754",
  #                               "ACHHNG001624",
  #                               "ACHHNG001641", "ACHHNG001642", "ACHHNG001521", "ACHHNG002167",
  #                               "ACHHNG002165",
  #                               "ACHHNG002169", "ACHHNG002164", "ACHHNG001692", "ACHHNG001939",
  #                               "ACHHNG001790",
  #                               "ACHHNG001359", "ACHHNG002152"),]
  # 
  # dsHHdd<-subset(dsHHdd, select=c(HHID,start))
  # 
  # dsHHdd<-dsHHdd[order(dsHHdd$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001521",
                                "ACHHNG001521",
                                "ACHHNG001521", "ACHHNG002060", "ACHHNG001611",
                                "ACHHNG002334", "ACHHNG001612", "ACHHNG001623", "ACHHNG001639",
                                "ACHHNG001671",
                                "ACHHNG001616", "ACHHNG001617", "ACHHNG001612", "ACHHNG001613",
                                "ACHHNG001619",
                                "ACHHNG001618", "ACHHNG001616", "ACHHNG001614", "ACHHNG001625",
                                "ACHHNG001620",
                                "ACHHNG001623", "ACHHNG001622", "ACHHNG001627", "ACHHNG001630",
                                "ACHHNG001626",
                                "ACHHNG001629", "ACHHNG001709", "ACHHNG001693", "ACHHNG001697",
                                "ACHHNG001695",
                                "ACHHNG002129", "ACHHNG001615", "ACHHNG001611", "ACHHNG001754",
                                "ACHHNG001624",
                                "ACHHNG001641", "ACHHNG001642", "ACHHNG001521", "ACHHNG002167",
                                "ACHHNG002165",
                                "ACHHNG002169", "ACHHNG002164", "ACHHNG001692", "ACHHNG001939",
                                "ACHHNG001790",
                                "ACHHNG001359", "ACHHNG002152") &
                 (dsHH$start %in% c("1525276691", "1524840355", "1524841924","1524842280",
                                    "1524842579","1525943934","1526537748","1525276693",
                                    "1526643271","1526646444","1526728699","1526662702","1526732162",
                                    "1526906795","1526632076","1526660551","1526638082",
                                    "1526652437","1525952208","1525957704","1525957704","1526540425",
                                    "1526115540","1526721987","1526575794","1526134864","1526561882",
                                    "1525884850","1526551851","1525276697","1525276755",
                                    "1525276696","1526214711","1525946619","1527092046",
                                    "1525946303","1526105957","1526398752","1525845823",
                                    "1526650424","1523972553","1524487802","1527851346",
                                    "1524570331","1524571484","1524570776","1524569562",
                                    "1526579471"))),]
  
  # dsHHdd2<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd2<-subset(dsHHdd2, select=c(HHID,start))
  # 
  # dsHHdd2<-dsHH[dsHH$HHID %in% c("ACHHNG000358",
  #                                "ACHHNG000436",
  #                                "ACHHNG001547",
  #                                "ACHHNG001721",
  #                                "ACHHNG001891",
  #                                "ACHHNG002006",
  #                                "ACHHNG002171",
  #                                "ACHHNG002176",
  #                                "ACHHNG002180",
  #                                "ACHHNG002189",
  #                                "ACHHNG002409"),]
  # 
  # dsHHdd2<-subset(dsHHdd2, select=c(HHID,start))
  # dsHHdd2<-dsHHdd2[order(dsHHdd2$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG000358",
                                "ACHHNG000436",
                                "ACHHNG001891",
                                "ACHHNG002006",
                                "ACHHNG002171",
                                "ACHHNG002176",
                                "ACHHNG002180") &
                 (dsHH$start %in% c("1528803885",
                                    "1528801433",
                                    "1527689478",
                                    "1526129282",
                                    "1524502855",
                                    "1524504269",
                                    "1524555817"))),]
  # dsHH[dsHH$HHID=="ACHHNG001547",]
  dsHH<-dsHH[!(dsHH$SubmissionDate %in% c("Jun 14, 2018 1:16:30 PM")),]
  
  dsHH<-dsHH[!(dsHH$phoneNrHH %in% c("8160915393")),]
  
  dsHH<-dsHH[!(dsHH$SubmissionDate %in% c("May 14, 2018 11:20:29 AM")),]
  
  dsHH<-dsHH[!(dsHH$SubmissionDate %in% c("May 9, 2018 5:33:39 PM")),]
  
  
  # dsHHdd3<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd3<-subset(dsHHdd3, select=c(HHID,start))
  # dsHHdd3<-dsHHdd3[order(dsHHdd3$HHID),]
  # dsHHdd3s<-subset(dsHHdd3, select=c(HHID))
  # 
  # dsHHdd3<-dsHH[dsHH$HHID %in% c("ACHHNG001741",
  #                                "ACHHNG001742",
  #                                "ACHHNG001743",
  #                                "ACHHNG001744",
  #                                "ACHHNG001745",
  #                                "ACHHNG001745",
  #                                "ACHHNG001746",
  #                                "ACHHNG001747",
  #                                "ACHHNG001748",
  #                                "ACHHNG001749",
  #                                "ACHHNG001750",
  #                                "ACHHNG002017"),]
  # 
  # dsHHdd3<-subset(dsHHdd3, select=c(HHID,start))
  # dsHHdd3<-dsHHdd3[order(dsHHdd3$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001741",
                                "ACHHNG001742",
                                "ACHHNG001743",
                                "ACHHNG001744",
                                "ACHHNG001745",
                                "ACHHNG001745",
                                "ACHHNG001746",
                                "ACHHNG001747",
                                "ACHHNG001748",
                                "ACHHNG001749",
                                "ACHHNG001750",
                                "ACHHNG002017") &
                 (dsHH$start %in% c("1526372665",
                                    "1526376213",
                                    "1527083415",
                                    "1526899512",
                                    "1526394224",
                                    "1532337734",
                                    "1526890701",
                                    "1526294756",
                                    "1526463794",
                                    "1527065326",
                                    "1526383592",
                                    "1526397882"))),]
  
  # dsHHdd4<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd4<-subset(dsHHdd4, select=c(HHID,start))
  # dsHHdd4<-dsHHdd4[order(dsHHdd4$HHID),]
  # dsHHdd4s<-subset(dsHHdd4, select=c(HHID))
  # 
  # dsHHdd4<-dsHH[dsHH$HHID %in% c("ACHHNG001473",
  #                                "ACHHNG001558",
  #                                "ACHHNG002321",
  #                                "ACHHNG002327",
  #                                "ACHHNG002329",
  #                                "ACHHNG002331",
  #                                "ACHHNG002334",
  #                                "ACHHNG002335",
  #                                "ACHHNG002337",
  #                                "ACHHNG002338",
  #                                "ACHHNG002339",
  #                                "ACHHNG002340",
  #                                "ACHHNG002418"),]
  # 
  # dsHHdd4<-subset(dsHHdd4, select=c(HHID,start))
  # dsHHdd4<-dsHHdd4[order(dsHHdd4$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001473",
                                "ACHHNG001558",
                                "ACHHNG002321",
                                "ACHHNG002327",
                                "ACHHNG002329",
                                "ACHHNG002331",
                                "ACHHNG002334",
                                "ACHHNG002335",
                                "ACHHNG002337",
                                "ACHHNG002338",
                                "ACHHNG002339",
                                "ACHHNG002340",
                                "ACHHNG002418") &
                 (dsHH$start %in% c("1527087383",
                                    "1524584784",
                                    "1528450119",
                                    "1528449322",
                                    "1451682832",
                                    "1526565895",
                                    "1526579960",
                                    "1526569995",
                                    "1526624875",
                                    "1526570265",
                                    "1526623858",
                                    "1526566559",
                                    "1525343507"))),]
  
  # dsHHdd5<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd5<-subset(dsHHdd5, select=c(HHID,start))
  # dsHHdd5<-dsHHdd5[order(dsHHdd5$HHID),]
  # dsHHdd5s<-subset(dsHHdd5, select=c(HHID))
  # 
  # dsHHdd5<-dsHH[dsHH$HHID %in% c("ACHHNG001525",
  #                                "ACHHNG002336"),]
  # 
  # dsHHdd5<-subset(dsHHdd5, select=c(HHID,start))
  # dsHHdd5<-dsHHdd5[order(dsHHdd5$HHID),]
  # 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001525",
                                "ACHHNG002336") &
                 (dsHH$start %in% c("1526553043",
                                    "1526578001"))),]
  
  # dsHHdd6<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd6<-subset(dsHHdd6, select=c(HHID,start))
  # dsHHdd6<-dsHHdd6[order(dsHHdd6$HHID),]
  # dsHHdd6s<-subset(dsHHdd6, select=c(HHID))
  # 
  # dsHHdd6<-dsHH[dsHH$HHID %in% c("ACHHNG001528",
  #                                "ACHHNG001529",
  #                                "ACHHNG001933"),]
  # 
  # dsHHdd6<-subset(dsHHdd6, select=c(HHID,start))
  # dsHHdd6<-dsHHdd6[order(dsHHdd6$HHID),]
  # 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001528",
                                "ACHHNG001529",
                                "ACHHNG001933") &
                 (dsHH$start %in% c("1534249404",
                                    "1533048612",
                                    "1526731438"))),]
  
  # dsHHdd7<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd7<-subset(dsHHdd7, select=c(HHID,start))
  # dsHHdd7<-dsHHdd7[order(dsHHdd7$HHID),]
  # dsHHdd7s<-subset(dsHHdd7, select=c(HHID))
  # 
  # dsHHdd7<-dsHH[dsHH$HHID %in% c("ACHHNG002052"
  # ),]
  # 
  # dsHHdd7<-subset(dsHHdd7, select=c(HHID,start))
  # dsHHdd7<-dsHHdd7[order(dsHHdd7$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG002052"
  ) &
    (dsHH$start %in% c("1526305077"
    ))),]
  
  # dsHHdd8<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd8<-subset(dsHHdd8, select=c(HHID,start))
  # dsHHdd8<-dsHHdd8[order(dsHHdd8$HHID),]
  # dsHHdd8s<-subset(dsHHdd8, select=c(HHID))
  # print(dsHHdd8s, row.names = FALSE)
  # 
  # dsHHdd8<-dsHH[dsHH$HHID %in% c("ACHHNG000358",
  #                                "ACHHNG001729",
  #                                "ACHHNG002163",
  #                                "ACHHNG002168",
  #                                "ACHHNG002170",
  #                                "ACHHNG002451"
  # ),]
  # 
  # dsHHdd8<-subset(dsHHdd8, select=c(HHID,start))
  # dsHHdd8<-dsHHdd8[order(dsHHdd8$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG000358",
                                "ACHHNG001729",
                                "ACHHNG002163",
                                "ACHHNG002168",
                                "ACHHNG002170",
                                "ACHHNG002451"
  ) &
    (dsHH$start %in% c("1530540933",
                       "1526538946",
                       "1527749434",
                       "1524572674",
                       "1527838219",
                       "1525963872"
    ))),]
  
  # dsHHdd9<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd9<-subset(dsHHdd9, select=c(HHID,start))
  # dsHHdd9<-dsHHdd9[order(dsHHdd9$HHID),]
  # dsHHdd9s<-subset(dsHHdd9, select=c(HHID))
  # print(dsHHdd9s, row.names = FALSE)
  # 
  # dsHHdd9<-dsHH[dsHH$HHID %in% c("ACHHNG001683",
  #                                "ACHHNG001693",
  #                                "ACHHNG001695",
  #                                "ACHHNG001698",
  #                                "ACHHNG001986",
  #                                "ACHHNG001987",
  #                                "ACHHNG001995",
  #                                "ACHHNG002022",
  #                                "ACHHNG002162",
  #                                "ACHHNG002164",
  #                                "ACHHNG002168",
  #                                "ACHHNG002215",
  #                                "ACHHNG002343",
  #                                "ACHHNG002344",
  #                                "ACHHNG002345"),]
  # 
  # dsHHdd9<-subset(dsHHdd9, select=c(HHID,start))
  # dsHHdd9<-dsHHdd9[order(dsHHdd9$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001683",
                                "ACHHNG001693",
                                "ACHHNG001695",
                                "ACHHNG001698",
                                "ACHHNG001986",
                                "ACHHNG001987",
                                "ACHHNG001995",
                                "ACHHNG002022",
                                "ACHHNG002162",
                                "ACHHNG002164",
                                "ACHHNG002168",
                                "ACHHNG002215",
                                "ACHHNG002343",
                                "ACHHNG002344",
                                "ACHHNG002345"
  ) &
    (dsHH$start %in% c("1525937875",
                       "1525947853",
                       "1527093368",
                       "1527096506",
                       "1537449120",
                       "1537514836",
                       "1537773428",
                       "1524038727",
                       "1524568832",
                       "1527930251",
                       "1537000154",
                       "1528185258",
                       "1531490263",
                       "1537715501",
                       "1537716247" ))),]
  
  # dsHHdd10<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd10<-subset(dsHHdd10, select=c(HHID,start))
  # dsHHdd10<-dsHHdd10[order(dsHHdd10$HHID),]
  # dsHHdd10s<-subset(dsHHdd10, select=c(HHID))
  # print(dsHHdd10s, row.names = FALSE)
  # 
  # dsHHdd10<-dsHH[dsHH$HHID %in% c("ACHHNG001877"),]
  # 
  # dsHHdd10<-subset(dsHHdd10, select=c(HHID,start))
  # dsHHdd10<-dsHHdd10[order(dsHHdd10$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001877") &
                 (dsHH$start %in% c("1525937875", "1538741137"))),]
  
  # dsHHdd11<-dsHH[duplicated(dsHH$HHID),]
  # #dsHHdd11<-subset(dsHHdd11, select=c(HHID,start))
  # dsHHdd11<-dsHHdd11[order(dsHHdd11$HHID),]
  # dsHHdd11s<-subset(dsHHdd11, select=c(HHID))
  # print(dsHHdd11s, row.names = FALSE)
  # 
  # dsHHdd11<-dsHH[dsHH$HHID %in% c("ACHHNG000609",
  #                                 "ACHHNG002142",
  #                                 "ACHHNG002143",
  #                                 "ACHHNG002143",
  #                                 "ACHHNG002144",
  #                                 "ACHHNG002147"),]
  # 
  # dsHHdd11<-subset(dsHHdd11, select=c(HHID,start))
  # dsHHdd11<-dsHHdd11[order(dsHHdd11$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG000609",
                                "ACHHNG002142",
                                "ACHHNG002143",
                                "ACHHNG002143",
                                "ACHHNG002144",
                                "ACHHNG002147") &
                 (dsHH$start %in% c("1538598841",
                                    "1528107249",
                                    "1528110040","1540215615",
                                    "1528111014",
                                    "1530348131"))),]
  
  # dsHHdd12<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd12<-dsHHdd12[order(dsHHdd12$HHID),]
  # dsHHdd12s<-subset(dsHHdd12, select=c(HHID))
  # print(dsHHdd12s, row.names = FALSE)
  # 
  # dsHHdd12<-dsHH[dsHH$HHID %in% c("ACHHNG001521",
  #                                 "ACHHNG002503"),]
  # 
  # dsHHdd12<-subset(dsHHdd12, select=c(HHID,start))
  # dsHHdd12<-dsHHdd12[order(dsHHdd12$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001521",
                                "ACHHNG002503") &
                 (dsHH$start %in% c("1524843201",
                                    "1539085808"))),]
  
  # dsHHdd13<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd13<-dsHHdd13[order(dsHHdd13$HHID),]
  # dsHHdd13s<-subset(dsHHdd13, select=c(HHID))
  # print(dsHHdd13s, row.names = FALSE)
  # 
  # dsHHdd13<-dsHH[dsHH$HHID %in% c("ACHHNG001453",
  #                                 "ACHHNG001513",
  #                                 "ACHHNG001663",
  #                                 "ACHHNG001664",
  #                                 "ACHHNG001665",
  #                                 "ACHHNG001666",
  #                                 "ACHHNG001667",
  #                                 "ACHHNG001668",
  #                                 "ACHHNG001669",
  #                                 "ACHHNG001670",
  #                                 "ACHHNG001671",
  #                                 "ACHHNG001672",
  #                                 "ACHHNG001895",
  #                                 "ACHHNG001900",
  #                                 "ACHHNG001933",
  #                                 "ACHHNG001934",
  #                                 "ACHHNG001935",
  #                                 "ACHHNG002451"),]
  # 
  # dsHHdd13<-subset(dsHHdd13, select=c(HHID,start))
  # dsHHdd13<-dsHHdd13[order(dsHHdd13$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001453",
                                "ACHHNG001513",
                                "ACHHNG001663",
                                "ACHHNG001664",
                                "ACHHNG001665",
                                "ACHHNG001666",
                                "ACHHNG001667",
                                "ACHHNG001668",
                                "ACHHNG001669",
                                "ACHHNG001670",
                                "ACHHNG001671",
                                "ACHHNG001672",
                                "ACHHNG001895",
                                "ACHHNG001900",
                                "ACHHNG001933",
                                "ACHHNG001934",
                                "ACHHNG001935",
                                "ACHHNG002451") &
                 (dsHH$start %in% c("1538834867",
                                    "1525185512",
                                    "1542179449",
                                    "1526656978",
                                    "1526657121",
                                    "1527090156",
                                    "1526659904",
                                    "1527004657",
                                    "1527004214",
                                    "1526826378",
                                    "1526831162",
                                    "1526827474",
                                    "1526661169",
                                    "1527750694",
                                    "1527586989",
                                    "1533451199",
                                    "1526732178",
                                    "1526742710",
                                    "1534863599"
                 ))),]
  
  # 
  # dsHHdd14<-dsHH[duplicated(dsHH$HHID),]
  # dsHHdd14<-dsHHdd14[order(dsHHdd14$HHID),]
  # dsHHdd14s<-subset(dsHHdd14, select=c(HHID))
  # print(dsHHdd14s, row.names = FALSE)
  # 
  # dsHHdd14<-dsHH[dsHH$HHID %in% c("ACHHNG001523",
  #                                 "ACHHNG001639",
  #                                 "ACHHNG002195"),]
  # 
  # dsHHdd14<-subset(dsHHdd14, select=c(HHID,start))
  # dsHHdd14<-dsHHdd14[order(dsHHdd14$HHID),]
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001523",
                                "ACHHNG001639",
                                "ACHHNG002195") &
                 (dsHH$start %in% c("1528464175",
                                    "1526553744",
                                    "1451667026"
                 ))),]
  
  
  # dsHHdd15<-dsHH[duplicated(dsHH$HHID),]
  
  #dsHH<-subset(dsHH, select=-c(SubmissionDate,start))
  ##make EA and HH data merged
  # colnames(dsEA) <- gsub("detailsEA.", "", names(dsEA))
  # colnames(dsHH) <- gsub("detailsHH.", "", names(dsHH))
  
  
  
  #Psaltry poxy
  dsHHPsaltry<-dsHH[dsHH$HHID %in% c("ACHHNG001877","ACHHNG001898", "ACHHNG001896","ACHHNG001895"),]
  
  dsHHPsaltry$firstNameHH <- "psaltry"
  
  dsHHPsaltry$surNameHH<-ifelse(dsHHPsaltry$HHID=="ACHHNG001877", "Psaltry1",
                                          
                                          ifelse(dsHHPsaltry$HHID=="ACHHNG001898", "Psaltry2",
                                                 
                                                 ifelse(dsHHPsaltry$HHID=="ACHHNG001896", "Psaltry3","Psaltry4")))
  
  dsHH<-dsHH[!dsHH$HHID %in% c("ACHHNG001877","ACHHNG001898", "ACHHNG001896","ACHHNG001895"),]
  
  dsHH<-rbind(dsHH,dsHHPsaltry)
  
  

  dsHH <- droplevels(dsHH[dsHH$HHID != "ACHHNG123456", ])
  dsEA <- droplevels(dsEA[dsEA$EAID != "ACEANG123456" & dsEA$EAID!="ACEANG000000",  ])
  
  dsEA<-dsEA[!dsEA$EAID=="ACEANG000247",]
  
  dsEAr<-dsEA[!dsEA$EAID %in% c("ACEANG000799",
                                "ACEANG000800",
                                "ACEANG000801",
                                "ACEANG000270"),]
  
  dsEAs<-dsEA[dsEA$EAID %in% c("ACEANG000799",
                               "ACEANG000800",
                               "ACEANG000801",
                               "ACEANG000270"),]
  
  dsEAs$partner<-"IITA_Research"
  
  dsEA<-rbind(dsEAr, dsEAs)
  
  dsHH<-dsHH[!dsHH$HHID %in% c("ACHHNG002168", 
                               "ACHHNG002162",
                               "ACHHNG002164",
                               "ACHHNG002163",
                               "ACHHNG002170"),]
  
  
  # dsEA <- Q2
  # dsHH <- Q1
  
  #TANZANIA data issues:
  #Removing multipleHHname 
  
  dsHH$firstNameHH <- gsub(" ", "", dsHH$firstNameHH)
  dsHH$surNameHH <- gsub(" ", "", dsHH$surNameHH)
  dsEA$firstNameEA <- gsub(" ", "", dsEA$firstNameEA)
  dsEA$surNameEA <- gsub(" ", "", dsEA$surNameEA)
  
  dsEA$EA_Name <- paste(dsEA$firstNameEA, dsEA$surNameEA, sep=" ")
  dsHH$HH_Name <- paste(dsHH$firstNameHH, dsHH$surNameHH, sep=" ")

  
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ001608") & dsHH$HH_Name == "Sadock Joseph"), ]
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ000567") & dsHH$HH_Name == "Mwanafa Mponda"), ]
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ000585") & dsHH$HH_Name == "Fatuma Millanzi"), ]
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ000245") & dsHH$HH_Name == "Omary Chande"), ]
  
  dsHH$HH_Name<- ifelse(dsHH$HH_Name %in% "Omari Saidi", "Omari Saidi Chande", as.character(dsHH$HH_Name))
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ000764") & dsHH$HH_Name == "Issa Issa"), ]
  dsHH$HH_Name<- ifelse(dsHH$HH_Name %in% "Issa MohamediIssa", "Issa Mohamed Issa", as.character(dsHH$HH_Name))
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ001853") & dsHH$HH_Name == "Maria Shindika"), ]   
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ001855") & dsHH$HH_Name == "VICTORIA NYANSWI"), ]  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ001368") & dsHH$HH_Name == "Omary Mwangu"), ] 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ000774") & dsHH$HH_Name == "Sophia Saidi"), ] 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ000774") & dsHH$HH_Name == "Hassan Namoli"), ] 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ001853") & dsHH$HH_Name == "Maria Shindika"), ] 
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ001367") & dsHH$HH_Name == "Asha Twalbu"), ] 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ001945") & dsHH$HH_Name == "Faustine Mahilu"), ] 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ001886") & dsHH$HH_Name == "Donant Kamali"), ] 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ001149") & dsHH$HH_Name == "RUKIA DOWELA"), ] 
  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ001007") & dsHH$HH_Name == "Abdall Sheha"), ] 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ002196") & dsHH$HH_Name == "Ali Khamis"), ] 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ000007") & dsHH$HH_Name == "Ali Khamis"), ] 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ000199") & dsHH$HH_Name == "Amour Makame"), ] 
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHNG001985") & dsHH$HH_Name == "hazzan rachel"), ]
  # dsEA$EA_Name[dsEA$EA_Name== "Bakar Bakar"] <- "Bakar Hamad Bakar"
  
  
  dsHH$HH_Name[dsHH$HHID== "ACHHTZ001044"] <- "Ali Suleiman Masoud"
  
  dsEA<-dsEA[!(dsEA$EA_Name %in% c("BakarHamad Bakar")), ]
  dsEA<-dsEA[!(dsEA$EA_Name %in% c("Halima Faki")), ]
  dsEA<-dsEA[!(dsEA$EA_Name %in% c("HassinaOmar Juma")), ]
  dsEA<-dsEA[!(dsEA$EA_Name %in% c("Hasina Juma")), ]
  dsEA<-dsEA[!(dsEA$EA_Name %in% c("Ali Ali")), ]
  dsEA<-dsEA[!(dsEA$EA_Name %in% c("AliKhamis Ali")), ]
  dsEA<-dsEA[!(dsEA$EA_Name %in% c("hazzan rachel")), ]

  
  dsHH<-dsHH[!(dsHH$HHID %in% c("ACHHTZ000149") & dsHH$HH_Name == "Halima Faki"), ]
  
  #find replace
  dsHH$HHID <- as.character( dsHH$HHID)
  dsHH$HHID[dsHH$HHID== "ACHHTZ001007"] <- "ACHHTZ001005"
  dsHH$HHID[dsHH$HH_Name== "Juma Mussa"] <- "ACHHTZ001037"
  
  dsHH$HHID <- as.factor(dsHH$HHID)
  
  
  # dsEAn<-dsEA[dsEA$EAID %in% c("ACEANG000799",
  #                              "ACEANG000800",
  #                              "ACEANG000801",
  #                              "ACEANG000270"),]
  
  dsEAs$partner<-"IITA_Research"
  
  
  
  return(list(dsEA = dsEA, dsHH= dsHH))
}



# dataVAl_FR <- droplevels(dataVAl_FR[dataVAl_FR$HHID != "ACHHNG123456" & dataVAl_FR$HHID!="ACHHNG000000", ])
# dataVAl_IC <- droplevels(dataVAl_IC[dataVAl_IC$HHID != "ACHHNG123456" & dataVAl_IC$HHID!="ACHHNG000000",  ])
# dataVAl_PP <- droplevels(dataVAl_PP[dataVAl_PP$HHID != "ACHHNG123456" & dataVAl_PP$HHID!="ACHHNG000000",  ])
# 
# 
# dataVAl_SPHS <- droplevels(dataVAl_SPHS[dataVAl_SPHS$HHID != "ACHHNG123456" & dataVAl_SPHS$HHID!="ACHHNG000000",  ])
# dataVAl_CIS <- droplevels(dataVAl_CIS[dataVAl_CIS$HHID != "ACHHNG123456" & dataVAl_CIS$HHID!="ACHHNG000000",  ])
# 
# dataVAl_FR <- droplevels(dataVAl_FR[dataVAl_FR$EAID != "ACEANG123456" & dataVAl_FR$EAID!="ACEANG000000",  ])
# dataVAl_IC <- droplevels(dataVAl_IC[dataVAl_IC$EAID != "ACEANG123456" & dataVAl_IC$EAID!="ACEANG000000",  ])
# dataVAl_PP <- droplevels(dataVAl_PP[dataVAl_PP$EAID != "ACEANG123456" & dataVAl_PP$EAID!="ACEANG000000",  ])
# dataVAl_SPHS <- droplevels(dataVAl_SPHS[dataVAl_SPHS$EAID != "ACEANG123456" & dataVAl_SPHS$EAID!="ACEANG000000",  ])
# dataVAl_CIS <- droplevels(dataVAl_CIS[dataVAl_CIS$EAID != "ACEANG123456" & dataVAl_CIS$EAID!="ACEANG000000",  ])
# 
# ind(VAl_CIS, VAl_CIS_dupPlDate)
# 
# #DST
# VAl_FR <- VAl_FR[!VAl_FR$HHID == "ACHHNG123456", ]
# VAl_IC <- VAl_IC[!VAl_IC$HHID == "ACHHNG123456", ]
# VAl_PP <- VAl_PP[!VAl_PP$HHID == "ACHHNG123456",]
# 
# #Remove OYSCGA destroyed farms
# VAl_PP <- VAl_PP[!VAl_PP$HHID %in% c(
#   "ACHHNG002350",
#   "ACHHNG002347",
#   "ACHHNG002346",
#   "ACHHNG002349",
#   "ACHHNG002447",
#   "ACHHNG002122",
#   "ACHHNG002413",
#   "ACHHNG002411"), ]
# 
# dataVAl_PP <- dataVAl_PP[!dataVAl_PP$HHID %in% c(
#   "ACHHNG002350",
#   "ACHHNG002347",
#   "ACHHNG002346",
#   "ACHHNG002349",
#   "ACHHNG002447",
#   "ACHHNG002122",
#   "ACHHNG002413",
#   "ACHHNG002411"), ]
# 
# VAl_SPHS <- VAl_SPHS[!VAl_SPHS$HHID == "ACHHNG123456", ]
# VAl_CIS <- VAl_CIS[!VAl_CIS$HHID == "ACHHNG123456", ]
# 
# VAl_FR <- VAl_FR[!VAl_FR$EAID == "ACEANG123456", ]
# VAl_IC <- VAl_IC[!VAl_IC$EAID == "ACEANG123456", ]
# VAl_PP <- VAl_PP[!VAl_PP$EAID == "ACEANG123456", ]
# VAl_SPHS <- VAl_SPHS[!VAl_SPHS$EAID == "ACEANG123456", ]
# VAl_CIS <- VAl_CIS[!VAl_CIS$EAID == "ACEANG123456", ]
# 
# VAl_FR <- VAl_FR[!(VAl_FR$HHID %in% c("ACHHNG001789",
#                                       "ACHHNG123456",
#                                       "ACHHNG001757",
#                                       "ACHHNG000446",
#                                       "ACHHNG001791",
#                                       "ACHHNG001771",
#                                       "ACHHNG001772",
#                                       "ACHHNG001776",
#                                       "ACHHNG001777",
#                                       "ACHHNG001775",
#                                       "ACHHNG001773",
#                                       "ACHHNG001774",
#                                       "ACHHNG001780",
#                                       "ACHHNG001779",
#                                       "ACHHNG001778") &
#                      (VAl_FR$start %in% c("1529680742",
#                                           "1531408986",
#                                           "1527182107",
#                                           "1529683103",
#                                           "1523451926"))),]
# 
# VAl_FR <- VAl_FR[!(VAl_FR$EAID %in% c("ACEANG000026") & (is.na(VAl_FR$quantityUrea))),]
# VAl_FR <- VAl_FR[!(VAl_FR$EAID %in% c("ACEANG000022") & (VAl_FR$SubmissionDate %in% c("1526972027"))),]
# 
# ss <- VAl_FR[VAl_FR$EAID %in% c("ACEANG000048", "ACHHNG002077"),]
# ss$plantingDate<-ss$plantingDate<-"Apr 27, 2018"
# 
# dsFRr <- VAl_FR[!VAl_FR$EAID %in% c("ACEANG000048", "ACHHNG002077"),]
# VAl_FR <- rbind(dsFRr, ss)
# 
# VAl_FR <- VAl_FR[!(VAl_FR$HHID %in% c("ACHHNG001653", "ACHHNG001656", "ACHHNG001659", "ACHHNG001662") &
#                      (VAl_FR$SubmissionDate %in% c("Jun 3, 2018 7:06:19 PM","Jun 1, 2018 8:53:18 PM", "Jun 1, 2018 8:55:11 PM", "Jun 1, 2018 8:44:18 PM"))),]
# VAl_FR <- VAl_FR[!(VAl_FR$HHID %in% c("ACHHNG001754") & (VAl_FR$SubmissionDate %in% c("Jun 3, 2018 7:06:19 PM", "May 16, 2018 6:43:56 PM"))),]
# VAl_FR <- droplevels(VAl_FR)
# 
# 
# #########################################################################
# ##### get EAHH CK
# # extracting data sets for main components to be read in for the DSTs and Reward payments
# # dsEA <- subset(dsEA, select=c(EAID,detailsEA.firstNameEA, detailsEA.surNameEA,
# #                               detailsEA.phoneNrEA, operationEA.levelName, organizationEA.partner,
# #                               SubmissionDate, start))
# # 
# # correction for Magaret Asuo in CR who has 2 EAIDs; 23 is used for the VALS - in theory...
# 
# 
# ## CK corrections
# dsICr<-dataVAl_IC[!dataVAl_IC$EAID=="ACEANG000247",]
# dsICs<-dataVAl_IC[dataVAl_IC$EAID=="ACEANG000247",]
# dsICs$EAID<-"ACEANG000023"
# dataVAl_IC<-rbind(dsICr,dsICs)
# dataVAl_IC <- dataVAl_IC[!dataVAl_IC$EAID=="ACEANG000105",]
# 
# dataVAl_PP <- dataVAl_PP[!dataVAl_PP$EAID %in% c("ACEATZ000504","ACEATZ000503","ACEANG123456"),]
# dataVAl_PP <- dataVAl_PP[!(dataVAl_PP$HHID %in% c("ACHHNG002412") & dataVAl_PP$start %in% c("Aug 22, 2018 3:40:40 PM")),]
# dataVAl_PP <- dataVAl_PP[!(dataVAl_PP$HHID %in% c("ACHHNG001992") & dataVAl_PP$start %in% c("Jan 1, 2016 6:11:42 AM")),] 
# dataVAl_PP <- dataVAl_PP[!(dataVAl_PP$HHID %in% c("ACHHNG001986") & dataVAl_PP$start %in% c("Sep 21, 2018 4:24:09 PM")),] 
# 
# dataVAl_FR <- dataVAl_FR[!dataVAl_FR$EAID == "ACEANG000023", ]
# 
# 
# 
# #dataVAl_FR <- dataVAl_FR[(dataVAl_FR$EAID %in% c("ACEANG000108") & dataVAl_FR$purpose.event %in% c("event8") & dataVAl_FR$SubmissionDate %in% c("May 19, 2018 5:23:08 PM")),]
# 
# 
# 
# VAl_PP <- VAl_PP[!(VAl_PP$EAID %in% c("ACEANG000799", # removing entries of IITAResearch
#                                       "ACEANG000800",
#                                       "ACEANG000801",
#                                       "ACEANG000270")),]
# 
# VAl_PP <- VAl_PP[!VAl_PP$HHID=="ACHHNG002164",]
# # pldateissue_PP <-  unique(VAl_PP[, c("HHID", "plantingDate")])
# # plpp <- as.data.frame(table(pldateissue_PP$HHID))
# # VAl_PP_DupPlDate <- droplevels(VAl_PP[VAl_PP$HHID %in% plpp[plpp$Freq >1, ]$Var1, ])
# # VAl_PP <- droplevels(VAl_PP[!VAl_PP$HHID %in% plpp[plpp$Freq >1, ]$Var1, ])
# # VAl_PP_DupPlDate <- getLatestPlDate(VAl_PP_DupPlDate)
# # VAl_PP <- rbind(VAl_PP, VAl_PP_DupPlDate)
# VAl_FR <- VAl_FR[!VAl_FR$EAID == "ACEANG000023", ]  

