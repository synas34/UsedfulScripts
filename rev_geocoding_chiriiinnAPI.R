
#*******************************************************************************
#*******************************************************************************
#*Reverse geocoding API from Chiriiin
#*Author: Parady
#*Start Date:   2023/3/27
#*Last modified: 
#*R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"
#*Grind the pepper!
#*******************************************************************************
#*******************************************************************************

library("httr")
library("dplyr")

rev_geocoding_GSI<-function(latList,lonList,addressOnly=T,sleeptime=0.2){
  if(length(latList)!=length(lonList)){
    stop("Length of latitude and longitude vectors must be the same.")
  }
  
  munic<-character(length(latList))
  addr<-character(length(latList))

  # Request object from API
  for (i in 1:length(addr)){
    
    this.url<-paste0(
      "https://mreversegeocoder.gsi.go.jp/reverse-geocoder/LonLatToAddress?lat=",
      latList[i],"&lon=",lonList[i])
    
    r<-GET(this.url,encoding="UTF-8")
    
    dataquery<-content(r)
    #distance given in meters
    tryCatch(munic[i]<-dataquery[["results"]][["muniCd"]], error=function(e) NULL)
    
    #time given in seconds
    tryCatch(addr[i]<-dataquery[["results"]][["lv01Nm"]], error=function(e) NULL)
    print(i)
    Sys.sleep(sleeptime)}
  
  if(addressOnly==T){
    output<-tibble(Address=addr)
  }else{
    output<-tibble(Address=addr,Municipality=munic,latitude=latList,longitude=lonList)
  }

  return(output)
}