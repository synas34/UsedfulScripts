library("httr")
library("data.table")

Geocoding<-function(AdList,Key){
  APIkey<-Key 
  addr<-AdList
  lati<-numeric(length(addr))
  long<-numeric(length(addr))
  # Request object from API
  for (i in 1:length(addr)){
    r<-GET(
      "https://maps.googleapis.com/maps/api/geocode/json?",
      query = list(
        address=addr[i],
        key=APIkey
      ),
      encoding="UTF-8"
    )
    
    dataquery<-content(r)
    #distance given in meters
    tryCatch(lati[i]<-dataquery$results[[1]]$geometry$location$lat, error=function(e) NULL)
    
    #time given in seconds
    tryCatch(long[i]<-dataquery$results[[1]]$geometry$location$lng, error=function(e) NULL)
    
    }

output<-data.table(address=addr,latitude=lati,longitude=long)
}

