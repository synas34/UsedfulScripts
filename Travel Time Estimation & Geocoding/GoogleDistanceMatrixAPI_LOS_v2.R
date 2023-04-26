
#*******************************************************************************
#*******************************************************************************
#*client code for google distance matrix API 
#*in japan only car and walking modes are available
#*Author: Parady
#*Start Date:   2023/3/28
#*R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"
#*Serenity now!
#*******************************************************************************
#*******************************************************************************

#load packages
library("RCurl")
library("httr")
library("rjson")
library("tibble")

LOS.google<-function(origin,destination,
                     mode="driving",
                     avoid="",traffic_model="",
                     departure_date="",departure_time="",
                     unit="metric",sourceEncoding="UTF-8",
                     APIkey,
                     rest.time=25){
  
  dist.m<-numeric(length(origin))
  time.s<-numeric(length(origin))
  time.in.traffic.s<-numeric(length(origin))
  
  #only car and walking are available modes
  if(!mode%in%c("driving","walking")){
    stop("Only \"driving\" and \"walking\" are available with this API")
  }
  
  #supported "avoid" parameters" (from API documentation)
  #tolls indicates that the calculated route should avoid toll roads/bridges.
  #highways indicates that the calculated route should avoid highways.
  #ferries indicates that the calculated route should avoid ferries.
  #indoor indicates that the calculated route should avoid indoor steps
  #for walking and transit directions. 
  if(!avoid%in%c("","tolls","highways","ferries","indoor")){
    stop("\"avoid\" parameters are limited to \"tolls\",\"highways\",
       \"ferries\",\"indoor\". Check input.")
  }
  
  #supported "traffic model" parameters  (from API documentation)
  #1.best_guess (default) indicates that the returned duration_in_traffic should be
  #the best estimate of travel time given what is known about both historical 
  #traffic conditions and live traffic. Live traffic becomes more important the 
  #closer the departure_time is to now.
  #2.pessimistic indicates that the returned duration_in_traffic should be 
  #longer than the actual travel time on most days, though occasional days with
  #particularly bad traffic conditions may exceed this value.
  #3.optimistic indicates that the returned duration_in_traffic should be shorter
  # than the actual travel time on most days, though occasional days with 
  # particularly good traffic conditions may be faster than this value. 
  
  #The default value of best_guess will give the most useful predictions 
  #for the vast majority of use cases. It is possible the best_guess travel time
  #prediction may be shorter than optimistic, or alternatively, longer than 
  #pessimistic, due to the way the best_guess prediction model integrates 
  #live traffic information. 
  
  if(!traffic_model%in%c("","best_guess","pessimistic","optimistic")){
    stop("\"traffic_model\" parameters are limited to \"best_guess\",\"pessimistic\",
       \"optimistic\". Check input.")
  }
  
  #departure time (from API documentation)
  #Specifies the desired time of departure. You can specify the time as an 
  #integer in seconds since midnight, January 1, 1970 UTC. If a departure_time 
  #later than 9999-12-31T23:59:59.999999999Z is specified, the API will fall back
  #the departure_time to 9999-12-31T23:59:59.999999999Z. Alternatively, you can 
  #specify a value of now, which sets the departure time to the current time 
  #(correct to the nearest second). The departure time may be specified in 
  #two cases:
  
  #1) For requests where the travel mode is transit: You can optionally specify
  # one of departure_time or arrival_time. If neither time is specified,
  # the departure_time defaults to now
  # (that is, the departure time defaults to the current time).
  # 
  #2) For requests where the travel mode is driving: 
  #You can specify the departure_time to receive a route and trip duration
  # (response field: duration_in_traffic) that take traffic conditions into account. 
  # The departure_time must be set to the current time or some time in the future.
  #  It cannot be in the past. 
  if(mode=="walking" | departure_date=="" | departure_time==""){
    dep_time=""
  }else{
    dep_time<-paste(departure_date,departure_time)
    #convert time to integer in seconds since 1,1,1970 UTC
    dep_time<-dep_time%>%strptime("%Y-%m-%d %H:%M:%S")%>%
      as.numeric(as.POSIXct(dep_time, tz="UTC", origin="1970-01-01"))
    
    if(is.na(dep_time) | is.nan(dep_time)){
      stop("attempting to convert departure returned NA or NAN. Check input:
           date format must be %Y-%m-%d and time format must be %H:%M:%S")
    }
  }

  
  # Request object from API
  for (i in 1:length(origin)){
    url<-paste0("maps.googleapis.com/maps/api/distancematrix/json?",
                "origins=",origin[i],"",
                "&destinations=",destination[i],
                "&mode=",mode,
                "&avoid=",avoid,
                "&traffic_model=",traffic_model,
                "&units=",unit,
                "&departure_time=",dep_time)
    
    encoded_url = utils::URLencode(paste0("https://", url, "&key=",APIkey))
    r<-GET(encoded_url,encoding="UTF-8")
    dataquery<-content(r)
    
    #distance given in meters
    tryCatch(dist.m[i]<-dataquery$rows[[1]]$elements[[1]]$distance$value,
             error=function(e) NULL)
    
    #time given in seconds
    tryCatch(time.s[i]<-dataquery$rows[[1]]$elements[[1]]$duration$value, 
             error=function(e) NULL)
    
    if(dep_time!=""){
      #time in traffic in seconds
      tryCatch(time.in.traffic.s[i]<-dataquery$rows[[1]]$elements[[1]]$duration_in_traffic$value, 
               error=function(e) NULL)
    }else{
      time.in.traffic.s[i]<-NA
    }
    
    if(i%%100==0){
      Sys.sleep(rest.time)
    }
    
  }
  
  output<-tibble(origin=origin, destination=destination,mode=mode,
                     distance.m=dist.m,time.s=time.s,time.in.traffic.s)
  
  return(output)
}


