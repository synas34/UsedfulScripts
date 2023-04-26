
#*******************************************************************************
#*******************************************************************************
#*tidy client code for transit travel time and cost using JORUDAN 
#*Author: Parady
#*Start Date:   2023/3/29
#*R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"
#*yada yada yada!
#*******************************************************************************
#*******************************************************************************

library(httr)
library(rvest)
library(jsonlite)
library(tidyverse)

origin<-"平塚"
destination<-"春日（東京）"
yearmonth=202305
day=14
hour=18
min1=3
min2=0

LOS.jorudan<-function(origin,destination,yearmonth,day=1,hour=8,min1=0,min2=0,restTime=5){
  
  if(length(origin)!=length(destination)){
    stop("Lenght of origin vector must equal lenght of destination vector")
  }
  
  if(!is.numeric(yearmonth)){
    stop("varible yearmonth must be numeric")
  }
  
  if(!is.numeric(day)){
    stop("varible day must be numeric")
  }
  
  if(!day%in%(c(1:31))){
    stop("varible day must be numeric between 1 and 31")
  }
  
  if(!is.numeric(hour)){
    stop("varible hour must be numeric")
  }
  
  if(!hour%in%(c(0:23))){
    stop("varible hour must be numeric between 0 and 23")
  }
  
  if(!is.numeric(min1)){
    stop("varible min1 must be numeric")
  }
  
  if(!min1%in%(c(0:5))){
    stop("varible min1 must be numeric between 0 and 5")
  }
  
  if(!is.numeric(min2)){
    stop("varible min2 must be numeric")
  }
  
  if(!min2%in%(c(0:9))){
    stop("varible min2 must be numeric between 0 and 9")
  }
  
  output<-list()
  n <- length(origin)
  o2 <- numeric(n)
  d2 <- numeric(n)
  
  #Encoding ODs
  for(i in 1:n){
    o2[i] <- paste(c("",charToRaw(origin[i])),collapse="%")
    d2[i] <- paste(c("",charToRaw(destination[i])),collapse="%")
  }
  
  #Extract data from Jorudan 
  for(i in 1:n){
    print(i)
    error_flag<-F # for trycatch function below
    
    #generate url
    url<-paste("http://www.jorudan.co.jp/norikae/cgi/nori.cgi?rf=top&eok1=&eok2=&pg=0&eki1=",
               o2[i],"&Cmap1=&eki2=",
               d2[i],"&Dym=",
               yearmonth,"&Ddd=",
               day,"&Dhh=",
               hour,"&Dmn1=",
               min1,"&Dmn2=",
               min2,"&Cway=0&Cfp=1&Czu=2&S.x=48&S.y=18&S=検索&Csg=1",sep="",
               encoding="UTF-8")
    
    #read url
    jorudan<-read_html(url) 
    
    #extract route list
    jorudan_tables<-jorudan%>%html_elements("table")%>%html_table()
    
    
    
    tryCatch(jorudan_tables<-jorudan_tables[[1]][,1:5],
             error=function(e) {
               print(paste("Error during scraping. Check OD pair",origin[i],"-",destination[i]))
               error_flag<<-TRUE}
    )
    
    if(error_flag==FALSE){
      x<-tibble(origin=origin[i], 
                destination=destination[i], 
                time=jorudan_tables$X3[1], 
                transfers=as.numeric(str_extract_all(jorudan_tables$X4[1],"(?<=乗換 )\\d+(?=回)")),
                costIC=str_remove(jorudan_tables$X5[1],"円")
      )
    }else{
      x<-tibble(origin=origin[i], 
                destination=destination[i], 
                time=NA, 
                transfers=NA,
                costIC=NA)
    }
    
    #add output to list        
    output[[i]]<-x
    
    if(i%%100==0){
      Sys.sleep(restTime)
    }
  }
  output<-output%>%bind_rows()
  return(output)
}

output<-list()
n <- length(origin)
o2 <- numeric(n)
d2 <- numeric(n)

#Encoding ODs
for(i in 1:n){
  o2[i] <- paste(c("",charToRaw(origin[i])),collapse="%")
  d2[i] <- paste(c("",charToRaw(destination[i])),collapse="%")
}

#Extract data from Jorudan 
for(i in 1:n){
  print(i)
  error_flag<-F # for trycatch function below
  
  #generate url
  url<-paste("http://www.jorudan.co.jp/norikae/cgi/nori.cgi?rf=top&eok1=&eok2=&pg=0&eki1=",
             o2[i],"&Cmap1=&eki2=",
             d2[i],"&Dym=",
             yearmonth,"&Ddd=",
             day,"&Dhh=",
             hour,"&Dmn1=",
             min1,"&Dmn2=",
             min2,"&Cway=0&Cfp=1&Czu=2&S.x=48&S.y=18&S=検索&Csg=1",sep="",
             encoding="UTF-8")
  
  #read url
  jorudan<-read_html(url) 
  
  #extract route list
  jorudan_tables<-jorudan%>%html_elements("table")%>%html_table()
}

test<-LOS.jorudan(origin,destination,202305)


