#2022/01 作成 2022/10 最終変更 coded by: 菊地穂澄（都市工学専攻修士課程まちづくり研究室） 
#Adlistには、日本語表記の住所を格納。表記ゆれや大字とは異なる地名も、ある程度は柔軟に対応してくれる。
#このコード以外に必要なものはないはず。

library("httr")
library("dplyr")

geocoding_GSI<-function(AdList){
  addr<-AdList
  lati<-numeric(length(addr))
  long<-numeric(length(addr))
  
  # Request object from API
  for (i in 1:length(addr)){
    r<-GET(paste0(
      "https://msearch.gsi.go.jp/address-search/AddressSearch?q=",
      addr[i]
    ),
    encoding="UTF-8"
    )
    
    dataquery<-content(r)
    #distance given in meters
    tryCatch(lati[i]<-dataquery[[1]][["geometry"]][["coordinates"]][[2]], error=function(e) NULL)
    
    #time given in seconds
    tryCatch(long[i]<-dataquery[[1]][["geometry"]][["coordinates"]][[1]], error=function(e) NULL)
    print(i)
    Sys.sleep(0.2) #地理院APIにアクセス間隔の要求はないが、念のため。4万地点の取得はこれでも拒否されなかった。
  }
  
  output<-tibble(Address=addr,latitude=lati,longitude=long)
}
