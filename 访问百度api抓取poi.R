rm(list = ls())

setwd("")#需要自己填写自己的地址


library(rjson)
library(bitops)
library(RCurl)
library(baidumap)

options(warn =-1)
showConnections(all = T)
closeAllConnections()


#定义我的AK
AK <- ""#这个部分需要自己申请
keyname <- "肯德基"
city <- "上海"

#找一个区域内的20页网页中的所有内容
find_inacity <- function(keyname,city){
          tem_result1 <- c()
          df <- c()
          location <- c()
          for(page_num_p in 0:19){
                    url <- paste0("http://api.map.baidu.com/place/v2/search?query=",keyname,"&page_size=20","&page_num=",page_num_p,"&scope=2","&region=",city,"&output=json&ak=",AK)
                    url_string <- URLencode(url)
                    connect <- url(url_string)
                    temp_geo <- fromJSON(paste(readLines(connect,warn = F), collapse = ""))
                    tem_result <- temp_geo$results
                    tem_result1<-c(tem_result1,tem_result)
                    
                    location<-data.frame(t(sapply(tem_result1,function(x) x$location)))
                    
                    if(dim(location)[2] == 0){
                              print("没找到这个地点")
                    }else{
                              name<-sapply(tem_result1,function(x) x$name)
                              address <- sapply(tem_result1,function(x) x$address)
                              detail_info <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$tag))
                              df <- cbind(name,location,address,detail_info)
                              df <- df[!duplicated(df),] 
                    }
                    closeAllConnections()
          }
          df
}



#写一个函数，在城市取肯德基,测试比较成功
df <- find_inacity("肯德基","上海")

# 矩形区域检索一页函数

url <- http://api.map.baidu.com/place/v2/search?query=美食&page_size=10&page_num=19&scope=1&bounds=39.915,116.404,39.975,116.414&output=json&ak={您的密钥}


find_page <- function(keyname,x0,y0,x1,y1,page_num_p) {
          tem_result1 <- c()
          df <- c()
          i <- 0
          
          url <- paste0("http://api.map.baidu.com/place/v2/search?query=",keyname,"&page_size=20","&page_num=",page_num_p,"&scope=2","&bounds=",x0,",",y0,",",x1,",",y1,"&output=json&ak=",AK)
          url_string <- URLencode(url)
          connect <- url(url_string)
          temp_geo <- fromJSON(paste(readLines(connect,warn = F), collapse = ""))
          tem_result <- temp_geo$results
          tem_result1<-c(tem_result1,tem_result)
          #print(paste0("爬了一页",as.character(i <- i+1)))
          closeAllConnections() 
          tem_result1
          location<-data.frame(t(sapply(tem_result1,function(x) x$location)))
          print(paste0("这一页的变量个数：",as.character(as.numeric(nrow(location)))))   
          as.numeric(nrow(location))
}

keyname <- '便利店'
x0 <- 38.915
y0 <- 115.404
x1 <- 39.975
y1 <- 116.414
page_num_p <- 19


find_page_content <- function(keyname,x0,y0,x1,y1,page_num_p) {
          tem_result1 <- c()
          df <- c()
          i <- 0
          
          url <- paste0("http://api.map.baidu.com/place/v2/search?query=",keyname,"&page_size=20","&page_num=",page_num_p,"&scope=2","&bounds=",x0,",",y0,",",x1,",",y1,"&output=json&ak=",AK)
          url_string <- URLencode(url)
          connect <- url(url_string)
          temp_geo <- fromJSON(paste(readLines(connect,warn = F), collapse = ""))
          tem_result <- temp_geo$results
          tem_result1<-c(tem_result1,tem_result)
          #print(paste0("爬了一页",as.character(i <- i+1)))
          closeAllConnections() 
          tem_result1
          location<-data.frame(t(sapply(tem_result1,function(x) x$location)))

          if(dim(location)[2] == 0){
                    print("小方框内没找到")
                    df <- NULL
          }else{
                    name<-sapply(tem_result1,function(x) x$name)
                    address <- sapply(tem_result1,function(x) x$address)
                    detail_info <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$tag))
                    df <- cbind(name,location,address,detail_info)
                    df <- df[!duplicated(df),] 
                    
                    print("提取了一个box数据")       
          }
          df
}

testdata1 <- find_page_content('便利店',38.915,115.404,39.975,116.414,19)

testdata0 <- find_page('便利店',39.915,116.404,39.975,116.414,19)

testdata1 <- find_page('便利店',38.915,115.404,39.975,116.414,19)


rbind(testdata0,testdata0)


testdata1 <- find_page('便利店',38.915,115.404,39.975,116.414,0)

testdata0 <- find_page('便利店',39.974,116.413,39.975,116.414,0)


#做矩形区域检索20页的函数的函数
find_inabox <- function(keyname,x0,y0,x1,y1){
          tem_result1 <- c()
          df <- c()
          i <- 0
          for(page_num_p in 0:19){
                    url <- paste0("http://api.map.baidu.com/place/v2/search?query=",keyname,"&page_size=20","&page_num=",page_num_p,"&scope=2","&bounds=",x0,",",y0,",",x1,",",y1,"&output=json&ak=",AK)
                    url_string <- URLencode(url)
                    connect <- url(url_string)
                    temp_geo <- fromJSON(paste(readLines(connect,warn = F), collapse = ""))
                    tem_result <- temp_geo$results
                    tem_result1<-c(tem_result1,tem_result)
                    Sys.sleep(0.03)
                    #print(paste0("爬了一页",as.character(i <- i+1)))
                    closeAllConnections()
          }
          location<-data.frame(t(sapply(tem_result1,function(x) x$location)))
          
          if(dim(location)[2] == 0){
                    print("小方框内没找到")
                    df <- "没找到"
          }else{
                    name<-sapply(tem_result1,function(x) x$name)
                    address <- sapply(tem_result1,function(x) x$address)
                    detail_info <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$tag))
                    df <- cbind(name,location,address,detail_info)
                    df <- df[!duplicated(df),] 
                    
                    print("提取了一个box数据")       
          }
          df
}


#测试
data_test <- find_inabox("厕所",39.915,116.404,39.975,116.414)




#写一个能自动调整长短的函数,按照瓦片，爬一行,代码失败了
smart_find_long <- function(keyname,x0,y0,x1,long,high,ratio,base){
          df2<-c()
          test <- c()
          x0s <- x0+long#初始拓展的点
          y0s <- y0+high#初始 拓展的点的高
          
          i <- 0
          i1 <- 0
          
          
          
          while(x0s<x1+long){
                    test1 <- find_page(keyname,x0,y0,x0s,y0s,0) 
                    Sys.sleep(0.1)
                    
                    
                    if(test1 == 1){
                              i <- i + 1
                              long <-  long * (1.1 ^ i)
                              x0s <- x0+long#拓展点换了
                              print(paste0("long的长度改变为：", as.character(long)))

                    }else if(test1 > 1 & test1 < 20){
                              print("开始爬取box")
                              df2<-rbind(df2,find_page_content(keyname,x0,y0,x0s,y0s,0))
                              Sys.sleep(0.1)
                              x0<-x0+long#小目标点右移动
                              x0s<-x0s+long#拓展点点右移动
                              print(paste0("box爬取结束line的box计数",as.character(i1 <- i1+1)))
                              #print(paste0("试图爬的点横坐标：",as.character(x0s)))
                              i <- 0
                              
                    }else if(test1 == 20){
                              i <- i + 1
                              print(paste0("long长度：",as.character(long)))
                              long <-   long * ((1 / 2) ^ i)
                              x0s <- x0+long#拓展点换了
                              print(paste0("long的长度改变为：", as.character(long)))
                    }else{
                              print('找不到合适的long值') 
                    }
                    
          }
          df2
}



long <- 0.17
high <- 0.05
i <-  0

x0 <- 38

#爬一行box
find_aline <- function(keyname,x0,y0,x1,long,high){
          df2<-c()
          test <- c()
          x0s <- x0+long#初始拓展的点
          y0s <- y0+high#初始 拓展的点的高

          
          
          while(x0s<x1+long){
                    i <-  0
                    test1 <- find_page(keyname,x0,y0,x0s,y0s,0) 
                    Sys.sleep(0.1)
                    test2 <- find_page(keyname,x0,y0,x0s,y0s,19) 
                    Sys.sleep(0.1)
                    print(paste0("long变量长度：",as.character(long)))
                    print(paste0("x0s变量长度：",as.character(x0s)))
                    print(paste0("y0s变量长度：",as.character(y0s)))
                    
                    
                    if(test1 == 1 & test2 == 1) {
                              df <- find_page_content(keyname,x0,y0,x0s,y0s,0)
                              Sys.sleep(0.1)
                              df2<-rbind(df2,df)
                              
                              i <- 0
                              long <- 0.17
                              
                              x0 <- x0s
                              x0s <- x0s + long
                              
                              
                    }else if(test2 == 20){
                              i <- i + 1
                              long <-  long * ((1 / 2) ^ i)#减少long的值
                              x0s <- x0 + long #不移动，调整一个值
                              print("不移动，调整一个值")
                    }else if(test1 <= 19){
                              df <- find_page_content(keyname,x0,y0,x0s,y0s,0)
                              Sys.sleep(0.1)
                              df2<-rbind(df2,df)
                              
                              i <- 0
                              long <- 0.17
                              
                              x0 <- x0s
                              x0s <- x0s + long
                              
                              
                    }else if(test1 > 1 & test2 < 20){
                              df2<-rbind(df2,find_inabox(keyname,x0,y0,x0s,y0s))
                              Sys.sleep(0.1)
                              
                              long <- 0.17
                              i <- 0
                              
                              x0 <- x0s#新起点
                              x0s<-x0s+long#拓展点点右移动
                              
                              
                    }
          }
          df2
}



#按照瓦片，爬一列
find_multiline <- function(keyname,x0,y0,x1,y1,long,high){
          df3<-c()
          y0s <- y0 + high
          i2 <- 0
          while(y0s<y1 + high){
                    df3<-rbind(df3,find_aline(keyname,x0,y0,x1,long,high))
                    Sys.sleep(0.1)
                    y0s<-y0s+high
                    y0<-y0+high
                    print(paste0("high的line计数",as.character(i2 <- i2+1)))
          }
          df3
}




#便利店数据收集
#115.125657,39.360168 117.669083,41.143237

data_beijing <- find_multiline("便利店",39.360168,115.125657,41.143237,117.669083,long=0.17,high=0.25)
data_beijing$lat <-  unlist(data_beijing$lat)
data_beijing$lng <-  unlist(data_beijing$lng)
library(sp)

beijin_poi_map_allcity<-SpatialPointsDataFrame(data.frame(as.numeric(data_beijing$lng),as.numeric(data_beijing$lat)),data_beijing)
plot(beijin_poi_map_allcity)


write.csv(data_beijing, file = "北京大华北便利店百度API.csv",row.names = F)

#120.699502,30.51525 122.20808,31.973411
data_shanghai <- find_multiline("便利店",30.51525,120.511504,31.973411,122.20808,long=0.16,high=0.17)
data_shanghai$lat <-  unlist(data_shanghai$lat)
data_shanghai$lng <-  unlist(data_shanghai$lng)

shanghai_poi_map_allcity<-SpatialPointsDataFrame(data.frame(as.numeric(data_shanghai$lng),as.numeric(data_shanghai$lat)),data_shanghai)
plot(shanghai_poi_map_allcity)
write.csv(data_shanghai, file = "上海大长三角便利店百度API.csv",row.names = F)


#小卖部数据收集
data_beijing <- find_multiline("超市",39.71852,116.1217,40.13975,116.6710,BoxStep=0.025)
data_beijing$lat <-  unlist(data_beijing$lat)
data_beijing$lng <-  unlist(data_beijing$lng)
write.csv(data_beijing, file = "北京超市百度API.csv",row.names = F)

#120.958214,30.967667  122.021807,31.531755
data_shanghai <- find_multiline("超市",31.01347,121.2139,31.48309,121.7632,BoxStep=0.025)
data_shanghai$lat <-  unlist(data_shanghai$lat)
data_shanghai$lng <-  unlist(data_shanghai$lng)
write.csv(data_shanghai, file = "上海超市百度API.csv",row.names = F)


library(ggmap)
library(baidumap)

p <- getBaiduMap(c(116.39565, 39.92999))
ggmap(p)


p1 <- getBaiduMap('北京',zoom=10)


map_layer_beijing <- ggmap(p1)
print(map_layer_beijing)
map_layer_beijing$data



center_shangahi <- c(31.2491617100151,121.487899485695)

p2 <- getBaiduMap('上海',zoom=10)
ggmap(p2)
map_layer_shanghai <- ggmap(p2)
print(map_layer_shanghai)
map_layer_shanghai$data

library(ggplot2)
mymap1 <- map_layer + geom_point(data = data_beijing,aes(x=lng ,y=lat),color="red")

mymap1 <- map_layer_beijing +
          stat_density2d(data = data_beijing,aes(lng, lat, alpha=..level.., fill=..level..),size=2,bins=100, geom="polygon")+
          scale_fill_gradient(low = "blue", high = "yellow")+
          scale_alpha(range = c(0.001, 0.03), guide = FALSE)+
          geom_density2d(data = data_beijing,aes(lng, lat), bins = 10,show.legend=F)


print(mymap1)


mymap2 <- map_layer_shanghai +
          stat_density2d(data = data_shanghai,aes(lng, lat, alpha=..level.., fill=..level..),size=2,bins=100, geom="polygon")+
          scale_fill_gradient(low = "blue", high = "yellow")+
          scale_alpha(range = c(0.001, 0.03), guide = FALSE)+
          geom_density2d(data = data_shanghai,aes(lng, lat), bins = 10,show.legend=F)


print(mymap2)



