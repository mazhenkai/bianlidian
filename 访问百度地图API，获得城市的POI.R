rm(list = ls())

#工作文档的设置
setwd("")


library(rjson)
library(bitops)
library(RCurl)
library(baidumap)

options(warn =-1)
showConnections(all = T)
closeAllConnections()


#定义我的AK
AK <- "" #这个部分需要向百度申请，输入AK码后才能访问百度api



#定义一个，找一个区域内的20页网页中的所有POI
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



#这是测试从上海找所有的肯德基POI，可删除
keyname <- "肯德基"
city <- "上海"

#写一个函数，在城市取肯德基,测试比较成功
df <- find_inacity("肯德基","上海")



# 矩形区域检索一页函数

#url <- http://api.map.baidu.com/place/v2/search?query=美食&page_size=10&page_num=19&scope=1&bounds=39.915,116.404,39.975,116.414&output=json&ak={您的密钥}


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


#这是一些用于测试的变量
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



#下列内容用于测试
#测试找一页和看方框内多少页的内容
testdata1 <- find_page_content('便利店',38.915,115.404,39.975,116.414,19)
testdata0 <- find_page('便利店',39.915,116.404,39.975,116.414,19)
testdata1 <- find_page('便利店',38.915,115.404,39.975,116.414,19)
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




#测试提取方框内所有的厕所
data_test <- find_inabox("厕所",39.915,116.404,39.975,116.414)



#爬一行box,做一个能爬一行的函数
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



#按照瓦片，从下到上爬多行。
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




#最终的收集

#便利店数据收集
#115.125657,39.360168 117.669083,41.143237

data_beijing <- find_multiline("便利店",39.360168,115.125657,41.143237,117.669083,long=0.17,high=0.25)
data_beijing$lat <-  unlist(data_beijing$lat)
data_beijing$lng <-  unlist(data_beijing$lng)
library(sp)

#地址变成r可以画图的对象
beijin_poi_map_allcity<-SpatialPointsDataFrame(data.frame(as.numeric(data_beijing$lng),as.numeric(data_beijing$lat)),data_beijing)
plot(beijin_poi_map_allcity)


#存储对象为csv
write.csv(data_beijing, file = "北京大华北便利店百度API.csv",row.names = F)



#在上海做同样的内容
#120.699502,30.51525 122.20808,31.973411
data_shanghai <- find_multiline("便利店",30.51525,120.511504,31.973411,122.20808,long=0.16,high=0.17)
data_shanghai$lat <-  unlist(data_shanghai$lat)
data_shanghai$lng <-  unlist(data_shanghai$lng)


#地址变成r可以画图的对象

shanghai_poi_map_allcity<-SpatialPointsDataFrame(data.frame(as.numeric(data_shanghai$lng),as.numeric(data_shanghai$lat)),data_shanghai)
plot(shanghai_poi_map_allcity)
write.csv(data_shanghai, file = "上海大长三角便利店百度API.csv",row.names = F)





