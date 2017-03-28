rm(list = ls())
#删除目前环境的所有东西

setwd("")#需要自己填写自己的地址
#设置自己的工作目录


library(rjson)
library(bitops)
library(RCurl)
library(baidumap)#这个包需要在github上下载，官方cran没有
#下面的包需要这些函数


options(warn =-1)#别那么多没用的warning报告心烦
showConnections(all = T)#看看现在r的网络链接情况
closeAllConnections()#关闭现在r形成的所有链接



#定义一些基本变量
AK <- ""
keyname <- "交通设施"
city <- "上海"
tags <- "交通设施;飞机场"
page_num_p <- 6


#形成访问api需要的url
url <- paste0("http://api.map.baidu.com/place/v2/search?query=",keyname,"&tag=",tags,"&page_size=20","&page_num=",page_num_p,"&scope=2","&region=",city,"&output=json&ak=","你的ak")
url


#找一个区域内的20页网页中的所有内容的函数
find_inacity <- function(keyname,city,tags){
          tem_result1 <- c()
          df <- c()
          location <- c()
          for(page_num_p in 0:19){
                    url <- paste0("http://api.map.baidu.com/place/v2/search?query=",keyname,"&tag=",tags,"&page_size=20","&page_num=",page_num_p,"&scope=2","&region=",city,"&output=json&ak=",AK)
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
                              detail_price <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$price))
                              detail_overall_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$overall_rating))
                              detail_service_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$service_rating))
                              detail_environment_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$environment_rating))
                              detail_technology_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$technology_rating))
                              
                              detail_image_num <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$image_num))
                              detail_comment_num <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$comment_num))
                              
                              
                              df <- cbind(name,location,address,detail_info,detail_price,
                                          detail_overall_rating,detail_service_rating,
                                          detail_environment_rating,detail_technology_rating,
                                          detail_image_num,detail_comment_num)
                              #数据清理
                              df <- df[!duplicated(df),]
                    }
                    closeAllConnections()
          }
          df
}

#测试一下，在上海找所有的飞机场
df_test <- find_inacity(keyname,city,tags)


#写一个函数，查在矩形区域内20页中的某一页有多少个poi，不再是基于城市的了
find_page <- function(keyname,x0,y0,x1,y1,tags,page_num_p) {
          tem_result1 <- c()
          df <- c()
          i <- 0
          url <- paste0("http://api.map.baidu.com/place/v2/search?query=",keyname,"&tag=",tags,"&page_size=20","&page_num=",page_num_p,"&scope=2","&bounds=",x0,",",y0,",",x1,",",y1,"&output=json&ak=",AK)
          
          url_string <- URLencode(url)
          connect <- url(url_string)
          temp_geo <- fromJSON(paste(readLines(connect,warn = F), collapse = ""))
          tem_result <- temp_geo$results
          tem_result1<-c(tem_result1,tem_result)
          #print(paste0("爬了一页",as.character(i <- i+1)))
          closeAllConnections() 
          tem_result1
          location<-data.frame(t(sapply(tem_result1,function(x) x$location)))
          nums <- as.numeric(nrow(location))
          print(paste0("这一页的变量个数：",as.character(as.numeric(nrow(location))))) 
          nums
}


#测试
keyname <- '购物'
x0 <- 38.915
y0 <- 115.404
x1 <- 39.975
y1 <- 116.414
page_num_p <- 1
tags <- "购物;便利店"

num_test1 <-  find_page(keyname,x0,y0,x1,y1,tags,page_num_p) 

#把这一页的内容提取出来
find_page_content <- function(keyname,x0,y0,x1,y1,tags,page_num_p) {
          tem_result1 <- c()
          df <- c()
          i <- 0
          
          url <- paste0("http://api.map.baidu.com/place/v2/search?query=",keyname,"&tag=",tags,"&page_size=20","&page_num=",page_num_p,"&scope=2","&bounds=",x0,",",y0,",",x1,",",y1,"&output=json&ak=",AK)
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
                    detail_price <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$price))
                    detail_overall_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$overall_rating))
                    detail_service_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$service_rating))
                    detail_environment_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$environment_rating))
                    detail_technology_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$technology_rating))
                    
                    detail_image_num <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$image_num))
                    detail_comment_num <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$comment_num))
                    
                    
                    df <- cbind(name,location,address,detail_info,detail_price,
                                detail_overall_rating,detail_service_rating,
                                detail_environment_rating,detail_technology_rating,
                                detail_image_num,detail_comment_num)
                    
                    #数据清理
                    df <- df[!duplicated(df),]
                    
                    print("提取了一个box数据")       
          }
          df
}


#写找20页内容的函数
find_inabox <- function(keyname,x0,y0,x1,y1,tags){
          tem_result1 <- c()
          df <- c()
          i <- 0
          for(page_num_p in 0:19){
                    url <- paste0("http://api.map.baidu.com/place/v2/search?query=",keyname,"&tag=",tags,"&page_size=20","&page_num=",page_num_p,"&scope=2","&bounds=",x0,",",y0,",",x1,",",y1,"&output=json&ak=",AK)
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
                    detail_price <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$price))
                    detail_overall_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$overall_rating))
                    detail_service_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$service_rating))
                    detail_environment_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$environment_rating))
                    detail_technology_rating <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$technology_rating))
                    
                    detail_image_num <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$image_num))
                    detail_comment_num <- gsub("NULL",NA,sapply(tem_result1,function(x) x$detail_info$comment_num))
                    
                    
                    df <- cbind(name,location,address,detail_info,detail_price,
                                detail_overall_rating,detail_service_rating,
                                detail_environment_rating,detail_technology_rating,
                                detail_image_num,detail_comment_num)
                    
                    #数据清理
                    df <- df[!duplicated(df),]
                    
                    
                    print("提取了一个box20页的数据")       
          }
          df
}

#测试
data_test1 <- find_inabox("生活服务",39.915,116.404,39.975,116.414,"生活服务;公共厕所")





#下面测试用的参数
long <- 0.17
high <- 1.5
keyname <- "生活服务"
factor <- 2
x0 <- 39.360168
y0 <- 115.125657
x1 <- 41.143237
tags <- "生活服务;公共厕所"


#爬一行box
find_aline <- function(keyname,x0,y0,x1,tags,long,high,factor){
          df2<-c()
          test <- c()
          x0s <- x0+long#初始拓展的点
          y0s <- y0+high#初始 拓展的点的高
          
          while(x0s < x1 + long){ #确保爬的在需要的框内
                    test1 <- find_page(keyname,x0,y0,x0s,y0s,tags,0) 
                    Sys.sleep(0.1)
                    test2 <- find_page(keyname,x0,y0,x0s,y0s,tags,19) 
                    Sys.sleep(0.1)
                    print(paste0("long变量长度：",as.character(long)))
                    print(paste0("x0s变量长度：",as.character(x0s)))
                    print(paste0("y0s变量长度：",as.character(y0s)))
                    
                    
                    if(test1 == 1 & test2 == 1) {#都没有数据
                              print("发现第一页和最后一页都是空的")
                              df <- find_page_content(keyname,x0,y0,x0s,y0s,tags,0)
                              Sys.sleep(0.1)
                              df2<-rbind(df2,df)
                              
                              long <- long * factor
                              
                              x0 <- x0s
                              x0s <- x0s + long
                              
                    }
                    else if(test2 == 20){#都没有数据最后一页满了，调整long
                              print("发现最后一页是满的")
                              long <-  long * ((1 / 2) ^ factor)#减少long的值
                              x0s <- x0 + long #不移动，调整一个值
                              print("不移动，调整一个值")
                              
                    }else if(test1 <= 19){
                              print("只有第一页只爬第一页")
                              df <- find_page_content(keyname,x0,y0,x0s,y0s,tags,0)
                              Sys.sleep(0.1)
                              df2<-rbind(df2,df)
                              
                              i <- 0
                              long <- long * factor
                              
                              x0 <- x0s
                              x0s <- x0s + long
                              
                              
                    }else if(test1 > 1 & test2 < 20){
                              print("发现20页内有数据")
                              
                              df2<-rbind(df2,find_inabox(keyname,x0,y0,x0s,y0s,tags))
                              Sys.sleep(0.1)
                              
                              long <- long * factor
                              
                              x0 <- x0s#新起点
                              x0s<-x0s+long#拓展点点右移动
                    }
          }
          df2
}
#测试
find_aline(keyname,22.53602,114.043989,22.538424,tags,long,high,factor)


#按照瓦片，爬一列
find_multiline <- function(keyname,x0,y0,x1,y1,tags,long,high,factor){
          df3<-c()
          y0s <- y0 + high
          i2 <- 0
          
          while(y0s<y1 + high){
                    df3<-rbind(df3,find_aline(keyname,x0,y0,x1,tags,long,high,factor))
                    Sys.sleep(0.1)
                    y0s<-y0s+high
                    y0<-y0+high
                    print(paste0("high的line计数",as.character(i2 <- i2+1)))
                    
          }
          df3
}


data_shenzhen <- find_multiline("肯德基",22.40665,113.766581,22.789959,114.364493,"美食",long=0.05,high=0.05,factor=2)
data_shenzhen$lat <-  as.numeric(as.character(data_shenzhen$lat)) 
data_shenzhen$lng <-  as.numeric(as.character(data_shenzhen$lng))

#简单看看散点图
library(sp)
data_shenzhen_map_allcity<-SpatialPointsDataFrame(data.frame(as.numeric(data_shenzhen$lng),as.numeric(data_shenzhen$lat)),data_shenzhen)
plot(data_shenzhen_map_allcity)



write.csv(data_shenzhen, file = "test.csv",row.names = F)
data_shenzhen <- read.csv("test.csv",stringsAsFactors = F)





#画图
library(ggmap)
library(ggplot2)

#使用百度地图，有较大偏移
p3 <- getBaiduMap(c(lon = 114.0441518,lat = 22.5550996),zoom = 11)
ggmap(p3)

#使用谷歌地图，有较小偏移，但是需要配置好梯子，不然会报错
p3 <- get_map(location = c(lon = 114.0441518,lat = 22.5550996),
              color = "color",
              source = "google",
              maptype = "roadmap",
              language = "ch",
              zoom = 11)
ggmap(p3)


map_layer_shenzhen <- ggmap(p3)
print(map_layer_shenzhen)
map_layer_shenzhen$data


#有点版
mymap3 <- map_layer_shenzhen +
          stat_density2d(data = data_shenzhen,aes(lng, lat, alpha=..level.., fill=..level..),size=2,bins=100, geom="polygon")+
          scale_fill_gradient(low = "blue", high = "yellow")+
          scale_alpha(range = c(0.001, 0.03), guide = FALSE)+
          geom_density2d(data = data_shenzhen,aes(lng, lat), bins = 10,show.legend=F)+
          geom_point(data = data_shenzhen,aes(lng, lat))
print(mymap3)



#无点版
mymap3 <- map_layer_shenzhen +
          stat_density2d(data = data_shenzhen,aes(lng, lat, alpha=..level.., fill=..level..),size=2,bins=100, geom="polygon")+
          scale_fill_gradient(low = "blue", high = "yellow")+
          scale_alpha(range = c(0.001, 0.03), guide = FALSE)+
          geom_density2d(data = data_shenzhen,aes(lng, lat), bins = 10,show.legend=F)


print(mymap3)
