library("httr") 

library("magrittr") 
library("jsonlite")
library(Rgctc)
library("dplyr")
library('plyr')
library('sf')
library(zoo)
library(maptools)
library(rgdal)
library(stringr)
library(rlist)
library(ggplot2)
options(digits=11)
get_location<- function(address){  
  key = '7c6b6c0d1b641f4aa9cdb7d2229ae728'
  
  url = 'http://restapi.amap.com/v3/config/district?' %>%
    paste('keywords=' , address ,  
          '&key=' ,key ,
          '&subdistrict=3' , 
          '&extensions=all',
          sep = '')  
  city<-GET(url)%>% content(as="text",encoding="UTF-8") %>% fromJSON(flatten = TRUE)
  return(city)
}
nj<-get_location('320106')[[6]]
nj_district<-nj$districts[[1]]
district_adcode<-nj_district$adcode
district_info<-nj_district$districts
names(district_info)<-nj_district$name
#提取镇名（name)、镇中心坐标（center)、行政级别（level）三个指标生成镇中心信息数据框。
town_center <- district_info%>% lapply(select,name,center,level) %>% list.rbind
town_center$district<-row.names(town_center)
#制作各区行政区划图，
#1、提取行政区划边界及中心坐标点信息
names(district_frame)<-nj_district$name
district_frame<- lapply(district_adcode,get_location) %>% list.map(districts)
district_center<-district_frame%>% lapply(select,adcode,name,center,level) %>% list.rbind
gc[428]
gc[429]
gc[429] %>% str_replace('\\|',';')
polyline<-lapply(district_frame,select,polyline) 
gc<-polyline[[1]] %>% str_split('\\|') %>%lapply(str_split,';') 
gc2<-list.apply(gc,list.apply,str_split,',')
gc[duplicated(gc)]
gc[1]==gc[428]
district_polyline<- lapply(district_frame,select,polyline) %>% lapply(str_split ,';') %>% lapply('[[',1) %>% lapply(str_split,',')
grep_verse<-function(x,y){grep(pattern = y,x=x)} # change parameter order of grep to meet the need of lapply
odd_po<-lapply(odd,grep_verse,3)
for (i in 1:length(district_polyline)){
  if (length(odd[[i]])>0){
    for(j in 1:length(odd[[i]])){
      
    }
    
  }
    
    else{ 
      district_polyline[[i]]<-district_polyline[[i]] %>% lapply(as.numeric) %>% list.rbind
      district_center$frame[i]<-data.frame(lng_wgs84=
                                             gcj02_wgs84_lng(district_polyline[[i]][,1],district_polyline[[i]][,2]),
                                           lat_wgs84=
                                             gcj02_wgs84_lat(district_polyline[[i]][,1],district_polyline[[i]][,2])) %>%
                                apply(1,list)%>% lapply(unlist)%>%list.rbind%>%list%>% 
                                st_polygon
    }
}



x=matrix(1:12,3,4)
row(x)
js_frame<-js$polyline %>% str_split(';') %>% lapply(str_split,',') %>% '[['(1)
t<-list.apply(js_frame,length)%>% unlist
odd<-which(t==3)
js_frame<-unlist(js_frame)
js_frame[[odd]]
odd<-list.findi(js_frame,length(.)==3)

frame_1<-frame%>% list.subset(1:odd-1)
frame_1[[30]]<-frame_1[[1]]
frame_1 <- frame_1 %>% lapply(as.numeric) %>% list.rbind %>% list %>% st_polygon %>% st_sfc(crs=4326)
 frame_1<-          st_sf(id='nj_1',geometry=frame_1)

frame_2<-frame%>% list.subset(31:length(frame)) %>%
         list.insert(1,frame[[length(frame)]])%>%lapply(as.numeric)%>%
         list.rbind %>% list %>%
         st_polygon %>% st_sfc(crs=4326)
frame_2<-          st_sf(id='nj_2',geometry=frame_2)
frame_nj<-list.merge(frame_1,frame_2)

frame_nj<-list()
frame_nj[[1]]<-frame_1
frame_nj[[2]]<-frame_2  
ggplot()+geom_sf(data=frame_1)+geom_sf(data=frame_2)

frame<-frame_n
frame<-list.exclude(frame,length(.)==3)
frame<-frame %>% lapply(as.numeric) %>% list.rbind
write.csv(frame,'frame.csv')
frame<-rbind(frame,frame[1,]) %>% list %>% st_polygon %>% st_sfc(crs=4326)
nj.frame<-st_sf(id='nj',geometry=frame)
ggplot()+geom_sf(data=nj.frame)
t<-list.apply(frame,length)
nj<-lapply(nj_d$name,get_location) %>% list.map(districts)
nj_d<-get_location('南京') %>%  '[['(6) %>% '[['(7) %>% '[['(1) %>% select('name','center')
