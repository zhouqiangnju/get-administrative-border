library("httr")
library("magrittr")
library("jsonlite")
library(Rgctc2)
library("dplyr")
library('plyr')
library('sf')

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
nj<-get_location('南京')[[6]]
nj_district<-nj$districts[[1]]
district_adcode<-nj_district$adcode
district_info<-nj_district$districts
names(district_info)<-nj_district$name
#提取镇名（name)、镇中心坐标（center)、行政级别（level）三个指标生成镇中心信息数据框。
town_center <- district_info%>% lapply(select,name,center,level) %>% list.rbind
town_center$district<-row.names(town_center)
#制作各区行政区划图，
#1、提取行政区划边界及中心坐标点信息

district_frame<- lapply(district_adcode,get_location) %>% list.map(districts)
names(district_frame)<-nj_district$name
district_center<-district_frame%>% lapply(select,adcode,name,center,level) %>% list.rbind

#2. 提取Level-0级行政区行政区划边界点坐标信息并转换成面对象
nj<-get_location('南京')  %>% '[['(6) %>% select(-districts)

nj$polyline<-nj$polyline %>% str_split('\\|') %>% lapply(str_split,';')  %>%'[['(1)%>%
          lapply(lapply,str_split,',')%>%
          lapply(lapply,lapply,as.numeric)%>% lapply(list.rbind)  %>% lapply(list.rbind)%>%
          lapply(gcj02_wgs84_matrix_matrix) %>% lapply(list)%>% #  由GCJ02坐标系转换为WGS84坐标系
          st_multipolygon %>%                                  #定义为sf类中的复多边形（multipolygon)对象
          st_sfc(crs=4326)                                     #将该列定义为sfc列，并将投影坐标系属性设定为WGS84投影
nj.sf<-st_sf(nj)                                               #将整个数据框定义为st对象
nj.sf$center<-nj.sf$center %>% str_split(',')  %>% lapply(as.numeric)
nj.sf$center_wgs84_lng<-gcj02_wgs84_lng(nj.sf$center[[1]][1],nj.sf$center[[1]][2])
nj.sf$center_wgs84_lat<-gcj02_wgs84_lat(nj.sf$center[[1]][1],nj.sf$center[[1]][2])

#3. 提取Level-1级行政区区划边界点坐标信息并转换成面对象
nj_district<-get_location('南京') %>% '[['('districts') %>% '[['('districts') %>% '[['(1)
district_poly <-lapply(nj_district$adcode,get_location) %>% list.map(districts) %>% lapply(select,polyline)
names(district_poly) <-nj_district$name
nj_district$poly<-district_poly %>% lapply(str_split,'\\|') %>% lapply(lapply,str_split,';') %>% lapply('[[',1)%>%
      lapply(lapply,str_split,',') %>% lapply(lapply,lapply,as.numeric) %>% lapply(lapply,list.rbind)%>%
      lapply(lapply,gcj02_wgs84_matrix_matrix) %>% lapply(lapply,list) %>%
      lapply(st_multipolygon) %>% st_sfc(crs=4326)
nj_district.sf<-st_sf(nj_district)

#4. 提取Level-2级行政区地理信息
nj_town<-nj_district %>% select(districts) %>% '[['(1) %>% lapply(select,-districts) %>% list.rbind
center<-nj_town$center %>% str_split(',')  %>% lapply(as.numeric)  %>% list.apply(rbind)%>%
                lapply(unlist) %>% list.rbind %>% gcj02_wgs84_matrix_df
nj_town<-cbind.data.frame(nj_town,center)

nj_town$center<-center %>% apply(1,list) %>% lapply (unlist) %>% lapply(st_point) %>% st_sfc(crs=4326)
nj_town.sf<-nj_town %>% st_sf
ggplot()+geom_sf(data=nj_town.sf)
setwd('F:/Administrator/Documents/R/Mapproject/JSframe/njframe')
saveRDS(nj_district.sf,'nj_district_sf.rds')
saveRDS(nj_town.sf,'nj_town_sf.rds')
#3. 转换坐标系

#4.
