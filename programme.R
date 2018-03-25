library(tidyverse)
library(rlist)
library(Rgctc2,lib.loc='~/GitHub/R_coordination_transformation')
library(sf)
library('httr','jsonlite')


setwd('F:/Administrator/Documents/R/Mapproject/JSframe')
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
sh<-get_location('310000')[['districts']]
sh<-get_location(310000) %>% '[['('districts')
regulartable(sh,col_keys = names(sh))
str(sh)
ft <- flextable(mtcars)
ft
js$polyline<-js$polyline %>% str_split('\\|') %>% lapply(str_split,';')%>% '[['(1)%>%
          lapply(str_split,',') %>% lapply(lapply,as.numeric) %>% lapply(list.rbind)%>%
          lapply(gcj02_wgs84_matrix_matrix) %>% lapply(list) %>% st_multipolygon %>% st_sfc(crs=4326)
center<-js$center %>% str_split(',') %>% sapply(as.numeric) %>% gcj02_wgs84_point
js$center_lng_wgs84<-center[1]
js$center_lat_wgs84<-center[2]
js<-select(js,adcode,name,polyline,center,level,center_lng_wgs84,center_lat_wgs84)
js.sf<-st_sf(js)
js.sf<-readRDS('js_amap.rds')


saveRDS(js.sf,'js_amap.rds')
saveRDS(js_center.sf,'js_center.rds')

#
js_districts<-js %>% select(districts) %>% '[['(1)
names(js_districts)<-js$name
district_center<-lapply(js_districts,select,-districts) %>% list.rbind
town_center<-lapply(js_districts,select,districts) %>% lapply('[[',1) %>% lapply(lapply,select,-districts) %>%
             lapply(list.rbind) %>% list.rbind
js_center<-rbind(district_center,town_center)
js_center$center<-str_split(js_center$center,',') %>% lapply(as.numeric) %>% list.rbind %>% gcj02_wgs84_matrix_df %>% apply(1,list)%>%
        lapply(unlist) %>% lapply(st_point) %>% st_sfc(crs=4326)
js_center<-cbind(js_center,center)
js_center.sf<-st_sf(js_center)

#
js_city<-get_location('½­ËÕ')[['districts']] %>% "["('districts') %>% '[['(1) %>% '[['(1)

js_city<-lapply(js_city$adcode,get_location)  %>%list.map(districts) %>% lapply(select,-districts) %>% list.rbind
js_city$polyline<-js_city$polyline %>% str_split('\\|') %>% lapply(str_split,';') %>% lapply(lapply,str_split,',') %>%
          lapply(lapply,lapply,as.numeric) %>% lapply(lapply, list.rbind) %>%
          lapply(lapply,gcj02_wgs84_matrix_matrix) %>% lapply(lapply,list) %>%
          lapply(st_multipolygon) %>%st_sfc(crs=4326)
js_city.sf<-st_sf(js_city)
ggplot()+geom_sf(data=js_city.sf)
saveRDS(js_city.sf,'js_city_sf.rds')
#
cn<-get_location('ÖÐ¹ú')[['districts']]
cn$polyline<-cn$polyline %>% str_split('\\|') %>% lapply(str_split,';')%>% '[['(1)%>%
  lapply(str_split,',') %>% lapply(lapply,as.numeric) %>% lapply(list.rbind)%>%
  lapply(gcj02_wgs84_matrix_matrix) %>% lapply(list) %>% st_multipolygon %>% st_sfc(crs=4326)
cn.sf<-st_sf(cn)
cn.sf<-select(cn.sf,-districts)
