library(tidyverse)
library(knitr)
library(geosampling)
library(rgdal)
library(rgeos)
library(raster)
library(leaflet)
library(gridExtra)
library(bookdown)
options(stringsAsFactors = FALSE)
lakes=readOGR('../../lakes/ne_10m_lakes.shp')
tanzania_adm3=readOGR('../../gadm/gadm36_TZA_shp/gadm36_TZA_3.shp')
kenya_adm0a=readOGR("../../gadm/gadm36_KEN_shp/gadm36_KEN_0.shp")
kenya_adm0=gDifference(kenya_adm0a,lakes)

# load polygons
shared_border1_with_kenya <- tanzania_adm3

# remove water bodies and unify Polygons
shared_border_with_kenya <- 
  shared_border1_with_kenya[shared_border1_with_kenya$TYPE_3!='Water body',] %>% 
  gUnaryUnion


border_tanz_ken <- shared_border_with_kenya %>% 
  as("SpatialLines") %>% # transform to SpatialLines
  crop(buffer_shape(kenya_adm0,0.001)[[2]]) # crop by Kenya

# select coordinates of border
border_tanz_ken_geom <- geom(border_tanz_ken) %>% 
  as.data.frame %>% 
  dplyr::select(x,y)

# select most Western and Eastern point. Here: start and end point of line
start_end <- border_tanz_ken_geom[c(1,nrow(border_tanz_ken_geom)),]


kenya_border_area_0_to_50 <- prepare_sampling_area(
  kenya_adm0,
  border_tanz_ken,
  lakes,
  width_in_km = 50)


kenya_border_area_50_to_100_1 <- prepare_sampling_area(
  kenya_adm0,
  border_tanz_ken,
  lakes,
  width_in_km = 100,
  split_width = 49)

kenya_border_area_50_to_100 <- gDifference(kenya_border_area_50_to_100_1,
                                           kenya_border_area_0_to_50)




kenya_sampling_bins <- geosampling::prepare_sampling_bins(
  a = kenya_border_area_0_to_50,
  b = kenya_border_area_50_to_100,
  start_end = start_end,
  number_of_bins = 5)

kenya_sampling_bins <- prepare_sampling_bins_try(
  a = kenya_border_area_0_to_50,
  b = kenya_border_area_50_to_100,
  start_end = start_end,
  number_of_bins = 5)
bbox(kenya_sampling_bins)
extent(kenya_sampling_bins) %>% 
  as.matrix

p1<-merge_area(kenya_border_area_0_to_50,kenya_border_area_50_to_100) 
p1 %>% extent %>% as.matrix
