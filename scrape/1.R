t1<-tanzania_adm0 %>% 
  as("SpatialLines")
t1@lines %>% length
t1@lines[[1]]@Lines %>% length


gIntersection(tanganyika_tanzania,split_line_tanzania_tangayika)

x<-1
line_list<-lapply(1:length(tanganyika_tanzania@lines[[1]]@Lines),function(x) {
  l1<-tanganyika_tanzania@lines[[1]]@Lines[[x]]@coords %>% 
  as.data.frame() %>%
  filter(y<=tanganyika_tanzania_split_point[1,2]) 
  if (nrow(l1)==0) return()
  list(coords_to_line(l1,proj4string(tanganyika_zambia)))
})
tan_line_to_lake<-from_list_to_poly3(line_list) %>%  
  crop(tanzania_adm0a) %>% 
  gDifference(buffer_shape(zambia_adm0a,0.01)[[2]])


g_prolong +
  geom_sf(size=1.2,color="red",data=tan_line_to_lake2 %>% 
            st_as_sf()) +
  # geom_sf(fill="blue",data=buffer_shape(zambia_adm0a,0.1)[[2]] %>% st_as_sf()) +
  coord_sf(xlim = bbox(tanganyika_full)[1,]+c(1,0.5),
           ylim = bbox(tanganyika_full)[2,]+c(-0.5,-4)) 







g_base +
  coord_sf(xlim = bbox(border_for_maw2)[1,]+c(-0.5,0.5),
           ylim = bbox(border_for_maw2)[2,]+c(-0.5,-0))




