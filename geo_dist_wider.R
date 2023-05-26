library(tidyverse)
library(readxl)
raw_geo_distance <- janitor::clean_names(read_xls("data/Geographical_distancesNOTS.xls"))
geo_distance_matrix<- raw_geo_distance %>% 
  select(., location_id,akt_id, near_dist)%>%
  pivot_wider(names_from = location_id, values_from = near_dist)%>%
  select(order(colnames(.)))

write.table(geo_distance_matrix,"data/geo_distance_matrix.csv",sep=";",row.names = F)
