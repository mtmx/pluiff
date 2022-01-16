
library(tidyverse)
library(sf)
######### carto admin

if (dir.exists("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18")) {
  # importer shape des communes France métro
  comm <- st_read("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/ADMINEXPRESS/1_DONNEES_LIVRAISON_2017-01-18/ADE_1-0_SHP_LAMB93_FR/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) %>%
    mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                          ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                                 ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
    group_by(CODGEO) %>%
    summarize(NOM_COMM = first(NOM_COM),
              STATUT = first(STATUT),
              POPULATION = sum(POPULATION)) %>% 
    st_buffer(dist = 0) %>%
    mutate(superficie_ha = as.numeric(st_area(.)) /10000)
  
  
} else {
  url_comm <- "https://wxs-telechargement.ign.fr/x02uy2aiwjo9bm8ce5plwqmr/telechargement/prepackage/ADMINEXPRESS-PACK_2017-01-18$ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/file/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z"
  download.file(url_comm, destfile = "/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z")
  system("7z x -o/tmp /tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18.7z")
  # importer shape des communes France métro
  
  comm <- st_read("/tmp/ADMINEXPRESS_1-0__SHP_LAMB93_FXX_2017-01-18/ADMINEXPRESS/1_DONNEES_LIVRAISON_2017-01-18/ADE_1-0_SHP_LAMB93_FR/COMMUNE.shp" , stringsAsFactors = F) %>% st_transform(crs = 2154) %>%
    mutate(CODGEO= ifelse(substr(INSEE_COM,1,2) == '75' ,'75056',
                          ifelse(substr(INSEE_COM,1,3) == '132' ,'13055',
                                 ifelse(substr(INSEE_COM,1,4) == '6938' ,'69123',INSEE_COM)))) %>%
    group_by(CODGEO) %>%
    summarize(NOM_COMM = first(NOM_COM),
              STATUT = first(STATUT),
              POPULATION = sum(POPULATION)) %>% 
    st_buffer(dist = 0) %>%
    mutate(superficie_ha = as.numeric(st_area(.)) /10000)
  
}

# shapes contours france
fr <- comm %>% mutate(dep = substr(CODGEO, 1,2))  %>% summarise(POPULATION = sum(POPULATION))
dep <- comm %>% mutate(dep = substr(CODGEO, 1,2))  %>% group_by(dep) %>% summarise(POPULATION = sum(POPULATION))
dep_ctr <- dep %>% st_centroid(of_largest_polygon = T)
#fr.sp <- as(fr , 'Spatial')

# sélection de communes majeures
comm.sel <- comm %>% 
  filter(POPULATION > 500000|STATUT %in% c('Préfecture de région')|STATUT %in% c('Préfecture') & !substr(CODGEO,1,2) %in% c('77','78','91','92','93','94','95')) %>%
  st_centroid()
#comm.sel.sp <- as(comm.sel , 'Spatial')

# shapes pays europe

if (file.exists("/tmp/ne_10m_admin_0_map_subunits.shp")) {
  
  pays_ned <- st_read('/tmp/ne_10m_admin_0_map_subunits.shp', stringsAsFactors = F) %>%
    filter(REGION_UN %in% 'Europe') %>%
    ungroup() %>%
    group_by(SOV_A3, SOVEREIGNT) %>%
    summarise() %>% ungroup() %>%
    st_transform(crs = 2154) %>%
    st_simplify(dTolerance = 2000) 
  
} else {
  
  url_pays_ned <- 'http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_map_subunits.zip'
  download.file(url_pays_ned, destfile = "/tmp/ne_10m_admin_0_map_subunits.zip")
  system("7z x -o/tmp /tmp/ne_10m_admin_0_map_subunits.zip")
  pays_ned <- st_read('/tmp/ne_10m_admin_0_map_subunits.shp', stringsAsFactors = F) %>%
    filter(REGION_UN %in% 'Europe') %>%
    ungroup() %>%
    group_by(SOV_A3, SOVEREIGNT) %>%
    summarise() %>% ungroup() %>%
    st_transform(crs = 2154) %>%
    st_simplify(dTolerance = 2000) 
}

# export

st_write(comm.sel, "./carto/comm_sel.shp")
st_write(fr, "./carto/fr.shp")
st_write(pays_ned , "./carto/pays_ned.shp")
