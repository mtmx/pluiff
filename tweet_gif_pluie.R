
# chargement des libraires
library(dplyr)
library(lubridate)
library(stringr)
library(raster)
library(rgdal)
library(sf)
library(scales)
library(viridis)
library(animation)
library(fields)
library(cartography)
library(png)
library(magick)
library(rtweet)
library(purrr)


# import couches carto
comm.sel <- st_read("./carto/comm_sel.shp")
fr <- st_read( "./carto/fr.shp")
pays_ned <- st_read( "./carto/pays_ned.shp")

# affichage des logos
logoing_func<-function(logo, x, y, size){
  dims<-dim(logo)[1:2]
  AR<-dims[1]/dims[2]
  par(usr=c(0, 1, 0, 1))
  rasterImage(logo, x-(size/2), y-(AR*size/2), x+(size/2), y+(AR*size/2), interpolate=TRUE)
}
logo_MF <- readPNG('./logos/logo_MF.png')
logo_IO <- readPNG('./logos/logo_IO.png')

#################################################
# fonction pour importer les tiffs d'une journee

jour_j <- as.character(Sys.Date())
jour_j1 <- as.character(Sys.Date() +1)
jour_j2 <- as.character(Sys.Date() +2)
# récupération du run de midi
ref_time <- paste0(jour_j,'T12:00:00')

# token pour accéder aux geoservices de meteo france (à demander à support.inspire@meteo.fr)
token_MF <- Sys.getenv("MF_TOKEN")

# emprise géographique en wgs84
lat_min <- 39
lat_max <- 53
lon_min <- -8
lon_max <- 13
bornes_lat <- paste0(as.character(lat_min),',',as.character(lat_max))
bornes_long <- paste0(as.character(lon_min),',',as.character(lon_max))

## téléchargment avec relance après échec
download.file.trycatch <- function(url, destfile , method){
  tryCatch(
    # premier essai
    {
      download.file(url, destfile , method)
    },
    # nouvel essai
    error=function(error_message) {
      message("retry in 10 seconds")
      Sys.sleep(10)
      download.file.trycatch(url, destfile , method)
    }
  )
}


# # téléchargement et nettoyage des tiffs
# get_tiff <- function(H){
#   
#   # horaire + 1 heure pour récupérer les precipitations tombées dans l'heure précédente
#   time <- data_frame(time = paste0( jour_j1,'T',H,':00:00')) %>%
#     mutate(time = ifelse(H ==24,  paste0( jour_j2,'T00:00:00'), time)) %>% as.character()
#   
#   # quantité de précipitation
#   indic <- 'TOTAL_PRECIPITATION__GROUND_OR_WATER_SURFACE'
#   #indic <- 'TOTAL_WATER_PRECIPITATION__GROUND_OR_WATER_SURFACE'
#   modele <- '001'
#   url_data <- paste0('https://geoservices.meteofrance.fr/api/',token_MF,'/MF-NWP-HIGHRES-AROME-',modele,'-FRANCE-WCS?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&coverageId=',indic,'___',ref_time,'Z_PT1H&subset=time(',time,'Z)&subset=lat(',bornes_lat,')&subset=long(',bornes_long,')')
#   
#   # url_data <- paste0('https://public-api.meteofrance.fr/public/arome/1.0/wcs/MF-NWP-HIGHRES-AROME-',modele,'-FRANCE-WCS/GetCoverage?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&coverageId=',indic,'___',ref_time,'Z_PT1H&subset=time(',time,'Z)&subset=lat(',bornes_lat,')&subset=long(',bornes_long,')')
#   # 
#   # téléchargement du tiff
#   #download.file(url_data, destfile = paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'),mode="wb")
#   # download.file(url_data, destfile = paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'),method = "libcurl")
#   download.file.trycatch(url = url_data, 
#                          destfile = paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'),
#                          method = "libcurl")
#   # download_retry(url = url_data, destfile = paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'),method = "libcurl", N.TRIES = 10)
#   # resoudre probleme fail dl ? https://cran.r-project.org/web/packages/downloader/downloader.pdf
#   
#   # lecture en spatial grid data frame
#   tif <- readGDAL(paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'))
#   #transformation en raster en proj wgs 84 avec bornes lat/long
#   gridded(tif) <- TRUE
#   r <- raster(tif)
#   xmin(r) <- lon_min
#   xmax(r) <- lon_max
#   ymin(r) <- lat_min
#   ymax(r) <- lat_max
#   crs(r) <- "+proj=longlat +datum=WGS84"
#   
#   # conversion en lambert 93
#   l93 <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#   r <- projectRaster(r, crs = l93)
#   assign(paste0("r_precip_",H), r,  envir=globalenv())
#   # #suppression du fichier tiff
#   if (file.exists(paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff')))
#     file.remove(paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'))
#   
# }
# 
# # liste des 24 heures en caractere
# liste_H <- seq(0,24,1) %>% as.character() %>% str_pad(., 2, pad = "0")
# # recupéreration de tous les tiffs (sauf h00)
# liste_H[-1] %>% purrr::map(get_tiff)

# téléchargement des tiffs
dl_tiff <- function(H){
  
  # horaire + 1 heure pour récupérer les precipitations tombées dans l'heure précédente
  time <- data_frame(time = paste0( jour_j1,'T',H,':00:00')) %>%
    mutate(time = ifelse(H ==24,  paste0( jour_j2,'T00:00:00'), time)) %>% as.character()
  
  # quantité de précipitation
  indic <- 'TOTAL_PRECIPITATION__GROUND_OR_WATER_SURFACE'
  #indic <- 'TOTAL_WATER_PRECIPITATION__GROUND_OR_WATER_SURFACE'
  modele <- '001'
  url_data <- paste0('https://geoservices.meteofrance.fr/api/',token_MF,'/MF-NWP-HIGHRES-AROME-',modele,'-FRANCE-WCS?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&coverageId=',indic,'___',ref_time,'Z_PT1H&subset=time(',time,'Z)&subset=lat(',bornes_lat,')&subset=long(',bornes_long,')')
  
  # url_data <- paste0('https://public-api.meteofrance.fr/public/arome/1.0/wcs/MF-NWP-HIGHRES-AROME-',modele,'-FRANCE-WCS/GetCoverage?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&coverageId=',indic,'___',ref_time,'Z_PT1H&subset=time(',time,'Z)&subset=lat(',bornes_lat,')&subset=long(',bornes_long,')')
  # 
  # téléchargement du tiff
  #download.file(url_data, destfile = paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'),mode="wb")
  # download.file(url_data, destfile = paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'),method = "libcurl")
  download.file.trycatch(url = url_data, 
                         destfile = paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'),
                         method = "libcurl")
  # download_retry(url = url_data, destfile = paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'),method = "libcurl", N.TRIES = 10)
  # resoudre probleme fail dl ? https://cran.r-project.org/web/packages/downloader/downloader.pdf
  
  
}

# liste des 24 heures en caractere
liste_H <- seq(0,24,1) %>% as.character() %>% str_pad(., 2, pad = "0")
print(liste_H)
# recupéreration de tous les tiffs (sauf h00)
liste_H[-1] %>% purrr::map(dl_tiff)


# lecture en spatial grid data frame
rd_tiff <- function(H){
  
    # horaire + 1 heure pour récupérer les precipitations tombées dans l'heure précédente
    time <- data_frame(time = paste0( jour_j1,'T',H,':00:00')) %>%
      mutate(time = ifelse(H ==24,  paste0( jour_j2,'T00:00:00'), time)) %>% as.character()

    # quantité de précipitation
    indic <- 'TOTAL_PRECIPITATION__GROUND_OR_WATER_SURFACE'
    modele <- '001'
  
  tif <- readGDAL(paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'))
  #transformation en raster en proj wgs 84 avec bornes lat/long
  gridded(tif) <- TRUE
  r <- raster(tif)
  xmin(r) <- lon_min
  xmax(r) <- lon_max
  ymin(r) <- lat_min
  ymax(r) <- lat_max
  crs(r) <- "+proj=longlat +datum=WGS84"
  
  # conversion en lambert 93
  l93 <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  r <- projectRaster(r, crs = l93)
  assign(paste0("r_precip_",H), r,  envir=globalenv())
  # assign(paste0("r_precip_",H), r)
  
}

# recupéreration de tous les tiffs (sauf h00)
liste_H[-1] %>% purrr::map(rd_tiff)

# #suppression des tiff
rm_tiff <- function(H){
  
    # horaire + 1 heure pour récupérer les precipitations tombées dans l'heure précédente
    time <- data_frame(time = paste0( jour_j1,'T',H,':00:00')) %>%
      mutate(time = ifelse(H ==24,  paste0( jour_j2,'T00:00:00'), time)) %>% as.character()

    # quantité de précipitation
    indic <- 'TOTAL_PRECIPITATION__GROUND_OR_WATER_SURFACE'
    modele <- '001'
    
  if (file.exists(paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff')))
    file.remove(paste0('./tiff/tiff_',indic,'_',gsub(':','-', ref_time),'_',gsub(':','-', time), '.tiff'))
  
}

# recupéreration de tous les tiffs (sauf h00)
liste_H[-1] %>% purrr::map(rm_tiff)

# ajouter couche H0
r_precip_00 <- r_precip_01
r_precip_00[r_precip_00 >= 0] <- 0

# liste des noms des 24 couches du raster
liste_r_H <- paste0("r_precip_",liste_H) 

# stack des 24 rasters
r_precip.stack <- stack(liste_r_H %>% purrr::map(get))

# nom des 24 couches du raster
names(r_precip.stack) <- liste_r_H

######################
########## carte

# palette de couleurs et valeurs
col_pal <- viridis(100, alpha = 0.55, option = "A") %>% rev()
rng <- c(0,250) %>% sqrt()
brks <- seq(rng[1], rng[2], length.out = length(col_pal) + 1)
lbls <- data_frame(at = c(0,10,50,100,200) %>% sqrt()) %>% 
  mutate(lbl = round(at^2,0))

# fonction pour générer les images par heure
plot_png <- function(i){
  
  r.sum <- sum(r_precip.stack[[1:i]])
  H <- gsub("r_precip_","",names(r_precip.stack[[i]]))
  r <- r.sum
  slope <- terrain(4000 * r, opt = "slope")
  aspect <- terrain(4000 * r, opt = "aspect")
  
  # modifs slope et aspect si H %in% "00"
  hill <- hillShade(slope, aspect, angle = 40, azimuth = 315)
  
  # création de la carte
  
  # export png
  png(paste0("./img/carto_pluie_cumul_",jour_j1,"_",H,".png"), width=18, height=18, units="cm", res=180)
  # marges
  opar <- par(mar = c(0,0,1.2,0))
  # emprise affichage
  plot(st_geometry(fr), border = NA, col = NA)
  # hillshade 
  if(!H %in% "00"){
    plot(hill, col = grey(0:100 / 100, alpha = 0.5), maxpixels = ncell(r), legend = FALSE, axes = FALSE, box = FALSE, add = T)
  } else{
    plot(hill, col = "#C9C9C980", maxpixels = ncell(r), legend = FALSE, axes = FALSE, box = FALSE, add = T)
  }
  # contours pays
  plot(st_geometry(pays_ned), border = 'black', col = NA, add=T)
  # villes principales
  plot(st_geometry(comm.sel) , pch = 16, cex=0.3, col = "grey30", add=T)
  # raster
  plot(sqrt(r), col = col_pal, breaks = brks, maxpixels = ncell(r),   legend = FALSE, add = TRUE)
  # anntations
  text(x = 1186215, y = 7100122,labels =paste0(H,"h00"),  offset = 0.5, font= 2, cex = 1.4, col = "grey50")
  #logos
  logoing_func(logo_MF, x=0.04, y=0.09, size=0.055)
  logoing_func(logo_IO, x=0.095, y=0.09, size=0.041)
  # legende
  image.plot(sqrt(r),
             col = col_pal, breaks = brks,
             zlim = rng,
             smallplot = c(0.92, 0.94, 0.45, 0.85),
             legend.only = TRUE, legend.shrink = 1, legend.width = 5,
             axis.args = list(at = lbls$at, labels = lbls$lbl,
                              fg = "grey30", col.axis = "grey30",
                              cex.axis = 0.6, lwd.ticks = 0.2),
             legend.args = list(text = "Quantité des\nprécipitations\ncumulées (en mm)",
                                col = "grey20", side = 1, cex = 0.7, line = 1.5))
  # titre 
  layoutLayer(title = paste0(weekdays(ymd(jour_j1))," ", day(ymd(jour_j1))," ", months(ymd(jour_j1))," - prévisions de précipitations"),
              col = "#949494",
              sources = "Source : METEO-FRANCE / Données de modèle atmosphérique à aire limitée à haute résolution", 
              author = "",
              scale = NULL,
              frame = T, south = F)
  dev.off()
  
}

# générer les 25 images
c(1:25) %>% purrr::map(plot_png)

###################
# création du gif

list.files(path = "./img", pattern = paste0("carto_pluie_cumul_",jour_j1), full.names = TRUE) %>%
  sort() %>% 
  purrr::map(image_read) %>% 
  image_join() %>% 
  image_scale("x550") %>%
  image_animate(fps=4, loop = 1) %>%
  # image_morph pour fluidifier visuellement la transition mais dégrade trop la qualité de l'image  
  # image_morph(frames = 4) %>%
  image_write(paste0("./gif/gif_meteo_cumul_v1_prev",jour_j1,".gif")) 


# authentication du compte twitter
# http://rtweet.info/articles/auth.html
## store api keys (these are fake example values; replace with your own keys)


pluiff_token <- rtweet::create_token(
  app = "rtweet_plouif",
  consumer_key =    Sys.getenv("TWITTER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_API_KEY_SECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)


# pb sur les tweets sans texte
#https://github.com/mkearney/rtweet/issues/329
#devtools::install_version("rtweet", version = "0.6.8", repos = "http://cran.us.r-project.org")

# post du tweet et de l'image HD
post_tweet(status = "", media = paste0("./gif/gif_meteo_cumul_v1_prev",jour_j1,".gif"), token = pluiff_token)
post_tweet(status = "", media = paste0("./img/carto_pluie_cumul_",jour_j1,"_24.png"), token = pluiff_token)

