source("DVF.r")
tmap_options(max.raster = c(plot = 2e+7, view = 2e+7))
no2file=("{DVFdata}/pollution/no2.2018.tif" %>% glue)
GDALinfo("{DVFdata}/pollution/no2.2018.tif" %>% glue)

no2.raster<-stack(no2file)
# no2.raster <- projectRaster(from=no2.raster, crs=st_crs(2154)$proj4string)
# no2.raster <- crop(no2.raster, xt_as_extent(cidf %>% filter(DEP==75)))

plotRGB(no2.raster)

urllegend="https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/mod_idf_no2_2018_moyenne_annuelle/MapServer/legend?"
# browseURL(urllegend)
html = read_html(urllegend)
images <- html %>% html_nodes("img") %>% discard(is.na) %>% as.character
numero <-  html %>% html_nodes("img , .restBody td+ td") %>% 
  html_text() %>% discard(~.=="") 

legende<-tibble(value=numero, png_raw=images) %>%
  mutate(png_raw= str_replace(png_raw,"<img src=\"data:image/png;base64,", "")) %>%
  mutate(png_raw = str_replace(png_raw, "\">", "")) %>%
  mutate(png_raw = map(png_raw, base64decode)) %>% 
  mutate(png_raw = map(png_raw, readPNG)) %>% 
  transmute(value=as.integer(value), 
            R=round(255*map_dbl(png_raw, ~(.[10,10,1]))),
            G=round(255*map_dbl(png_raw, ~(.[10,10,2]))),
            B=round(255*map_dbl(png_raw, ~(.[10,10,3]))))
legende <- bind_rows(tibble(value=0, R=0, G=0, B=0), legende)

mR <- no2.raster$no2.2018.1 %>% as.matrix
mG <- no2.raster$no2.2018.2 %>% as.matrix
mB <- no2.raster$no2.2018.3 %>% as.matrix

mm<- matrix(0L, nrow=nrow(mR), ncol=ncol(mR))
lR <- legende$R
lG <- legende$G
lB <- legende$B
lv <- legende$value 
Rcpp::cppFunction("IntegerMatrix rcpp_RGB2PCS(
      IntegerVector lr,
      IntegerVector lg,
      IntegerVector lb,
      IntegerVector lv,
      IntegerMatrix mR, 
      IntegerMatrix mG,
      IntegerMatrix mB)
      {
      int n_row = mR.nrow();
      int n_col = mR.ncol();
      int l_l = lr.length();
      IntegerMatrix mm(n_row, n_col);
      IntegerVector d (l_l);
      
      for(int i=0; i<n_row; ++i) 
        {
        for(int j=0; j<n_col; ++j) 
          {
          for(int k=0; k<l_l; k++) 
            {
            d[k] = pow(lr[k]-mR(i,j),2)+pow(lg[k]-mG(i,j),2)+pow(lb[k]-mB(i,j),2);
            }
            mm(i,j) = lv[which_min(d)];
          }
        }
      return mm;
      }")
tic()
mm <- rcpp_RGB2PCS(lR,lG,lB,lv, mR, mG, mB)
toc()

no2.PCB <- raster(x=mm,template=no2.raster)
plot(no2.PCB)

rayons<- c(50, 100, 125, 150, 200)
no2.kgs <- future_map(rayons, ~focal(no2.PCB,
                                     focalWeight(no2.PCB, ., "Gauss"),
                                     na.rm=TRUE,
                                     padValue=0))
names(no2.kgs)<- str_c("no2.18.kgs", rayons) 

save_DVF(no2.kgs)

# t_idf <- load_DVF("t_idf")
# dvfplus <- load_DVF("dvfplus")
# 
# t_idf.co <- t_idf %>% st_as_sf %>% st_transform(2154) %>% st_coordinates
# dvfplus.co <- dvfplus %>% st_as_sf %>% st_transform(2154) %>% st_coordinates
# 
# t_idf[, no2.18 := raster::extract(no2.PCB, t_idf.co)]
# dvfplus <- dvfplus %>% mutate(no2.18=raster::extract(no2.PCB, dvfplus.co))
# 
# walk2(no2.kgs, names(no2.kgs), function(x,y) t_idf[, (y):= raster::extract(..x, t_idf.co) ])
# walk2(no2.kgs, names(no2.kgs), function(x,y) dvfplus <<-dvfplus %>% mutate(!!y:= raster::extract(!!x, dvfplus.co) ))
# 
# save_DVF(t_idf)
# save_DVF(dvfplus)

# 
# stack("https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/mod_idf_no2_2018_moyenne_annuelle/MapServer")
# 
# GET("https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/MOD_IDF_NO2_2014_Bilan_annuel/MapServer/0") %>% content
# 
# GET("https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/MOD_IDF_NO2_2014_Bilan_annuel/MapServer?view=wmtsview&cacheKey=9f843285c75a8843")
# 
# 
# base_url = "https://tiles.arcgis.com"
# endpoint = "/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/"
# q = list(request = "GetCapabilities")
# qtile= list(request= "GetTile", layer="0", crs="EPSG:3857", level="23")
# res = httr::GET(url = httr::modify_url(base_url, path = endpoint), query = q)
# res
# browseURL(res$url)
# 
# qf = list(request = "Map")
# file = tempfile(fileext = ".gml")
# httr::GET(url = httr::modify_url(base_url, path = endpoint), query = qf)
# 
# 
# 
# Source
# url='https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/mod_idf_no2_2018_moyenne_annuelle/MapServer' 
# 
# table="" sql=
# crs='EPSG:3857'
# format='' 
# layer='0' 
# 
# url="https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/MOD_IDF_NO2_2014_Bilan_annuel/MapServer/WMTS/tile/1.0.0/MOD_IDF_NO2_2014_Bilan_annuel/"
# Style="default"
# TileMatrixSet="default028mm"
# TileMatrix="16"
# TileRow="1"
# TileCol="1"
# q="{Style}/{TileMatrixSet}/{TileMatrix}/{TileRow}/{TileCol}" %>% glue
# req=str_c(url, q)
# GET(req)
# 
# library(httr)
# base_url = "https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ"
# endpoint = "/arcgis/rest/services/mod_idf_no2_2018_moyenne_annuelle/MapServer/legend"
# q = list(request = "GetCapabilities")
# res = httr::GET(url = str_c(base_url, endpoint), query = q)
# browseURL(res$url)
# txt = httr::content(res, "text")
# xml = xml2::read_html(txt)
# print(xml)
# httr::modify_url(base_url, path = endpoint)
# 
# 
# 
# "https://wxs.ign.fr/CLEF/geoportail/wmts?SERVICE=WMTS&REQUEST=GetTile
# &VERSION=1.0.0&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIXSET=PM
# &TILEMATRIX=14&TILECOL=8180&TILEROW=5905&STYLE=normal&FORMAT=image/jpeg"	
# https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/MOD_IDF_NO2_2014_Bilan_annuel/MapServer/WMTS?
# ref="https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/MOD_IDF_NO2_2014_Bilan_annuel/MapServer/WMTS?"
# req1="service=WMTS&request=GetTile&version=1.0.0&layer=MOD_IDF_NO2_2014_Bilan_annuel"
# req2="&TileMatrixSet=default028mm&TileMatrix=16&TileCol=33400&TileRow=22700&style=default&format=image/jpegpng"
# r<-GET(url=str_c(ref, req1, req2))
# rc <- content(r)
# rc %>% str
# df
# 
# 
# url="https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/MOD_IDF_NO2_2014_Bilan_annuel/MapServer/WMTS?"
# request="SERVICE=WMTS&REQUEST=GetCapabilities&VERSION=1.0.0"
# r <- GET(str_c(url, request))
# browseURL(r$url)
# 
# library(xml2)
# library(png)
# library(httr)
# library(rvest)
# library(png)
# library(base64enc)
# 
# urllegend="https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/MOD_IDF_NO2_2014_Bilan_annuel/MapServer/legend?"
# r <- GET(str_c(urllegend))
# browseURL(r$url)
# 
# txt = httr::content(r, "text")
# html = read_html(urllegend)
# 
# images <- html %>% html_nodes("img") %>% discard(is.na) %>% as.character
# numero <-  html %>% html_nodes("img , .restBody td+ td") %>% 
#   html_text() %>% discard(~.=="") 
# 
# legende<-tibble(value=numero, png_raw=images) %>%
#   mutate(png_raw= str_replace(png_raw,"<img src=\"data:image/png;base64,", "")) %>%
#   mutate(png_raw = str_replace(png_raw, "\">", "")) %>%
#   mutate(png_raw = map(png_raw, base64decode)) %>% 
#   mutate(png_raw = map(png_raw, readPNG)) %>% 
#   transmute(value, 
#             R=map_dbl(png_raw, ~(.[10,10,1])),
#             G=map_dbl(png_raw, ~(.[10,10,2])),
#             B=map_dbl(png_raw, ~(.[10,10,3])))
#          
# l <- xml_find_all(xml, "body")
# 
# legend <- l[[1]]$div$table$tr$td$table
# df<-map(legend, ~(list(valeur=.[[3]][[1]] , img=attr(.$td$img, "src"))))
# df
# image_read(base64enc::base64decode(df$img[[1]]))
# 
# df2 <- df %>%  mutate(R=png::readPNG(base64enc::base64decode(str_sub(img, 23)))[3,3,1],
#                G=png::readPNG(base64enc::base64decode(str_sub(img, 23)))[3,3,2],
#                B=png::readPNG(base64enc::base64decode(str_sub(img, 23)))[3,3,3])
# view(df2)
# library(magick)
# 
# readPNG(as.raw("iVBORw0KGgoAAAANSUhEUgAAABQAAAAUCAYAAACNiR0NAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAOxAAADsQBlSsOGwAAAC9JREFUOI1jYaAyYKGdgWdW/afIJJMwRlQDqQRGDRw1cNTAUQPpbSC0PKOegVQCADWaA9zebsd0AAAAAElFTkSuQmCC"))
# content(GET("https://tiles.arcgis.com/tiles/gtmasQsdfwbDAQSQ/arcgis/rest/services/MOD_IDF_NO2_2014_Bilan_annuel/MapServer/WMTS/1.0.0/WMTSCapabilities.xml"))
# 
# df$img[[1]]
# library(rgdal)
# ogrDrivers()$name
