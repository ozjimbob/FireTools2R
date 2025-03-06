# Render maps
library(tidyverse)
library(sf)
library(raster)
library(tmap)
library(tmaptools)
library(htmlwidgets)

#source("../config/global_config.r")
#source("../config/config_linux.r")
#source("fire_cell_function.r")

log_it("Creating map output directory")
# Set up output folders
dir.create(paste0(rast_temp,"/maps"))

nsw_bg = stack("nsw.tif")
nsw_bg = raster::subset(nsw_bg,c(1,2,3))
###nsw_bw <- projectRaster(nsw_bg,)

log_it("Plot biodiversity map")
v = read_sf(paste0(rast_temp,"/v_vegout.gpkg"))
v = dplyr::select(v,BioStatus)
v <-v %>%  mutate(color = case_when(BioStatus=="NoFireRegime" ~ "#ffffff22",
                                    BioStatus=="TooFrequentlyBurnt" ~ "#ff000099",
                                    BioStatus=="Vulnerable" ~ "#ff660099",
                                    BioStatus=="WithinThreshold" ~ "#99999999",
                                    BioStatus=="LongUnburnt" ~ "#00ffff99",
                                    BioStatus=="Unknown" ~ "#cccccc99"
))
log_it("Downloading OSM Background")
#CBS_bb <- bb(v)
#CBS_osm1 <- read_osm(CBS_bb, type="osm")

log_it("Project background map")
crs(tmprast)=crs(nsw_bg)
nsw_bg <- crop(nsw_bg,projectRaster(tmprast,crs=nsw_bg))
nsw_bg <- projectRaster(nsw_bg,crs=tmprast,method="bilinear")
nsw_bg[nsw_bg>255]=255
nsw_bg[nsw_bg<0]=0
log_it("Plot heritage status map")

log_it("Cropping")

ax = st_bbox(v)
stex = crop(nsw_bg,extent(ax[1],ax[3],ax[2],ax[4]))
tm = tm_shape(stex,is.master = TRUE) + tm_rgb() +
  tm_shape(v,name="Heritage Threshold Status") +
  tm_fill(col="color",
          style="cat",
          alpha = 0.7,
          title=paste0("Heritage Threshold Status ",current_year))+
  tm_add_legend(type="fill",labels=c("NoFireRegime","TooFrequentlyBurnt","Vulnerable","WithinThreshold","LongUnburnt",
                                     "Unknown"),
                col=c("white","red","orange","grey","cyan","grey20"))
log_it("Saving")
out_path = paste0(rast_temp,"/maps/heritage.png")
tmap_save(tm,out_path,width=800,units="px",dpi=150)




log_it("Map rendering complete") 

