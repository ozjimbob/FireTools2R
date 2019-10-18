# Render maps
library(tidyverse)
library(sf)
library(raster)
library(tmap)
library(tmaptools)
library(htmlwidgets)

source("../config/global_config.r")
source("../config/config_linux.r")
source("fire_cell_function.r")

log_it("Creating map output directory")
# Set up output folders
dir.create(paste0(rast_temp,"/maps"))




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
CBS_bb <- bb(v)
CBS_osm1 <- read_osm(CBS_bb, type="osm")



tm = tm_shape(CBS_osm1) + tm_raster() + 
  tm_shape(v,name="Heritage Threshold Status") +
  tm_fill(col="color",
          style="cat",
          alpha = 0.7,
          title=paste0("Heritage Threshold Status ",current_year))+
  tm_add_legend(type="fill",labels=c("NoFireRegime","TooFrequentlyBurnt","Vulnerable","WithinThreshold","LongUnburnt",
                                     "Unknown"),
                col=c("white","red","orange","grey","cyan","grey20"))

out_path = paste0(rast_temp,"/maps/heritage.png")
tmap_save(tm,out_path,width=800,units="px",dpi=150)



log_it("Plot fmz map")
v = read_sf(paste0(rast_temp,"/v_fmzout.gpkg"))
v = dplyr::select(v,FMZStatus)
v <-v %>%  mutate(color = case_when(FMZStatus=="NoFireRegime" ~ "#ffffff22",
                                    FMZStatus=="TooFrequentlyBurnt" ~ "#ff000099",
                                    FMZStatus=="Vulnerable" ~ "#ff660099",
                                    FMZStatus=="WithinThreshold" ~ "#99999999",
                                    FMZStatus=="LongUnburnt" ~ "#00ffff99",
                                    FMZStatus=="Unknown" ~ "#cccccc99"
))


tm = tm_shape(CBS_osm1) + tm_raster() +
  tm_shape(v,name="Fire Management Blocks Threshold Status") +
  tm_fill(col="color",
          style="cat",
          alpha = 0.7,
          title=paste0("Fire Management Blocks Threshold Status ",current_year))+
  tm_add_legend(type="fill",labels=c("NoFireRegime","TooFrequentlyBurnt","Vulnerable","WithinThreshold","LongUnburnt",
                                     "Unknown"),
                col=c("white","red","orange","grey","cyan","grey20"))



out_path = paste0(rast_temp,"/maps/fmz.png")
tmap_save(tm,out_path,width=800,units="px",dpi=150)







log_it("Plot SFAZ map")
v = read_sf(paste0(rast_temp,"/v_tsl_sfaz.gpkg"))
v = dplyr::select(v,SFAZStatusText)
v <-v %>%  mutate(color = case_when(SFAZStatusText=="Recently Treated" ~ "#99FF9999",
                                    SFAZStatusText=="Monitor OFH in the field" ~ "#22662299",
                                    SFAZStatusText=="Priority for Assessment and Treatment" ~ "#00ff0099"
))


tm = tm_shape(CBS_osm1) + tm_raster() +
  tm_shape(v,name="SFAZ Treatment Status") +
  tm_fill(col="color",
          style="cat",
          alpha = 0.7,
          title=paste0("SFAZ Treatment Status ",current_year))+
  tm_add_legend(type="fill",labels=c("Recently Treated","Monitor OFH In the Field","Priority for Assessment and Treatment"),
                col=c("lightgreen","darkgreen","green"))


out_path = paste0(rast_temp,"/maps/sfaz.png")
tmap_save(tm,out_path,width=800,units="px",dpi=150)



log_it("Plot SFAZ + fmz + bio map") 
v = read_sf(paste0(rast_temp,"/v_sfaz_fmz_bio_out.gpkg"))
v = dplyr::select(v,FinalStatus)
v <-v %>%  mutate(color = case_when(FinalStatus=="NoFireRegime" ~ "#ffffff22",
                                    FinalStatus=="TooFrequentlyBurnt" ~ "#ff000099",
                                    FinalStatus=="Vulnerable" ~ "#ff660099",
                                    FinalStatus=="WithinThreshold" ~ "#99999999",
                                    FinalStatus=="LongUnburnt" ~ "#00ffff99",
                                    FinalStatus=="Unknown" ~ "#cccccc99",
                                    FinalStatus=="Recently Treated" ~ "#99FF9999",
                                    FinalStatus=="Monitor OFH In the Field" ~ "#22662299",
                                    FinalStatus=="Priority for Assessment and Treatment" ~ "#00ff0099"
))


tm = tm_shape(CBS_osm1) + tm_raster() + 
  tm_shape(v,name="Heritage Fire Blocks and SFAZ Status") +
  tm_fill(col="color",
          style="cat",
          alpha = 0.7,
          title=paste0("Heritage Fire Block and SFAZ Status ",current_year))+
  tm_add_legend(type="fill",labels=c("NoFireRegime","TooFrequentlyBurnt","Vulnerable","WithinThreshold","LongUnburnt",
                                     "Unknown","Recently Treated","Monitor OFH In the Field","Priority for Assessment and Treatment"),
                col=c("white","red","orange","grey","cyan","grey20","lightgreen","darkgreen","green"))


out_path = paste0(rast_temp,"/maps/sfaz_fmz_bio.png")
tmap_save(tm,out_path,width=800,units="px",dpi=150)



log_it("Plot SFAZ + fmz map") 
v = read_sf(paste0(rast_temp,"/v_sfaz_fmz_out.gpkg"))
v = dplyr::select(v,FinalStatus)
v <-v %>%  mutate(color = case_when(FinalStatus=="NoFireRegime" ~ "#ffffff22",
                                    FinalStatus=="TooFrequentlyBurnt" ~ "#ff000099",
                                    FinalStatus=="Vulnerable" ~ "#ff660099",
                                    FinalStatus=="WithinThreshold" ~ "#99999999",
                                    FinalStatus=="LongUnburnt" ~ "#00ffff99",
                                    FinalStatus=="Unknown" ~ "#cccccc99",
                                    FinalStatus=="Recently Treated" ~ "#99FF9999",
                                    FinalStatus=="Monitor OFH In the Field" ~ "#22662299",
                                    FinalStatus=="Priority for Assessment and Treatment" ~ "#00ff0099"
))


tm =  tm_shape(CBS_osm1) + tm_raster() +
  tm_shape(v,name="FMZ SFAZ Status") +
  tm_fill(col="color",
          style="cat",
          alpha = 0.7,
          title=paste0("FMZ and SFAZ Status ",current_year))+
  tm_add_legend(type="fill",labels=c("NoFireRegime","TooFrequentlyBurnt","Vulnerable","WithinThreshold","LongUnburnt",
                                     "Unknown","Recently Treated","Monitor OFH In the Field","Priority for Assessment and Treatment"),
                col=c("white","red","orange","grey","cyan","grey20","lightgreen","darkgreen","green"))



out_path = paste0(rast_temp,"/maps/sfaz_fmz.png")
tmap_save(tm,out_path,width=800,units="px",dpi=150)



log_it("Plot TSL Fire map") 

v = read_sf(paste0(rast_temp,"/v_tsl.gpkg"))


tm = tm_shape(CBS_osm1) + tm_raster()+
  tm_shape(v,name="Time Since Last Fire") +
  tm_fill(col="TSL",style="fixed",
          alpha = 0.7,
          breaks=c(0,10,20,30,40,50,60,70,80,90,Inf),
          title=paste0("Time Since Last r. ",current_year),palette=c("red","yellow","green"))+
  tm_view(view.legend.position=c("right","top"),set.zoom.limits=c(8,11))





out_path = paste0(rast_temp,"/maps/tsl.png")
tmap_save(tm,out_path,width=800,units="px",dpi=150)


log_it("Plot Times Burnt map") 

v = read_sf(paste0(rast_temp,"/v_timesburnt.gpkg"))


tm = tm_shape(CBS_osm1) + tm_raster() +
  tm_shape(v,name="Number of Times Burnt") +
  tm_fill(col="TimesBurnt",style="fixed",
          alpha = 0.7,
          breaks=c(0,1,2,3,4,5,6,7,8,9,Inf),
          title=paste0("Number of Times Burnt r. ",current_year),palette=c("green","yellow","red","black"))


out_path = paste0(rast_temp,"/maps/ntb.png")
tmap_save(tm,out_path,width=800,units="px",dpi=150)


log_it("Map rendering complete") 