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

# Get list of years
yl <- read_csv(paste0(rast_temp,"/yearlist.csv"))
log_it("Deleting individual fire year rasters")

# Delete yearly TIFs
for(this_year in year_list){
  unlink(paste0(rast_temp,"/",this_year,".tif"))
}

# Delete yearlist
log_it("Deleting ancillary files")
unlink(paste0(rast_temp,"/yearlist.csv"))

log_it("Writing Manifest")

file.copy("manifest.txt",paste0(rast_temp,"/MANIFEST.txt"))

log_it("Writing raster tables")



rx_write=function(file,outfile){
  rtable = data.frame(ID = c(1,2,3,4,5,6,7,8,9),
                      Status = c("NoFireRegime",
                                 "TooFrequentlyBurnt",
                                 "Vulnerable",
                                 "LongUnburnt",
                                 "WithinThreshold",
                                 "Recently Treated",
                                 "Monitor OFH In the Field",
                                 "Priority for Assessment and Treatment",
                                 "Unknown"))
  col_vec = c("#ffffff",
              "#ffffff","#ff0000","#ff6600","#00ffff","#999999","#99FF99","#226622","#00ff00","#cccccc")
  
  tr <- raster(paste0(rast_temp,"/",file))
  tr <- ratify(tr)
  rat <- levels(tr)[[1]]
  rat <- left_join(rat,rtable)
  levels(tr) <- rat
  colortable(tr) <- col_vec
  bigWrite(tr,paste0(rast_temp,"/",outfile))
  unlink(paste0(rast_temp,"/",file))
  
}

log_it("Writing heritage raster table")
rx_write("r_vegout.tif","r_heritage_threshold_status.tif")
log_it("Writing fmz raster table")
rx_write("r_fmzout.tif","r_fmz_threshold_status.tif")
log_it("Writing Heritage plus FMZ raster table")
rx_write("r_fmz_bio_out.tif","r_heritage_fmz_threshold_status.tif")
log_it("Writing FMZ plus SFAZ raster table")
rx_write("r_sfaz_fmz_out.tif","r_fmz_sfaz_threshold_status.tif")
log_it("Writing combined raster table")
rx_write("r_sfaz_fmz_bio_out.tif","r_heritage_fmz_sfaz_threshold_status.tif")

log_it("Renaming files")

file.rename(paste0(rast_temp,"/rLastYearBurnt.tif"),paste0(rast_temp,"/r_LastYearBurnt.tif"))
file.rename(paste0(rast_temp,"/rNumTimesBurnt.tif"),paste0(rast_temp,"/r_NumTimesBurnt.tif"))
file.rename(paste0(rast_temp,"/rTimeSinceLast.tif"),paste0(rast_temp,"/r_TimeSinceLast.tif"))

file.rename(paste0(rast_temp,"/v_vegout.gpkg"),paste0(rast_temp,"/v_heritage_threshold_status.gpkg"))
file.rename(paste0(rast_temp,"/v_fmzout.gpkg"),paste0(rast_temp,"/v_fmz_threshold_status.gpkg"))
file.rename(paste0(rast_temp,"/v_fmz_bio_out.gpkg"),paste0(rast_temp,"/v_heritage_fmz_threshold_status.gpkg"))
file.rename(paste0(rast_temp,"/v_sfaz_fmz_out.gpkg"),paste0(rast_temp,"/v_fmz_sfaz_threshold_status.gpkg"))
file.rename(paste0(rast_temp,"/v_sfaz_fmz_bio_out.gpkg"),paste0(rast_temp,"/v_heritage_fmz_sfaz_threshold_status.gpkg"))

## Write tiles


if(OS=="Windows"){
  log_it("Rendering Tiles")
  tile_win("r_heritage_fmz_sfaz_threshold_status")
  tile_win("r_heritage_threshold_status")
  tile_win("r_fmz_threshold_status")
  tile_win("r_heritage_fmz_threshold_status")
  tile_win("r_fmz_sfaz_threshold_status")
}else{
  log_it("Rendering Tiles")
  tile_linux("r_heritage_fmz_sfaz_threshold_status")
  tile_linux("r_heritage_threshold_status")
  tile_linux("r_fmz_threshold_status")
  tile_linux("r_heritage_fmz_threshold_status")
  tile_linux("r_fmz_sfaz_threshold_status")
}

