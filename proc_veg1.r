# Do bio processing 1
library(tidyverse)
library(sf)
library(velox)
library(raster)
library(spatial.tools)
library(foreach)
library(doParallel)

# Setup
source("../config/global_config.r")
source("../config/config_linux.r")
source("fire_cell_function.r")

### Load rasters
log_it("Loading year list")
year_list = read_csv(paste0(rast_temp,"/yearlist.csv"))
file_list = paste0(rast_temp,"/",year_list$year,".tif")
int_list = year_list$year

# Set up variables
log_it("Calculating TSFF")
if(v_TSFF == ""){
  TSFF = current_year-min(int_list)
}else{
  TSFF = as.numeric(v_TSFF)
}

log_it("Loading template raster")
tmprast = raster(paste0(rast_temp,"/rTimeSinceLast.tif"))

###



library(future)
if(OS=="Windows"){
  print("Loading future.apply")
  library("future.apply")
}
plan(tweak(multiprocess, workers = clustNo,gc=TRUE))
options(future.globals.maxSize = +Inf)

log_it(paste0("Starting biodiversity threshold function application on ",nrow(tmprast)," slices"))
o = future_lapply(1:nrow(tmprast),FUN=proccell2,future.scheduling=3)
log_it("Biodiversity threshold calculation complete")



log_it("Rasterizing biodiversity threshold and writing to disk")
oul = unlist(o)
values(tmprast)=oul
o <- NULL
oul <- NULL
rm(o)
rm(oul)
gc()




bigWrite(tmprast,paste0(rast_temp,"/r_vegout.tif"))
log_it("Biodiversity threshold write complete")

