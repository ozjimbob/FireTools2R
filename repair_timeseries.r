#!/usr/bin/Rscript
## repair timeseries files


## NSW Test
library(tidyverse)
library(sf)
library(raster)
library(foreach)
library(doParallel)
library(mapview)
library(fasterize)
library(velox)
library(lwgeom)
library(sfheaders)
source("../config/global_config.r")
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  source("../config/config_post.r")
}else{
  source(args[1])
}



if("future.apply" %in% rownames(installed.packages()) | OS=="Windows"){
  library(future.apply)
}

c_sfaz = ""



# Determine resolution etc. from existing raster
template_r <- raster(paste0(fire_folder,"/rLastYearBurnt.tif"))
ras_res <- res(template_r)[1]
proj_crs <- proj4string(template_r)
source("fire_cell_function.r")


current_year = as.numeric(current_year)
ras_res = as.numeric(ras_res)




## Run file
log_it(paste0("Env Var:",Sys.getenv("TMPDIR")))
log_it(paste0("Temp Dir: ",tempdir()))


if(single_year=="timeseries"){
  to_repair <- list.files(rast_temp,pattern=".tif")
  sub <- gsub("vegout","heritage_threshold_status",to_repair)
  for(i in seq_along(to_repair)){
    print(to_repair[i])
    rx_write(to_repair[i],sub[i])
    esri_output(sub[i])
    gc()
  }
  
}


log_it("#!#!#!# ANALYSIS COMPLETE")