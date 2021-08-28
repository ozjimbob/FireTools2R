#!/usr/bin/Rscript

# Fire History Pre-processing

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

# Load template raster to determine resolution


# Prepare empty environment
prepare()

current_year = as.numeric(current_year)
ras_res = as.numeric(ras_res)




## Run file
log_it(paste0("Env Var:",Sys.getenv("TMPDIR")))
log_it(paste0("Temp Dir: ",tempdir()))
log_it("**** Preparing fire history  rasters")


source("heritage_postprocess.r")




log_it("#!#!#!# ANALYSIS COMPLETE")



