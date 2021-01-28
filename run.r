#!/usr/bin/Rscript

## NSW Test
library(tidyverse)
library(sf)
library(raster)
#library(spatial.tools)
library(foreach)
library(doParallel)
library(mapview)
library(fasterize)
library(velox)
library(lwgeom)

source("../config/global_config.r")
source("../config/config_linux.r")


if("future.apply" %in% rownames(installed.packages()) | OS=="Windows"){
  library(future.apply)
}

source("fire_cell_function.r")

# Prepare empty environment
prepare()

#log_it("Converting string to numeric variables")
c_sfaz = as.numeric(c_sfaz)
current_year = as.numeric(current_year)
ras_res = as.numeric(ras_res)
f_sfaz_custom = as.numeric(f_sfaz_custom)

if(!exists("d_spatial_unit")){
  d_spatial_unit=""
}

## Run file
log_it("**** Preparing fire history and vegetation rasters")
source("fire_proc1_t.r")
log_it("**** Processing heritage vegetation thresholds")
source("proc_veg1.r")
log_it("**** Vectorizing vegetation")
source("veg_vectorize.r")
log_it("**** Processing Fire Management Blocks")
source("proc_fmz.r")
log_it("**** Vectorizing Fire Management Blocks")
source("fmz_vectorize.r")
log_it("**** Processing merged vegetation and Fire Management Blocks")
source("proc_fmz_veg_merge.r")
log_it("**** Processing Strategic Fire Advantage Zone Thresholds")
source("proc_sfaz.r")
log_it("**** Rendering output maps")
source("render_maps.r")

log_it("**** Clean output")
source("clean_output.r")
#log_it("**** Intersecting output polygon layer")
log_it("#!#!#!# ANALYSIS COMPLETE")
#source("intersect_poly_output.r")


