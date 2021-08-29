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
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  source("../config/config_veg.r")
}else{
  source(args[1])
}


if("future.apply" %in% rownames(installed.packages()) | OS=="Windows"){
  library(future.apply)
}
current_year=0
c_sfaz = ""

source("fire_cell_function.r")

# Prepare empty environment
prepare()

#log_it("Converting string to numeric variables")
current_year = as.numeric(current_year)
ras_res = as.numeric(ras_res)

if(!exists("d_spatial_unit")){
  d_spatial_unit=""
}

## Run file
log_it(paste0("Env Var:",Sys.getenv("TMPDIR")))
log_it(paste0("Temp Dir: ",tempdir()))
log_it("**** Preparing fire history  rasters")
source("veg_proc_preprocess.r")



log_it("#!#!#!# ANALYSIS COMPLETE")



