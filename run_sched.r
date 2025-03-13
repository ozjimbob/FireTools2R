#!/usr/bin/Rscript

## NSW Test
library(tidyverse)
library(sf)
library(raster)
library(foreach)
library(doParallel)
library(mapview)
library(fasterize)
library(lwgeom)


source("../config/global_config.r")
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  source("../config/config_sched.r")
}else{
  source(args[1])
}


if("future.apply" %in% rownames(installed.packages()) | OS=="Windows"){
  library(future.apply)
}
# Code for Strategic Fire Advantage Zone
c_sfaz<-"6103"

source("fire_cell_function.r")

# Prepare empty environment
prepare()

#log_it("Converting string to numeric variables")
current_year = as.numeric(current_year)
ras_res = as.numeric(ras_res)


## Run file
log_it("**** Preparing fire history and vegetation rasters")
source("fire_proc_sched.r")


log_it("**** Processing heritage vegetation thresholds")
source("proc_veg_sched.r")

#log_it("**** Vectorizing vegetation")
#source("veg_vectorize.r")


#log_it("**** Rendering output maps")
#source("render_maps_sched.r")

log_it("**** Clean output")
source("clean_output_sched.r")
#log_it("**** Intersecting output polygon layer")
log_it("#!#!#!# ANALYSIS COMPLETE")
#source("intersect_poly_output.r")


