#!/usr/bin/Rscript
# RUN SDC FESM OVERLAY

library(tidyverse)
library(sf)
library(raster)
library(lwgeom)
library(sfheaders)
library(terra)


source("../config/global_config.r")

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  source("../config/config_post_fesm_overlay.r")
}else{
  source(args[1])
}

c_sfaz = ""


# Determine resolution etc. from existing raster
# We can assume, given new processing chain, that heritage folder includes a vegmax raster
template_r <- raster(paste0(heritage_folder,"/r_heritage_threshold_status_",current_year,".tif"))
ras_res <- res(template_r)[1]
proj_crs <- proj4string(template_r)
source("fire_cell_function.r")

# Load template raster to determine resolution


# Prepare empty environment
# need rast_temp
prepare()

# need current_year and will run fesm output on this
current_year = as.numeric(current_year)
ras_res = as.numeric(ras_res)


## Run file
log_it(paste0("Env Var:",Sys.getenv("TMPDIR")))
log_it(paste0("Temp Dir: ",tempdir()))
log_it("**** Preparing fesm overlay")

# get FESM input
fesm_statewide <- terra::rast(paste0(fesm_dir,"/fesm_overlay.vrt"))

fst <- align(ext(fesm_statewide),rast(template_r))
ext(fesm_statewide)<-fst
fesm_statewide = terra::crop(fesm_statewide,rast(template_r))

# Load region mask
r_mask <- read_sf(paste0(heritage_folder,"/v_region.gpkg"))
fesm_statewide <- mask(fesm_statewide,vect(r_mask))

writeRaster(fesm_statewide,paste0(rast_temp,"/fesm_clip.tif"))

fesm_statewide <- terra::rast(paste0(rast_temp,"/fesm_clip.tif"))

fesm_statewide <- fesm_statewide * 10

crs(fesm_statewide) <- crs(rast(template_r))
fesm_statewide[is.nan(fesm_statewide)]=0
fesm_statewide[fesm_statewide %in% c(20,30)]=10
fesm_statewide[fesm_statewide %in% c(40,50)]=20

fesm_overlay <- fesm_statewide + rast(template_r)

# Outputs
# Fesm classes:
# 0 = unburnt
# 10 = Low
# 20 = High

# Heritage outputs
# 1 = NoFireRegime
# 2 = TooFrequentlyBurnt
# 3 = Vulnerable
# 4 = LongUnburnt
# 5 = Within Threshold
# 9 = Unknown

fesm_table = data.frame(ID = c(0,
                           1,2,3,4,5,9,
                           11,12,13,14,15,19,
                           21,22,23,24,25,29),
                    Status = c("External",
                               "NoSev_NoFireRegime","NoSev_TooFrequentlyBurnt","NoSev_Vulnerable","NoSev_LongUnburnt","NoSev_WithinThreshold","NoSev_Unknown",
                               "LowSev_NoFireRegime","LowSev_TooFrequentlyBurnt","LowSev_Vulnerable","LowSev_LongUnburnt","LowSev_WithinThreshold","LowSev_Unknown",
                               "HighSev_NoFireRegime","HighSev_TooFrequentlyBurnt","HighSev_Vulnerable","HighSev_LongUnburnt","HighSev_WithinThreshold","HighSev_Unknown"))

levels(fesm_overlay)<-fesm_table

col_table = data.frame(ID = c(0,
                               1,2,3,4,5,6,7,8,9,
                               10,11,12,13,14,15,16,17,18,19,
                               20,21,22,23,24,25,26,27,28,29),
                        Status = c("white",
                                   "white","#e8929e","#c28f5d","#a2dbce","#cdd1d0","white","white","white","#dabfe3",
                                   "white","white","#962024","#855321","#3a9e87","#919191","white","white","white","#937b9c",
                                   "white","white","#4d0306","#4a2603","#065240","#4d4d4d","white","white","white","#49384f"))

coltab(fesm_overlay) <- col_table$Status


writeRaster(fesm_overlay,paste0(rast_temp,"/fesm_overlay.tif"))

log_it("#!#!#!# ANALYSIS COMPLETE")



