
### test upscale bec data
library(tidyverse)
library(raster)
library(terra)
library(foreach)
library(doParallel)

source("../config/global_config.r")

args = commandArgs(trailingOnly=TRUE)
source(args[1])

if("future.apply" %in% rownames(installed.packages()) | OS=="Windows"){
  library(future.apply)
}
print("making output dir")
out <- output_dir
dir.create(out)
print("reading table")
do_table <- read_csv(fesm_table)

print("setting up parallel")
plan(tweak(multiprocess, workers = clustNo,gc=TRUE))
options(future.globals.maxSize = +Inf)

print("Defining year function")
proc_year <- function(idx){
  
  
  print(idx)
  this_file <- paste0(do_table$file[idx])
  this_year <- do_table$year[idx]
  
  print("Loading")
  r <- rast(this_file)
  
  align_grid <- rast("../config/grid.tif")
  
  tmp_extent = ext(r)
  tmp_extent = align(tmp_extent,align_grid)
  
  print("Making template")
  template <-rast(ext=tmp_extent, res=c(30,30), crs=crs(r))
  print("resampling")
  r <- resample(r,template,method="near")
  print("Writing")
  writeRaster(r,paste0(out,"/fesm_",this_year,".tif"),overwrite=TRUE)
  
  
}
print("Launching processing")

#o = future_lapply(1:nrow(do_table),FUN=proc_year,future.scheduling=3)
for(i in 1:nrow(do_table)){
  print(i)
  proc_year(i)
}
print("Done")

system(paste0("gdalbuildvrt -r nearest -srcnodata 0 ",out,"/fesm_overlay.vrt ",out,"/fesm*.tif"))

#grid <- raster("G:/ft_work/ecohealth/ecohealth_royal_30m_veg/r_vegcode.tif")
