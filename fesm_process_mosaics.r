
### test upscale bec data
library(tidyverse)
library(raster)
library(foreach)
library(doParallel)

source("../config/global_config.r")

if("future.apply" %in% rownames(installed.packages()) | OS=="Windows"){
  library(future.apply)
}
print("making output dir")
out <- output_dir
dir.create(out)
print("reading table")
do_table <- read_csv(fesm_table)

#r <- raster("G:/ft_work/fesm/mosaics/cvmsre_NSW_20202021_ag1l0.img")

#root <- "G:/ft_work/fesm/mosaics/"

#rlist <- c("cvmsre_NSW_20192020_ag1l0.img",
#           "cvmsre_NSW_20182019_ag1l0.img",
#           "cvmsre_NSW_20172018_ag1l0.img",
#           "cvmsre_NSW_20162017_ag1l0.img")

#ylist <- c("2019","2018","2017","2016")

#do_table <- tibble(file=rlist,year=ylist)
print("setting up parallel")
plan(tweak(multiprocess, workers = clustNo,gc=TRUE))
options(future.globals.maxSize = +Inf)

print("Defining year function")
proc_year <- function(idx){
  
  
  print(idx)
  this_file <- paste0(do_table$file[idx])
  this_year <- do_table$year[idx]
  
  print("Loading")
  r <- raster(this_file)
  
  align_grid <- raster("../config/grid.tif")
  
  tmp_extent = extent(r)
  tmp_extent = alignExtent(tmp_extent,align_grid)
  
  print("Making template")
  template <-raster(ext=tmp_extent, res=c(30,30), crs=crs(r))
  print("resampling")
  r <- resample(r,template,method="ngb")
  print("Writing")
  writeRaster(r,paste0(out,"/fesm_",this_year,".tif"),overwrite=TRUE)
  
  
}
print("Launching processing")
o = future_lapply(1:nrow(do_table),FUN=proc_year,future.scheduling=3)
print("Done")

system(paste0("gdalbuildvrt -r nearest -srcnodata 0 ",out,"/fesm_overlay.vrt ",out,"/fesm*.tif"))

#grid <- raster("G:/ft_work/ecohealth/ecohealth_royal_30m_veg/r_vegcode.tif")
