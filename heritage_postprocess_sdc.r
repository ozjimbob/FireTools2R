# Do bio processing 1
library(tidyverse)
library(sf)
library(velox)
library(raster)
#library(spatial.tools)
library(foreach)
library(doParallel)
library(Rcpp)
# Setup
#source("../config/global_config.r")
#source("../config/config_linux.r")
#source("fire_cell_function.r")

### Load rasters
log_it("Loading year list")
year_list = read_csv(paste0(fire_folder,"/yearlist.csv"))
file_list = paste0(fire_folder,"/",year_list$year,".tif")
int_list = year_list$year

#### Set up subregion clip and mask for SDC regional processing based on full-state input


v_regions = read_sf(corp_gdb,i_vt_boundary)

if(d_spatial_unit != ""){
  log_it(paste0("Filtering region layer to ROI:",d_spatial_unit))
  v_thisregion = filter(v_regions,(!!rlang::sym(f_spatial_unit)) == d_spatial_unit)
  log_it("Filtering fire history to ROI complete")
}else{
  log_it("No filtering of region, creating union")
  v_regions$flag=1
  v_thisregion <- v_regions %>% group_by(flag) %>% summarise()
  log_it("Union complete")
}


log_it("Projecting and repairing region")
v_thisregion = st_transform(v_thisregion,crs=proj_crs)
v_thisregion = st_make_valid(v_thisregion)
log_it("Region projection complete")

log_it("Writing region template")
write_sf(v_thisregion,paste0(rast_temp,"/v_region.gpkg"))
log_it("Finished writing region template")

bbox = st_bbox(v_thisregion)


# Load 25m NSW Alignment grid
if(!exists("grid_file")){
  log_it("No Grid File")
  align_grid = raster("../config/grid.tif")
}else{
  log_it("Custom Grid File")
  align_grid = raster(grid_file)
}
log_it("Getting bbox extent")
tmp_extent = extent(bbox[c(1,3,2,4)])
log_it(tmp_extent)
tmp_extent = alignExtent(tmp_extent,align_grid)
log_it(tmp_extent)

# Make template raster
log_it("Generating template raster")
tmprast = raster(ext=tmp_extent, res=c(ras_res,ras_res), crs=proj_crs)

####### NEW
####### CROP RASTERS AND WRITE TO OUTPUT

log_it("Pre-cropping and aligning veg attributes")
r_vegmin = terra::rast(paste0(veg_folder,"/r_vegmin.tif"))
fst <- align(ext(r_vegmin),rast(tmprast))
ext(r_vegmin)<-fst
r_vegmin = terra::crop(r_vegmin,rast(tmprast))
writeRaster(r_vegmin,paste0(rast_temp,"/r_vegmin.tif"))

r_vegmax= terra::rast(paste0(veg_folder,"/r_vegmax.tif"))
fst <- align(ext(r_vegmax),rast(tmprast))
ext(r_vegmax)<-fst
r_vegmax = terra::crop(r_vegmax,rast(tmprast))
writeRaster(r_vegmax,paste0(rast_temp,"/r_vegmax.tif"))

rm(r_vegmin)
rm(r_vegmax)

log_it("Pre-cropping and aligning fire history")

temp_fire_dir = paste0(rast_temp,"/fire")
dir.create(temp_fire_dir)


for(ii in 1:length(int_list)){
  
  this_year = int_list[ii]
  
  # We may have one less year than interval if current year has no fire
  if(file.exists(file_list[ii])){
    log_it(paste0("Cropping year: ",this_year))
    log_it("binary fire")
    this_binary = terra::rast(file_list[ii])
    fst <- align(ext(this_binary),rast(tmprast))
    ext(this_binary)<-fst
    log_it("cropping")
    this_binary = terra::crop(this_binary,rast(tmprast))
    
    log_it("writing")
    writeRaster(this_binary,paste0(temp_fire_dir,"/",this_year,".tif"),datatype="INT1U",overwrite=TRUE)
  }
  
  
  log_it("times burnt")
  r_timesburnt= terra::rast(paste0(fire_folder,"/rNumTimesBurnt_",this_year,".tif"))
  fst <- align(ext(r_timesburnt),rast(tmprast))
  ext(r_timesburnt)<-fst
  log_it("times since last")
  r_tsl= terra::rast(paste0(fire_folder,"/rTimeSinceLast_",this_year,".tif"))
  fst <- align(ext(r_tsl),rast(tmprast))
  ext(r_tsl)<-fst
  
  gc()
  
  log_it("cropping")
  #st = terra::crop(st,the_tmprast)
  r_tsl = terra::crop(r_tsl,rast(tmprast))
  r_timesburnt = terra::crop(r_timesburnt,rast(tmprast))
  
  log_it("writing")
  writeRaster(r_tsl,paste0(temp_fire_dir,"/rTimeSinceLast_",this_year,".tif"),overwrite=TRUE)
  writeRaster(r_timesburnt,paste0(temp_fire_dir,"/rNumTimesBurnt_",this_year,".tif"),overwrite=TRUE)
  
}

new_file_list = paste0(temp_fire_dir,"/",year_list$year,".tif")


gc()


############
##############
#############



# Compile functions
#sourceCpp('core.cpp')

library(future)
if("future.apply" %in% rownames(installed.packages()) | OS=="Windows"){
  print("Loading future.apply")
  library("future.apply")
}
plan(tweak(multiprocess, workers = clustNo,gc=TRUE))
options(future.globals.maxSize = +Inf)


if(single_year=="no_timeseries"){
  log_it("Single Year = No Timeseries")
  # Set up variables
  log_it("Calculating TSFF")
  
  TSFF = current_year-min(int_list)
  TSFF = as.numeric(TSFF)
  
  log_it("Loading template raster")
  #tmprast = raster(paste0(fire_folder,"/rTimeSinceLast.tif"))
  
  
  ###############
  #####
  ##### LOAD ROI from geodatabase
  ##### Generate mask
  ##### CROP temprast to ROI
  #####
  
  
  
  

  
  log_it(paste0("Starting biodiversity threshold function application on ",nrow(tmprast)," slices"))
  ttemprast = rast(tmprast)
  o = future_lapply(1:nrow(tmprast),FUN=proccell2_post_sdc,future.scheduling=3,the_tmprast=ttemprast)
  log_it("Biodiversity threshold calculation complete")
  
  
  
  log_it("Rasterizing biodiversity threshold and writing to disk")
  oul = unlist(o)
  
  raster::values(tmprast)=oul
  o <- NULL
  oul <- NULL
  rm(o)
  rm(oul)
  gc()
  
  
  bigWrite(tmprast,paste0(rast_temp,"/r_vegout.tif"))
  
}


if(single_year=="timeseries"){
  log_it("Single Year = TimeSeries")
  all_years <- list.files(fire_folder)
  
  
  all_years <- all_years[grep(pattern="rNumTimesBurnt_",all_years)]
  all_years <- as.numeric(substr(all_years,16,19))
  
  if(exists("start_year")){
    wyear <- which.min(abs(all_years-start_year))
  }else{
    wyear <- 2
  }
  
  for(year_idx in wyear:length(all_years)){
    this_year <- all_years[year_idx]
    log_it(this_year)
    # Set up variables
    log_it("Calculating TSFF")
    
    TSFF = this_year-min(int_list)
    TSFF = as.numeric(TSFF)
    
    log_it("Loading template raster")
   
    
    log_it(paste0("Starting biodiversity threshold function application on ",nrow(tmprast)," slices"))
    ttemprast = rast(tmprast)
    o = future_lapply(1:nrow(tmprast),FUN=proccell2_post_sdc,future.scheduling=1, cyear=this_year,the_tmprast=ttemprast)
    
    log_it("Biodiversity threshold calculation complete")
    
    
    
    log_it("Rasterizing biodiversity threshold and writing to disk")
    
    oul = unlist(o)
    
    #log_it(paste0("Number values in oul: ",length(oul)))
    #log_it(paste0("Number of values in temprast: ",length(tmprast)))
    #log_it(paste0("Res of temprast: ",res(tmprast)))
    #log_it(paste0("Res of raw input: ",res(raster(paste0(veg_folder,"/r_vegmin.tif"),values=FALSE))))
    #log_it(paste0("extent of raw input: ",extent(raster(paste0(veg_folder,"/r_vegmin.tif"),values=FALSE))))
    
    raster::values(tmprast)=oul
    
    #log_it(paste0("Unique: ",unique(values(tmprast))))
    
    o <- NULL
    oul <- NULL
    rm(o)
    rm(oul)
    gc()
    
    
    bigWrite(tmprast,paste0(rast_temp,"/r_vegout_",this_year,".tif"))
  }
}





if(single_year=="selected"){
  log_it("Single Year = Selected Year")
  all_years <- list.files(fire_folder)
    all_years <- all_years[grep(pattern="rNumTimesBurnt_",all_years)]
  all_years <- as.numeric(substr(all_years,16,19))
  
  
  year_idx = which(all_years==current_year)
  
    this_year <- all_years[year_idx]
    log_it(this_year)
    # Set up variables
    log_it("Calculating TSFF")
    
    TSFF = this_year-min(int_list)
    TSFF = as.numeric(TSFF)
    
    log_it("Loading template raster")
    #tmprast = raster(paste0(fire_folder,"/rTimeSinceLast.tif"))
    
    ###
    
    
    
    
   
    log_it(paste0("Starting biodiversity threshold function application on ",nrow(tmprast)," slices"))
    ttemprast = rast(tmprast)
    o = future_lapply(1:nrow(tmprast),FUN=proccell2_post_sdc,future.scheduling=3, cyear=this_year,the_tmprast=ttemprast)
    
    log_it("Biodiversity threshold calculation complete")
    
    oul = unlist(o)
    
    log_it("Rasterizing biodiversity threshold and writing to disk")
    log_it(paste0("Number values in oul: ",length(oul)))
    log_it(paste0("Number of values in temprast: ",length(tmprast)))
    log_it(paste0("Res of temprast: ",res(tmprast)))
    log_it(paste0("Res of raw input: ",res(raster(paste0(veg_folder,"/r_vegmin.tif"),values=FALSE))))
    log_it(paste0("extent of raw input: ",extent(raster(paste0(veg_folder,"/r_vegmin.tif"),values=FALSE))))
    

    raster::values(tmprast)=oul
    o <- NULL
    oul <- NULL
    rm(o)
    rm(oul)
    gc()
    
    
    bigWrite(tmprast,paste0(rast_temp,"/r_vegout_",this_year,".tif"))
  
}




log_it("Biodiversity threshold write complete")
log_it("clearing temporary rasters")

unlink(paste0(rast_temp,"/r_vegmax.tif"))
unlink(paste0(rast_temp,"/r_vegmin.tif"))
unlink(paste0(rast_temp,"/test"), recursive=TRUE)

log_it("Writing raster tables")

if(single_year=="no_timeseries"){
  rx_write("r_vegout.tif","r_heritage_threshold_status.tif")
  esri_output("r_heritage_threshold_status.tif")
  gc()
}

## Write maps
if(single_year=="timeseries"){
  to_repair <- list.files(rast_temp,pattern=".tif")
  sub <- gsub("vegout","heritage_threshold_status",to_repair)
  for(i in seq_along(to_repair)){
    print(to_repair[i])
    rx_write(to_repair[i],sub[i])
    esri_output(sub[i])
    gc()
  }
  log_it("Creating map dir")
  dir.create(paste0(rast_temp,"/maps"))
  log_it("Looping through years")
  log_it(wyear)
  log_it(all_years)
  log_it("Looping")
  for(year_idx in wyear:length(all_years)){
    log_it(year_idx)
    r = rast(paste0(rast_temp,"/r_heritage_threshold_status_",all_years[year_idx],".tif"))
    log_it("writing")
    png(paste0(rast_temp,"/maps/map_heritage_",all_years[year_idx],".tif"),type="cairo-png",width=1000,height=1300)
    print(plot(r,plg=list(title=all_years[year_idx], title.cex=1.25)))
    dev.off()
    log_it("Done")
  }
  log_it("Finished maps")
}


if(single_year=="selected"){
  to_repair <- paste0("r_vegout_",current_year,".tif")
  sub <- gsub("vegout","heritage_threshold_status",to_repair)
    print(to_repair)
    rx_write(to_repair,sub)
    esri_output(sub)
    gc()
  }
  
