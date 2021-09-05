# Do bio processing 1
library(tidyverse)
library(sf)
library(velox)
library(raster)
#library(spatial.tools)
library(foreach)
library(doParallel)

# Setup
#source("../config/global_config.r")
#source("../config/config_linux.r")
#source("fire_cell_function.r")

### Load rasters
log_it("Loading year list")
year_list = read_csv(paste0(fire_folder,"/yearlist.csv"))
file_list = paste0(fire_folder,"/",year_list$year,".tif")
int_list = year_list$year


if(single_year=="no_timeseries"){
  
  # Set up variables
  log_it("Calculating TSFF")
  
  TSFF = current_year-min(int_list)
  TSFF = as.numeric(TSFF)
  
  log_it("Loading template raster")
  tmprast = raster(paste0(fire_folder,"/rTimeSinceLast.tif"))
  
  ###
  
  
  
  
  library(future)
  if("future.apply" %in% rownames(installed.packages()) | OS=="Windows"){
    print("Loading future.apply")
    library("future.apply")
  }
  plan(tweak(multiprocess, workers = clustNo,gc=TRUE))
  options(future.globals.maxSize = +Inf)
  
  log_it(paste0("Starting biodiversity threshold function application on ",nrow(tmprast)," slices"))
  o = future_lapply(1:nrow(tmprast),FUN=proccell2_post,future.scheduling=3)
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
  
}


if(single_year=="timeseries"){
  
  all_years <- list.files(fire_folder)
  
  
  all_years <- all_years[grep(pattern="rNumTimesBurnt_",all_years)]
  all_years <- as.numeric(substr(all_years,16,19))
  
  
  for(year_idx in 2:length(all_years)){
    this_year <- all_years[year_idx]
    log_it(this_year)
    # Set up variables
    log_it("Calculating TSFF")
    
    TSFF = this_year-min(int_list)
    TSFF = as.numeric(TSFF)
    
    log_it("Loading template raster")
    tmprast = raster(paste0(fire_folder,"/rTimeSinceLast.tif"))
    
    ###
    
    
    
    
    library(future)
    if("future.apply" %in% rownames(installed.packages()) | OS=="Windows"){
      print("Loading future.apply")
      library("future.apply")
    }
    plan(tweak(multiprocess, workers = clustNo,gc=TRUE))
    options(future.globals.maxSize = +Inf)
    
    log_it(paste0("Starting biodiversity threshold function application on ",nrow(tmprast)," slices"))
    
    o = future_lapply(1:nrow(tmprast),FUN=proccell2_post,future.scheduling=3, cyear=this_year)
    
    log_it("Biodiversity threshold calculation complete")
    
    
    
    log_it("Rasterizing biodiversity threshold and writing to disk")
    oul = unlist(o)
    values(tmprast)=oul
    o <- NULL
    oul <- NULL
    rm(o)
    rm(oul)
    gc()
    
    
    bigWrite(tmprast,paste0(rast_temp,"/r_vegout_",this_year,".tif"))
  }
}





if(single_year=="selected"){
  
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
    tmprast = raster(paste0(fire_folder,"/rTimeSinceLast.tif"))
    
    ###
    
    
    
    
    library(future)
    if("future.apply" %in% rownames(installed.packages()) | OS=="Windows"){
      print("Loading future.apply")
      library("future.apply")
    }
    plan(tweak(multiprocess, workers = clustNo,gc=TRUE))
    options(future.globals.maxSize = +Inf)
    
    log_it(paste0("Starting biodiversity threshold function application on ",nrow(tmprast)," slices"))
    
    o = future_lapply(1:nrow(tmprast),FUN=proccell2_post,future.scheduling=3, cyear=this_year)
    
    log_it("Biodiversity threshold calculation complete")
    
    
    
    log_it("Rasterizing biodiversity threshold and writing to disk")
    oul = unlist(o)
    values(tmprast)=oul
    o <- NULL
    oul <- NULL
    rm(o)
    rm(oul)
    gc()
    
    
    bigWrite(tmprast,paste0(rast_temp,"/r_vegout_",this_year,".tif"))
  
}




log_it("Biodiversity threshold write complete")


log_it("Writing raster tables")

if(single_year=="no_timeseries"){
  rx_write("r_vegout.tif","r_heritage_threshold_status.tif")
  esri_output("r_heritage_threshold_status.tif")
  gc()
}

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


if(single_year=="selected"){
  to_repair <- paste0("r_vegout_",current_year,".tif")
  sub <- gsub("vegout","heritage_threshold_status",to_repair)
    print(to_repair)
    rx_write(to_repair,sub)
    esri_output(sub)
    gc()
  }
  
