#!/usr/bin/Rscript

## Get and process fire history
library(tidyverse)
library(sf)

args = commandArgs(trailingOnly=TRUE)

this_year <- as.numeric(format(Sys.Date(),"%Y"))
this_month <- as.numeric(format(Sys.Date(),"%m"))
if(this_month >6){this_year = this_year+1}

source(args[1])


#temp = "G:/temp_fd"
dir.create(temp)
lf <- list.files(temp)
unlink(lf)

download.file(fire_url,paste0(temp,"/firehistory.zip"))

unzip(paste0(temp,"/firehistory.zip"),exdir=temp)

fire_file = list.files(temp,".shp")[1]
a = read_sf(paste0(temp,"/",fire_file))
a$year = as.numeric(substr(a$Label,1,4))


a$year[is.na(a$StartDate)] = substr(a$Label,1,4)[is.na(a$StartDate)]
a$year <- as.character(a$year)

#### 2
dir.create(temp_2)
lf <- list.files(temp_2)
unlink(lf)

download.file(fire_url_2,paste0(temp_2,"/firehistory_2.zip"))

unzip(paste0(temp_2,"/firehistory_2.zip"),exdir=temp_2)

fire_file_2 = list.files(temp_2,".shp")[1]
a_2 = read_sf(paste0(temp_2,"/",fire_file_2))
a_2$year = as.numeric(substr(a$FireSeason,1,4))
a_2$year <- as.character(a$year)

a <- dplyr::select(a,year)
a_2 <- dplyr::select(a_2,year)

a <- bind_rows(a,a2)


dir.create(keep)
write_sf(a,fire_gdb)




passed=TRUE
#cat(str,file=paste0(temp,"fire_config.r"))

source("run_fire_preprocess.r")
fl <- list.files(temp)
unlink(fl)
fl <- list.files(temp_2)
unlink(fl)