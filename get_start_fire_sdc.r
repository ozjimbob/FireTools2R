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

dir.create(keep)
write_sf(a,fire_gdb)





passed=TRUE
#cat(str,file=paste0(temp,"fire_config.r"))

source("run_fire_preprocess.r")
fl <- list.files(temp)
unlink(fl)
