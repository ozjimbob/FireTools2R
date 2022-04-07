## Get and process fire history
library(tidyverse)
library(sf)

temp = "G:/temp_fd"
dir.create(temp)
lf <- list.files(temp)
unlink(lf)
download.file("https://datasets.seed.nsw.gov.au/dataset/1d05e145-80cb-4275-af9b-327a1536798d/resource/0674ee5b-033a-41ff-bb25-dd2242a9e093/download/fire_npwsfirehistory.zip",paste0(temp,"firehistory.zip"))

unzip(paste0(temp,"firehistory.zip"),exdir=temp)

fire_file = list.files(temp,".shp")[1]
a = read_sf(paste0(temp,"/",fire_file))
a$year = as.numeric(substr(a$StartDate,1,4))
a$month = as.numeric(substr(a$StartDate,6,7))
a <- a %>% mutate(year = case_when(month <= 6 ~ year-1,
                                   month >6 ~ year))

a$year[is.na(a$StartDate)] = substr(a$Label,1,4)[is.na(a$StartDate)]
a$year <- as.character(a$year)
keep ="G:/temp_fd/fire_history.gpkg"
write_sf(a,keep)


this_year <- as.numeric(format(Sys.Date(),"%Y"))
this_month <- as.numeric(format(Sys.Date(),"%m"))
if(this_month >6){this_year = this_year+1}

corp_gdb<-"../FireToolsSeverity/FESM_demo/FT_Input/Corporate.gdb"
i_vt_boundary<-"NPWS_Branches"
f_spatial_unit<-"NAME"
d_spatial_unit<-""
fire_gdb<-"G:/temp_fd/fire_history.gpkg"
i_vt_fire_history<-"fire_history"
f_fireseason<-"year"
proj_crs<-"+proj=lcc +lat_1=-30.75 +lat_2=-35.75 +lat_0=-33.25 +lon_0=147 +x_0=9300000 +y_0=4500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ras_res<-250
current_year<-this_year
v_TSFF<-""
future_years = 0
subextent=NULL
rast_temp<-"G:/fire_temp"

passed=TRUE
#cat(str,file=paste0(temp,"fire_config.r"))

source("run_fire_preprocess.r")

