# Calculate fire management zone thresholds
library(tidyverse)
library(sf)
library(velox)
library(raster)
library(spatial.tools)
library(foreach)
library(doParallel)

# Setup
source("../config/global_config.r")
source("../config/config_linux.r")
source("fire_cell_function.r")

log_it("Starting fire management zone analysis")
### Load rasters
log_it("Loading year list")
year_list = read_csv(paste0(rast_temp,"/yearlist.csv"))
file_list = paste0(rast_temp,"/",year_list$year,".tif")
int_list = year_list$year


log_it("Loading template raster")
tmprast = raster(paste0(rast_temp,"/rTimeSinceLast.tif"))

#### Read fire management zones
# Read fire history table and transform
log_it("Reading fire management zones, projecting and repairing")
v_fmz= read_sf(asset_gdb,i_vt_fmz)
v_fmz = st_transform(v_fmz,crs=proj_crs)
v_fmz = st_cast(v_fmz,"MULTIPOLYGON") # Multisurface features cause errors
v_fmz = st_make_valid(v_fmz) # repair invalid geometries
log_it("Fire management zone import complete")

log_it("Loading region boundary")
v_thisregion = read_sf(paste0(rast_temp,"/v_region.gpkg"))

log_it("Clipping fire management zone to ROI")
v_fmz = st_intersection(v_fmz,v_thisregion)
log_it("Clipping  fire management zone complete")

log_it("Loading FMZ look up table")
v_fmz_table = st_read(fire_gdb,i_vt_fmz_lut)
log_it("Fixing names for joining")
names(v_fmz_table)[names(v_fmz_table) == f_vt_fmz] = f_fmz
v_fmz = left_join(v_fmz,v_fmz_table,by=f_fmz)


log_it("Rasterizing fire management zone layer Max Interval")
write_sf(v_fmz,paste0(rast_temp,"/v_fmz.gpkg"))
rex = paste(extent(tmprast)[c(1,3,2,4)],collapse=" ")
rres = res(tmprast)
#cmd = paste0(gdal_rasterize," -burn 1 -l year_fire -of GTiff ",
#             "-te ",rex," -tr ",rres[1]," ",rres[2]," -ot byte -co COMPRESS=PACKBITS ",
#            paste0(rast_temp,"/","year_fire.gpkg")," ",paste0(rast_temp,"/",int_list[yr],".tif"))
cmd = g_rasterize("v_fmz","v_fmz.gpkg",paste0(rast_temp,"/r_fmz.tif"),attribute=f_vt_maxint)
system(cmd)
unlink(paste0(rast_temp,"/v_fmz.gpkg"))
log_it("Finished rasterizing fire management zone layer")
v_fmz <- NULL
rm(v_fmz)
gc()


# Generate min/max table
# Process classification




library(future)
plan(tweak(multiprocess, workers = clustNo,gc=TRUE))
options(future.globals.maxSize = +Inf)
log_it(paste0("Starting fire management zone threshold function application on ",nrow(tmprast)," slices"))
o = future_lapply(1:nrow(tmprast),FUN=proccell_fmz)
log_it("Fire management zone threshold calculation complete")


log_it("Rasterizing fire management zone threshold and writing to disk")
oul = unlist(o)
values(tmprast)=oul
o <- NULL
oul <- NULL
rm(o)
rm(oul)
gc()

s2 <- writeStart(tmprast, filename=paste0(rast_temp,"/r_fmzout.tif"), format='GTiff', overwrite=TRUE)
tr <- blockSize(tmprast)
for (i in tr$n:1) {
  v <- getValuesBlock(tmprast, row=tr$row[i], nrows=tr$nrows[i])
  s2 <- writeValues(s2, v, tr$row[i])
}
s2 <- writeStop(s2)
log_it("Fire management zone threshold write complete")

# Generate merged fmz/biodiversity


