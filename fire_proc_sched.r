
# Read the regions table, filter to region of interest, transform
log_it("Loading regions")
library(terra)

print("Region file:")
print(v_boundary)
print("Root files:")
print(list.files("~"))
print("Input files:")
print(list.files("~/inputs"))

v_regions <- vect(v_boundary)
v_thisregion <- v_regions


#### Create template raster

# Define bounding box of raster
bbox = ext(v_thisregion)

# Generate template raster of this extent and resolution
log_it("Creating template raster")


# Load 25m NSW Alignment grid

align_grid = rast(grid_file)


tmp_extent = bbox(align_grid)

# Make template raster
log_it("Generating template raster")
tmprast = align_grid

#rex = paste(ext(tmprast)[c(1,3,2,4)],collapse=" ")
#rres = res(tmprast)


# Make mask raster for ROI
v_thisregion$flag = 1
log_it("Rasterizing ROI Mask")
mask_tif = rasterize(v_thisregion,tmprast,field="flag")
log_it("Writing ROI Mask")
writeRaster(mask_tif,paste0(rast_temp,"/roi_mask.tif"))
rm(mask_tif)
#mask_tif=raster(paste0(rast_temp,"/roi_mask.tif"))
#mask_tif=raster(paste0(rast_temp,"/roi_mask.tif"))



#########
# Read fire history table and transform
log_it("Reading fire history, projecting and repairing")
v_fire = read_sf(fire_gdb)

v_fire = st_cast(v_fire,"MULTIPOLYGON") # Multisurface features cause errors
v_fire <- filter(v_fire,!st_is_empty(v_fire))
v_fire <- try(remove_invalid_poly(v_fire))
v_fire <- remove_invalid_poly_multi(v_fire)


zros = which(as.numeric(st_area(v_fire))==0)

if(length(zros)>0){
  v_fire <- v_fire[-zros,]
}

v_fire = st_make_valid(v_fire) # repair invalid geometries
log_it("Fire history import complete")


# Add numeric year field to fire and a count flag field
v_fire$numYear = v_fire$FireYear
v_fire$count = 1

# If v_TSFF is defined, subset v_fire to only years including or after this fireseason
if(v_TSFF != ""){
  v_fire = filter(v_fire,numYear >= as.numeric(v_TSFF))
}





##### Make fire count raster

# Get list of seasons, and also numeric years
v_fire = st_buffer(v_fire,0)
v_firex = filter(v_fire,st_geometry_type(v_fire)=="GEOMETRYCOLLECTION")
v_fire= filter(v_fire,!st_geometry_type(v_fire)=="GEOMETRYCOLLECTION")

if(nrow(v_firex)>0){
  v_firex = st_collection_extract(v_firex, "POLYGON") 
  v_fire = rbind(v_fire,v_firex)
}

int_list = sort(unique(v_fire$numYear))
##

int_list = c(int_list,current_year)

int_list<- unique(int_list)
int_list <- as.numeric(int_list)
##
year_list = as.numeric(substr(int_list,1,4))

log_it("Writing list of fire seasons")
write_csv(tibble(year = int_list),paste0(rast_temp,"/yearlist.csv"))

# Simplify fire spatial file to save memory
v_fire = dplyr::select(v_fire,numYear,count)
gc()
# Store rasters in a list
orast = list()


log_it(paste0("Rasterizing ",length(int_list)," fire seasons"))
#il2 = int_list[3:(length(int_list))]
for(yr in seq_along(int_list)){
#for(yr in seq_along(int_list)){
  log_it(int_list[yr])
  if(file.exists(paste0(rast_temp,"/",int_list[yr],".tif"))){
    log_it("Skipping year as it exists from a previous run.")
    next
  }
  datx = filter(v_fire,numYear==int_list[yr])
  datx = st_cast(datx,"MULTIPOLYGON")
  # Ship out to gdal
  log_it("Writing Temp gpkg")
  
  
  write_sf(datx,paste0(rast_temp,"/","year_fire.gpkg"))
  
  rex = paste(ext(tmprast)[c(1,3,2,4)],collapse=" ")
  rres = res(tmprast)
  tr <- terra::rast(tmprast)
  
  
  # Write year raster to disk
  if(nrow(datx)>0){
    log_it("rasterize this year")
    
    
    tbl <- vect(paste0(rast_temp,"/year_fire.gpkg"))
    
    r_vfp = terra::rasterize(tbl,tr,field="count",filename=paste0(rast_temp,"/",int_list[yr],".tif"),overwrite=TRUE,wopt=list(datatype="INT1U"))
    
    
    #cmd = g_rasterize("year_fire","year_fire.gpkg",paste0(rast_temp,"/",int_list[yr],".tif"),otype="byte")
    
    #system(cmd)
    #log_it("Loading this year")
    # Load this year, multiply by year
    this_year <- terra::rast(paste0(rast_temp,"/",int_list[yr],".tif"))
  }else{
    log_it("NO FIRE FOUND skipping to blank raster.")
    this_year <- tr
    terra::values(this_year)<-0
  }
  
  
  
  # Write progressive rasters
  if(yr==1){      
    log_it("Year 1 - calculating r_lastb")
    #r_lastb = calc(this_year, fun=function(x){x* int_list[yr]},filename=paste0(rast_temp,"/",'rLastYearBurnt.tif'),overwrite=TRUE)
    r_lastb = this_year * int_list[yr]
    writeRaster(r_lastb,paste0(rast_temp,"/",'rLastYearBurnt.tif'),overwrite=TRUE)
    
    file.copy(paste0(rast_temp,"/",'rLastYearBurnt.tif'),paste0(rast_temp,"/",'rLastYearBurnt_',int_list[yr],'.tif'))
    log_it("Year 1 - calculating r_timesbunr")
    #r_timesburnt = calc(this_year,fun = function(x){0},filename=paste0(rast_temp,"/rNumTimesBurnt.tif"),overwrite=TRUE)
    r_timesburnt = deepcopy(r_lastb)
    log_it("Zeroing")
    terra::values(r_timesburnt)=0
    log_it("writing")
    terra::writeRaster(r_timesburnt,paste0(rast_temp,"/",'rNumTimesBurnt.tif'),overwrite=TRUE)
    log_it("Writing complete")
    gc()
    log_it("Garbage Collected")
  }  else {
    log_it("loading r_lasb")
    r_lastb = terra::rast(paste0(rast_temp,"/",'rLastYearBurnt.tif'))
    log_it("loading r_timesbunrt")
    r_timesburnt = terra::rast(paste0(rast_temp,"/rNumTimesBurnt.tif"))
  }
  
  # We now have two temp files
  log_it("Adding to stack")
  log_it("New maximum")
  writeRaster(this_year * int_list[yr],paste0(rast_temp,"/temp_ty.tif"),overwrite=TRUE)
  tyt <- rast(paste0(rast_temp,"/temp_ty.tif"))
  st_test <- c(tyt,r_lastb)
  gc()
  r_lastb <- max(st_test,na.rm=TRUE)
  rm(st_test)
  gc()
  #print(plot(r_lastb))
  
  
  
  
  # r_lastb = max(stack(this_year* int_list[yr],r_lastb),na.rm=TRUE) # Four temp files
  log_it("Adding to count")
  log_it("Writing intermediate rasters")
  #print(plot(r_lastb))
  writeRaster(r_lastb,paste0(rast_temp,"/",'rLastYearBurnt.tif'),overwrite=TRUE)
  writeRaster(r_timesburnt + this_year,paste0(rast_temp,"/",'rNumTimesBurnt.tif'),overwrite=TRUE)
  
  file.copy(paste0(rast_temp,"/",'rLastYearBurnt.tif'),paste0(rast_temp,"/",'rLastYearBurnt_',int_list[yr],'.tif'))
  file.copy(paste0(rast_temp,"/",'rNumTimesBurnt.tif'),paste0(rast_temp,"/",'rNumTimesBurnt_',int_list[yr],'.tif'))
  
  log_it("Deleting")
  rm(r_lastb)
  rm(r_timesburnt)
  gc()
  #print(plot(r_timesburnt))
  log_it("removing temp files")
  removeTmpFiles(h=0)
  unlink(paste0(rast_temp,"/","year_fire.gpkg"))
  
  
  orast[[yr]]=paste0(rast_temp,"/",int_list[yr],".tif")
}

###---##@#

#rm(comp_stack)
gc()
endCluster()


datx <- NULL
rm(datx)
gc()
log_it("Rasterizing seasons complete")



log_it("Removing fire history form memory")
v_fire = NULL
rm(v_fire)
dat_x = NULL
rm(dat_x)
gc()





#rm(cl)
log_it("Last burnt zero to NA")

#r_lastb = raster(paste0(rast_temp,"/",'rLastYearBurnt.tif'))
#rclmat = matrix(c(0,NA),nrow=1)
#r_lastb = reclassify(r_lastb,rclmat)
#writeRaster(r_lastb,paste0(rast_temp,"/",'rLastYearBurnt.tif'),overwrite=TRUE)

zero_raster('rLastYearBurnt.tif')






for(yr in seq_along(int_list)){
  #r_lastb = raster(paste0(rast_temp,"/",'rLastYearBurnt_',int_list[yr],'.tif'))
  #r_lastb = reclassify(r_lastb,rclmat)
  zero_raster(paste0('rLastYearBurnt_',int_list[yr],'.tif'))
  }


log_it("Last burnt raster complete")


# Write time since last
log_it("Loading Last Year Burnt raster")
r_lastb = rast(paste0(rast_temp,"/",'rLastYearBurnt.tif'))
log_it("Subtracting")
r_tsl = current_year - r_lastb
log_it("Writing main Time Since Last raster")
# Exclude external
#log_it("Masking time since last fire raster")
writeRaster(r_tsl,paste0(rast_temp,"/",'rTimeSinceLast.tif'))



log_it("Generating year list")
full_year_list = min(int_list):(current_year)
log_it(full_year_list)

log_it("Looping through year list to generate incremental time since last")


for(this_year in full_year_list){
#for(this_year in k_year_list){
  log_it(paste0("Year: ",this_year))
  nearest_year = int_list - this_year
  nearest_year = max(nearest_year[!nearest_year > 0]) + this_year
  log_it(paste0("Nearest past year: ",nearest_year))
  log_it(paste0("Past year exists? ", file.exists(paste0(rast_temp,"/",'rLastYearBurnt_',nearest_year,'.tif'))))
  log_it("Loading file")
  r_lastb = rast(paste0(rast_temp,"/",'rLastYearBurnt_',nearest_year,'.tif'))
  Sys.sleep(1)
  log_it("File info:")
  log_it(print(r_lastb))
  log_it(paste0("Subtracting from year: ",this_year))
  r_tsl = this_year - r_lastb
  Sys.sleep(1)
  log_it("Subtraction Complete")
  
  log_it("Writing incremental Time Since Last")
  try(writeRaster(r_tsl,paste0(rast_temp,"/",'rTimeSinceLast_',this_year,'.tif')))
  Sys.sleep(1)
  log_it("Cleaning up")
  
  r_lastb <- NULL
  r_tsl <- NULL
  
  rm(r_lastb)
  rm(r_tsl)
  
  gc()
}



# process times burnt
log_it("Processing times burnt raster")

log_it("Loading Number of Times Burnt")
r_timesburnt = raster(paste0(rast_temp,"/rNumTimesBurnt.tif"))
log_it("Zero to NA reclassify")
rclmat = matrix(c(0,NA),nrow=1)
r_timesburnt = reclassify(r_timesburnt,rclmat)
#mask_tif<-raster(paste0(rast_temp,"/roi_mask.tif"))
#r_timesburnt = r_timesburnt * mask_tif

log_it("Writing Number of Times Burnt raster")
bigWrite(r_timesburnt,paste0(rast_temp,"/rNumTimesBurnt.tif"))

log_it("Processing incremental Number of Times Burnt")
for(this_year in full_year_list){
  log_it(paste0("Year: ",this_year))
  nearest_year = int_list - this_year
  nearest_year = max(nearest_year[!nearest_year > 0]) + this_year
  log_it(paste0("Nearest past year: ",nearest_year))
  log_it(paste0("Past year exists? ", file.exists(paste0(rast_temp,"/rNumTimesBurnt_",nearest_year,".tif"))))
  #r_timesburnt = raster(paste0(rast_temp,"/rNumTimesBurnt_",nearest_year,".tif"))
  #r_timesburnt = reclassify(r_timesburnt,rclmat)
  #r_timesburnt = r_timesburnt * mask_tif
  #bigWrite(r_timesburnt,paste0(rast_temp,"/",'rNumTimesBurnt_',this_year,'.tif'))
  if(nearest_year != this_year){
    file.copy(paste0(rast_temp,"/rNumTimesBurnt_",nearest_year,".tif"),paste0(rast_temp,"/rNumTimesBurnt_",this_year,".tif"),overwrite = TRUE)
  }
  log_it("Copying to current year")
  
}

#rm(mask_tif)
log_it("Processing times burnt raster complete")

