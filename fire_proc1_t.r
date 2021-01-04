
# Read the regions table, filter to region of interest, transform
log_it("Loading regions")
v_regions = read_sf(corp_gdb,i_vt_boundary)

if(d_spatial_unit != ""){
  log_it(paste0("Filtering region layer to ROI:",d_spatial_unit))
  v_thisregion = filter(v_regions,(!!rlang::sym(f_spatial_unit)) == d_spatial_unit)
  log_it("Filtering fire history to ROI complete")
}else{
  log_it("No filtering of region, creating union")
  v_thisregion = st_union(v_regions)
  log_it("Union complete")
}

log_it("Projecting and repairing region")
v_thisregion = st_transform(v_thisregion,crs=proj_crs)
v_thisregion = st_make_valid(v_thisregion)
log_it("Region projection complete")

log_it("Writing region template")
write_sf(v_thisregion,paste0(rast_temp,"/v_region.gpkg"))
log_it("Finished writing region template")

#### Create template raster

# Define bounding box of raster
bbox = st_bbox(v_thisregion)

# Generate template raster of this extent and resolution
log_it("Creating template raster")

# Load 25m NSW Alignment grid
align_grid = raster("../config/grid.tif")
tmp_extent = extent(bbox[c(1,3,2,4)])
tmp_extent = alignExtent(tmp_extent,align_grid)

# Make template raster
tmprast = raster(ext=tmp_extent, res=c(ras_res,ras_res), crs=proj_crs)

# If we have defined a subextent, crop the template to that
if(!is.null(subextent)){
  subextent = alignExtent(subextent,align_grid)
  log_it("Clipping template raster to subextent")
  tmprast = crop(tmprast,subextent)
  v_thisregion = st_as_sfc(st_bbox(tmprast))
  write_sf(v_thisregion,paste0(rast_temp,"/v_region.gpkg"))
  log_it("Clipping complete")
}

# Make mask raster for ROI
v_thisregion$flag = 1
log_it("Rasterizing ROI Mask")
mask_tif = fasterize(v_thisregion,tmprast,field="flag")
log_it("Writing ROI Mask")
bigWrite(mask_tif,paste0(rast_temp,"/roi_mask.tif"))
rm(mask_tif)
#mask_tif=raster(paste0(rast_temp,"/roi_mask.tif"))



#########
# Read fire history table and transform
log_it("Reading fire history, projecting and repairing")
v_fire = read_sf(fire_gdb,i_vt_fire_history)
v_fire = st_transform(v_fire,crs=proj_crs)
v_fire = st_cast(v_fire,"MULTIPOLYGON") # Multisurface features cause errors
v_fire = st_make_valid(v_fire) # repair invalid geometries
log_it("Fire history import complete")


# Clip fire to region of interest
log_it("Clipping fire history to ROI")
v_fire = st_intersection(v_fire,v_thisregion)
log_it("Clipping fire history complete")

# Add numeric year field to fire and a count flag field
v_fire$numYear = as.numeric(substr(as.character(v_fire[[f_fireseason]]),1,4))
v_fire$count = 1

# If v_TSFF is defined, subset v_fire to only years including or after this fireseason
if(v_TSFF != ""){
  v_fire = filter(v_fire,numYear >= as.numeric(v_TSFF))
}

do_gazette=FALSE
### If present load gazette data
if(gazette_gdb != ""){
  do_gazette=TRUE
  log_it("Loading gazetted parks layer")
  v_gaz = v_regions = read_sf(gazette_gdb,i_vt_gazette)
  log_it("Transforming gazette parks to project CRS")
  v_gaz = st_transform(v_gaz,crs=proj_crs)
  log_it("Repairing gazetted park polygons")
  v_gaz = st_make_valid(v_gaz)
  log_it("Converting gazette year")
  v_gaz = v_gaz %>% mutate(GAZ_YEAR = as.numeric(format((!!rlang::sym(f_gazette_date)),"%Y")))
  log_it("Writing gazette polygons")
  write_sf(v_gaz,paste0(rast_temp,"/","v_gazette.gpkg"))
  rex = paste(extent(tmprast)[c(1,3,2,4)],collapse=" ")
  rres = res(tmprast)
  log_it("Rasterizing gazette polygons")
  cmd = g_rasterize("v_gazette","v_gazette.gpkg",paste0(rast_temp,"/r_gazdate.tif"),attribute="GAZ_YEAR")
  system(cmd)
}


#### Gazette processing

if(do_gazette){
  
  log_it("Cleaning gazette and fire history columns")
  v_gaz = dplyr::select(v_gaz,GAZ_YEAR)
  v_fire = dplyr::select(v_fire,numYear)
  names(v_gaz)[names(v_gaz)==attr(v_gaz, "sf_column")] = "geometry"
  attr(v_gaz, "sf_column") = "geometry"
  names(v_fire)[names(v_fire)==attr(v_fire, "sf_column")] = "geometry"
  attr(v_fire, "sf_column") = "geometry"
  
  log_it("Merging gazette and fire history polygons")
  
  kk = st_or(v_gaz,v_fire)
  yy=kk
  
  # Where we have no fire year, this would be part of a park that hasn't been burned
  # Therefore move the gazettal year into the fire year column
  log_it("Selecting non-burnt gazette years")
  kk$numYear[is.na(kk$numYear)]=kk$GAZ_YEAR[is.na(kk$numYear)]
  
  log_it("Finding gazette years earlier than fire years")
  # Find polygons where we have a gazettal year less than the fire year
  kk_gaz_less_than_fire = filter(kk,GAZ_YEAR < numYear)
  # Copy the gazette year to the fire year
  kk_gaz_less_than_fire$numYear = kk_gaz_less_than_fire$GAZ_YEAR
  # Add these rows to the table where they will counts as new fires
  # There will be *multiple* copies of the polygons, but that doesn't matter as the
  # rasterization process will ignore that.
  log_it("Binding additional gazette years to fire polygons")
  v_fire = rbind(kk,kk_gaz_less_than_fire)
  v_fire$count=1
}


#################


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
year_list = as.numeric(substr(int_list,1,4))

log_it("Writing list of fire seasons")
write_csv(tibble(year = int_list),paste0(rast_temp,"/yearlist.csv"))

# Simplify fire spatial file to save memory
v_fire = dplyr::select(v_fire,numYear,count)
gc()
# Store rasters in a list
orast = list()


log_it(paste0("Rasterizing ",length(int_list)," fire seasons"))
rast_method = "external"
#beginCluster(clustNo)
for(yr in seq_along(int_list)){
  print(int_list[yr])
  datx = filter(v_fire,numYear==int_list[yr])
  datx = st_cast(datx,"MULTIPOLYGON")
  
  if(rast_method == "internal"){
    # Use fasterize
    tt=fasterize(datx,tmprast,field="count",fun="sum")
    #tt = velox(tt)
    #tt$write(paste0(rast_temp,"/",int_list[yr],".tif"),overwrite=TRUE)
    bigWriteBinary(tt,paste0(rast_temp,"/",int_list[yr],".tif"))
    rm(tt)
    gc()
  }else{
    # Ship out to gdal
    log_it("Writing Temp gpkg")
    write_sf(datx,paste0(rast_temp,"/","year_fire.gpkg"))
    rex = paste(extent(tmprast)[c(1,3,2,4)],collapse=" ")
    rres = res(tmprast)
    #cmd = paste0(gdal_rasterize," -burn 1 -l year_fire -of GTiff ",
    #             "-te ",rex," -tr ",rres[1]," ",rres[2]," -ot byte -co COMPRESS=PACKBITS ",
    #            paste0(rast_temp,"/","year_fire.gpkg")," ",paste0(rast_temp,"/",int_list[yr],".tif"))
    log_it("rasterize this year")
    cmd = g_rasterize("year_fire","year_fire.gpkg",paste0(rast_temp,"/",int_list[yr],".tif"),otype="byte")
    system(cmd)
    log_it("Loading this year")
    # Load this year, multiply by year
    this_year = raster(paste0(rast_temp,"/",int_list[yr],".tif"))
    if(yr==1){
      log_it("Year 1 - calculating r_lastb")
      r_lastb = calc(this_year, fun=function(x){x* int_list[yr]},filename=paste0(rast_temp,"/",'rLastYearBurnt.tif'),overwrite=TRUE)
      log_it("Year 1 - calculating r_timesbunr")
      #r_timesburnt = calc(this_year,fun = function(x){0},filename=paste0(rast_temp,"/rNumTimesBurnt.tif"),overwrite=TRUE)
      r_timesburnt = r_lastb
      log_it("Zeroing")
      raster::values(r_timesburnt)=0
      log_it("writing")
      bigWrite(r_timesburnt,paste0(rast_temp,"/",'rNumTimesBurnt.tif'))
      log_it("Writing complete")
      gc()
      log_it("Garbage Collected")
    }  else {
      log_it("loading r_lasb")
      r_lastb = raster(paste0(rast_temp,"/",'rLastYearBurnt.tif'))
      log_it("loading r_timesbunrt")
      r_timesburnt = raster(paste0(rast_temp,"/rNumTimesBurnt.tif"))
    }# We now have two temp files
    log_it("Adding to stack")
    log_it("New maximum")
    r_lastb = max(stack(this_year* int_list[yr],r_lastb),na.rm=TRUE) # Four temp files
    log_it("Adding to count")
    log_it("Writing intermediate rasters")
    #print(plot(r_lastb))
    bigWrite(r_lastb,paste0(rast_temp,"/",'rLastYearBurnt.tif'))
    bigWrite(r_timesburnt + this_year,paste0(rast_temp,"/",'rNumTimesBurnt.tif'))
    log_it("Deleting")
    rm(r_lastb)
    rm(r_timesburnt)
    gc()
    #print(plot(r_timesburnt))
    log_it("removing temp files")
    removeTmpFiles(h=0)
    unlink(paste0(rast_temp,"/","year_fire.gpkg"))
    
  }
  
  orast[[yr]]=paste0(rast_temp,"/",int_list[yr],".tif")
}


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



# process last year burnt
#log_it("Processing last burnt raster")
#st = stack(orast)
#beginCluster(clustNo)
#cl=getCluster()
#clusterExport(cl,"current_year")
#clusterExport(cl,"year_list")
#r_lastb<- invisible(clusterR(st, calc, args=list(fun=calc_tsl),filename=paste0(rast_temp,"/",'rLastYearBurnt.tif'), overwrite=TRUE,m=2))

#invisible(capture.output(r_lastb<-clusterR(st, calc, args=list(fun=calc_tsl),filename=paste0(rast_temp,"/",'rLastYearBurnt.tif'), overwrite=TRUE,m=4)))

#####
#####
#  At this point, experiment with layer-based merging year-by-year


#####
#####

#endCluster()

### Split by extent
### Multiply by year vec
### Find maximum year
### Merge rasters
### write year into rLastYearBurnt
#st = stack(orast,quick=TRUE)
#beginCluster(clustNo)
#st = st * year_list
#st = max(st,na.rm=TRUE)
#endCluster()
#gc()
#bigWrite(st,paste0(rast_temp,"/",'rLastYearBurnt.tif'))

#st <- NULL
#rm(st)
#gc()


#rm(cl)
log_it("Last burnt zero to NA")
r_lastb = raster(paste0(rast_temp,"/",'rLastYearBurnt.tif'))
rclmat = matrix(c(0,NA),nrow=1)
r_lastb = reclassify(r_lastb,rclmat)
writeRaster(r_lastb,paste0(rast_temp,"/",'rLastYearBurnt.tif'),overwrite=TRUE)
log_it("Last burnt raster complete")


# Write time since last
log_it("Writing time since last fire raster")
r_tsl = current_year - r_lastb

# Exclude external
log_it("Masking time since last fire raster")
writeRaster(r_tsl,paste0(rast_temp,"/",'rTimeSinceLast.tif'),overwrite=TRUE)


log_it("Vectorizing time since last burnt")


if(OS=="Windows"){
  v_tsl = polygonizer_win(r_tsl)
}else{
  v_tsl = polygonizer(r_tsl)
}

v_tsl = st_as_sf(v_tsl)
st_crs(v_tsl)=proj_crs


log_it("Dissolving Time Since Last Fire polygons")
v_tsl = st_make_valid(v_tsl)
v_tsl = st_buffer(v_tsl,0)
v_tsl = v_tsl %>% st_cast("MULTIPOLYGON") %>% group_by(DN) %>% summarise()
names(v_tsl)[1]="TSL"


log_it("Repairing Time Since Last Fire polygons")
v_tsl = filter(v_tsl,as.numeric(st_area(v_tsl))>0)
v_tsl = st_make_valid(v_tsl)

log_it("Writing time since last fire polygons")
write_sf(v_tsl,paste0(rast_temp,"/v_tsl.gpkg"))

log_it("Writing time since last fire complete")

# Clean up time since last
log_it("Removing time since last from memory")
r_tsl <- NULL
rm(r_tsl)
gc()

# process times burnt
log_it("Processing times burnt raster")
#beginCluster(clustNo)
#cl=getCluster()
#clusterExport(cl,"year_list")
#invisible(capture.output(r_timesburnt <- clusterR(st, calc, args=list(fun=calc_timesburnt),filename=paste0(rast_temp,"/",'rNumTimesBurnt.tif'), overwrite=TRUE,m=10)))
#invisible(capture.output(r_timesburnt <- clusterR(st, fun=sum,filename=paste0(rast_temp,"/",'rNumTimesBurnt.tif'), overwrite=TRUE,m=4)))
#st = stack(orast)
r_timesburnt = raster(paste0(rast_temp,"/rNumTimesBurnt.tif"))
rclmat = matrix(c(0,NA),nrow=1)
#raster::values(r_timesburnt)[raster::values(r_timesburnt)==0]=NA
r_timesburnt = reclassify(r_timesburnt,rclmat)
mask_tif<-raster(paste0(rast_temp,"/roi_mask.tif"))
r_timesburnt = r_timesburnt * mask_tif
rm(mask_tif)
log_it("Writing time since last fire raster")


bigWrite(r_timesburnt,paste0(rast_temp,"/rNumTimesBurnt.tif"))
#r_timesburnt <- raster(paste0(rast_temp,"/rNumTimesBurnt.tif"))
#endCluster()
log_it("Processing times burnt raster complete")


##<-

log_it("Vectorizing times burnt")


if(OS=="Windows"){
  v_timesburnt = polygonizer_win(r_timesburnt)
}else{
  v_timesburnt = polygonizer(r_timesburnt)
}

v_timesburnt = st_as_sf(v_timesburnt)
st_crs(v_timesburnt)=proj_crs


log_it("Dissolving times burnt polygons")
v_timesburnt = st_make_valid(v_timesburnt)
v_timesburnt = st_buffer(v_timesburnt,0)
v_timesburnt = v_timesburnt %>% st_cast("MULTIPOLYGON") %>% group_by(DN) %>% summarise()
names(v_timesburnt)[1]="TimesBurnt"


log_it("Repairing times burnt  polygons")
v_timesburnt = filter(v_timesburnt,as.numeric(st_area(v_timesburnt))>0)
v_timesburnt = st_make_valid(v_timesburnt)

log_it("Writing times burnt  polygons")
write_sf(v_timesburnt,paste0(rast_temp,"/v_timesburnt.gpkg"))



# Clean up times burnt
log_it("Removing time since last from memory")
r_timesburnt <- NULL
v_timesburnt <- NULL
rm(v_timesburnt)
rm(v_tsl)
r_lastb <- NULL
rm(r_lastb)
gc()





# Write static rasters for current, earliest year
#log_it("Generating static current and first year rasters")
#r_this_year = tmprast
#values(r_this_year) = current_year
#writeRaster(r_this_year,paste0(rast_temp,"/",'rThisYear.nc'),overwrite=TRUE)
#r_first_year = tmprast
#values(r_first_year) = first(int_list)
#writeRaster(r_first_year,paste0(rast_temp,"/",'rFirstYear.nc'),overwrite=TRUE)

### Vegetation
# - Rasterize the veg, with numeric code
# - Convert to max/min frequency rasters

# Load vegetation polygons
# Read fire history table and transform
log_it("Reading vegetation layer")
if(length(i_vt_veg)>1){
  log_it("Merging vegetation layers")
  vlist = list()
  for(veg_lyr_idx in seq_along(i_vt_veg)){
    vlist[[veg_lyr_idx]] = read_sf(veg_gdb,i_vt_veg[veg_lyr_idx])
  }
  v_veg = do.call(rbind,vlist)
  vlist <- NULL
  rm(vlist)
  gc()
  log_it("Merged")
}else{
  log_it("Loading single layer")
  v_veg = read_sf(veg_gdb,i_vt_veg)
}


log_it("Projecting and repairing vegetation layer")
v_veg = st_transform(v_veg,crs=proj_crs)
v_veg = st_cast(v_veg,"MULTIPOLYGON") # Multisurface features cause errors
# Clip veg to region of interest
log_it("Clipping vegetation layer")
v_veg=st_buffer(v_veg,0)


#v_veg = st_intersection(v_veg,v_thisregion)
log_it("Clipping vegetation complete")
#v_veg = st_make_valid(v_veg) # repair invalid geometries
log_it("Projecting vegetation complete")

# <-

# Remove empty polygons
log_it("Cleaning vegetation layer")
v_veg = filter(v_veg,as.numeric(st_area(v_veg))>0)
v_veg = filter(v_veg,st_geometry_type(v_veg) %in% c("POLYGON","MULTIPOLYGON"))
v_veg = st_cast(v_veg,"MULTIPOLYGON")
log_it("Cleaning vegetation layer complete")

# join fire parameters
log_it("Reading vegetation-fire LUT")
v_vegfire_table = st_read(fire_gdb,i_vt_veg_lut) 
#v_vegfire_table = read_csv(i_vt_veg_lut)  ########################################### <<<<<<<
log_it("Aligning veg type fieldnames")
names(v_veg)[toupper(names(v_veg))==toupper(f_vegid)] = f_vegid


names(v_vegfire_table)[toupper(names(v_vegfire_table))==toupper(f_vegid)] = f_vegid

log_it("Checking and clearing pre-join names")
to_remove=c(f_vegmin,f_vegmax,f_vegfireprone,f_vegadv)
veg_enames = names(v_veg)
to_remove = intersect(to_remove,veg_enames)
v_veg = v_veg %>% dplyr::select(-to_remove)

log_it("Joining vegetation to LUT")
v_veg = left_join(v_veg,v_vegfire_table,by=f_vegid)
log_it("Fixing Missing")
v_veg[[f_vegmax]][is.na(v_veg[[f_vegmax]])]=0
v_veg[[f_vegmin]][is.na(v_veg[[f_vegmin]])]=0
v_veg[[f_vegfireprone]][is.na(v_veg[[f_vegfireprone]])]=0
v_veg[[f_vegadv]][is.na(v_veg[[f_vegadv]])]=0

#### Fill in missing here?


log_it("Saving Vegetation polygon layer")
#v_veg = dplyr::select(v_veg,Code,Description,PWGArea,EstateName,PWGRegion,NSWCommunity,NSWClass,NSWFormation,VegSource,Region,VEG,MAX,MIN,FireProneV,ADV)
write_sf(v_veg,paste0(rast_temp,"/v_vegBase.gpkg"),quiet = FALSE)
log_it("Vegetation polygon base saved")

v_veg = NULL
rm(v_veg)
gc()


# Rasterize
log_it("Rasterizing vegetation ID")
cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegcode.tif"),attribute=f_vegid)
system(cmd)



#r_vegcode = fasterize(v_veg,tmprast,field=f_vegid)
#writeRaster(r_vegcode,paste0(rast_temp,"/r_vegcode.tif"),overwrite=TRUE)
#r_vegcode <- NULL
#rm(r_vegcode)
log_it("Rasterizing vegetation ID complete")

log_it("Rasterizing vegetation minimum interval")
cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegmin.tif"),attribute=f_vegmin)
system(cmd)
#r_vegmin = fasterize(v_veg,tmprast,field=f_vegmin)
#writeRaster(r_vegmin,filename=paste0(rast_temp,"/r_vegmin.tif"),overwrite=TRUE)
#r_vegmin <- NULL
#rm(r_vegmin)
log_it("Rasterizing vegetation minimum interval complete")

log_it("Rasterizing vegetation maximum interval")
cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegmax.tif"),attribute=f_vegmax)
system(cmd)
#r_vegmax = fasterize(v_veg,tmprast,field=f_vegmax)
#writeRaster(r_vegmax,filename=paste0(rast_temp,"/r_vegmax.tif"),overwrite=TRUE)
#r_vegmax <- NULL
#rm(r_vegmax)
log_it("Rasterizing vegetation maximum interval complete")

log_it("Rasterizing vegetation fire prone")
cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegfireprone.tif"),attribute=f_vegfireprone)
system(cmd)
#r_vegfireprone = fasterize(v_veg,tmprast,field=f_vegfireprone)
#writeRaster(r_vegfireprone,filename=paste0(rast_temp,"/r_vegfireprone.tif"),overwrite=TRUE)
#r_vegfireprone <- NULL
#rm(r_vegfireprone)
log_it("Rasterizing vegetation fire prone complete")

log_it("Rasterizing vegetation fire advantage")
cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegadv.tif"),attribute=f_vegadv)
system(cmd)
#r_vegadv = fasterize(v_veg,tmprast,field=f_vegadv)
#writeRaster(r_vegadv,filename=paste0(rast_temp,"/r_vegadv.tif"),overwrite=TRUE)
#r_vegadv <- NULL
#rm(r_vegadv)
log_it("Rasterizing vegetation fire advantage complete")

# Clean up
log_it("Cleaning up memory")
rm(st)
rm(r_timesburnt)
rm(tmprast)
gc()
log_it("Memory clean up complete")

### Done - launch veg proc

# Stack them into a cube
log_it("Stacking seasons")

log_it("Launching biodiversity threshold processing")






## Vectorize and summarise veg



## Then launch management proc


