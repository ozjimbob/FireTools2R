
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

#### Create template raster

# Define bounding box of raster
bbox = st_bbox(v_thisregion)

# Generate template raster of this extent and resolution
log_it("Creating template raster")
tmprast = raster(ext=extent(bbox[c(1,3,2,4)]), res=c(ras_res,ras_res), crs=proj_crs)

# If we have defined a subextent, crop the template to that
if(!is.null(subextent)){
  log_it("Clipping template raster to subextent")
  tmprast = crop(tmprast,subextent)
  v_thisregion = st_as_sfc(st_bbox(tmprast))
  write_sf(v_thisregion,paste0(rast_temp,"/v_region.gpkg"))
  log_it("Clipping complete")
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
v_fire = st_make_valid(v_fire)

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

# Store rasters in a list
orast = list()


log_it(paste0("Rasterizing ",length(int_list)," fire seasons"))
rast_method = "external"
for(yr in seq_along(int_list)){
  datx = filter(v_fire,numYear==int_list[yr])
  datx = st_cast(datx,"MULTIPOLYGON")
  
  if(rast_method == "internal"){
    # Use fasterize
    tt=fasterize(datx,tmprast,field="count",fun="sum")
    #tt = velox(tt)
    #tt$write(paste0(rast_temp,"/",int_list[yr],".tif"),overwrite=TRUE)
    writeRaster(tt,filename=paste0(rast_temp,"/",int_list[yr],".tif"),progress="text",overwrite=TRUE,datatype="INT1U")
    rm(tt)
    gc()
  }else{
    # Ship out to gdal
    write_sf(datx,paste0(rast_temp,"/","year_fire.gpkg"))
    rex = paste(extent(tmprast)[c(1,3,2,4)],collapse=" ")
    rres = res(tmprast)
    #cmd = paste0(gdal_rasterize," -burn 1 -l year_fire -of GTiff ",
    #             "-te ",rex," -tr ",rres[1]," ",rres[2]," -ot byte -co COMPRESS=PACKBITS ",
    #            paste0(rast_temp,"/","year_fire.gpkg")," ",paste0(rast_temp,"/",int_list[yr],".tif"))
    cmd = g_rasterize("year_fire","year_fire.gpkg",paste0(rast_temp,"/",int_list[yr],".tif"),otype="byte")
    system(cmd)
    unlink(paste0(rast_temp,"/","year_fire.gpkg"))
  }
  orast[[yr]]=paste0(rast_temp,"/",int_list[yr],".tif")
}
log_it("Rasterizing seasons complete")



log_it("Removing fire history form memory")
v_fire = NULL
rm(v_fire)
dat_x = NULL
rm(dat_x)
gc()

st = stack(orast)


# process last year burnt
log_it("Processing last burnt raster")
beginCluster(clustNo)
cl=getCluster()
clusterExport(cl,"current_year")
clusterExport(cl,"year_list")
r_lastb<- invisible(clusterR(st, calc, args=list(fun=calc_tsl),filename=paste0(rast_temp,"/",'rLastYearBurnt.tif'), overwrite=TRUE,m=2,progess="text"))
endCluster()
rm(cl)
log_it("Last burnt raster complete")

# Write time since last
log_it("Writing time since last fire raster")
r_tsl = current_year - r_lastb
writeRaster(r_tsl,paste0(rast_temp,"/",'rTimeSinceLast.tif'),overwrite=TRUE)


log_it("Vectorizing time since last burnt")


if(OS=="Windows"){
v_tsl = polygonizer_win(r_tsl,
                        pypath="C:/OSGeo4W64/bin/gdal_polygonize.py")
}else{
  v_tsl = polygonizer(r_tsl)
}

v_tsl = st_as_sf(v_tsl)
st_crs(v_tsl)=proj_crs


log_it("Dissolving Time Since Last Fire polygons")
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
beginCluster(clustNo)
cl=getCluster()
clusterExport(cl,"year_list")
r_timesburnt <- invisible(clusterR(st, calc, args=list(fun=calc_timesburnt),filename=paste0(rast_temp,"/",'rNumTimesBurnt.tif'), overwrite=TRUE,m=10))
endCluster()
log_it("Processing times burnt raster complete")

# Clean up times burnt
log_it("Removing time since last from memory")
r_timesburnt <- NULL
rm(r_tsl)
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
v_veg = st_make_valid(v_veg) # repair invalid geometries
log_it("Projecting vegetation complete")

# Clip veg to region of interest
log_it("Clipping vegetation layer")
v_veg = st_intersection(v_veg,v_thisregion)
log_it("Clipping vegetation complete")

# Remove empty polygons
log_it("Cleaning vegetation layer")
v_veg = filter(v_veg,as.numeric(st_area(v_veg))>0)
v_veg = filter(v_veg,st_geometry_type(v_veg) %in% c("POLYGON","MULTIPOLYGON"))
v_veg = st_cast(v_veg,"MULTIPOLYGON")
log_it("Cleaning vegetation layer complete")

# join fire parameters
log_it("Reading vegetation-fire LUT")
v_vegfire_table = st_read(fire_gdb,i_vt_veg_lut)
log_it("Aligning veg type fieldnames")
names(v_veg)[toupper(names(v_veg))==toupper(f_vegid)] = f_vegid
names(v_vegfire_table)[toupper(names(v_vegfire_table))==toupper(f_vegid)] = f_vegid
log_it("Joining vegetation to LUT")
v_veg = left_join(v_veg,v_vegfire_table,by=f_vegid)
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


