
# Read the regions table, filter to region of interest, transform
log_it("Loading regions")

if(file.exists(paste0(rast_temp,"/v_region.gpkg"))){
  log_it("region found")
  v_thisregion <- read_sf(v_thisregion)
}else{
  
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
}
#### Create template raster

# Define bounding box of raster
bbox = st_bbox(v_thisregion)

# Generate template raster of this extent and resolution
log_it("Creating template raster")

# Load 25m NSW Alignment grid
if(!exists("grid_file")){
  align_grid = raster("../config/grid.tif")
}else{
  align_grid = raster(grid_file)
}
tmp_extent = extent(bbox[c(1,3,2,4)])
tmp_extent = alignExtent(tmp_extent,align_grid)

# Make template raster
tmprast = raster(ext=tmp_extent, res=c(ras_res,ras_res), crs=proj_crs)
rex = paste(extent(tmprast)[c(1,3,2,4)],collapse=" ")
rres = res(tmprast)

# Make mask raster for ROI
v_thisregion$flag = 1
log_it("Rasterizing ROI Mask")
mask_tif = fasterize(v_thisregion,tmprast,field="flag")
log_it("Writing ROI Mask")
bigWrite(mask_tif,paste0(rast_temp,"/roi_mask.tif"))
rm(mask_tif)
#mask_tif=raster(paste0(rast_temp,"/roi_mask.tif"))





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
#v_veg = st_cast(v_veg,"MULTIPOLYGON") # Multisurface features cause errors
#ii = st_is_empty(v_veg)
# NEW - remove invalid polygons, rather than buffer to 0?



# Clip veg to region of interest

#v_veg = st_make_valid(v_veg) # repair invalid geometries
log_it("Projecting vegetation complete")

# <-

# Remove empty polygons
log_it("Cleaning vegetation layer")
v_veg = filter(v_veg,as.numeric(st_area(v_veg))>0)
v_veg = filter(v_veg,st_geometry_type(v_veg) %in% c("POLYGON","MULTIPOLYGON"))
v_veg = sf_cast(v_veg,"MULTIPOLYGON",close=FALSE)

#v_veg <-remove_invalid_poly_multi(v_veg)

#v_veg = sf_cast(v_veg,"POLYGON",close=FALSE) # Changed from multipolygon
#v_veg <- remove_invalid_poly(v_veg)
#v_veg <- remove_invalid_poly(v_veg)
#i=1
#xx=v_veg
#tst = 
#mn <- min(sapply(st_geometry(xx)[[i]], function(x) nrow(x[[1]])))

#xx <- sf_cast(v_veg,"POLYGON",close=FALSE)

log_it("Cleaning vegetation layer complete")

# join fire parameters
log_it("Reading vegetation-fire LUT")
v_vegfire_table = st_read(lut_gdb,i_vt_veg_lut) 
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

log_it("Clipping vegetation layer")

aa = as.numeric(st_area(v_veg))

v_veg=v_veg[aa >1,]
v_veg=st_buffer(v_veg,0)


v_veg = st_intersection(v_veg,v_thisregion)
log_it("Clipping vegetation complete")


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
log_it("Rasterizing vegetation ID Complete")


log_it("Rasterizing vegetation ID complete")

log_it("Rasterizing vegetation minimum interval")
cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegmin.tif"),attribute=f_vegmin)
system(cmd)
log_it("Rasterizing vegetation minimum interval complete")

log_it("Rasterizing vegetation maximum interval")
cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegmax.tif"),attribute=f_vegmax)
system(cmd)
log_it("Rasterizing vegetation maximum interval complete")

log_it("Rasterizing vegetation fire prone")
cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegfireprone.tif"),attribute=f_vegfireprone)
system(cmd)
log_it("Rasterizing vegetation fire prone complete")

log_it("Rasterizing vegetation fire advantage")
cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegadv.tif"),attribute=f_vegadv)
system(cmd)
log_it("Rasterizing vegetation fire advantage complete")

# Clean up
log_it("Cleaning up memory")
rm(tmprast)
gc()
log_it("Memory clean up complete")



