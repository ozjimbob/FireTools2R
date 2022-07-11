
# Read the regions table, filter to region of interest, transform
log_it("Loading regions")
library(terra)

if(file.exists(paste0(rast_temp,"/v_region.gpkg"))){
  log_it("region found")
  v_thisregion <- vect(paste0(rast_temp,"/v_region.gpkg"))
}else{
  
  v_regions = vect(corp_gdb,layer=i_vt_boundary)
  
  if(d_spatial_unit != ""){
    log_it(paste0("Filtering region layer to ROI:",d_spatial_unit))
    v_thisregion = filter(corp_gdb,layer=i_vt_boundary,query=paste0("select * from ",i_vt_boundary," where ",f_spatial_unit," = '",d_spatial_unit,"';"))
    log_it("Filtering fire history to ROI complete")
  }else{
    #log_it("No filtering of region, creating union")
    #v_regions$flag=1
    v_thisregion <- aggregate(v_regions)
    log_it("Union complete")
  }
  
  
  log_it("Projecting and repairing region")
  v_thisregion = terra::project(v_thisregion,proj_crs)
  #v_thisregion = st_make_valid(v_thisregion)
  log_it("Region projection complete")
  
  log_it("Writing region template")
  writeVector(v_thisregion,paste0(rast_temp,"/v_region.gpkg"))
  log_it("Finished writing region template")
}
#### Create template raster

# Define bounding box of raster
bbox = ext(v_thisregion)

# Generate template raster of this extent and resolution
log_it("Creating template raster")

# Load 25m NSW Alignment grid
if(!exists("grid_file")){
  align_grid = rast("../config/grid.tif")
}else{
  align_grid = rast(grid_file)
}
tmp_extent = bbox
tmp_extent = align(tmp_extent,align_grid)

# Make template raster
tmprast = rast(tmp_extent, res=c(ras_res,ras_res), crs=proj_crs)

rex = paste(ext(tmprast)[c(1,3,2,4)],collapse=" ")
rres = res(tmprast)

# Make mask raster for ROI
v_thisregion$flag = 1
log_it("Rasterizing ROI Mask")
mask_tif = rasterize(v_thisregion,tmprast,field="flag")
log_it("Writing ROI Mask")
writeRaster(mask_tif,paste0(rast_temp,"/roi_mask.tif"))
rm(mask_tif)
#mask_tif=raster(paste0(rast_temp,"/roi_mask.tif"))





log_it("Reading vegetation layer")
#if(length(i_vt_veg)>1){
#  log_it("Merging vegetation layers")
#  vlist = list()
#  for(veg_lyr_idx in seq_along(i_vt_veg)){
#    vlist[[veg_lyr_idx]] = read_sf(veg_gdb,i_vt_veg[veg_lyr_idx])
#  }
#  v_veg = do.call(rbind,vlist)
#  vlist <- NULL
#  rm(vlist)
#  gc()
#  log_it("Merged")
#}else{
log_it("Loading single layer")
#v_veg = vect(veg_gdb,layer=i_vt_veg)
v_veg = vect(veg_gdb,layer=i_vt_veg,query = paste0("select ",i_vt_veg,".*, ",i_vt_veg_lut,".* from ",i_vt_veg," left join ",i_vt_veg_lut," on ",i_vt_veg,".VEG = ", i_vt_veg_lut,".VEG"))

#v_veg = vect(veg_gdb,layer=i_vt_veg,query=paste0("select ",f_vegid," from ",i_vt_veg,";"))
#}

#v_veg = v_veg[, f_vegid]
log_it("Projecting and repairing vegetation layer")
v_veg = project(v_veg,proj_crs)


#v_veg = st_cast(v_veg,"MULTIPOLYGON") # Multisurface features cause errors
#ii = st_is_empty(v_veg)
# NEW - remove invalid polygons, rather than buffer to 0?



# Clip veg to region of interest

#v_veg = st_make_valid(v_veg) # repair invalid geometries
log_it("Projecting vegetation complete")

# <-

# Remove empty polygons
log_it("Cleaning vegetation layer")
#v_veg = terra::subset(v_veg,expanse(v_veg)>0)

#v_veg = filter(v_veg,as.numeric(st_area(v_veg))>0)
#v_veg = filter(v_veg,st_geometry_type(v_veg) %in% c("POLYGON","MULTIPOLYGON"))
#v_veg = sf_cast(v_veg,"MULTIPOLYGON",close=FALSE)


log_it("Cleaning vegetation layer complete")

# join fire parameters
log_it("Reading vegetation-fire LUT")
#v_vegfire_table = st_read(lut_gdb,i_vt_veg_lut) 

log_it("Aligning veg type fieldnames")
##names(v_veg)[toupper(names(v_veg))==toupper(f_vegid)] = f_vegid

#names(v_vegfire_table)[toupper(names(v_vegfire_table))==toupper(f_vegid)] = f_vegid


log_it("Checking and clearing pre-join names")
#to_remove=c(f_vegmin,f_vegmax,f_vegfireprone,f_vegadv)
#veg_enames = names(v_veg)
#to_remove = intersect(to_remove,veg_enames)
#v_veg = v_veg %>% dplyr::select(-to_remove)

log_it("Joining vegetation to LUT")

#v_veg = left_join(v_veg,v_vegfire_table,by=f_vegid)
#v_veg <- merge(v_veg,v_vegfire_table,by.x="VEG",by.y="VEG")





log_it("Fixing Missing")

na_max = as.vector(is.na(v_veg[[f_vegmax]]))
v_veg[,f_vegmax][na_max]=0
rm(na_max)
na_min = as.vector(is.na(v_veg[[f_vegmin]]))
v_veg[,f_vegmin][na_min]=0
rm(na_min)
na_fireprone = as.vector(is.na(v_veg[[f_vegfireprone]]))
v_veg[,f_vegfireprone][na_fireprone]=0
rm(na_fireprone)
na_adv = as.vector(is.na(v_veg[[f_vegadv]]))
v_veg[,f_vegadv][na_adv]=0
rm(na_adv)






#### Fill in missing here?

log_it("Clipping vegetation layer")

#aa = as.numeric(st_area(v_veg))

#v_veg=v_veg[aa >1,]
#v_veg=st_buffer(v_veg,0)
#aa = expanse(v_veg)
#to_rem = which(aa==0)
#v_veg = v_veg[-to_rem,]

#v_veg = terra::makeValid(v_veg)
#v_veg = terra::intersect(v_veg,v_thisregion)
#v_veg = terra::makeValid(v_veg)


log_it("Clipping vegetation complete")


log_it("Saving Vegetation polygon layer")
#v_veg = dplyr::select(v_veg,Code,Description,PWGArea,EstateName,PWGRegion,NSWCommunity,NSWClass,NSWFormation,VegSource,Region,VEG,MAX,MIN,FireProneV,ADV)
writeVector(v_veg,paste0(rast_temp,"/v_vegBase.gpkg"))

r_vegcode = rasterize(v_veg,tmprast,field="VEG")

log_it("Vegetation polygon base saved")

#v_veg = NULL
#rm(v_veg)
gc()


# Rasterize
log_it("Rasterizing vegetation ID")


r_vegcode = terra::rasterize(v_veg,tmprast,field="VEG",filename=paste0(rast_temp,"/r_vegcode3.tif"))
rm(r_vegcode)


log_it("Rasterizing vegetation minimum interval")
#cmd = g_rasterize("v_vegBase","v_vegBase.gpkg",paste0(rast_temp,"/r_vegmin.tif"),attribute=f_vegmin)
r_vegmin = terra::rasterize(v_veg,tmprast,field="MIN",filename=paste0(rast_temp,"/r_vegmin.tif"))
rm(r_vegmin)
gc()
log_it("Rasterizing vegetation minimum interval complete")

log_it("Rasterizing vegetation maximum interval")
r_vegmax = terra::rasterize(v_veg,tmprast,field="MAX",filename=paste0(rast_temp,"/r_vegmax.tif"))
rm(r_vegmax)
gc()
log_it("Rasterizing vegetation maximum interval complete")


log_it("Rasterizing vegetation fire prone")
r_vegfireprone = terra::rasterize(v_veg,tmprast,field="FireProneV",filename=paste0(rast_temp,"/r_vegfireprone.tif"))
rm(r_vegfireprone)
gc()
log_it("Rasterizing vegetation fire prone complete")

log_it("Rasterizing vegetation fire advantage")
r_vegadv = terra::rasterize(v_veg,tmprast,field="ADV",filename=paste0(rast_temp,"/r_vegadv.tif"))
rm(r_vegadv)
log_it("Rasterizing vegetation fire advantage complete")

# Clean up
log_it("Cleaning up memory")
rm(tmprast)
gc()
log_it("Memory clean up complete")


## tbl
Sys.time()


