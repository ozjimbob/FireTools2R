# Calculate fire management zone thresholds
library(tidyverse)
library(sf)
#library(velox)
library(raster)
#library(spatial.tools)
library(foreach)
library(doParallel)

# Setup
#source("../config/global_config.r")
#source("../config/config_linux.r")
#source("fire_cell_function.r")

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

#v_fmz = st_make_valid(v_fmz) # repair invalid geometries
log_it("Fire management zone import complete")


log_it("Loading region boundary")
v_thisregion = read_sf(paste0(rast_temp,"/v_region.gpkg"))

#log_it("Clipping fire management zone to ROI")
#v_fmz = st_crop(st_buffer(v_fmz,0),v_thisregion)
log_it("Converting region to raw polygon")
sub_thisregion = st_as_sfc(st_buffer(v_thisregion,0))
#v_fmz = st_intersection(v_fmz,sub_thisregion)
log_it("Cropping FMZ to region boundary with correction buffer")
v_fmz = st_intersection(st_buffer(v_fmz,0),sub_thisregion)

#log_it("Clipping  fire management zone complete")

log_it("Extracting SFAZ polygons")
v_sfaz = filter(v_fmz,(!!rlang::sym(f_fmz)) == c_sfaz)

log_it("Repairing SFAZ polygons")
#v_sfaz = st_make_valid(v_sfaz)
v_sfaz = st_buffer(v_sfaz,0)

log_it("Writing SFAZ polygons")
v_sfaz <- st_cast(v_sfaz,"MULTIPOLYGON")
write_sf(v_sfaz,paste0(rast_temp,"/v_sfaz.gpkg"))



log_it("Rasterizing SFAZ polygons")
#rex = paste(extent(tmprast)[c(1,3,2,4)],collapse=" ")
#rres = res(tmprast)

#cmd = g_rasterize("v_sfaz","v_sfaz.gpkg",paste0(rast_temp,"/r_fmz.tif"),attribute="")
#system(cmd)
###########
####
####

tr <- terra::rast(tmprast)
vv_sfaz <- vect(paste0(rast_temp,"/v_sfaz.gpkg"))
vv_sfaz$temp = 1

cmd = terra::rasterize(vv_sfaz,tr,field="temp",filename=paste0(rast_temp,"/r_fmz.tif"),overwrite=TRUE)
rm(cmd)
rm(vv_sfaz)

#####
#######
#############




unlink(paste0(rast_temp,"/v_sfaz.gpkg"))
log_it("Finished rasterizing SFAZ layer")


log_it("Loading time since fire vector layer")
v_tsl = read_sf(paste0(rast_temp,"/v_tsl.gpkg"))

log_it("Intersecting SFAZ and TSF layers")
v_tsl_sfaz = st_intersection(v_sfaz,v_tsl)

# Difference
v_tsl$id=NA
any_fire = v_tsl %>% group_by(id) %>% summarise()

v_tsl_sfaz2 = st_difference(v_sfaz,any_fire)
#write_sf(v_tsl_sfaz,paste0(rast_temp,"/v_sfaz_j_tsl.gpkg"))
#write_sf(v_tsl_sfaz2,paste0(rast_temp,"/v_sfaz_j2_tsl.gpkg"))
names(v_tsl_sfaz2)[which(names(v_tsl_sfaz2)=="id")]="TSL"
v_tsl_sfaz = rbind(v_tsl_sfaz,v_tsl_sfaz2)
rm(v_tsl_sfaz2)
gc()
log_it("Generating SFAZ threshold class")
#v_tsl_sfaz$SFAZStatus = 0


log_it("Extracting finding MAXINT")
v_sfaz_table = filter(v_fmz_table,(!!rlang::sym(f_fmz)) == c_sfaz)
this_maxint = v_sfaz_table[[f_vt_maxint]]
log_it(paste0("Maxint is: ",this_maxint))
f_sfaz_custom = as.numeric(f_sfaz_custom)
log_it(paste0("SFAZ treatment custom is: ",f_sfaz_custom))

v_tsl_sfaz_c = v_tsl_sfaz

log_it("Categorizing standard SFAZ")
v_tsl_sfaz= v_tsl_sfaz %>% mutate(SFAZStatus = case_when(is.na(TSL) ~ 10,
                                        TSL<=6 ~ 6,
                                         TSL >6 & TSL <= 10 ~ 7,
                                         TSL >10 ~ 8))


#v_tsl_sfaz$SFAZStatusText = ""
v_tsl_sfaz= v_tsl_sfaz %>% mutate(SFAZStatusText = case_when(is.na(TSL) ~ "Priority for Assessment and Treatment",
                                                             TSL<=6 ~ "Recently Treated",
                                                         TSL >6 & TSL <= 10 ~ "Monitor OFH in the field",
                                                         TSL >10 ~ "Priority for Assessment and Treatment"))


v_tsl_sfaz$TSL[is.na(v_tsl_sfaz$TSL)]=9999


log_it("Categorizing custom SFAZ")
v_tsl_sfaz_c= v_tsl_sfaz_c %>% mutate(SFAZStatus = case_when(is.na(TSL) ~ 10,
                                                             TSL<=f_sfaz_custom ~ 6,
                                                         (TSL >f_sfaz_custom) & (TSL <= this_maxint) ~ 7,
                                                         TSL >this_maxint ~ 8))


#v_tsl_sfaz_c$SFAZStatusText = ""
v_tsl_sfaz_c= v_tsl_sfaz_c %>% mutate(SFAZStatusText = case_when(is.na(TSL) ~ "Priority for Assessment and Treatment",
                                                                 TSL<=f_sfaz_custom ~ "Recently Treated",
                                                             (TSL >f_sfaz_custom) & (TSL <= this_maxint) ~ "Monitor OFH in the field",
                                                             TSL >this_maxint ~ "Priority for Assessment and Treatment"))

v_tsl_sfaz_c$TSL[is.na(v_tsl_sfaz_c$TSL)]=9999


log_it("Writing SFAZ threshold polygons")
v_tsl_sfaz = v_tsl_sfaz %>% st_cast("MULTIPOLYGON")
v_tsl_sfaz = filter(v_tsl_sfaz,as.numeric(st_area(v_tsl_sfaz))>0)


v_tsl_sfaz_c = v_tsl_sfaz_c %>% st_cast("MULTIPOLYGON")
v_tsl_sfaz_c = filter(v_tsl_sfaz_c,as.numeric(st_area(v_tsl_sfaz_c))>0)


log_it("Clipping to region of interest")
v_thisregion = read_sf(paste0(rast_temp,"/v_region.gpkg"))
v_tsl_sfaz = st_crop(st_buffer(v_tsl_sfaz,0),v_thisregion)
v_tsl_sfaz_c = st_crop(st_buffer(v_tsl_sfaz_c,0),v_thisregion)
v_thisregion <- NULL
rm(v_thisregion)
gc()

log_it("Saving SFAZ threshold polygons")

v_tsl_sfaz <- st_cast(v_tsl_sfaz,"MULTIPOLYGON")
v_tsl_sfaz_c<- st_cast(v_tsl_sfaz_c,"MULTIPOLYGON")

write_sf(v_tsl_sfaz,paste0(rast_temp,"/v_tsl_sfaz.gpkg"))
write_sf(v_tsl_sfaz_c,paste0(rast_temp,"/v_sfaz_candidate_blocks.gpkg"))
log_it("SFAZ thresholds saved. Cleaning up")


log_it("Rasterizing SFAZ categories")
rex = paste(extent(tmprast)[c(1,3,2,4)],collapse=" ")
rres = res(tmprast)




tr <- terra::rast(tmprast)
cmd = terra::rasterize(vect(paste0(rast_temp,"/v_tsl_sfaz.gpkg")),tr,field="SFAZStatus",filename=paste0(rast_temp,"/r_tsl_sfaz.tif"),overwrite=TRUE)
cmd = terra::rasterize(vect(paste0(rast_temp,"/v_sfaz_candidate_blocks.gpkg")),tr,field="SFAZStatus",filename=paste0(rast_temp,"/v_sfaz_candidate_blocks.tif"),overwrite=TRUE)

rm(cmd)


#cmd = g_rasterize("v_tsl_sfaz","v_tsl_sfaz.gpkg",paste0(rast_temp,"/r_tsl_sfaz.tif"),attribute="SFAZStatus")
#system(cmd)
#cmd = g_rasterize("v_sfaz_candidate_blocks","v_sfaz_candidate_blocks.gpkg",paste0(rast_temp,"/v_sfaz_candidate_blocks.tif"),attribute="SFAZStatus")
#system(cmd)

log_it("Loading SFAZ raster")
r_tsl_sfaz = rast(paste0(rast_temp,"/r_tsl_sfaz.tif"))

#bigWrite(r_tsl_sfaz,paste0(rast_temp,"/r_tsl_sfaz.tif"))  <-----

log_it("Loading biodiversity and fire zone raster")
r_fmz_bio = rast(paste0(rast_temp,"/r_fmz_bio_out.tif"))

#bigWrite(r_fmz_bio,paste0(rast_temp,"/r_fmz_bio_out.tif"))

log_it("Loading fire zone raster")
r_fmz = rast(paste0(rast_temp,"/r_fmzout.tif"))


## Load r_tsl



log_it("Merging SFAZ to combined FMZ raster")
log_it(paste0("System Memory Available: ",getFreeMemoryKB()))




  r_tsl_sfaz = classify(r_tsl_sfaz, cbind(0, NA), right=FALSE)
  r_comb <-cover(r_tsl_sfaz,r_fmz)

gc()




log_it("Saving SFAZ - FMZ combined raster")
writeRaster(r_comb,paste0(rast_temp,"/r_sfaz_fmz_out.tif"),overwrite=TRUE)

r_comb <- raster(paste0(rast_temp,"/r_sfaz_fmz_out.tif"))
#####################
log_it("Vectorizing SFAZ - FMZ  combined categories")

#r_comb = raster(paste0(rast_temp,"/r_sfaz_fmz_bio_out.tif"))
log_it("Converting SFAZ - FMZ raster to polygons")

if(OS == "Windows"){
  v_sfaz_fmz_out = polygonizer_terra(r_comb)
}else{
  v_sfaz_fmz_out = polygonizer_terra(r_comb)
}


#v_sfaz_fmz_out = st_as_sf(v_sfaz_fmz_out)
st_crs(v_sfaz_fmz_out)=proj_crs

log_it("Dissolving  SFAZ - FMZ  polygons")
v_sfaz_fmz_out = v_sfaz_fmz_out %>% st_cast("MULTIPOLYGON") #%>% group_by(DN) %>% summarise()




log_it("Repairing  SFAZ - FMZ - Heritage polygons")
v_sfaz_fmz_out = filter(v_sfaz_fmz_out,as.numeric(st_area(v_sfaz_fmz_out))>0)

#v_sfaz_fmz_out = st_make_valid(v_sfaz_fmz_out)


log_it("Clipping to region of interest")
v_thisregion = read_sf(paste0(rast_temp,"/v_region.gpkg"))
#v_sfaz_fmz_out = st_intersection(v_sfaz_fmz_out,v_thisregion)


t_threshold=tibble(DN=c(1,2,3,4,5,9,6,7,8,10,NA),
                   FinalStatus = c("NoFireRegime",
                                   "TooFrequentlyBurnt",
                                   "Vulnerable",
                                   "LongUnburnt",
                                   "WithinThreshold",
                                   "Unknown",
                                   "Recently Treated",
                                   "Monitor OFH In the Field",
                                   "Priority for Assessment and Treatment",
                                   "Priority for Assessment and Treatment",NA))

log_it("Joining  SFAZ - FMZ labels to polygons")
names(v_sfaz_fmz_out)[1]="DN"

v_sfaz_fmz_out = left_join(v_sfaz_fmz_out,t_threshold)
v_sfaz_fmz_out$DN = NULL


log_it("Saving SFAZ - FMZ polygons")
write_sf(v_sfaz_fmz_out,paste0(rast_temp,"/v_sfaz_fmz_out.gpkg"))
log_it(" SFAZ - FMZ saved. Cleaning up")

log_it("Cleaning up")
r_comb <- NULL
rm(r_comb)
gc()



#<<<<----

#############################################################


log_it("Merging SFAZ to combined heritage and FMZ raster")

log_it(paste0("System Memory Available: ",getFreeMemoryKB()))


#if(old){
#  c_func = function(x,y){ifelse(x==0,y,x)}
#  s = stack(r_tsl_sfaz,r_fmz_bio)
#  
#  invisible(capture.output({
#    beginCluster(clustNo)
#    r_comb <- try(clusterR(s,overlay,args=list(fun=c_func)),silent = TRUE)
#    if(class(r_comb)=="try-error"){
#      r_comb = overlay(s,fun=function(x,y){ifelse(x==0,y,x)})
#    }
#    endCluster()
#  }))
  
#  s <- NULL
#  rm(s)
#}else{
  r_tsl_sfaz = classify(r_tsl_sfaz, cbind(0, NA), right=FALSE)
  r_comb <-cover(r_tsl_sfaz,r_fmz_bio)
#}
gc()



log_it("Saving SFAZ - FMZ - Heritage combined raster")
writeRaster(r_comb,paste0(rast_temp,"/r_sfaz_fmz_bio_out.tif"),overwrite=TRUE)



r_comb <- raster(paste0(rast_temp,"/r_sfaz_fmz_bio_out.tif"))

#####################
log_it("Vectorizing SFAZ - FMZ - Heritage combined categories")

#r_comb = raster(paste0(rast_temp,"/r_sfaz_fmz_bio_out.tif"))
log_it("Converting SFAZ - FMZ - Heritage raster to polygons")

if(OS == "Windows"){
v_sfaz_all_out = polygonizer_terra(r_comb)
}else{
  v_sfaz_all_out = polygonizer_terra(r_comb)
}


######### <---------------------------------------------

#v_sfaz_all_out = st_as_sf(v_sfaz_all_out)
st_crs(v_sfaz_all_out)=proj_crs

log_it("Dissolving  SFAZ - FMZ - Heritage polygons")
v_sfaz_all_out = v_sfaz_all_out %>% st_cast("MULTIPOLYGON") #%>% group_by(DN) %>% summarise()




log_it("Repairing  SFAZ - FMZ - Heritage polygons")
v_sfaz_all_out = filter(v_sfaz_all_out,as.numeric(st_area(v_sfaz_all_out))>0)

#v_sfaz_all_out = st_make_valid(v_sfaz_all_out)


log_it("Clipping to region of interest")
v_thisregion = read_sf(paste0(rast_temp,"/v_region.gpkg"))
#v_sfaz_all_out = st_intersection(v_sfaz_all_out,v_thisregion)


t_threshold=tibble(DN=c(1,2,3,4,5,9,6,7,8,10,NA),
                   FinalStatus = c("NoFireRegime",
                                 "TooFrequentlyBurnt",
                                 "Vulnerable",
                                 "LongUnburnt",
                                 "WithinThreshold",
                                 "Unknown",
                                 "Recently Treated",
                                 "Monitor OFH In the Field",
                                 "Priority for Assessment and Treatment",
                                 "Priority for Assessment and Treatment",NA))

log_it("Joining  SFAZ - FMZ - Heritage labels to polygons")
names(v_sfaz_all_out)[1] = "DN"
v_sfaz_all_out = left_join(v_sfaz_all_out,t_threshold)
v_sfaz_all_out$DN = NULL


log_it("Saving f SFAZ - FMZ - Heritage polygons")
write_sf(v_sfaz_all_out,paste0(rast_temp,"/v_sfaz_fmz_bio_out.gpkg"))
log_it(" SFAZ - FMZ - Heritage saved. Cleaning up")

log_it("Cleaning up")
r_comb <- NULL
r_tsl_sfaz <- NULL
r_fmz <- NULL
r_fmzbio <- NULL
v_tsl_sfaz<- NULL
v_sfaz_all_out<- NULL
v_sfaz_fmz_out<- NULL
v_fmzbio<- NULL
v_fmz<- NULL
rm(r_comb)
rm(r_tsl_sfaz)
rm(r_fmz)
rm(r_fmzbio)
rm(v_tsl_sfaz)
rm(v_tsl_sfaz_c)
rm(v_sfaz_all_out)
rm(v_sfaz_fmz_out)
rm(v_fmzbio)
rm(v_fmz)
gc()

