
#tmprast<-"G:/ft_work/41fdc3b0-0b59-11f0-9d44-638075dc0bf7/output/"




#### Future burn 

log_it("YULU Processing Start")
log_it("Loading source rasters")

hts <- rast(paste0(rast_temp,"/r_vegout.tif"))
tsl <- rast(paste0(rast_temp,"/rTimeSinceLast.tif"))
max <- rast(paste0(rast_temp,"/r_vegmax.tif"))
min <- rast(paste0(rast_temp,"/r_vegmin.tif"))

log_it("Creating 9999 cover mask")
# Grab a max/min=9999 mask we can use later
#max_9999 <- subst(max,9999,9999,others=NA)
#min_9999 <- subst(min,9999,9999,others=NA)

max_9999 <- ifel(max==9999,9999,NA)
min_9999 <- ifel(min==9999,9999,NA)

maxmin_c <- cover(max_9999,min_9999)
rm(max_9999)
rm(min_9999)

# Calculate YearsUntillLongUnburnt (YULU)

# Max-TSL calculation
log_it("Calculating max/min2x")
max_tsl <- max - tsl
min2x <- (2 * min)-(tsl+(max-min))

log_it("Classifying 1")
# Matrix to recode heritage 1
rcl_1 <- matrix(c(1,2,3,4,5,9,NA,NA,1,1,1,NA),ncol=2)
isolate_1 <- classify(hts,rcl_1)

log_it("Classifying 2")
# Matrix to recode heritage 2
rcl_2 <- matrix(c(1,2,3,4,5,9,NA,1,NA,NA,NA,NA),ncol=2)
isolate_2 <- classify(hts,rcl_2)


log_it("Calculating HTS overlays")
hts_1 <- isolate_1 * max_tsl
hts_2 <- isolate_2 * min2x
rm(isolate_1)
rm(isolate_2)
rm(max_tsl)
rm(min2x)
gc()

log_it("Calculating YULU Cover")
hts_YULU = cover(hts_1,hts_2)
rm(hts_1)
rm(hts_2)

gc()
log_it("Calculating YULU Cover 9999")
hts_YULU <- cover(maxmin_c,hts_YULU)

log_it("Write YULU Raster")
writeRaster(hts_YULU,paste0(rast_temp,"/r_YULU.tif"),overwrite=TRUE)
#rm(hts_YULU)
gc()

log_it("Calculating min/min2x")
# Calcualte YearsUntilWithinThreshold
min_tsl <- min - tsl
min2x <- (2 * min)-tsl
log_it("Classifying 1")
# Matrix to recode heritage 1
rcl_1 <- matrix(c(1,2,3,4,5,9,NA,1,1,NA,NA,NA),ncol=2)
isolate_1 <- classify(hts,rcl_1)

log_it("Classifying 2")
# Matrix to recode heritage 2
rcl_2 <- matrix(c(1,2,3,4,5,9,NA,NA,NA,1,1,NA),ncol=2)
isolate_2 <- classify(hts,rcl_2)

log_it("Calculating HTS overlays")
hts_1 <- isolate_1 * min_tsl
hts_2 <- isolate_2 * min2x
rm(isolate_1)
rm(isolate_2)
rm(min2x)

log_it("Calculating YUWT Cover")
hts_YUWT = cover(hts_1,hts_2)
rm(hts_1)
rm(hts_2)


gc()
log_it("Calculating YUWT Cover 9999")
hts_YUWT <- cover(maxmin_c,hts_YUWT)

log_it("Write YUWT Raster")
writeRaster(hts_YUWT,paste0(rast_temp,"/r_YUWT.tif"),overwrite=TRUE)
#rm(hts_YUWT)
rm(maxmin_c)
gc()


# Vectorize
###
log_it("Vectorize YULU")
v_YULU = as.polygons(hts_YULU,trunc=TRUE,dissolve=TRUE)
v_YULU <- disagg(v_YULU)
v_YULU = st_as_sf(v_YULU)
names(v_YULU)[1]<-"YULU"

log_it("Write YULU Geopackage")
write_sf(v_YULU,paste0(rast_temp,"/v_YULU.gpkg"))

log_it("Write YULU Shapefile")
write_sf(v_YULU,paste0(rast_temp,"/v_YULU.shp"))
rm(v_YULU)
gc()

log_it("Vectorize YUWT")
v_YUWT= as.polygons(hts_YUWT,trunc=TRUE,dissolve=TRUE)
v_YUWT <- disagg(v_YUWT)
v_YUWT = st_as_sf(v_YUWT)
names(v_YUWT)[1]<-"YUWT"

log_it("Write YUWT Geopackage")
write_sf(v_YUWT,paste0(rast_temp,"/v_YUWT.gpkg"))

log_it("Write YUWT Shapefile")
write_sf(v_YUWT,paste0(rast_temp,"/v_YUWT.shp"))
### TO DO - LOGGING, SHAPEFILE

rm(v_YUWT)
gc()
log_it("Done YULU")

