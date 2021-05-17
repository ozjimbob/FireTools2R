# Vectorize and summarise vegetation raster

# Load vegetation raster
log_it("Loading fire management zone threshold output raster")



log_it("Converting fire management zone threshold raster to polygons")
if(OS=="Windows"){
  r_fmzout = raster(paste0(rast_temp,"/r_fmzout.tif"))
  v_fmzout = polygonizer_win(r_fmzout)
}else{
  v_fmzout = polygonize_by_name(paste0(rast_temp,"/r_fmzout.tif"))
  
}

v_fmzout = st_as_sf(v_fmzout)
st_crs(v_fmzout)=proj_crs

log_it("Dissolving fire management zone threshold polygons")
v_fmzout = v_fmzout %>% st_cast("MULTIPOLYGON")# %>% group_by(DN) %>% summarise()




log_it("Repairing biodiversity threshold polygons")
v_fmzout = filter(v_fmzout,as.numeric(st_area(v_fmzout))>0)

#v_fmzout = st_make_valid(v_fmzout)


log_it("Clipping to region of interest")
v_thisregion = read_sf(paste0(rast_temp,"/v_region.gpkg"))
v_fmzout = st_buffer(v_fmzout,0)
v_fmzout = st_make_valid(v_fmzout)
v_thisregion = st_buffer(v_thisregion,0)

v_fmzout = st_intersection(v_fmzout,v_thisregion)


t_threshold=tibble(DN=c(1,2,3,4,5,9,NA),
                   FMZStatus = c("NoFireRegime",
                                 "TooFrequentlyBurnt",
                                 "Vulnerable",
                                 "LongUnburnt",
                                 "WithinThreshold",
                                 "Unknown",NA))

log_it("Joining threshold labels to polygons")
v_fmzout = left_join(v_fmzout,t_threshold)
v_fmzout$DN = NULL


log_it("Saving fire management zone threshold polygons")
write_sf(v_fmzout,paste0(rast_temp,"/v_fmzout.gpkg"))
log_it("Fire management zones thresholds saved. Cleaning up")

v_fmzout <- NULL
tmprast <- NULL
r_fmzout <- NULL
rm(r_fmzout)
rm(v_fmzout)
rm(tmprast)
v_vegThreshold <- NULL
rm(v_vegThreshold)
gc()


