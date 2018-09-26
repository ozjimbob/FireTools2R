# Vectorize and summarise vegetation raster

# Load vegetation raster
log_it("Loading vegetation biodiversity threshold output raster")
r_vegThreshold = raster(paste0(rast_temp,"/r_vegout.tif"))

log_it("Converting vegetation biodiversity threshold raster to polygons")

if(OS=="Windows"){
v_vegThreshold = polygonizer_win(r_vegThreshold,
                                 pypath="C:/OSGeo4W64/bin/gdal_polygonize.py")
}else{
  v_vegThreshold = polygonizer(r_vegThreshold)
}

v_vegThreshold = st_as_sf(v_vegThreshold)
st_crs(v_vegThreshold)=proj_crs

log_it("Dissolving biodiversity threshold polygons")
v_vegThreshold = v_vegThreshold %>% st_cast("MULTIPOLYGON") %>% group_by(DN) %>% summarise()




log_it("Repairing biodiversity threshold polygons")
v_vegThreshold = filter(v_vegThreshold,as.numeric(st_area(v_vegThreshold))>0)
v_vegThreshold = st_make_valid(v_vegThreshold) # repair invalid geometries

log_it("Clipping to region of interest")
v_vegThreshold = st_intersection(v_vegThreshold,v_thisregion)


t_threshold=tibble(DN=c(1,2,3,4,5,9,NA),
                   BioStatus = c("NoFireRegime",
                                 "TooFrequentlyBurnt",
                                 "Vulnerable",
                                 "LongUnburnt",
                                 "WithinThreshold",
                                 "Unknown",
                                 NA))

log_it("Joining threshold labels to polygons")
v_vegThreshold = left_join(v_vegThreshold,t_threshold)
v_vegThreshold$DN = NULL

log_it("Saving veg threshold polygons")
write_sf(v_vegThreshold,paste0(rast_temp,"/v_vegout.gpkg"))

r_vegThreshold <- NULL
rm(r_vegThreshold)
gc()