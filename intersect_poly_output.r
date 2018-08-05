# Intersect final polygon output layer and clean up

v_veg = read_sf(paste0(rast_temp,"/v_vegBase.gpkg"))
v_vegThreshold = read_sf(paste0(rast_temp,"/v_vegout.gpkg"))

v_veg_vegthreshold = st_or(v_veg,v_vegThreshold)

v_tsl = read_sf(paste0(rast_temp,"/v_tsl.gpkg"))
v_veg_vegthreshold = st_or(v_veg_vegthreshold,v_tsl)

v_fmz = read_sf(paste0(rast_temp,"/v_fmzout.gpkg"))
v_veg_vegthreshold = st_or(v_veg_vegthreshold,v_fmz)
