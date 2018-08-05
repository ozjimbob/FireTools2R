# Generage merged fmz and biodiversity thresholds
library(tidyverse)
library(sf)
library(velox)
library(raster)
library(spatial.tools)
library(foreach)
library(doParallel)


log_it("Loading biodiversity threshold raster")
r_vegout = raster(paste0(rast_temp,"/r_vegout.tif"))
log_it("Loading fire management zone threshold raster")
r_fmzout = raster(paste0(rast_temp,"/r_fmzout.tif"))

log_it("Overlaying Rasters")
beginCluster(clustNo)

c_func = function(x,y){ifelse(is.na(x),y,x)}
s = stack(r_fmzout,r_vegout)
r_comb = clusterR(s,overlay,args=list(fun=c_func))
s <- NULL
rm(s)
gc()

#r_comb = overlay(r_fmzout,r_vegout,fun=function(x,y){ifelse(is.na(x),y,x)})
endCluster()

log_it("Writing combined biodiversity and fire management zone threshold raster")
bigWrite(r_comb,paste0(rast_temp,"/r_fmz_bio_out.tif"))


log_it("Converting combined biodiversity and fire management zone threshold raster to polygons")
if(OS=="Windows"){
v_fmzbio = polygonizer_win(r_comb,
                                 pypath="C:/OSGeo4W64/bin/gdal_polygonize.py",
                                 quietish = FALSE)
}else{
v_fmzbio = polygonizer(r_comb, quietish = FALSE)
}

v_fmzbio = st_as_sf(v_fmzbio)
st_crs(v_fmzbio)=proj_crs

log_it("Dissolving combined biodiversity and fire management zone threshold polygons")
v_fmzbio = v_fmzbio %>% st_cast("MULTIPOLYGON") %>% group_by(DN) %>% summarise()




log_it("Repairing combined biodiversity and fire management zone threshold polygons")
v_fmzbio = filter(v_fmzbio,as.numeric(st_area(v_fmzbio))>0)
v_fmzbio = st_make_valid(v_fmzbio) # repair invalid geometries

log_it("Clipping to region of interest")
v_thisregion = read_sf(paste0(rast_temp,"/v_region.gpkg"))
v_fmzbio = st_intersection(v_fmzbio,v_thisregion)


t_threshold=tibble(DN=c(1,2,3,4,5,9,NA),
                   BioStatus = c("NoFireRegime",
                                 "TooFrequentlyBurnt",
                                 "Vulnerable",
                                 "LongUnburnt",
                                 "WithinThreshold",
                                 "Unknown",NA))

log_it("Joining threshold labels to polygons")
v_fmzbio = left_join(v_fmzbio,t_threshold)
v_fmzbio$DN = NULL

log_it("Saving combined biodiversity and fire management zone threshold polygons")
write_sf(v_fmzbio,paste0(rast_temp,"/v_fmz_bio_out.gpkg"))



