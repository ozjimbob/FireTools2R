# Render maps
library(tidyverse)
library(sf)
library(raster)
library(tmap)
library(tmaptools)
library(htmlwidgets)

#source("../config/global_config.r")
#source("../config/config_linux.r")
#source("fire_cell_function.r")

# Get list of years
yl <- read_csv(paste0(rast_temp, "/yearlist.csv"))
log_it("Deleting individual fire year rasters")

# Delete yearly TIFs
for (this_year in min(year_list):max(year_list)) {
  unlink(paste0(rast_temp, "/", this_year, ".tif"))
  unlink(paste0(rast_temp, "/rLastYearBurnt_", this_year, ".tif"))
  unlink(paste0(rast_temp, "/rNumTimesBurnt_", this_year, ".tif"))
  unlink(paste0(rast_temp, "/rTimeSinceLast_", this_year, ".tif"))
}

# Delete yearlist
log_it("Deleting ancillary files")
unlink(paste0(rast_temp, "/yearlist.csv"))

log_it("Writing Manifest")

file.copy("manifest.txt", paste0(rast_temp, "/MANIFEST.txt"))

log_it("Writing raster tables")


mask_tif <- raster(paste0(rast_temp, "/roi_mask.tif"))
mask_tif_t <- rast(paste0(rast_temp, "/roi_mask.tif"))
rm(v_sfaz)
gc()
rm(tm)
rm(stex)
rm(nsw_bg)
rm(v)


rm(v_thisregion)
gc()

endCluster()


log_it("Writing heritage raster table")
rx_write("r_vegout.tif", "r_heritage_threshold_status.tif")
esri_output("r_heritage_threshold_status.tif")
gc()


log_it("Renaming and masking files")

file.rename(
  paste0(rast_temp, "/rLastYearBurnt.tif"),
  paste0(rast_temp, "/r_LastYearBurnt.tif")
)
temp_d = rast(paste0(rast_temp, "/r_LastYearBurnt.tif"))
temp_d = temp_d * mask_tif_t
terra::writeRaster(temp_d, paste0(rast_temp, "/r_LastYearBurnt.tif"), overwrite =
                     TRUE)
esri_output("r_LastYearBurnt.tif")
rm(temp_d)
gc()


file.rename(
  paste0(rast_temp, "/rNumTimesBurnt.tif"),
  paste0(rast_temp, "/r_NumTimesBurnt.tif")
)
temp_d = rast(paste0(rast_temp, "/r_NumTimesBurnt.tif"))
temp_d = temp_d * mask_tif_t
terra::writeRaster(temp_d, paste0(rast_temp, "/r_NumTimesBurnt.tif"), overwrite =
                     TRUE)
esri_output("r_NumTimesBurnt.tif")
rm(temp_d)
gc()

file.rename(
  paste0(rast_temp, "/rTimeSinceLast.tif"),
  paste0(rast_temp, "/r_TimeSinceLast.tif")
)
temp_d = rast(paste0(rast_temp, "/r_TimeSinceLast.tif"))
temp_d = temp_d * mask_tif_t
terra::writeRaster(temp_d, paste0(rast_temp, "/r_TimeSinceLast.tif"), overwrite =
                     TRUE)
esri_output("r_TimeSinceLast.tif")
rm(temp_d)
gc()


file.rename(
  paste0(rast_temp, "/v_vegout.gpkg"),
  paste0(rast_temp, "/v_heritage_threshold_status.gpkg")
)

log_it("Setting Proj File")
proj_file = "3308.prj"
if(proj_crs=="+proj=utm +zone=57 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"){
  proj_file = "28357.prj"
}


log_it("Writing Shapefile version")


log_it("Writing v_heritage_threshold_status")
try({
  er = try({
  in_file = vect(paste0(rast_temp, "/v_heritage_threshold_status.gpkg"))
  })
  if(class(er)=="try-error"){
    in_file = vect(read_sf(paste0(rast_temp, "/v_heritage_threshold_status.gpkg")))
  }
  writeVector(in_file,
              paste0(rast_temp, "/v_heritage_threshold_status.shp"),
              overwrite = TRUE)
  file.copy(proj_file,
            paste0(rast_temp, "/v_heritage_threshold_status.prj"),
            overwrite = TRUE)
})




