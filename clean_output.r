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
yl <- read_csv(paste0(rast_temp,"/yearlist.csv"))
log_it("Deleting individual fire year rasters")

# Delete yearly TIFs
for(this_year in year_list){
  unlink(paste0(rast_temp,"/",this_year,".tif"))
}

# Delete yearlist
log_it("Deleting ancillary files")
unlink(paste0(rast_temp,"/yearlist.csv"))

log_it("Writing Manifest")

file.copy("manifest.txt",paste0(rast_temp,"/MANIFEST.txt"))

log_it("Writing raster tables")


mask_tif<-raster(paste0(rast_temp,"/roi_mask.tif"))

rm(v_sfaz)
gc()
rm(tm)
rm(stex)
rm(nsw_bg)
rm(v)
rm(v_tsl)
rm(any_fire)
rm(v_thisregion)
gc()

endCluster()


log_it("Writing heritage raster table")
rx_write("r_vegout.tif","r_heritage_threshold_status.tif")
esri_output("r_heritage_threshold_status.tif")
gc()

log_it("Writing fmz raster table")
rx_write("r_fmzout.tif","r_fmz_threshold_status.tif")
esri_output("r_fmz_threshold_status.tif")
gc()

log_it("Writing Heritage plus FMZ raster table")
rx_write("r_fmz_bio_out.tif","r_heritage_fmz_threshold_status.tif")
esri_output("r_heritage_fmz_threshold_status.tif")
gc()

log_it("Writing FMZ plus SFAZ raster table")
rx_write("r_sfaz_fmz_out.tif","r_fmz_sfaz_threshold_status.tif")
esri_output("r_fmz_sfaz_threshold_status.tif")
gc()

log_it("Writing combined raster table")
rx_write("r_sfaz_fmz_bio_out.tif","r_heritage_fmz_sfaz_threshold_status.tif")
esri_output("r_heritage_fmz_sfaz_threshold_status.tif")
gc()
# Add table to vegetation map
log_it("Adding labels to vegetation raster")

log_it("Loading vegetation raster and vector")
vegbase = read_sf(paste0(rast_temp,"/v_vegBase.gpkg"))
vegcode = raster(paste0(rast_temp,"/r_vegcode.tif"))
vegcode = vegcode * mask_tif

log_it("Generating code table")
codelist = tibble(ID=unique(vegbase[[f_vegid]]))
codelist$category = ""
vt = which(substr(names(vegbase),1,7)=="VEGTEXT")[1]
for(i in seq_along(codelist$ID)){
  thisveg = filter(vegbase,!!rlang::sym(f_vegid)==codelist$ID[i])
  codelist$category[i] = as.character(thisveg[1,vt])[1]
}

codelist = as.data.frame(codelist)



tr <- ratify(vegcode)

rat <- levels(tr)[[1]]

rat <- left_join(rat,codelist)
rat$category[is.na(rat$category)]=""

log_it("Generating colour table")
col_vec = colorRampPalette(c("white","brown","green","red","blue","yellow","pink","purple"))(nrow(rat))

log_it("Assigning colours and names")
ids = as.numeric(rownames(rat))
codes = rat$ID

tr = reclassify(tr,cbind(codes,ids))
rat$ID = ids

levels(tr) <- rat

require(foreign)
require(sp)
atable = levels(tr)[[1]]
names(atable)=c("VALUE","CATEGORY")
x = as.data.frame(table(raster::values(tr)))
names(x)=c("VALUE","COUNT")
x$VALUE = as.numeric(as.character(x$VALUE))
a2 = left_join(atable,x)
a2$COUNT[is.na(a2$COUNT)]=0
a2 = dplyr::select(a2,VALUE,COUNT,CATEGORY)
write.dbf(a2,paste0(rast_temp,"/r_vegcode.tif.vat.dbf"))


col_vec[1]="#ffffff"
col_vec=c("#ffffff",col_vec)
colortable(tr) <- col_vec

log_it("Writing file")
bigWrite(tr,paste0(rast_temp,"/r_vegcode2.tif"))
esri_output("r_vegcode2.tif")

log_it("Removing old files")
unlink(paste0(rast_temp,"/r_vegcode.tif"))
unlink(paste0(rast_temp,"/r_vegcode.tif.aux.xml"))
file.rename(paste0(rast_temp,"/r_vegcode2.tif"),paste0(rast_temp,"/r_vegcode.tif"))
file.rename(paste0(rast_temp,"/r_vegcode2.tif.aux.xml"),paste0(rast_temp,"/r_vegcode.tif.aux.xml"))

rm(vegbase)
rm(tr)
rm(vegcode)
gc()


log_it("Renaming and masking files")

file.rename(paste0(rast_temp,"/rLastYearBurnt.tif"),paste0(rast_temp,"/r_LastYearBurnt.tif"))
temp_d = raster(paste0(rast_temp,"/r_LastYearBurnt.tif"))
temp_d = temp_d * mask_tif
bigWrite(temp_d,paste0(rast_temp,"/r_LastYearBurnt.tif"))
esri_output("r_LastYearBurnt.tif")
rm(temp_d)
gc()


file.rename(paste0(rast_temp,"/rNumTimesBurnt.tif"),paste0(rast_temp,"/r_NumTimesBurnt.tif"))
temp_d = raster(paste0(rast_temp,"/r_NumTimesBurnt.tif"))
temp_d = temp_d * mask_tif
bigWrite(temp_d,paste0(rast_temp,"/r_NumTimesBurnt.tif"))
esri_output("r_NumTimesBurnt.tif")
rm(temp_d)
gc()

file.rename(paste0(rast_temp,"/rTimeSinceLast.tif"),paste0(rast_temp,"/r_TimeSinceLast.tif"))
temp_d = raster(paste0(rast_temp,"/r_TimeSinceLast.tif"))
temp_d = temp_d * mask_tif
bigWrite(temp_d,paste0(rast_temp,"/r_TimeSinceLast.tif"))
esri_output("r_TimeSinceLast.tif")
rm(temp_d)
gc()


file.rename(paste0(rast_temp,"/v_vegout.gpkg"),paste0(rast_temp,"/v_heritage_threshold_status.gpkg"))
file.rename(paste0(rast_temp,"/v_fmzout.gpkg"),paste0(rast_temp,"/v_fmz_threshold_status.gpkg"))
file.rename(paste0(rast_temp,"/v_fmz_bio_out.gpkg"),paste0(rast_temp,"/v_heritage_fmz_threshold_status.gpkg"))
file.rename(paste0(rast_temp,"/v_sfaz_fmz_out.gpkg"),paste0(rast_temp,"/v_fmz_sfaz_threshold_status.gpkg"))
file.rename(paste0(rast_temp,"/v_sfaz_fmz_bio_out.gpkg"),paste0(rast_temp,"/v_heritage_fmz_sfaz_threshold_status.gpkg"))

## Write tiles


if(OS=="Windows"){
  log_it("Rendering Tiles")
  tile_win("r_heritage_fmz_sfaz_threshold_status")
  tile_win("r_heritage_threshold_status")
  tile_win("r_fmz_threshold_status")
  tile_win("r_heritage_fmz_threshold_status")
  tile_win("r_fmz_sfaz_threshold_status")
}else{
  log_it("Rendering Tiles")
  tile_linux("r_heritage_fmz_sfaz_threshold_status")
  tile_linux("r_heritage_threshold_status")
  tile_linux("r_fmz_threshold_status")
  tile_linux("r_heritage_fmz_threshold_status")
  tile_linux("r_fmz_sfaz_threshold_status")
}

log_it("Writing Shapefile version")

## Shapefile test
log_it("Writing v_fmz_sfaz_threshold_status")
in_file = read_sf(paste0(rast_temp,"/v_fmz_sfaz_threshold_status.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_fmz_sfaz_threshold_status.shp"))
file.copy("3308.prj",paste0(rast_temp,"/v_fmz_sfaz_threshold_status.prj"),overwrite=TRUE)

log_it("Writing v_fmz_threshold_status")
in_file = read_sf(paste0(rast_temp,"/v_fmz_threshold_status.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_fmz_threshold_status.shp"))
file.copy("3308.prj",paste0(rast_temp,"/v_fmz_threshold_status.prj"),overwrite=TRUE)

log_it("Writing v_heritage_fmz_threshold_status")
in_file = read_sf(paste0(rast_temp,"/v_heritage_fmz_threshold_status.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_heritage_fmz_threshold_status.shp"))
file.copy("3308.prj",paste0(rast_temp,"/v_heritage_fmz_threshold_status.prj"),overwrite=TRUE)

log_it("Writing v_heritage_threshold_status")
in_file = read_sf(paste0(rast_temp,"/v_heritage_threshold_status.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_heritage_threshold_status.shp"))
file.copy("3308.prj",paste0(rast_temp,"/v_heritage_threshold_status.prj"),overwrite=TRUE)

log_it("Writing v_heritage_fmz_sfaz_threshold_status.shp")
in_file = read_sf(paste0(rast_temp,"/v_heritage_fmz_sfaz_threshold_status.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_heritage_fmz_sfaz_threshold_status.shp"))
file.copy("3308.prj",paste0(rast_temp,"/v_heritage_fmz_sfaz_threshold_status.prj"),overwrite=TRUE)

log_it("Writing v_region")
in_file = read_sf(paste0(rast_temp,"/v_region.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_region.shp"))
file.copy("3308.prj",paste0(rast_temp,"/v_region.prj"),overwrite=TRUE)

log_it("Writing v_tsl")
in_file = read_sf(paste0(rast_temp,"/v_tsl.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_tsl.shp"))
file.copy("3308.prj",paste0(rast_temp,"/v_tsl.prj"),overwrite=TRUE)

log_it("Writing v_timesburnt.shp")
in_file = read_sf(paste0(rast_temp,"/v_timesburnt.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_timesburnt.shp"))
file.copy("3308.prj",paste0(rast_temp,"/v_timesburnt.prj"),overwrite=TRUE)

log_it("Writing v_tsl_sfaz")
in_file = read_sf(paste0(rast_temp,"/v_tsl_sfaz.gpkg"))
in_file = dplyr::select(in_file,TSL,SFAZStatus,SFAZStatusText )
write_sf(in_file,paste0(rast_temp,"/v_tsl_sfaz.shp"),delete_dsn=TRUE,delete_layer=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_tsl_sfaz.prj"),overwrite=TRUE)

log_it("Writing v_sfaz_candidate_blocks")
in_file = read_sf(paste0(rast_temp,"/v_sfaz_candidate_blocks.gpkg"))
in_file = dplyr::select(in_file,TSL,SFAZStatus,SFAZStatusText )
write_sf(in_file,paste0(rast_temp,"/v_sfaz_candidate_blocks.shp"),delete_dsn=TRUE,delete_layer=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_sfaz_candidate_blocks.prj"),overwrite=TRUE)

log_it("Writing v_vegBase")
in_file = read_sf(paste0(rast_temp,"/v_vegBase.gpkg"))
nl=names(in_file)[1:(length(names(in_file))-1)]
nl = substr(nl,1,6)
short = which(nchar(nl)<6)
nl[-short] = paste0(nl[-short],1:length(nl[-short]))
names(in_file)[1:(length(names(in_file))-1)]=nl
write_sf(in_file,paste0(rast_temp,"/v_vegBase.shp"),delete_dsn=TRUE,delete_layer=TRUE)
file.copy("3308.prj",paste0(rast_temp,"/v_vegBase.prj"),overwrite=TRUE)

