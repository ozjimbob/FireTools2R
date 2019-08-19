# Render maps
library(tidyverse)
library(sf)
library(raster)
library(tmap)
library(tmaptools)
library(htmlwidgets)

source("../config/global_config.r")
source("../config/config_linux.r")
source("fire_cell_function.r")

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



rx_write=function(file,outfile){
  require(foreign)
  require(sp)
  require(raster)
  rtable = data.frame(ID = c(1,2,3,4,5,6,7,8,9),
                      Status = c("NoFireRegime",
                                 "TooFrequentlyBurnt",
                                 "Vulnerable",
                                 "LongUnburnt",
                                 "WithinThreshold",
                                 "Recently Treated",
                                 "Monitor OFH In the Field",
                                 "Priority for Assessment and Treatment",
                                 "Unknown"))
  col_vec = c("#ffffff",
              "#ffffff","#ff0000","#ff6600","#00ffff","#999999","#99FF99","#226622","#00ff00","#cccccc")
  
  tr <- raster(paste0(rast_temp,"/",file))
  tr <- ratify(tr)
  rat <- levels(tr)[[1]]
  rat <- left_join(rat,rtable)
  levels(tr) <- rat
  colortable(tr) <- col_vec
  
  # Write ESRI DB
  
  atable = levels(tr)[[1]]
  names(atable)=c("VALUE","CATEGORY")
  x = as.data.frame(table(raster::values(tr)))
  names(x)=c("VALUE","COUNT")
  x$VALUE = as.numeric(as.character(x$VALUE))
  a2 = left_join(atable,x)
  a2$COUNT[is.na(a2$COUNT)]=0
  a2 = dplyr::select(a2,VALUE,COUNT,CATEGORY)
  write.dbf(a2,paste0(rast_temp,"/",outfile,".vat.dbf"))
  
  # Fix projection
  
  crs(tr) <- CRS('+init=epsg:3308')
  bigWrite(tr,paste0(rast_temp,"/",outfile))
  unlink(paste0(rast_temp,"/",file))
  
}

# Insert 3308 projection into TIF output
esri_output = function(tfile){
  log_it("Generating ESRI projection")
  infile = paste0(rast_temp,"/",tfile)
  tempfile = paste0(rast_temp,"/",tfile,".tmp")
  gt = Sys.which("gdal_translate")
  cmd=paste0(gt," ",infile," -a_srs 3308.prj -co COMPRESS=LZW ",tempfile)
  cout = system(cmd,intern=TRUE)
  log_it(cout)
  unlink(infile)
  file.rename(tempfile,infile)
}


log_it("Writing heritage raster table")
rx_write("r_vegout.tif","r_heritage_threshold_status.tif")
esri_output("r_heritage_threshold_status.tif")

log_it("Writing fmz raster table")
rx_write("r_fmzout.tif","r_fmz_threshold_status.tif")
esri_output("r_fmz_threshold_status.tif")

log_it("Writing Heritage plus FMZ raster table")
rx_write("r_fmz_bio_out.tif","r_heritage_fmz_threshold_status.tif")
esri_output("r_heritage_fmz_threshold_status.tif")

log_it("Writing FMZ plus SFAZ raster table")
rx_write("r_sfaz_fmz_out.tif","r_fmz_sfaz_threshold_status.tif")
esri_output("r_fmz_sfaz_threshold_status.tif")

log_it("Writing combined raster table")
rx_write("r_sfaz_fmz_bio_out.tif","r_heritage_fmz_sfaz_threshold_status.tif")
esri_output("r_heritage_fmz_sfaz_threshold_status.tif")

# Add table to vegetation map
log_it("Adding labels to vegetation raster")

log_it("Loading vegetation raster and vector")
vegbase = read_sf(paste0(rast_temp,"/v_vegBase.gpkg"))
vegcode = raster(paste0(rast_temp,"/r_vegcode.tif"))


log_it("Generating code table")
codelist = tibble(ID=unique(vegbase[[f_vegid]]))
codelist$category = ""
for(i in seq_along(codelist$ID)){
  thisveg = filter(vegbase,!!rlang::sym(f_vegid)==codelist$ID[i])
  codelist$category[i] = thisveg$VEGTEXT[1]
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



log_it("Renaming files")

file.rename(paste0(rast_temp,"/rLastYearBurnt.tif"),paste0(rast_temp,"/r_LastYearBurnt.tif"))
esri_output("r_LastYearBurnt.tif")
file.rename(paste0(rast_temp,"/rNumTimesBurnt.tif"),paste0(rast_temp,"/r_NumTimesBurnt.tif"))
esri_output("r_NumTimesBurnt.tif")
file.rename(paste0(rast_temp,"/rTimeSinceLast.tif"),paste0(rast_temp,"/r_TimeSinceLast.tif"))
esri_output("r_TimeSinceLast.tif")

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
in_file = read_sf(paste0(rast_temp,"/v_fmz_sfaz_threshold_status.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_fmz_sfaz_threshold_status.shp"))

in_file = read_sf(paste0(rast_temp,"/v_fmz_threshold_status.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_fmz_threshold_status.shp"))

in_file = read_sf(paste0(rast_temp,"/v_heritage_fmz_threshold_status.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_heritage_fmz_threshold_status.shp"))

in_file = read_sf(paste0(rast_temp,"/v_heritage_threshold_status.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_heritage_threshold_status.shp"))

in_file = read_sf(paste0(rast_temp,"/v_heritage_fmz_sfaz_threshold_status.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_heritage_fmz_sfaz_threshold_status.shp"))

in_file = read_sf(paste0(rast_temp,"/v_region.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_region.shp"))

in_file = read_sf(paste0(rast_temp,"/v_tsl.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_tsl.shp"))

in_file = read_sf(paste0(rast_temp,"/v_timesburnt.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_timesburnt.shp"))

in_file = read_sf(paste0(rast_temp,"/v_tsl_sfaz.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_tsl_sfaz.shp"))

in_file = read_sf(paste0(rast_temp,"/v_sfaz_candidate_blocks.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_sfaz_candidate_blocks.shp"))

in_file = read_sf(paste0(rast_temp,"/v_vegBase.gpkg"))
write_sf(in_file,paste0(rast_temp,"/v_vegBase.shp"))