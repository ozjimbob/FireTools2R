#### Merge tables for Nav
library(tidyverse)
library(terra)

t1 <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/cent_t.csv")
t1 <- select(t1,PCTID,PCTName,vegetationFormation,vegetationClass)
names(t1)<-c("PCTID","PCTName","Formation","Class")
t1 <- distinct(t1)

t2 <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/cent_west.csv")
t2 <- select(t2,PCTID,PCTName,Formation,Class)
t2 <- distinct(t2)

t3 <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/east.csv")
t3 <- select(t3,PCTID,PCTName,vegForm,vegClass)
names(t3)<-c("PCTID","PCTName","Formation","Class")
t3 <- distinct(t3)

t4 <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/nbrg.csv") # Border Rivers
t4 <- select(t4,BRGN_PCTv2,PCTname,KeithForm,KeithClass)
names(t4)<-c("PCTID","PCTName","Formation","Class")
t4 <- distinct(t4)

t5 <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/riverina.csv")
t5 <- select(t5,PCTID,PCTName,vegetationFormation,vegetationClass)
names(t5)<-c("PCTID","PCTName","Formation","Class")
t5 <- distinct(t5)

t6 <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/sydney.csv")
t6 <- select(t6,PCTID,PCTName,StateForm,StateClass)
names(t6)<-c("PCTID","PCTName","Formation","Class")
t6 <- distinct(t6)

t7 <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/up_hunt.txt")
t7 <- select(t7,PCTID,PCTName,vegetationFormation,vegetationClass)
names(t7)<-c("PCTID","PCTName","Formation","Class")
t7 <- distinct(t7)

t8 <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/west.txt")
t8 <- select(t8,PCTID,PCTName,vegetationFormation,vegetationClass)
names(t8)<-c("PCTID","PCTName","Formation","Class")
t8 <- distinct(t8)

#names(t4)=c("PCTID","PCTName")

tall <- bind_rows(t1,t2,t3,t4,t5,t6,t7,t8)
tall_d = distinct(tall)
tall_d <- arrange(tall_d,PCTID)
write_csv(tall_d,"R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/all_PCT.csv")





tbl <- vect("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/gpkg/tablelands.gpkg")
r_vfp = terra::rasterize(tbl,tmprast,field="PCTID",filename=paste0(rast_temp,"/r_tabl_PCT.tif"))

rm(tbl)
gc()


tbl <- vect("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/gpkg/cent_west.gpkg")
r_vfp = terra::rasterize(tbl,tmprast,field="PCTID",filename=paste0(rast_temp,"/r_cwest_PCTID.tif"))

rm(tbl)
gc()

tbl <- vect("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/gpkg/riverina.gpkg")
r_vfp = terra::rasterize(tbl,tmprast,field="PCTID",filename=paste0(rast_temp,"/r_riverina_PCTID.tif"))




#rm(tbl)
#gc()
#library(terra)
#rast_temp="G:/ft_work/statewide_veg_parks"
#library(sf)
#tbl <- read_sf("G:/ft_work/statewide_veg_parks/west.gpkg")
#tbl <- st_cast(tbl,"MULTIPOLYGON")
#write_sf(tbl,"G:/ft_work/statewide_veg_parks/west2.gpkg")
#rm(tbl)
#gc()

tbl <- read_sf("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/gpkg/west.gpkg")
tbl <- st_cast(tbl,"MULTIPOLYGON")
write_sf(tbl,"R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/gpkg/west2.gpkg")
rm(tbl)
tbl <- vect("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/gpkg/west2.gpkg")

exr_vfp = terra::rasterize(tbl,tmprast,field="PCTID",filename=paste0(rast_temp,"/r_west_PCTID.tif"))
rm(tbl)
gc()

### Now try others

tbl <- vect("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/gpkg/bord_riv.gpkg")
r_vfp = terra::rasterize(tbl,tmprast,field="BRGN_PCTv2",filename=paste0(rast_temp,"/r_bord_riv_PCTID.tif"))
rm(tbl)
gc()



tbl <- vect("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/gpkg/upper_hunt.gpkg")
r_vfp = terra::rasterize(tbl,tmprast,field="PCTID",filename=paste0(rast_temp,"/r_upper_hunt_PCTID.tif"))
rm(tbl)
gc()

tbl <- vect("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/gpkg/syd.gpkg")
r_vfp = terra::rasterize(tbl,tmprast,field="PCTID",filename=paste0(rast_temp,"/r_syd_PCTID.tif"))

rm(tbl)
gc()



### raster cover


Sys.time()
max_list <-c("/r_syd_PCTID.tif","/r_upper_hunt_PCTID.tif","/r_bord_riv_PCTID.tif","/r_west_PCTID.tif","/r_riverina_PCTID.tif","/r_cwest_PCTID.tif","/r_tabl_PCT.tif")
max_list <- paste0(rast_temp,max_list)
max_files <- rast(max_list)
max_files <- max(max_files,na.rm=TRUE)
writeRaster(max_files,paste0(rast_temp,"/rast_done/sw_PCTID.tif"))
Sys.time()

### NOW EAST



rf=foreign::read.dbf("G:/ft_work/statewide_veg_parks/SVTM_ENSW_PCT_VIS5111_P_20220207/SVTM_ENSW_PCT_VIS5111_P_20220207.tif.vat.dbf")
## VALUE / vegClass


rf <- dplyr::select(rf,Value,PCTID)
st_write(rf,"G:/ft_work/statewide_veg_parks/east_test.gpkg",layer="LUT2")

east <- rast("G:/ft_work/statewide_veg_parks/SVTM_ENSW_PCT_VIS5111_P_20220207/east_dn.tif")
#new_lut <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/PCT_Thresholds.csv")
east <- resample(east, tmprast,method="near")
writeRaster(east,paste0(rast_temp,"/rast_done/east_PCTID.tif"))

#vr = vect()
#v_veg = vect("G:/ft_work/statewide_veg_parks/east_test.gpkg",layer="east_test",
#             query = paste0("select east_test.*, LUT2.* from east_test left join LUT on east_test.DN = LUT2.VALUE"))




## Now we have joined the formation name, we need to join the max/min as before
#LUT <- read_sf(veg_gdb,layer="LUT")
#names(LUT)[10]="vegForm"
#LUT <- dplyr::select(LUT,vegForm,MIN,MAX,VEG)

#v_veg <- merge(v_veg,LUT,by.x="vegForm",by.y="vegForm")

## TRY AGAIN!
#v_veg = read_sf("G:/ft_work/statewide_veg_parks/east_test.gpkg",layer="east_test",
#             query = paste0("select east_test.*, LUT.* from east_test left join LUT on east_test.DN = LUT.VALUE"))
#v_veg$DN <- NULL
#write_sf(v_veg,"G:/ft_work/statewide_veg_parks/east_test_joined.gpkg")

#v_veg <- left_join(v_veg,LUT)

## TRY AGAIN

#tbl <- vect("G:/ft_work/statewide_veg_parks/east_test_joined_LUT.gpkg")
#names(tbl)=c("Value","VegForm","MIN","MAX","VEG")

#r_vfp = terra::rasterize(tbl,tmprast,field="VEG",filename=paste0(rast_temp,"/r_east_vegcode.tif"))
#r_vfp = terra::rasterize(tbl,tmprast,field="MAX",filename=paste0(rast_temp,"/r_east_max.tif"))
#r_vfp = terra::rasterize(tbl,tmprast,field="MIN",filename=paste0(rast_temp,"/r_east_min.tif"))
#rm(tbl)
#gc()

#layer = "east_test_joined_LUT"
#filename = "G:/ft_work/statewide_veg_parks/east_test_joined_LUT.gpkg"
#output = paste0(rast_temp,"/r_east_vegcode.tif")
#attribute = "VEG"
#system(g_rasterize_raw(layer,filename,output,attribute))

#attribute = "MAX"
#output = paste0(rast_temp,"/r_east_max.tif")
#system(g_rasterize_raw(layer,filename,output,attribute))

#attribute = "MIN"
#output = paste0(rast_temp,"/r_east_min.tif")
#system(g_rasterize_raw(layer,filename,output,attribute))



#max_list <-c("/r_syd_vegcode.tif","/r_upper_hunt_vegcode.tif","/r_bord_riv_vegcode.tif","/r_west_vegcode.tif","/r_riverina_vegcode.tif","/r_cwest_vegcode.tif","/r_tabl_vegcode.tif")
#max_list <- paste0(rast_temp,max_list)
#max_files <- rast(max_list)
#max_files <- max(max_files,na.rm=TRUE)
#writeRaster(max_files,paste0(rast_temp,"/rast_done/sw_vegcode_01.tif"))

### COVER
east_max <- rast("G:/ft_work/statewide_veg_parks/rast_done/east_PCTID.tif")
west_max <- rast("G:/ft_work/statewide_veg_parks/rast_done/sw_PCTID.tif")
#west_max <- resample(west_max, tmprast,method="near")


#east_max[east_max==0]=NA
#west_max[west_max==0]=NA

cc = cover(east_max,west_max,filename="G:/ft_work/statewide_veg_parks/rast_done/allstate_PCTID_01.tif",overwrite=TRUE)

### recode
form_lut <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/PCT_Thresholds.csv")
form_lut <- dplyr::select(form_lut,PCTID,Form,FormID)

new_lut <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/All_PCT_Thresholds2.csv")
new_lut <- dplyr::select(new_lut,PCTID,Min,Max)

form_lut <- left_join(form_lut,new_lut) %>% distinct()

max_mat <- cbind(form_lut$PCTID,form_lut$Max)
min_mat <- cbind(form_lut$PCTID,form_lut$Min)
form_mat<- cbind(form_lut$PCTID,form_lut$FormID)

cc_max <- classify(cc,max_mat,othersNA=TRUE,filename="G:/ft_work/statewide_veg_parks/rast_done/allstate_MAX_01.tif",overwrite=TRUE)
cc_min <- classify(cc,min_mat,othersNA=TRUE,filename="G:/ft_work/statewide_veg_parks/rast_done/allstate_MIN_01.tif",overwrite=TRUE)
cc_form <- classify(cc,form_mat,othersNA=TRUE,filename="G:/ft_work/statewide_veg_parks/rast_done/allstate_FORM_01.tif",overwrite=TRUE)

### DO VEG FORM CODING
# Make simple formation lut
new_lut <- dplyr::select(new_lut,FormID,Form)

### REDO with giant SVTM at 5m
cc = rast("G:/ft_work/statewide_veg_parks/svtm/SVTM_NSW_Extant_PCT_5m.tif")
ofs = rast("G:/ft_work/statewide_veg_parks/rast_done/sw_PCTID.tif")
cc = resample(cc,ofs,method="mode")
writeRaster(cc,"G:/ft_work/statewide_veg_parks/rast_done/r_vegcode.tif",overwrite=TRUE)

cc <- rast("G:/ft_work/statewide_veg_parks/rast_done/r_vegcode.tif")
form_lut <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/PCT_Thresholds.csv")
form_lut <- dplyr::select(form_lut,PCTID,Form,FormID)

new_lut <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/All_PCT_Thresholds3.csv")
new_lut <- dplyr::select(new_lut,PCTID,MIN,MAX)

full_lut <- left_join(form_lut,new_lut)
write_csv(full_lut,"R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/veg_lut.csv")

form_lut <- full_lut %>% select(FormID,Form) %>% distinct()


write_csv(form_lut,"R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/tables/form_lut.csv")


max_mat <- cbind(full_lut$PCTID,full_lut$MAX)
min_mat <- cbind(full_lut$PCTID,full_lut$MIN)
form_mat<- cbind(full_lut$PCTID,full_lut$FormID)

cc_max <- classify(cc,max_mat,othersNA=TRUE,filename="G:/ft_work/statewide_veg_parks/rast_done/r_vegmax.tif",overwrite=TRUE)
cc_min <- classify(cc,min_mat,othersNA=TRUE,filename="G:/ft_work/statewide_veg_parks/rast_done/r_vegmin.tif",overwrite=TRUE)
cc_form <- classify(cc,form_mat,othersNA=TRUE,filename="G:/ft_work/statewide_veg_parks/rast_done/r_vegform.tif",overwrite=TRUE)

