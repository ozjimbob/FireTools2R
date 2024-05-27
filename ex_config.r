corp_gdb<-"R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/gpkg/branch.gpkg"
i_vt_boundary<-"branch"
f_spatial_unit<-"Branch"
d_spatial_unit<-""

i_vt_veg_lut<-"LUT"
#i_vt_veg_lut<-"lut"

f_vegid<-"VEG"
f_vegmax<-"MAX"
f_vegmin<-"MIN"
f_vegadv<-"ADV"
f_vegfireprone<-"FireProneV"

#veg_gdb<-"/mnt/scratch_lustre/FireTools/Inputs/ecohealth/veg.gpkg"
#lut_gdb<-"/mnt/scratch_lustre/FireTools/Inputs/ecohealth/lut.gpkg"
veg_gdb<-"R:/SET/PlantSci/ForestEcology/FireHub/Inputs/ParksVeg/all_veg_2021.gpkg"
lut_gdb<-"R:/SET/PlantSci/ForestEcology/FireHub/Inputs/ParksVeg/all_veg_2021.gpkg"

#i_vt_veg <- "veg"
i_vt_veg<-"all_veg_2021"

proj_crs<-"+proj=lcc +lat_1=-30.75 +lat_2=-35.75 +lat_0=-33.25 +lon_0=147 +x_0=9300000 +y_0=4500000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ras_res<-"30"

subextent=NULL

rast_temp<-"G:/ft_work/statewide_veg_parks"
gazette_gdb<-""
