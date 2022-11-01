
library(tidyverse)
library(sf)
library(raster)
library(cowplot)
library(scales)
library(fs)
library(glue)
library(terra)

print("STart FESM Process")

input_folder <- FESM_source
veg_folder <- rast_temp
rast_temp <- paste0(rast_temp,"/FESM_metrics")
year_list <- FESM_years
clustNo=3

dir.create(rast_temp)
year_list2 <- year_list+1


file_list <- glue("{input_folder}cvmsre_{FESM_name}_{year_list}{year_list2}_ag1l0.img")


exist <- file.exists(file_list)

file_list <- file_list[exist]
year_list <- year_list[exist]


template_lr <- rast(file_list[1])
terra::values(template_lr)=1
mr <- vect(paste0(veg_folder,"/v_region.gpkg"))
maskt = mask(template_lr,mr)


library(landscapemetrics)

veg <- rast(paste0(veg_folder,"/veg/r_vegform.tif"))
forms <- read_csv(paste0(veg_folder,"/form_lut.csv"))


library(doParallel)
library(foreach)
library(future.apply)
plan(tweak(multiprocess, workers = clustNo,gc=TRUE))
options(future.globals.maxSize = +Inf)

unq_veg <- unique(terra::values(veg))
unq_veg <- unq_veg[!is.na(unq_veg)]
unq_veg_tib <- tibble(FormID = unq_veg)
unq_veg_tib <- left_join(unq_veg_tib,forms)
unq_veg_tib <- unq_veg_tib %>% arrange(Form)


########### PROCESS!!
out_list = list()


for(j in seq_along(unq_veg_tib$FormID)){
  
  this_veg <- unq_veg_tib$Form[j]
  print(paste0("Processing: ",this_veg))
  
  this_veg_overlay <- deepcopy(veg)
  this_veg_overlay[this_veg_overlay != unq_veg_tib$FormID[j]]=NA
  
  print("Veg Masking")  
  template_lr <- rast(file_list[1])
  this_veg_overlay <- project(this_veg_overlay,crs(template_lr),method="near")
  this_veg_overlay <- resample(this_veg_overlay,template_lr,method="near")
  
  terra::values(template_lr) <- 1
  template_lr <- mask(template_lr,this_veg_overlay)
  
  #template_hr <- raster(file_list[length(file_list)])
  #values(template_hr) <- 1
  # template_hr <- mask(template_hr,this_veg_overlay)
  
  
  
  do_year = function(i){
    
    
    this_year = year_list[i]
    
    this_file <- rast(file_list[i])
    
    print("low")
    this_file <- project(this_file,template_lr,method="near")
    this_file <- this_file * template_lr
    this_file = this_file * maskt
    
    
    print("Calcualte LSM")
    output_class <- calculate_lsm(this_file,
                                  level = "class",
                                  classes_max = length(unique(raster::values(this_file))),
                                  full_name=TRUE, metric = c("area", 'contag', 'np', 'enn'),
                                  progress=TRUE)
    
    output_landscape <- calculate_lsm(this_file,
                                      level = "landscape",
                                      classes_max = length(unique(raster::values(this_file))),
                                      full_name=TRUE, metric = c("area", 'contag', 'np', 'enn'),
                                      progress=TRUE)
    
    
    output_landscape <- output_landscape %>% dplyr::select(name,value,function_name)
    
    output_class <- output_class %>% dplyr::select(name,class,value,function_name)
    
    output_landscape$Year <- this_year
    output_class$Year <- this_year
    output_landscape$Veg <- this_veg
    output_class$Veg <- this_veg
    
    list(output_landscape,output_class)
  }
  # Load example raster
  o = map_df(seq_along(year_list),do_year)
  o = bind_rows(o)
  out_list[[j]]=o
  
}

j=j+1


### Now do one for ALL veg unmasked

  print(paste0("Processing: All"))
  
  print("Veg Masking")  
  template_lr <- rast(file_list[1])
  #this_veg_overlay <- project(this_veg_overlay,crs(template_lr),method="near")
  #this_veg_overlay <- resample(this_veg_overlay,template_lr,method="near")
  
  terra::values(template_lr) <- 1
  template_lr <- mask(template_lr,mr)
  
  #template_hr <- raster(file_list[length(file_list)])
  #values(template_hr) <- 1
  # template_hr <- mask(template_hr,this_veg_overlay)
  
  
  
  do_year = function(i){
    
    
    this_year = year_list[i]
    
    this_file <- rast(file_list[i])
    
    print("low")
    this_file <- project(this_file,template_lr,method="near")
    this_file <- this_file * template_lr
    this_file = this_file * maskt
    
    
    print("Calcualte LSM")
    output_class <- calculate_lsm(this_file,
                                  level = "class",
                                  classes_max = length(unique(raster::values(this_file))),
                                  full_name=TRUE, metric = c("area", 'contag', 'np', 'enn'),
                                  progress=TRUE)
    
    output_landscape <- calculate_lsm(this_file,
                                      level = "landscape",
                                      classes_max = length(unique(raster::values(this_file))),
                                      full_name=TRUE, metric = c("area", 'contag', 'np', 'enn'),
                                      progress=TRUE)
    
    
    output_landscape <- output_landscape %>% dplyr::select(name,value,function_name)
    
    output_class <- output_class %>% dplyr::select(name,class,value,function_name)
    
    output_landscape$Year <- this_year
    output_class$Year <- this_year
    output_landscape$Veg <- "All"
    output_class$Veg <- "All"
    
    list(output_landscape,output_class)
  }
  
  # Load example raster
  o = map_df(seq_along(year_list),do_year)
  o = bind_rows(o)
  out_list[[j]]=o
  

out_list = bind_rows(out_list)


out_list$class <- factor(out_list$class,levels=c(0,1,2,3,4,5),labels=c(c( "Unburnt","Unburnt2",
                                                                          "Low",
                                                                          "Moderate",
                                                                          "High",
                                                                          "Severe")))


write_csv(out_list,paste0(rast_temp,"/spatial_metrics_formation_all.csv"))
write_csv(filter(out_list,is.na(class)),paste0(rast_temp,"/spatial_metrics_formation_landscape.csv"))
write_csv(filter(out_list,!is.na(class)),paste0(rast_temp,"/spatial_metrics_formation_class.csv"))

