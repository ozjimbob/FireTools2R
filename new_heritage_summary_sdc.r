# SDC Heritage summary and landscape metrics - NEW AND IMPROVED
library(tidyverse)
library(raster)
library(terra)
library(sf)
library(landscapemetrics)
library(cowplot)
library(fs)
library(furrr)

# Load config files
source("../config/global_config.r")


args <- commandArgs(trailingOnly=TRUE)
source(args[1])

if(exists("sCPU")){
  clustNo <- sCPU
}

log_it = function(strn){
  cat(paste0(Sys.time()," - ",strn),file=paste0(rast_temp,"/log.txt"),sep="\n",append=TRUE)
  print(paste0("[FT2] ",Sys.time()," - ",strn))
}

dir.create(rast_temp)
log_it("Starting workers.")
plan(multisession, workers = clustNo)

# Make output folkders

log_it(paste0("Output folder: ",rast_temp))
dir.create(paste0(rast_temp,"/summary"))
dir.create(paste0(rast_temp,"/metrics"))

## Overall summary table
log_it("Overall summary table")
year_list <- start_year:current_year

log_it("Template raster")
rl <- paste0(rast_temp,"/r_heritage_threshold_status_",year_list,".tif")
vv <- rast(rl)

#vvp = as.polygons(vv[[1]])
#vva = expanse(vvp)
log_it("Calculating frequencies")
aa <- as_tibble(freq(vv))
aa$layer<-year_list[aa$layer]
rres <- res(vv)[1] * res(vv)[2]
aa$count <- aa$count * rres / 10000

#aa <- read_csv("G:/ft_work/statewide_veg_parks/summary.csv")

col_vec = c("Unknown" = "#ffffff",
            "TooFrequentlyBurnt" = "#ff0000",
            "Vulnerable" = "#ff6600",
            "WithinThreshold" = "#999999",
            "LongUnburnt" = "#00ffff",
            "NoFireRegime"="#ffffff")

aa$value   <- factor(aa$value ,labels=c("LongUnburnt","WithinThreshold",
                                        "Vulnerable",
                                        "TooFrequentlyBurnt",
                                        "Unknown","NoFireRegime"),levels=c("LongUnburnt","WithinThreshold",
                                                                           "Vulnerable",
                                                                           "TooFrequentlyBurnt",
                                                                           "Unknown","NoFireRegime"))

rng <- aa %>% group_by(layer) %>% filter(!value %in% c("Unknown","NoFireRegime")) %>% summarise(area = sum(count)) %>% pull(area) %>% max()
log_it("Making plot")
pl <- ggplot(aa, aes(x=layer,fill=value ,y=count)) + 
  geom_bar(stat="identity",position = position_stack(reverse = TRUE)) + 
  theme_cowplot() + 
  labs(x="Year",y="Area (ha)",fill="Heritage Threshold Status") + 
  scale_fill_manual(values=col_vec,drop=TRUE) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  lims(y=c(0,rng))

log_it("Saving plot")
ggsave2(paste0(rast_temp,"/summary/area_ALL.pdf"),pl,width=1000,height=800,units="px",bg="white",scale=3)

names(aa)<- c("Year","Status","Area")
aa <- aa %>% pivot_wider(id_cols=c("Year"),names_from="Status",values_from = "Area")

aa <- aa %>% rowwise() %>% mutate(Total = sum(c_across(!Year),na.rm=TRUE))
aa <- aa %>% replace(is.na(.), 0)
log_it("Saving CSV aerial summary")
write_csv(aa,paste0(rast_temp,"/summary/area_ALL.csv"))

# Now by veg type


crosstab_year <- function(i){
  this_yr <- rast(paste0(rast_temp,"/r_heritage_threshold_status_",year_list[i],".tif"))
  form <- rast(paste0(rast_temp,"/veg/r_vegform.tif"))
  form_lut <- read_csv(paste0(rast_temp,"/form_lut.csv"))
  st <- c(this_yr,form)
  ct <- terra::crosstab(st)
  ct <- as_tibble(ct)
  names(ct)[2] = "FormID"
  names(ct)[3]= "Area"
  ct$Area <- ct$Area * rres / 10000
  ct$Status <- factor(ct$Status,levels=c("1","2","3","4","5","9"),labels=c("NoFireRegime","TooFrequentlyBurnt","Vulnerable","LongUnburnt","WithinThreshold","Unknown"))
  ct$FormID <- as.numeric(ct$FormID)
  ct <- left_join(ct,form_lut)
  ct$FormID <- NULL
  ct$Year = year_list[i]
  ct
}

log_it("Area by formation")
log_it("Starting crosstab")
smr_form <- future_map_dfr(seq_along(year_list),crosstab_year)

form_list <- unique(smr_form$Form)
log_it("Making plots:")
for(this_form in form_list){
  log_it(paste0("Formation: ",this_form))
  this_smr <- filter(smr_form,Form==this_form)
  this_smr$Form <- NULL
  this_smr$Status <- factor(this_smr$Status ,labels=c("LongUnburnt","WithinThreshold",
                                          "Vulnerable",
                                          "TooFrequentlyBurnt",
                                          "Unknown","NoFireRegime"),levels=c("LongUnburnt","WithinThreshold",
                                                                             "Vulnerable",
                                                                             "TooFrequentlyBurnt",
                                                                             "Unknown","NoFireRegime"))
  
  rng <- this_smr %>% group_by(Year) %>% filter(!Status %in% c("Unknown","NoFireRegime")) %>% summarise(Area = sum(Area)) %>% pull(Area) %>% max()
  
  pl <- ggplot(this_smr, aes(x=Year,fill=Status ,y=Area)) + 
    geom_bar(stat="identity",position = position_stack(reverse = TRUE)) + 
    theme_cowplot() + 
    labs(x="Year",y="Area (ha)",fill="Heritage Threshold Status",title=this_form) + 
    scale_fill_manual(values=col_vec,drop=TRUE) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
    lims(y=c(0,rng))
  short_name <- path_sanitize(this_form)
  ggsave2(paste0(rast_temp,"/summary/area_",short_name,".pdf"),pl,width=1000,height=800,units="px",bg="white",scale=3)
  
  
  this_smr <- this_smr %>% pivot_wider(id_cols=c("Year"),names_from="Status",values_from = "Area")
  
  this_smr <- this_smr %>% rowwise() %>% mutate(Total = sum(c_across(!Year), na.rm=TRUE))
  this_smr <- this_smr %>% replace(is.na(.), 0)
  write_csv(this_smr,paste0(rast_temp,"/summary/area_",short_name,".csv"))
}                     

gc()




if(exists("calculate_metrics")){
  log_it("Calculating landscape metrics")
  ### LANDSCAPE METRIC
  
  # Overall metrics
  lm_year <- function(i){
    this_yr <- rast(paste0(rast_temp,"/r_heritage_threshold_status_",year_list[i],".tif"))
  
    output_class <- calculate_lsm(this_yr,
                                  level = "class",
                                  classes_max = length(unique(raster::values(this_file))),
                                  full_name=TRUE,metric = c("area", 'contag', 'np', 'enn'))
    
    output_landscape <- calculate_lsm(this_yr,
                                      level = "landscape",
                                      classes_max = length(unique(raster::values(this_file))),
                                      full_name=TRUE, metric = c("area", 'contag', 'np', 'enn'))
    
    
    output_landscape <- output_landscape %>% dplyr::select(name,value,function_name)
    
    
    
    output_class <- output_class %>% dplyr::select(name,class,value,function_name)
    
    
    output_landscape$Year <- year_list[i]
    output_class$Year <- year_list[i]
    
    bind_rows(list(output_landscape,output_class))
  }
  
  lm_all <- future_map_dfr(seq_along(year_list),lm_year)
  
  lm_all$class   <- factor(lm_all$class ,levels=c(1,2,3,4,5,9),labels=c("NoFireRegime","TooFrequentlyBurnt",
                                                                        "Vulnerable","LongUnburnt","WithinThreshold","Unknown"))
  log_it("Writing landscapemetrics")
  write_csv(lm_all,paste0(rast_temp,"/metrics/landscape_metrics_all.csv"))
  
  
  
  rm(lm_all)
  ### By formation - mask with terra
  
  
  
  
  
  log_it("Landscape metrics by formation")
  gc()
  form <- rast(paste0(rast_temp,"/veg/r_vegform.tif"))
  form_lut <- read_csv(paste0(rast_temp,"/form_lut.csv"))
  
  unq_form_list <- unique(form)[,1]
  
  out_lm_form <- list()
  
  lm_year_form <- function(i){
    this_yr <- rast(paste0(rast_temp,"/r_heritage_threshold_status_",year_list[i],".tif"))
    tmp_crs <- crs(rast(paste0(rast_temp,"/veg/this_mask.tif")))
    crs(this_yr) <- tmp_crs
    
    this_yr = this_yr * rast(paste0(rast_temp,"/veg/this_mask.tif"))
    output_class <- calculate_lsm(this_yr,
                                  level = "class",
                                  classes_max = length(unique(raster::values(this_file))),
                                  full_name=TRUE,metric = c("area", 'contag', 'np', 'enn'))
    
    output_landscape <- calculate_lsm(this_yr,
                                      level = "landscape",
                                      classes_max = length(unique(raster::values(this_file))),
                                      full_name=TRUE, metric = c("area", 'contag', 'np', 'enn'))
    
    
    output_landscape <- output_landscape %>% dplyr::select(name,value,function_name)
    
    
    
    output_class <- output_class %>% dplyr::select(name,class,value,function_name)
    
    
    output_landscape$Year <- year_list[i]
    output_class$Year <- year_list[i]
    
    bind_rows(list(output_landscape,output_class))
  }
  
  
  for(i in seq_along(unq_form_list)){
    
    this_form = form_lut$Form[form_lut$FormID==unq_form_list[i]]
    log_it(paste0("Formation: ",this_form))
    log_it("Masking")
    
    aa <- showMethods(classify,includeDefs=TRUE,printTo=FALSE)
    kk <- grep("othersNA",aa)
    
    if(length(kk)==0){
      mask_tf = classify(form,rcl=cbind(unq_form_list[i],1),others=NA)
    }else{
      mask_tf = classify(form,rcl=cbind(unq_form_list[i],1),othersNA=TRUE)
    }
    
    log_it("Writing mask")
    writeRaster(mask_tf,paste0(rast_temp,"/veg/this_mask.tif"),overwrite=TRUE)
    print(this_form)
    # Overall metrics
    
    log_it("Running Landscape Metrics")
    lm_tf <- future_map_dfr(seq_along(year_list),lm_year_form,.progress=TRUE)
    unlink(paste0(rast_temp,"/veg/this_mask.tif"))
    lm_tf$formation = this_form
    out_lm_form[[i]]<-lm_tf
  }
  
  log_it("Writing formation landscape metrics")
  out_lm_form<- bind_rows(out_lm_form)
  
  out_lm_form$class   <- factor(out_lm_form$class ,levels=c(1,2,3,4,5,9),labels=c("NoFireRegime","TooFrequentlyBurnt",
                                                                        "Vulnerable","LongUnburnt","WithinThreshold","Unknown"))
  
  write_csv(out_lm_form,paste0(rast_temp,"/metrics/landscape_metrics_formation.csv"))
  out_lm_form1 <- filter(out_lm_form,!is.na(class))
  out_lm_form2 <- filter(out_lm_form,is.na(class))
 
  write_csv(out_lm_form1,paste0(rast_temp,"/metrics/spatial_metrics_formation_class.csv"))
  out_lm_form1$class <- NULL
  write_csv(out_lm_form2,paste0(rast_temp,"/metrics/spatial_metrics_formation_landscape.csv"))
  
  
  rm(out_lm_form)
  gc()
}

## FESM OVERLAY
log_it("FESM overlay")
dir.create(paste0(rast_temp,"/fesm_overlay"))
#fesm_dir <-"G:/ft_work/fesm/sdc_mosaics"

fesm_statewide <- terra::rast(paste0(fesm_dir,"/fesm_overlay.vrt"))
this_heritage <- rast(paste0(rast_temp,"/r_heritage_threshold_status_",year_list[length(year_list)],".tif"))
log_it("Aligning extents")
fst <- align(ext(fesm_statewide),this_heritage)
ext(fesm_statewide)<-fst
fesm_statewide = terra::crop(fesm_statewide,this_heritage)

fesm_statewide = mask(fesm_statewide,vect(paste0(rast_temp,"/v_region.gpkg")))

log_it("Writing clipped FESM")
writeRaster(fesm_statewide,paste0(rast_temp,"/fesm_overlay/fesm_clip.tif"),overwrite=TRUE)
fesm_statewide <- rast(paste0(rast_temp,"/fesm_overlay/fesm_clip.tif")) * 10

fesm_statewide[is.nan(fesm_statewide)]=0
fesm_statewide[fesm_statewide %in% c(20,30)]=10
fesm_statewide[fesm_statewide %in% c(40,50)]=20




log_it("Mask and overlay")
fesm_statewide = mask(fesm_statewide,vect(paste0(rast_temp,"/v_region.gpkg")))
fesm_overlay <- fesm_statewide + this_heritage

rcd <- cbind(c(0,1,2,3,4,5,9,11,12,13,14,15,19,21,22,23,24,25,29),
             c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))
fesm_overlay <- terra::classify(fesm_overlay,rcd)

fesm_table = data.frame(ID = c(0,
                               1,2,3,4,5,6,
                               7,8,9,10,11,12,
                               13,14,15,16,17,18),
                        Status = c("External",
                                   "NoSev_NoFireRegime","NoSev_TooFrequentlyBurnt","NoSev_Vulnerable","NoSev_LongUnburnt","NoSev_WithinThreshold","NoSev_Unknown",
                                   "LowSev_NoFireRegime","LowSev_TooFrequentlyBurnt","LowSev_Vulnerable","LowSev_LongUnburnt","LowSev_WithinThreshold","LowSev_Unknown",
                                   "HighSev_NoFireRegime","HighSev_TooFrequentlyBurnt","HighSev_Vulnerable","HighSev_LongUnburnt","HighSev_WithinThreshold","HighSev_Unknown"))

levels(fesm_overlay)<-fesm_table

col_table = data.frame(ID = c(0,
                              1,2,3,4,5,6,
                              7,8,9,10,11,12,
                              13,14,15,16,17,18),
                       Status = c("white",
                                  "white","#e8929e","#c28f5d","#a2dbce","#cdd1d0","#dabfe3",
                                  "white","#962024","#855321","#3a9e87","#919191","#937b9c",
                                  "white","#4d0306","#4a2603","#065240","#4d4d4d","#49384f"))



coltab(fesm_overlay) <- col_table$Status

log_it("Writing map")
bitmap(paste0(rast_temp,"/fesm_overlay/fesm_overlay.png"),width=1000,height=1300,units="px")
plot(fesm_overlay,legend=FALSE)
dev.off()

log_it("Writing raster")
terra::writeRaster(fesm_overlay,paste0(rast_temp,"/fesm_overlay/fesm_overlay.tif"),overwrite=TRUE)

# Summary - FESM x Heritage Area
###
# We now have the FESM overlay in fesm_overlay


fesm_overlay <- rast(paste0(rast_temp,"/fesm_overlay/fesm_overlay.tif"))
log_it("Calculating frequencies")
aa <- as_tibble(freq(fesm_overlay))
aa$layer<-current_year
rres <- res(vv)[1] * res(vv)[2]
aa$count <- aa$count / rres * 10000


col_vec = c("External"="white",
              "NoSev_NoFireRegime"="white",
              "NoSev_TooFrequentlyBurnt"="#e8929e",
              "NoSev_Vulnerable"="#c28f5d",
              "NoSev_LongUnburnt"="#a2dbce",
              "NoSev_WithinThreshold"="#cdd1d0",
              "NoSev_Unknown"="#dabfe3",
              "LowSev_NoFireRegime"="white",
              "LowSev_TooFrequentlyBurnt"="#962024",
              "LowSev_Vulnerable"="#855321",
              "LowSev_LongUnburnt"="#3a9e87",
              "LowSev_WithinThreshold"="#919191",
              "LowSev_Unknown"="#937b9c",
              "HighSev_NoFireRegime"="white",
              "HighSev_TooFrequentlyBurnt"="#4d0306",
              "HighSev_Vulnerable"="#4a2603",
              "HighSev_LongUnburnt"="#065240",
              "HighSev_WithinThreshold"="#4d4d4d",
              "HighSev_Unknown"="#49384f")

aa$value   <- factor(aa$value ,labels=c("External",
                                        "NoSev_NoFireRegime","NoSev_TooFrequentlyBurnt","NoSev_Vulnerable","NoSev_LongUnburnt","NoSev_WithinThreshold","NoSev_Unknown",
                                        "LowSev_NoFireRegime","LowSev_TooFrequentlyBurnt","LowSev_Vulnerable","LowSev_LongUnburnt","LowSev_WithinThreshold","LowSev_Unknown",
                                        "HighSev_NoFireRegime","HighSev_TooFrequentlyBurnt","HighSev_Vulnerable","HighSev_LongUnburnt","HighSev_WithinThreshold","HighSev_Unknown"),
                     levels=c("External",
                              "NoSev_NoFireRegime","NoSev_TooFrequentlyBurnt","NoSev_Vulnerable","NoSev_LongUnburnt","NoSev_WithinThreshold","NoSev_Unknown",
                              "LowSev_NoFireRegime","LowSev_TooFrequentlyBurnt","LowSev_Vulnerable","LowSev_LongUnburnt","LowSev_WithinThreshold","LowSev_Unknown",
                              "HighSev_NoFireRegime","HighSev_TooFrequentlyBurnt","HighSev_Vulnerable","HighSev_LongUnburnt","HighSev_WithinThreshold","HighSev_Unknown"))

rng <- aa %>% filter(!value %in% c("Unknown","NoFireRegime","External","NoSev_NoFireRegime","LowSev_NoFireRegime","HighSev_NoFireRegime",
                                   "NoSev_Unknown","LowSev_Unknown","HighSev_Unknown")) %>% summarise(area = sum(count)) %>% pull(area) %>% max()

log_it("Making plot")
pl <- ggplot(aa, aes(x=value,fill=value ,y=count)) + 
  geom_bar(stat="identity",position = position_stack(reverse = TRUE)) + 
  theme_cowplot() + 
  labs(x="Heritage/FESM Status",y="Area (ha)",fill="Heritage/FESM Status") + 
  scale_fill_manual(values=col_vec,drop=TRUE) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  lims(y=c(0,rng))

log_it("Saving plot")
ggsave2(paste0(rast_temp,"/summary/area_ALL_FESM.pdf"),pl,width=1000,height=1000,units="px",bg="white",scale=3)

names(aa)<- c("Year","Status","Area")
aa <- aa %>% pivot_wider(id_cols=c("Year"),names_from="Status",values_from = "Area")

aa <- aa %>% rowwise() %>% mutate(Total = sum(c_across(!Year)))

log_it("Saving CSV FESMaerial summary")
write_csv(aa,paste0(rast_temp,"/summary/area_ALL_FESM.csv"))


### By formation
form <- rast(paste0(rast_temp,"/veg/r_vegform.tif"))
form_lut <- read_csv(paste0(rast_temp,"/form_lut.csv"))
st <- c(fesm_overlay,form)
ct <- terra::crosstab(st)
ct <- as_tibble(ct)

names(ct)[2] = "FormID"
names(ct)[3]= "Area"
ct$Area <- ct$Area / rres * 10000

ct$Status <- factor(ct$Status,levels=as.character(fesm_table$ID),labels=fesm_table$Status)
ct$FormID <- as.numeric(ct$FormID)
ct <- left_join(ct,form_lut)
ct$FormID <- NULL
ct$Year = current_year

write_csv(ct,paste0(rast_temp,"/summary/area_formation_FESM.csv"))

if(exists("calculate_FESM_series")){
  if(calculate_FESM_series){
    source("new_heritage_FESM_summary_sdc.r")
  }
}
