#!/usr/bin/Rscript

# Summary statistics - Heritage

#############
#####
#### crosstab() form x threshold
#### as.data.frame() the crosstab
#### repeat by year
#### replace vegform and status via look-up table
####


## NSW Test
library(tidyverse)
library(sf)
library(raster)
library(cowplot)
library(scales)
library(fs)
library(terra)

# Load config files
source("../config/global_config.r")


args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  source("../config/config_summary_heritage.r")
}else{
  source(args[1])
}




# Make output folder
dir.create(rast_temp)

# Get list of files and years from the fire input folder
file_list <- list.files(heritage_folder,pattern="[[:digit:]]?tif$")

year_list <- substr(file_list,29,32)
if(exists("filter_year")){
  keep = which(as.numeric(year_list)>= as.numeric(filter_year))
  year_list=year_list[keep]
  file_list <- file_list[keep]
  
}


### 
# ALT LUT
#lut <- read_csv("R:/SET/PlantSci/ForestEcology/FireHub/fullstate_100m/lut.csv")
# Load veg
veg <- read_sf(paste0(veg_folder,"/","v_vegBase.gpkg"))
veg_r <- rast(paste0(veg_folder,"/","r_vegcode.tif"))

####################
### CLIP TO TEMPLATE
### Should shift this to heritage postproces, generate standard formation raster from statewide formation raster?
template <- rast(paste0(heritage_folder,"/",file_list[1]))
veg_r <- project(veg_r,template,method="near")
writeRaster(veg_r,paste0(rast_temp,"/veg_code.tif"))
veg_r <- raster(paste0(rast_temp,"/veg_code.tif"))
####################

# Join ad create lookup table for vegetation names
# Based on input vegetation file
vc <- raster::values(veg_r)
vc <- tibble(VEG = vc)
veg_ns <- veg #veg_ns = lut
st_geometry(veg_ns)<-NULL
veg_ns <- dplyr::select(veg_ns,VEG,(!!rlang::sym(veg_field)))
veg_ns[[veg_field]] <- path_sanitize(veg_ns[[veg_field]])
veg_ns[[veg_field]] <- factor(veg_ns[[veg_field]])
veg_ns <- distinct(veg_ns)
veg_ns <- left_join(vc,veg_ns)
veg_ns[[veg_field]]<-toupper(veg_ns[[veg_field]])

rm(veg)
rm(veg_r)
gc()


# For each year, generate 5-year interval TSF counts by veg
out <- list()


rtable = data.frame(ID = c(1,2,3,4,5,9),
                    Status = factor(c("NoFireRegime",
                                      "TooFrequentlyBurnt",
                                      "Vulnerable",
                                      "LongUnburnt",
                                      "WithinThreshold",
                                      "Unknown"),
                                    levels=c("Unknown",
                                             "LongUnburnt","WithinThreshold",
                                             "Vulnerable",
                                             "TooFrequentlyBurnt",
                                             "NoFireRegime")))



for(i in seq_along(year_list)){
  #for(i in 77:78){
  this_year <- year_list[i]
  print(this_year)
  this_file <- paste0(heritage_folder,"/",file_list[i])
  r <- raster(this_file)
  
  
  ii <- values(r)
  ii[ii==9]=6
  ii <- rtable$Status[ii]
  ii <- addNA(ii)
  ii <- tibble(year = this_year, Status = ii, Veg = veg_ns[[veg_field]])
  
  ii <- ii %>% group_by(year,Status,Veg,.drop=FALSE) %>% summarise(count = n())
  out[[i]] <- ii
}


ii <- bind_rows(out)
ii <- filter(ii, !is.na(Veg))
ii$Veg[ii$Veg=="<NULL>"] = "UNKNOWN"

# Numerical summaries here?
current_table <- filter(ii,year==as.numeric(current_year))
current_table <- filter(current_table, !is.na(Veg))
current_table$Veg[current_table$Veg=="<NULL>"] = "UNKNOWN"
current_table$area = current_table$count * (res(r)[1] * res(r)[2])
current_table$area = current_table$area / 10000

current_wide <- pivot_wider(current_table,id_cols = Veg,names_from=Status,values_from=area)
current_wide <- mutate(current_wide, across(!contains("Veg"), ~replace_na(.x, 0)))
vmat = as.matrix(current_wide[1:nrow(current_wide),2:ncol(current_wide)])
rows = rowSums(vmat)
cols = colSums(vmat)

current_wide$Total = rows
grand_total = sum(rows)
newrow <- tibble(Veg="Total",
                 Unknown = cols[1],
                 WithinThreshold = cols[2],
                 LongUnburnt = cols[3],
                 Vulnerable = cols[4],
                 TooFrequentlyBurnt = cols[5],
                 NoFireRegime = cols[6],
                 Total = grand_total)
current_wide <- bind_rows(current_wide,newrow)

write_csv(current_wide,paste0(rast_temp,"/Heritage_current_year_veg_summary.csv"))



#ii <- filter(ii,as.character(Status) != "NoFireRegime")
#ii <- droplevels(ii)
# Make a vegetation table that is unsummarised
# and strip vegetation from main table
veg_table <- ii
ii <- ii %>% summarise(count=sum(count))

# All veg plot
cell_size <- res(r)[1] * res(r)[2]
#col_vec = rev(c("#ffffff","#ff0000","#ff6600","#999999","#00ffff","#ffffffff"))
col_vec = c("Unknown" = "#ffffff",
            "TooFrequentlyBurnt" = "#ff0000",
            "Vulnerable" = "#ff6600",
            "WithinThreshold" = "#999999",
            "LongUnburnt" = "#00ffff",
            "NoFireRegime"="#ffffff")

ii$area <- ii$count * cell_size / 10000
ii <- ii %>% filter(!is.na(as.character(Status)))

rng <- ii  %>% group_by(year) %>% summarise(area = sum(area)) %>% pull(area) %>% max()
ii$Status <- factor(ii$Status,levels=c("LongUnburnt","WithinThreshold",
                                       "Vulnerable",
                                       "TooFrequentlyBurnt",
                                       "NoFireRegime","Unknown"))

pl <- ggplot(ii, aes(x=year,fill=Status,y=area)) + 
  geom_bar(stat="identity",position = position_stack(reverse = TRUE)) + 
  theme_cowplot() + 
  labs(x="Year",y="Area (ha)",fill="Heritage Threshold Status") + 
  scale_fill_manual(values=col_vec,drop=TRUE) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  lims(y=c(0,rng))


ggsave2(paste0(rast_temp,"/heritage_bar_all.png"),pl,width=20,height=10,units="cm",dpi=300,scale=2,bg="white")

########################
#########################
### NUMBERS OF ALL VEG

veg_ss <- veg_table
veg_ss$area <- veg_ss$count * cell_size / 10000
current_wide <- veg_ss %>% group_by(year,Status) %>% summarise(area=sum(area))
current_wide <- pivot_wider(current_wide,id_cols = year,names_from=Status,values_from=area)
current_wide <- mutate(current_wide, across(!contains("Veg"), ~replace_na(.x, 0)),
                       Total = sum(Unknown,LongUnburnt,WithinThreshold,Vulnerable,TooFrequentlyBurnt,NoFireRegime))


write_csv(current_wide,paste0(rast_temp,"/Heritage_area_summary_COMBINED.csv"))




######################
########################


# Filter missing vegetation types
veg_table <- filter(veg_table, !is.na(Veg) & Veg != "<NULL>")
unq_veg <- unique(veg_table$Veg)

# Plot for each vegetation type
for(this_veg in unq_veg){
  
  veg_ss <- filter(veg_table,Veg == this_veg)
  veg_ss$Status <- factor(veg_ss$Status,levels=c("LongUnburnt","WithinThreshold",
                                         "Vulnerable",
                                         "TooFrequentlyBurnt",
                                         "NoFireRegime","Unknown"))
  cell_size <- res(r)[1] * res(r)[2]
  #cc <- gradient_n_pal(colours=c("red","orange","yellow","green","blue"))(seq(0,1,length.out=length(levels(ii$Status))))
  veg_ss$area <- veg_ss$count * cell_size / 10000
  rng <- veg_ss %>% filter(!is.na(as.character(Status))) %>% group_by(year) %>% summarise(area = sum(area)) %>% pull(area) %>% max()
  
  
  pl <- ggplot(veg_ss, aes(x=year,fill=Status,y=area)) + 
    geom_bar(stat="identity",position = position_stack(reverse = TRUE)) + 
    theme_cowplot() + 
    labs(x="Year",y="Area (ha)",fill="Heritage Threshold Status",title=this_veg) + 
    scale_fill_manual(values=col_vec) + theme(axis.text.x=element_text(angle = -90, hjust = 0)) + lims(y=c(0,rng))
  
  this_veg <- gsub(" ", "_", this_veg)
  ggsave2(paste0(rast_temp,"/heritage_bar_",this_veg,".png"),pl,width=20,height=10,units="cm",bg="white",dpi=300,scale=2)
}

#######################
#######################
######### NUMERICAL SUMMARIES FOR EACH VEGETATION TYPE


# Summary Tables for each vegetation type
for(this_veg in unq_veg){
  veg_ss <- filter(veg_table,Veg == this_veg)
  veg_ss$area <- veg_ss$count * cell_size / 10000
  
  current_wide <- pivot_wider(veg_ss,id_cols = year,names_from=Status,values_from=area)
  current_wide <- mutate(current_wide, across(!contains("Veg"), ~replace_na(.x, 0)))
  current_wide <- ungroup(current_wide)
  current_wide <- current_wide %>% mutate(Total = rowSums(dplyr::select(., !year), na.rm = TRUE))
  
  
  write_csv(current_wide,paste0(rast_temp,"/Heritage_area_summary_",this_veg,".csv"))
}


#################################
##################################


# Landscape metrics

library(landscapemetrics)


veg <- read_sf(paste0(veg_folder,"/","v_vegBase.gpkg"))
veg <- dplyr::select(veg,VEG,(!!rlang::sym(veg_field)))
veg[[veg_field]] = toupper(path_sanitize(veg[[veg_field]]))



# This section needs to be parralelized

library(doParallel)
library(foreach)
library(future.apply)
plan(tweak(multiprocess, workers = clustNo,gc=TRUE))
options(future.globals.maxSize = +Inf)

rm(veg_ns)
#rm(veg)
rm(vc)
gc()

out_list = list()

######
### CONVERT TO TERRA - need real data.

for(j in seq_along(unq_veg)){
  this_veg <- unq_veg[j]
  print(paste0("Processing: ",this_veg))
  
  this_veg_overlay <- filter(veg,toupper(!!rlang::sym(veg_field))==this_veg)
  template_r <- raster(paste0(heritage_folder,"/",file_list[1]))
  values(template_r) <- 1
  print("Veg Masking")
  template_r <- mask(template_r,this_veg_overlay)
  
  
  do_year = function(i){
    this_year = year_list[i]
    
    this_file <- raster(paste0(heritage_folder,"/",file_list[i]))
    this_file[this_file==1 | this_file==9] = NA
    print("masking")
    this_file <- this_file * template_r
    
    # Filter to veg types?
    
    
    # CLASS LEVEL:
    #   - lsm_c_area_mn, lsm_c_enn_mn, lsm_c_mesh
    # LANDSCAPE LEVEL:
    #   - lsm_l_mesh, lsm_l_iji
    print("Calcualte LSM")
    output_class <- calculate_lsm(this_file,
                                  level = "class",
                                  classes_max = length(unique(raster::values(this_file))),
                                  full_name=TRUE,metric = c("area", 'contag', 'np', 'enn'))
    
    output_landscape <- calculate_lsm(this_file,
                                      level = "landscape",
                                      classes_max = length(unique(raster::values(this_file))),
                                      full_name=TRUE, metric = c("area", 'contag', 'np', 'enn'))
    

    output_landscape <- output_landscape %>% dplyr::select(name,value,function_name)
    
 
    
    output_class <- output_class %>% dplyr::select(name,class,value,function_name)
    
    output_landscape$Year <- this_year
    output_class$Year <- this_year
    output_landscape$Veg <- this_veg
    output_class$Veg <- this_veg
    
    list(output_landscape,output_class)
  }
  # Load example raster
  o = future_lapply(seq_along(year_list),FUN=do_year,future.scheduling=3)
  o = bind_rows(o)
  out_list[[j]]=o
  
}

#out_ls <- bind_rows(out_ls)
#out_cl <- bind_rows(out_cl)

out_list = bind_rows(out_list)


out_list$class <- factor(out_list$class,levels=c(1,2,3,4,5,9),labels=c("NoFireRegime",
                                                                   "TooFrequentlyBurnt",
                                                                   "Vulnerable",
                                                                   "LongUnburnt",
                                                                   "WithinThreshold",
                                                                   "Unknown"))

write_csv(out_list,paste0(rast_temp,"/spatial_metrics_formation_all.csv"))
write_csv(filter(out_list,is.na(class)),paste0(rast_temp,"/spatial_metrics_formation_landscape.csv"))
write_csv(filter(out_list,!is.na(class)),paste0(rast_temp,"/spatial_metrics_formation_class.csv"))
out_list <- read_csv(paste0(rast_temp,"/spatial_metrics_formation_all.csv"))

# Plot landscape statistics for each vegetation class

# lsm_l_contag
# lsm_l_area_mn
# lsm_l_np
# lsm_c_enn_mn
# lsm_c_area_mn
# lsm_c_np

out_list = list()

do_year = function(i){
   this_year = year_list[i]
    
   this_file <- raster(paste0(heritage_folder,"/",file_list[i]))
   this_file[this_file==1 | this_file==9] = NA

    # Filter to veg types?
    
    
    # CLASS LEVEL:
    #   - lsm_c_area_mn, lsm_c_enn_mn, lsm_c_mesh
    # LANDSCAPE LEVEL:
    #   - lsm_l_mesh, lsm_l_iji
    print("Calcualte LSM")
    output_class <- calculate_lsm(this_file,
                                  level = "class",
                                  classes_max = length(unique(raster::values(this_file))),
                                  full_name=TRUE, metric = c("area", 'contag', 'np', 'enn'))
    
    output_landscape <- calculate_lsm(this_file,
                                      level = "landscape",
                                      classes_max = length(unique(raster::values(this_file))),
                                      full_name=TRUE, metric = c("area", 'contag', 'np', 'enn'))
    

    output_landscape <- output_landscape %>% dplyr::select(name,value,function_name)
    

    output_class <- output_class %>% dplyr::select(name,class,value,function_name)
    
    output_landscape$Year <- this_year
    output_class$Year <- this_year

    list(output_landscape,output_class)
}

  # Load example raster
o = future_lapply(seq_along(year_list),FUN=do_year,future.scheduling=3)
o = bind_rows(o)


o$class <- factor(o$class,levels=c(1,2,3,4,5,9),labels=c("NoFireRegime",
                                                                       "TooFrequentlyBurnt",
                                                                       "Vulnerable",
                                                                       "LongUnburnt",
                                                                       "WithinThreshold",
                                                                       "Unknown"))

write_csv(o,paste0(rast_temp,"/spatial_metrics_all.csv"))
write_csv(filter(o,is.na(class)),paste0(rast_temp,"/spatial_metrics_landscape.csv"))
write_csv(filter(o,!is.na(class)),paste0(rast_temp,"/spatial_metrics_class.csv"))

