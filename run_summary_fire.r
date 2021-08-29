#!/usr/bin/Rscript

# Summary statistics - fire intervals

## NSW Test
library(tidyverse)
library(sf)
library(raster)
library(cowplot)
library(scales)

# Load config files
source("../config/global_config.r")
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  source("../config/config_summary_fire.r")
}else{
  source(args[1])
}




# Make output folder
dir.create(rast_temp)

# Get list of files and years from the fire input folder
file_list <- list.files(fire_folder,pattern="rTimeSinceLast_")
year_list <- substr(file_list,16,19)

# Work out maximum time since fire for the categories
max_tsf <- max(as.numeric(year_list)) - min(as.numeric(year_list))
max_tsf <- ceiling(max_tsf/5)*5


# Load veg
veg <- read_sf(paste0(veg_folder,"/","v_vegBase.gpkg"))
veg_r <- raster(paste0(veg_folder,"/","r_vegcode.tif"))

# Join ad create lookup table for vegetation names
# Based on input vegetation file
vc <- values(veg_r)
vc <- tibble(VEG = vc)
veg_ns <- veg
st_geometry(veg_ns)<-NULL
veg_ns <- dplyr::select(veg_ns,VEG,(!!rlang::sym(veg_field)))
veg_ns[[veg_field]] <- factor(veg_ns[[veg_field]])
veg_ns <- distinct(veg_ns)
veg_ns <- left_join(vc,veg_ns)
veg_ns[[veg_field]]<-toupper(veg_ns[[veg_field]])

rm(veg)
rm(veg_r)
rm(vc)

gc()

# For each year, generate 5-year interval TSF counts by veg
out <- list()

for(i in seq_along(year_list)){
  this_year <- year_list[i]
  print(this_year)
  this_file <- paste0(fire_folder,"/",file_list[i])
  r <- raster(this_file)
  
  ii <- values(r)
  ii <- cut(ii,seq(0,max_tsf,5),seq(0,max_tsf-5,5),right=FALSE)
  ii <- addNA(ii)
  ii <- tibble(year = this_year, TSF = ii, Veg = veg_ns[[veg_field]])
  
  ii <- ii %>% group_by(year,TSF,Veg,.drop=FALSE) %>% summarise(count = n())
  out[[i]] <- ii
}
  
  
ii <- bind_rows(out)
rm(out)
gc()

# Numerical summaries here?
current_table <- filter(ii,year==current_year)
current_table <- filter(current_table, !is.na(Veg))
current_table$Veg[current_table$Veg=="<NULL>"] = "UNKNOWN"
current_table$area = current_table$count * (res(r)[1] * res(r)[2])
current_table$area = current_table$area / 10000

current_table$TSF2 <- as.numeric(as.character(current_table$TSF))
current_table <- current_table %>% mutate(TSF_Range = paste0(TSF2,"-",TSF2+5))
max_burn = max(current_table$TSF2,na.rm=TRUE)
current_table$TSF_Range[current_table$TSF_Range == "NA-NA"] = paste0(">",max_burn+5)

current_wide <- pivot_wider(current_table,id_cols = Veg,names_from=TSF_Range,values_from=area)
current_wide <- mutate(current_wide, across(everything(), ~replace_na(.x, 0)))

#vmat = as.matrix(current_wide[1:nrow(current_wide),2:ncol(current_wide)])
#rows = rowSums(vmat)
#cols = colSums(vmat)

current_wide <- current_wide %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))
current_wide <- current_wide %>% rowwise() %>% mutate(Total =  sum(across(!contains("Veg"))))

write_csv(current_wide,paste0(rast_temp,"/fire_current_year_veg_summary.csv"))


# Make a vegetation table that is unsummarised
# and strip vegetation from main table
veg_table <- ii
ii <- ii %>% summarise(count=sum(count))

# All veg plot
cell_size <- res(r)[1] * res(r)[2]
cc <- gradient_n_pal(colours=c("red","orange","yellow","green","blue"))(seq(0,1,length.out=length(levels(ii$TSF))))
ii$area <- ii$count * cell_size / 10000
rng <- ii %>% filter(!is.na(as.character(TSF))) %>% group_by(year) %>% summarise(area = sum(area)) %>% pull(area) %>% max()

pl <- ggplot(ii, aes(x=year,fill=TSF,y=area)) + 
  geom_bar(stat="identity",position = position_stack(reverse = TRUE)) + 
  theme_cowplot() + 
  labs(x="Year",y="Area (ha)",fill="Time Since Fire") + 
  scale_fill_manual(values=cc) + theme(axis.text.x=element_text(angle = -90, hjust = 0)) + lims(y=c(0,rng))
pl

ggsave2(paste0(rast_temp,"/TSF_bar_all.png"),pl,width=20,height=10,units="cm",dpi=300,scale=2)

# Filter missing vegetation types
veg_table <- filter(veg_table, !is.na(Veg) & Veg != "<NULL>")
unq_veg <- unique(veg_table$Veg)

# Plot for each vegetation type
for(this_veg in unq_veg){
  
  veg_ss <- filter(veg_table,Veg == this_veg)

  cell_size <- res(r)[1] * res(r)[2]
  cc <- gradient_n_pal(colours=c("red","orange","yellow","green","blue"))(seq(0,1,length.out=length(levels(ii$TSF))))
  veg_ss$area <- veg_ss$count * cell_size / 10000
  rng <- veg_ss %>% filter(!is.na(as.character(TSF))) %>% group_by(year) %>% summarise(area = sum(area)) %>% pull(area) %>% max()
  
  pl <- ggplot(veg_ss, aes(x=year,fill=TSF,y=area)) + 
    geom_bar(stat="identity",position = position_stack(reverse = TRUE)) + 
    theme_cowplot() + 
    labs(x="Year",y="Area (ha)",fill="Time Since Fire",title=this_veg) + 
    scale_fill_manual(values=cc) + theme(axis.text.x=element_text(angle = -90, hjust = 0)) + lims(y=c(0,rng))
  pl
  this_veg <- gsub(" ", "_", this_veg)
  ggsave2(paste0(rast_temp,"/TSF_bar_",this_veg,".png"),pl,width=20,height=10,units="cm",dpi=300,scale=2)
}

## Regenerate table with actual year rather than 5-year intervals
out <- list()
for(i in seq_along(year_list)){
  this_year <- year_list[i]
  print(this_year)
  this_file <- paste0(fire_folder,"/",file_list[i])
  r <- raster(this_file)
  
  ii <- values(r)
  ii <- tibble(year = this_year, TSF = ii, Veg = veg_ns[[veg_field]])
  
  ii <- ii %>% group_by(year,TSF,Veg,.drop=FALSE) %>% summarise(count = n())
  out[[i]] <- ii
}

ii <- bind_rows(out)
veg_table <- filter(ii, !is.na(Veg) & Veg != "<NULL>")
unq_veg <- unique(veg_table$Veg)
rm(out)
rm(veg_ns)
gc()

# Summary Tables for each vegetation type
for(this_veg in unq_veg){
  veg_ss <- filter(veg_table,Veg == this_veg)
  veg_ss <- filter(veg_ss,!is.na(as.character(TSF)))
  veg_ss$area <- veg_ss$count * cell_size / 10000
  #veg_sc <- uncount(veg_ss,count) 
  veg_sc <- veg_ss
  veg_sc$TSF <- as.numeric(as.character(veg_sc$TSF))

  
  #sumr <- veg_sc %>% group_by(year) %>% summarise(MeanTSF = mean(TSF),
  #                                               MedianTSF = median(TSF),
  #                                               MaxTSF = max(TSF),
  #                                               MinTSF = min(TSF))
  
  sumr <- veg_sc %>% group_by(year) %>% summarise(MeanTSF = mean(rep(TSF,count)),
                                                  MedianTSF = median(rep(TSF,count)),
                                                  MaxTSF = max(rep(TSF,count)),
                                                  MinTSF = min(rep(TSF,count)))
  
  
  o2 <- list()
  for(this_year_idx in seq_along(sumr$year)){
    this_year <- sumr$year[this_year_idx]
    veg_ssf <- filter(veg_ss,year==this_year)
    this_min <- sumr$MinTSF[this_year_idx]
    this_max <- sumr$MaxTSF[this_year_idx]
    min_area <- veg_ssf$area[veg_ssf$TSF == this_min]
    max_area <- veg_ssf$area[veg_ssf$TSF == this_max]
    o2[[this_year_idx]] <- tibble(AreaMinTSF = min_area,
                      AreaMaxTSF = max_area)
  }
  o2 <- bind_rows(o2)
  sumr <- bind_cols(sumr,o2)
  write_csv(sumr,paste0(rast_temp,"/TSF_area_summary_",this_veg,".csv"))
}

# Landscape metrics

library(landscapemetrics)

# Load example raster
out_ls <- list()
out_cl <- list()
for(i in seq_along(year_list)){
  this_year <- year_list[i]
  print(this_year)
  this_file <- paste0(fire_folder,"/",file_list[i])
  r <- raster(this_file)
  this_file <- raster(paste0(fire_folder,"/",file_list[i]))
  this_file <- cut(this_file,seq(0,max_tsf,5))*5
  
  output_class <- calculate_lsm(this_file,
                                level = "class",
                                classes_max = length(unique(values(this_file))),
                                full_name=TRUE)
  
  output_landscape <- calculate_lsm(this_file,
                                    level = "landscape",
                                    classes_max = length(unique(values(this_file))),
                                    full_name=TRUE)
  
  output_landscape <- filter(output_landscape, function_name %in% c("lsm_l_iji","lsm_l_area_mn",
                                                                    "lsm_l_mesh","lsm_l_split",
                                                                    "lsm_l_shdi","lsm_l_division",
                                                                    "lsm_l_contag","lsm_l_frac_mn",
                                                                    "lsm_l_np","lsm_l_contig_mn"))
  output_landscape <- output_landscape %>% dplyr::select(name,value,function_name)
  
  output_class <- filter(output_class,function_name %in% c("lsm_c_ai",
                                                           "lsm_c_area_mn",
                                                           "lsm_c_clumpy",
                                                           "lsm_c_cohesion",
                                                           "lsm_c_contig_mn",
                                                           "lsm_c_division",
                                                           "lsm_c_enn_mn",
                                                           "lsm_c_frac_mn",
                                                           "lsm_c_iji",
                                                           "lsm_c_mesh",
                                                           "lsm_c_np",
                                                           "lsm_c_split"))
  
  output_class <- output_class %>% dplyr::select(name,class,value,function_name)
  
  output_landscape$Year <- this_year
  output_class$Year <- this_year
  out_ls[[i]] <- output_landscape
  out_cl[[i]] <- output_class
  gc()
}


out_ls <- bind_rows(out_ls)
out_cl <- bind_rows(out_cl)

write_csv(out_ls,paste0(rast_temp,"/metrics_landscape.csv"))
write_csv(out_cl,paste0(rast_temp,"/metrics_class.csv"))


out_ls<-read_csv(paste0(rast_temp,"/metrics_landscape.csv"))
out_cl<-read_csv(paste0(rast_temp,"/metrics_class.csv"))


#unique(out_ls$function_name)



# Plot landscape statistics

# Landscape Contiguity Index
out_ls$Year <- as.numeric(out_ls$Year)
frac <- out_ls %>% 
  filter(function_name=="lsm_l_contig_mn") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line() + lims(y=c(0,1)) + theme_cowplot() +
  labs(y="Landscape Contiguity Index")
frac

ggsave2(paste0(rast_temp,"/fire_lsi_landscape_contiguity.png"),frac,width=10,height=6,units="cm",dpi=300,scale=2)


# Shannons Diveristy Landscape
shdi <- out_ls %>% 
  filter(function_name=="lsm_l_shdi") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Shannon Diversity Index")
shdi
ggsave2(paste0(rast_temp,"/fire_lsi_landscape_shannon_diversity.png"),shdi,width=10,height=6,units="cm",dpi=300,scale=2)


# Landscape Splitting Index
spliti     <- out_ls %>% 
  filter(function_name=="lsm_l_split") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Landscape Splitting Index")
spliti  
ggsave2(paste0(rast_temp,"/fire_lsi_landscape_splitting.png"),spliti,width=10,height=6,units="cm",dpi=300,scale=2)

# Landscape Effective Mesh Size Index
meshi     <- out_ls %>% 
  filter(function_name=="lsm_l_mesh") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Landscape Effective Mesh Size")
meshi  
ggsave2(paste0(rast_temp,"/fire_lsi_landscape_ef_mesh_size.png"),meshi,width=10,height=6,units="cm",dpi=300,scale=2)

# Landscape Effective Mesh Size Index
frac     <- out_ls %>% 
  filter(function_name=="lsm_l_frac_mn") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Mean Fractal Dimension")
frac  
ggsave2(paste0(rast_temp,"/fire_lsi_landscape_mean_frac_d.png"),frac,width=10,height=6,units="cm",dpi=300,scale=2)

# Landscape Contagion Index
contag     <- out_ls %>% 
  filter(function_name=="lsm_l_contag") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Landscape Contagion Index")
contag  
ggsave2(paste0(rast_temp,"/fire_lsi_landscape_contag.png"),contag,width=10,height=6,units="cm",dpi=300,scale=2)


# Landscape Interspersion/Juxtaposition Index
iji     <- out_ls %>% 
  filter(function_name=="lsm_l_iji") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Interspersion/Juxtaposition")
iji  
ggsave2(paste0(rast_temp,"/fire_lsi_landscape_interjux.png"),iji,width=10,height=6,units="cm",dpi=300,scale=2)

# Landscape Number of patches
np     <- out_ls %>% 
  filter(function_name=="lsm_l_np") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Number of Patches")
np  
ggsave2(paste0(rast_temp,"/fire_lsi_landscape_number_patch.png"),np,width=10,height=6,units="cm",dpi=300,scale=2)

# Landscape  mean area
marea     <- out_ls %>% 
  filter(function_name=="lsm_l_area_mn") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Mean Patch Area")
marea  
ggsave2(paste0(rast_temp,"/fire_lsi_landscape_mean_patch_area.png"),marea,width=10,height=6,units="cm",dpi=300,scale=2)

########## NOte on all this - statistics inappropraite when there is a large area unburnt eg. pre-1975 - limit graphs to this
# area or indicate cuttoff area with shading or a line?







# Plot class statistics

out_cl$Year <- as.numeric(out_cl$Year)
out_cl <- filter(out_cl,!is.na(class))

out_cl <- filter(out_cl, class %in% c(5,15,25))

# "lsm_c_ai"$$        "lsm_c_area_mn"$$   "lsm_c_clumpy" ##$   "lsm_c_cohesion"##  "lsm_c_contig_mn"## "lsm_c_division" ##
# "lsm_c_enn_mn"$$    "lsm_c_frac_mn"##   "lsm_c_iji" ##      "lsm_c_mesh" ##     "lsm_c_np" ##       "lsm_c_split" 


# Contiguity index by class
contigi     <- out_cl %>% 
  filter(function_name=="lsm_c_contig_mn") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot()  +
  labs(y="Contiguity Index",col="Time Since Fire") +
  scale_color_gradientn(colors=c("red","orange","darkgreen"))
contigi  
ggsave2(paste0(rast_temp,"/fire_lsi_class_contiguity.png"),contigi,width=10,height=6,units="cm",dpi=300,scale=2)


# Aggregation index by class
ai     <- out_cl %>% 
  filter(function_name=="lsm_c_ai") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_gradientn(colors=c("red","orange","darkgreen")) +
  labs(y="Aggregation Index",col="Time Since Fire")
ai  
ggsave2(paste0(rast_temp,"/fire_lsi_class_aggregation_index.png"),ai,width=10,height=6,units="cm",dpi=300,scale=2)

# Clumpyness index by class
# Equivalent to Aggregation Index
#clumpy     <- out_cl %>% 
#  filter(function_name=="lsm_c_clumpy") %>% 
#  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
#  scale_color_manual(values=col_vec) +
#  labs(y="Clumpiness Index",col="Status")
#clumpy  
#ggsave2(paste0(rast_temp,"/heritage_lsi_class_clumpiness.png"),clumpy,width=10,height=6,units="cm",dpi=300,scale=2)

# Cohesion index by class
coh     <- out_cl %>% 
  filter(function_name=="lsm_c_cohesion") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_gradientn(colors=c("red","orange","darkgreen")) +
  labs(y="Cohesion Index",col="Time Since Fire")
coh  
ggsave2(paste0(rast_temp,"/fire_lsi_class_cohesion_index.png"),coh,width=10,height=6,units="cm",dpi=300,scale=2)

# Division index by class
divis     <- out_cl %>% 
  filter(function_name=="lsm_c_division") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_gradientn(colors=c("red","orange","darkgreen")) +
  labs(y="Division Index",col="Time Since Fire")
divis  
ggsave2(paste0(rast_temp,"/fire_lsi_class_division_index.png"),divis,width=10,height=6,units="cm",dpi=300,scale=2)

# Euclidean Nearest Neighbour index by class
enn     <- out_cl %>% 
  filter(function_name=="lsm_c_enn_mn") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_gradientn(colors=c("red","orange","darkgreen")) +
  labs(y="Euclidean NN Distance",col="Time Since Fire")
enn  
ggsave2(paste0(rast_temp,"/fire_lsi_class_ENN_Dist.png"),enn,width=10,height=6,units="cm",dpi=300,scale=2)

# Mean Fractal Dimension index by class
frac     <- out_cl %>% 
  filter(function_name=="lsm_c_frac_mn") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_gradientn(colors=c("red","orange","darkgreen")) +
  labs(y="Mean Fractal Dimenstion",col="Time Since Fire")
frac  
ggsave2(paste0(rast_temp,"/fire_lsi_class_mean_fracd.png"),frac,width=10,height=6,units="cm",dpi=300,scale=2)

# Interspersion/Justaposition index by class
iji     <- out_cl %>% 
  filter(function_name=="lsm_c_iji") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_gradientn(colors=c("red","orange","darkgreen")) +
  labs(y="Interspersion/Justaposition",col="Time Since Fire")
iji  
ggsave2(paste0(rast_temp,"/fire_lsi_class_interjux.png"),iji,width=10,height=6,units="cm",dpi=300,scale=2)

# Effective Mesh Size by class
mesh     <- out_cl %>% 
  filter(function_name=="lsm_c_mesh") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_gradientn(colors=c("red","orange","darkgreen")) +
  labs(y="Effective Mesh Size",col="Time Since Fire")
mesh  
ggsave2(paste0(rast_temp,"/fire_lsi_class_effective_mesh.png"),mesh,width=10,height=6,units="cm",dpi=300,scale=2)

# Number of Patches
np     <- out_cl %>% 
  filter(function_name=="lsm_c_np") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_gradientn(colors=c("red","orange","darkgreen")) +
  labs(y="Number of Patches",col="Time Since Fire")
np  
ggsave2(paste0(rast_temp,"/fire_lsi_class_number_patches.png"),np,width=10,height=6,units="cm",dpi=300,scale=2)

# Mean Patch Area
np     <- out_cl %>% 
  filter(function_name=="lsm_c_area_mn") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_gradientn(colors=c("red","orange","darkgreen")) +
  labs(y="Mean Patch Area",col="Time Since Fire")
np  
ggsave2(paste0(rast_temp,"/fire_lsi_class_mean_patch_area.png"),np,width=10,height=6,units="cm",dpi=300,scale=2)



# Splitting Index
## Doesn't work, values too extreme?
#spl     <- out_cl %>% 
#  filter(function_name=="lsm_c_split") %>% 
#  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
#  scale_color_manual(values=col_vec) +
#  labs(y="Splitting Index",col="Status")
#spl  
#ggsave2(paste0(rast_temp,"/heritage_lsi_class_splitting.png"),spl,width=10,height=6,units="cm",dpi=300,scale=2)

## Correlogram?

library(corrgram)

out_cl<-read_csv(paste0(rast_temp,"/metrics_class.csv"))
out_cl$Year <- as.numeric(out_cl$Year)
out_cl <- filter(out_cl,!is.na(class))


out_min <- dplyr::select(out_cl,function_name,value)
wd <- pivot_wider(out_min,names_from = function_name,values_from=value)

### UNLIST THE COLUMNS
wd2 <- tibble( Aggreg=unlist(wd$lsm_c_ai),
               MnArea=unlist(wd$lsm_c_area_mn),
               Cohesian=unlist(wd$lsm_c_cohesion),
               MeanContig=unlist(wd$lsm_c_contig_mn),
               Division=unlist(wd$lsm_c_division),
               MnENN=unlist(wd$lsm_c_enn_mn),
               MnFrac=unlist(wd$lsm_c_frac_mn),
               IJI=unlist(wd$lsm_c_iji),
               Mesh=unlist(wd$lsm_c_mesh),
               NP=unlist(wd$lsm_c_np))


png(paste0(rast_temp,"/fire_lsi_corrgram.png"),width = 12,height=9,units="cm",type="cairo-png",res=300)
corrgram(wd2,order=TRUE,
         upper.panel=panel.cor,cex.labels=.5)
dev.off()


## Correlogram?
out_min <- dplyr::select(out_ls,function_name,value)
wd <- pivot_wider(out_min,names_from = function_name,values_from=value)

### UNLIST THE COLUMNS
wd2 <- tibble( MnArea=unlist(wd$lsm_l_area_mn),
               Contag = unlist(wd$lsm_l_contag),
               MeanContig = unlist(wd$lsm_l_contig_mn),
               Division = unlist(wd$lsm_l_division),
               MnFrac = unlist(wd$lsm_l_frac_mn),
               IJI = unlist(wd$lsm_l_iji),
               Mesh = unlist(wd$lsm_l_mesh),
               NP = unlist(wd$lsm_l_np),
               Shannon = unlist(wd$lsm_l_shdi),
               Split = unlist(wd$lsm_l_split))


png(paste0(rast_temp,"/fire_lsi_corrgram_landscape.png"),width = 12,height=9,units="cm",type="cairo-png",res=300)
corrgram(wd2,order=TRUE,
         upper.panel=panel.cor,cex.labels=.5)
dev.off()
