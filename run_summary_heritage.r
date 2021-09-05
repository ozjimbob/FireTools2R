#!/usr/bin/Rscript

# Summary statistics - Heritage

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
  source("../config/config_summary_heritage.r")
}else{
  source(args[1])
}




# Make output folder
dir.create(rast_temp)

# Get list of files and years from the fire input folder
file_list <- list.files(heritage_folder,pattern="[[:digit:]]?tif$")
year_list <- substr(file_list,10,13)


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
  ii <- rtable$Status[ii]
  ii <- addNA(ii)
  ii <- tibble(year = this_year, Status = ii, Veg = veg_ns[[veg_field]])
  
  ii <- ii %>% group_by(year,Status,Veg,.drop=FALSE) %>% summarise(count = n())
  out[[i]] <- ii
}


ii <- bind_rows(out)

# Numerical summaries here?
current_table <- filter(ii,year==current_year)
current_table <- filter(current_table, !is.na(Veg))
current_table$Veg[current_table$Veg=="<NULL>"] = "UNKNOWN"
current_table$area = current_table$count * (res(r)[1] * res(r)[2])
current_table$area = current_table$area / 10000

current_wide <- pivot_wider(current_table,id_cols = Veg,names_from=Status,values_from=area)
current_wide <- mutate(current_wide, across(everything(), ~replace_na(.x, 0)))
vmat = as.matrix(current_wide[1:nrow(current_wide),2:ncol(current_wide)])
rows = rowSums(vmat)
cols = colSums(vmat)

current_wide$Total = rows
grand_total = sum(rows)
newrow <- tibble(Veg="Total",
                 WithinThreshold = cols[1],
                 LongUnburnt = cols[2],
                 Vulnerable = cols[3],
                 TooFrequentlyBurnt = cols[4],
                 NoFireRegime = cols[5],
                 Total = grand_total)
current_wide <- bind_rows(current_wide,newrow)

write_csv(current_wide,paste0(rast_temp,"/Heritage_current_year_veg_summary.csv"))



ii <- filter(ii,as.character(Status) != "NoFireRegime")
ii <- droplevels(ii)
# Make a vegetation table that is unsummarised
# and strip vegetation from main table
veg_table <- ii
ii <- ii %>% summarise(count=sum(count))

# All veg plot
cell_size <- res(r)[1] * res(r)[2]
col_vec = rev(c("#ffffff","#ff0000","#ff6600","#999999","#00ffff","#ffffffff"))

ii$area <- ii$count * cell_size / 10000
ii <- ii %>% filter(!is.na(as.character(Status)))

rng <- ii  %>% group_by(year) %>% summarise(area = sum(area)) %>% pull(area) %>% max()


pl <- ggplot(ii, aes(x=year,fill=Status,y=area)) + 
  geom_bar(stat="identity",position = position_stack(reverse = TRUE)) + 
  theme_cowplot() + 
  labs(x="Year",y="Area (ha)",fill="Heritage Threshold Status") + 
  scale_fill_manual(values=col_vec) + theme(axis.text.x=element_text(angle = -90, hjust = 0)) + lims(y=c(0,rng))
pl

ggsave2(paste0(rast_temp,"/heritage_bar_all.png"),pl,width=20,height=10,units="cm",dpi=300,scale=2)



###



# Filter missing vegetation types
veg_table <- filter(veg_table, !is.na(Veg) & Veg != "<NULL>")
unq_veg <- unique(veg_table$Veg)

# Plot for each vegetation type
for(this_veg in unq_veg){
  
  veg_ss <- filter(veg_table,Veg == this_veg)
  
  cell_size <- res(r)[1] * res(r)[2]
  #cc <- gradient_n_pal(colours=c("red","orange","yellow","green","blue"))(seq(0,1,length.out=length(levels(ii$Status))))
  veg_ss$area <- veg_ss$count * cell_size / 10000
  rng <- veg_ss %>% filter(!is.na(as.character(Status))) %>% group_by(year) %>% summarise(area = sum(area)) %>% pull(area) %>% max()
  
  
  pl <- ggplot(veg_ss, aes(x=year,fill=Status,y=area)) + 
    geom_bar(stat="identity",position = position_stack(reverse = TRUE)) + 
    theme_cowplot() + 
    labs(x="Year",y="Area (ha)",fill="Heritage Threshold Status",title=this_veg) + 
    scale_fill_manual(values=col_vec[2:5]) + theme(axis.text.x=element_text(angle = -90, hjust = 0)) + lims(y=c(0,rng))
  pl
  this_veg <- gsub(" ", "_", this_veg)
  ggsave2(paste0(rast_temp,"/heritage_bar_",this_veg,".png"),pl,width=20,height=10,units="cm",dpi=300,scale=2)
}

#######################
#######################
######### NUMERICAL SUMMARIES FOR EACH VEGETATION TYPE


# Summary Tables for each vegetation type
for(this_veg in unq_veg){
  veg_ss <- filter(veg_table,Veg == this_veg)
  veg_ss$area <- veg_ss$count * cell_size / 10000
  
  current_wide <- pivot_wider(veg_ss,id_cols = year,names_from=Status,values_from=area)
  current_wide <- mutate(current_wide, across(everything(), ~replace_na(.x, 0)))
  

  write_csv(current_wide,paste0(rast_temp,"/Heritage_area_summary_",this_veg,".csv"))
}


#################################
##################################


# Landscape metrics

library(landscapemetrics)

# Load example raster
out_ls <- list()
out_cl <- list()
for(i in seq_along(year_list)){
  this_year <- year_list[i]
  print(this_year)

  this_file <- raster(paste0(heritage_folder,"/",file_list[i]))
  this_file[this_file==1 | this_file==9] = NA
  #this_file <- cut(this_file,seq(0,max_tsf,5))*5
  
  
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
  
}


out_ls <- bind_rows(out_ls)
out_cl <- bind_rows(out_cl)

write_csv(out_ls,paste0(rast_temp,"/metrics_landscape.csv"))
out_cl$class <- factor(out_cl$class,levels=c(1,2,3,4,5,9),labels=c("NoFireRegime",
                                                                   "TooFrequentlyBurnt",
                                                                   "Vulnerable",
                                                                   "LongUnburnt",
                                                                   "WithinThreshold",
                                                                   "Unknown"))

write_csv(out_cl,paste0(rast_temp,"/metrics_class.csv"))


out_ls<-read_csv(paste0(rast_temp,"/metrics_landscape.csv"))
out_cl<-read_csv(paste0(rast_temp,"/metrics_class.csv"))

out_cl$class <- factor(out_cl$class,levels=c("TooFrequentlyBurnt","Vulnerable","LongUnburnt","WithinThreshold"),labels=c("TooFrequentlyBurnt",
                                                                   "Vulnerable",
                                                                   "LongUnburnt",
                                                                   "WithinThreshold"))

# Plot landscape statistics

# Landscape Contiguity Index
out_ls$Year <- as.numeric(out_ls$Year)
frac <- out_ls %>% 
  filter(function_name=="lsm_l_contig_mn") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line() + lims(y=c(0.35,.45)) + theme_cowplot() +
  labs(y="Landscape Contiguity Index")
frac
ggsave2(paste0(rast_temp,"/heritage_lsi_landscape_contiguity.png"),frac,width=10,height=6,units="cm",dpi=300,scale=2)


# Shannons Diveristy Landscape
shdi <- out_ls %>% 
  filter(function_name=="lsm_l_shdi") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Shannon Diversity Index")
shdi
ggsave2(paste0(rast_temp,"/heritage_lsi_landscape_shannon_diversity.png"),shdi,width=10,height=6,units="cm",dpi=300,scale=2)


# Landscape Splitting Index
spliti     <- out_ls %>% 
  filter(function_name=="lsm_l_split") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Landscape Splitting Index")
spliti  
ggsave2(paste0(rast_temp,"/heritage_lsi_landscape_splitting.png"),spliti,width=10,height=6,units="cm",dpi=300,scale=2)

# Landscape Effective Mesh Size Index
meshi     <- out_ls %>% 
  filter(function_name=="lsm_l_mesh") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Landscape Effective Mesh Size")
meshi  
ggsave2(paste0(rast_temp,"/heritage_lsi_landscape_ef_mesh_size.png"),meshi,width=10,height=6,units="cm",dpi=300,scale=2)

# Landscape Mean Fracgtal Dimension
frac     <- out_ls %>% 
  filter(function_name=="lsm_l_frac_mn") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Mean Fractal Dimension")
frac  
ggsave2(paste0(rast_temp,"/heritage_lsi_landscape_mean_frac_d.png"),frac,width=10,height=6,units="cm",dpi=300,scale=2)

# Landscape Contagion Index
contag     <- out_ls %>% 
  filter(function_name=="lsm_l_contag") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Landscape Contagion Index")
contag  
ggsave2(paste0(rast_temp,"/heritage_lsi_landscape_contag.png"),contag,width=10,height=6,units="cm",dpi=300,scale=2)


# Landscape Interspersion/Juxtaposition Index
iji     <- out_ls %>% 
  filter(function_name=="lsm_l_iji") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Interspersion/Juxtaposition")
iji  
ggsave2(paste0(rast_temp,"/heritage_lsi_landscape_interjux.png"),iji,width=10,height=6,units="cm",dpi=300,scale=2)

# Landscape Number of patches
np     <- out_ls %>% 
  filter(function_name=="lsm_l_np") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Number of Patches")
np  
ggsave2(paste0(rast_temp,"/heritage_lsi_landscape_number_patch.png"),np,width=10,height=6,units="cm",dpi=300,scale=2)

# Landscape  mean area
marea     <- out_ls %>% 
  filter(function_name=="lsm_l_area_mn") %>% 
  ggplot(aes(x=Year,y=value)) + geom_line()  + theme_cowplot()+
  labs(y="Mean Patch Area")
marea  
ggsave2(paste0(rast_temp,"/heritage_lsi_landscape_mean_patch_area.png"),marea,width=10,height=6,units="cm",dpi=300,scale=2)

########## NOte on all this - statistics inappropraite when there is a large area unburnt eg. pre-1975 - limit graphs to this
# area or indicate cuttoff area with shading or a line?



# Plot class statistics

out_cl$Year <- as.numeric(out_cl$Year)


#col_vec = c("#dddddd","#ff0000","#ff6600","#00aaaa","#555555","#dddddd")

#out_cl <- filter(out_cl,!class %in% c("NoFireRegime","Unknown"))
col_vec = c("#ff0000","#ff6600","#00aaaa","#555555")
# "lsm_c_ai"$$        "lsm_c_area_mn"$$   "lsm_c_clumpy" ##$   "lsm_c_cohesion"##  "lsm_c_contig_mn"## "lsm_c_division" ##
# "lsm_c_enn_mn"$$    "lsm_c_frac_mn"##   "lsm_c_iji" ##      "lsm_c_mesh" ##     "lsm_c_np" ##       "lsm_c_split" 

# Contiguity index by class
contigi     <- out_cl %>% 
  filter(function_name=="lsm_c_contig_mn") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_manual(values=col_vec) +
  labs(y="Contiguity Index",col="Status")
contigi  
ggsave2(paste0(rast_temp,"/heritage_lsi_class_contiguity.png"),contigi,width=10,height=6,units="cm",dpi=300,scale=2)


# Aggregation index by class
ai     <- out_cl %>% 
  filter(function_name=="lsm_c_ai") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_manual(values=col_vec) +
  labs(y="Aggregation Index",col="Status")
ai  
ggsave2(paste0(rast_temp,"/heritage_lsi_class_aggregation_index.png"),ai,width=10,height=6,units="cm",dpi=300,scale=2)

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
  scale_color_manual(values=col_vec) +
  labs(y="Cohesion Index",col="Status")
coh  
ggsave2(paste0(rast_temp,"/heritage_lsi_class_cohesion_index.png"),coh,width=10,height=6,units="cm",dpi=300,scale=2)

# Division index by class
divis     <- out_cl %>% 
  filter(function_name=="lsm_c_division") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_manual(values=col_vec) +
  labs(y="Division Index",col="Status")
divis  
ggsave2(paste0(rast_temp,"/heritage_lsi_class_division_index.png"),divis,width=10,height=6,units="cm",dpi=300,scale=2)

# Euclidean Nearest Neighbour index by class
enn     <- out_cl %>% 
  filter(function_name=="lsm_c_enn_mn") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_manual(values=col_vec) +
  labs(y="Euclidean NN Distance",col="Status") 
enn  
ggsave2(paste0(rast_temp,"/heritage_lsi_class_ENN_Dist.png"),enn,width=10,height=6,units="cm",dpi=300,scale=2)

# Mean Fractal Dimension index by class
frac     <- out_cl %>% 
  filter(function_name=="lsm_c_frac_mn") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_manual(values=col_vec) +
  labs(y="Mean Fractal Dimenstion",col="Status")
frac  
ggsave2(paste0(rast_temp,"/heritage_lsi_class_mean_fracd.png"),frac,width=10,height=6,units="cm",dpi=300,scale=2)

# Interspersion/Justaposition index by class
iji     <- out_cl %>% 
  filter(function_name=="lsm_c_iji") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_manual(values=col_vec) +
  labs(y="Interspersion/Justaposition",col="Status")
iji  
ggsave2(paste0(rast_temp,"/heritage_lsi_class_interjux.png"),iji,width=10,height=6,units="cm",dpi=300,scale=2)

# Effective Mesh Size by class
mesh     <- out_cl %>% 
  filter(function_name=="lsm_c_mesh") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_manual(values=col_vec) +
  labs(y="Effective Mesh Size",col="Status")
mesh  
ggsave2(paste0(rast_temp,"/heritage_lsi_class_effective_mesh.png"),mesh,width=10,height=6,units="cm",dpi=300,scale=2)

# Number of Patches
np     <- out_cl %>% 
  filter(function_name=="lsm_c_np") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_manual(values=col_vec) +
  labs(y="Number of Patches",col="Status")
np  
ggsave2(paste0(rast_temp,"/heritage_lsi_class_number_patches.png"),np,width=10,height=6,units="cm",dpi=300,scale=2)

# Mean Patch Area
np     <- out_cl %>% 
  filter(function_name=="lsm_c_area_mn") %>% 
  ggplot(aes(x=Year,y=value,col=class, group=class)) + geom_line() + theme_cowplot() +
  scale_color_manual(values=col_vec) +
  labs(y="Mean Patch Area",col="Status")
np  
ggsave2(paste0(rast_temp,"/heritage_lsi_class_mean_patch_area.png"),np,width=10,height=6,units="cm",dpi=300,scale=2)


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


png(paste0(rast_temp,"/heritage_lsi_corrgram.png"),width = 12,height=9,units="cm",type="cairo-png",res=300)
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


png(paste0(rast_temp,"/heritage_lsi_corrgram_landscape.png"),width = 12,height=9,units="cm",type="cairo-png",res=300)
corrgram(wd2,order=TRUE,
         upper.panel=panel.cor,cex.labels=.5)
dev.off()
