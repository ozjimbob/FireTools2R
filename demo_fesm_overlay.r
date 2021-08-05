#### DEMO Development - FESM Integration
library(tidyverse)
library(sf)
library(raster)

fesm_root <- "G:/ft_work/fesm/"
template <- raster("G:/ft_work/veg_bmnts_study_30m/r_vegadv.tif")

# List FESM files
fesm_list <- list.files(fesm_root,pattern="img")
sp <- str_split(fesm_list,"_",simplify = TRUE)
fesm_date <- as.Date(sp[,3],"%Y%m%d")

fesm_frame <- tibble(File = paste0(fesm_root,fesm_list),Date=fesm_date)
fesm_frame <- arrange(fesm_frame,rev(Date))

for(i in 1:nrow(fesm_frame)){
  print(i)
  r <- raster(fesm_frame$File[i])
  r <- projectRaster(r,template,method="ngb")
  r <- resample(r,template,method="ngb")
  values(r)[values(r)==0]=NA
  if(i ==1){
    rx=r
  }else{
    rx = cover(rx,r)
  }
}

# load region boundary
boundary <- raster("G:/ft_work/veg_bmnts_study_30m/roi_mask.tif")
rx <- rx * boundary


writeRaster(rx,"G:/ft_work/fesm_bmnts_study_30m/merged_fesm.tif",overwrite=TRUE)
values(rx)[values(rx) %in% c(2,3)] = 1
values(rx)[values(rx) %in% c(4,5)] = 2
writeRaster(rx,"G:/ft_work/fesm_bmnts_study_30m/merged_fesm_2class.tif",overwrite=TRUE)

# We now have 2-class fesm, time to combine with heritage threshold!

heritage <- raster("G:/ft_work/full_analysis_bmnts_study_30m/r_heritage_threshold_status.tif")
values(rx)[values(rx)==2]=10
heritage <- heritage * rx

# Define colours
# remove missing areas
values(heritage)[values(heritage) %in% c(1,9,10,90)] = NA

rtable = data.frame(ID = c(2,3,4,5),
                    Status = c("TooFrequentlyBurnt",
                                      "Vulnerable",
                                      "LongUnburnt",
                                      "WithinThreshold"
                                      ),
                    Colour = c("#ff0000","#ff9900","#00ffff","#999999"))

stable = data.frame(ID = c(20,30,40,50),
                    Status = c("TooFrequentlyBurnt - Canopy Loss",
                               "Vulnerable - Canopy Loss",
                               "LongUnburnt - Canopy Loss",
                               "WithinThreshold - Canopy Loss"),
                    Colour = c("#aa0000","#aa6600","#00aaaa","#333333"))

ctable <- bind_rows(rtable,stable)

#library(tmap)
#m <- tm_shape(heritage,raster.downsample = FALSE) + tm_raster(breaks=ctable$ID,palette = ctable$Colour,style="cat",labels = ctable$Status)
#m

# Ratify and colour table the output raster
heritage <- ratify(heritage)
rat <- levels(heritage)[[1]]
rat <- left_join(rat,ctable)
ids = as.numeric(rownames(rat))
codes = rat$ID
heritage = reclassify(heritage,cbind(codes,ids))
rat$ID = ids
levels(heritage) <- rat

require(foreign)
atable = levels(heritage)[[1]]
names(atable)=c("VALUE","CATEGORY","COLOUR")
x = as.data.frame(table(raster::values(heritage)))
names(x)=c("VALUE","COUNT")
x$VALUE = as.numeric(as.character(x$VALUE))
a2 = left_join(atable,x)
a2$COUNT[is.na(a2$COUNT)]=0
a2 = dplyr::select(a2,VALUE,COUNT,CATEGORY)
write.dbf(a2,"G:/ft_work/fesm_bmnts_study_30m/heritage_fesm.tif.vat.dbf")
colortable(heritage) <- c("#ffffff",ctable$Colour)
writeRaster(heritage,"G:/ft_work/fesm_bmnts_study_30m/heritage_fesm.tif",overwrite=TRUE)

# Now veg overlay and tabulation, as for fire/heritage
# Load veg
source("../config/config_summary_heritage.r")
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

# Overlay heritage/veg
ii <- values(heritage)
rtable <- levels(heritage)[[1]]
ii <- rtable$Status[ii]
ii[is.na(ii)]="No Status"
ii <- factor(ii)
#ii <- addNA(ii)
ii <- tibble(Status = ii, Veg = veg_ns[[veg_field]])

ii <- ii %>% group_by(Status,Veg,.drop=FALSE) %>% summarise(count = n())



current_table <- filter(ii, !is.na(Veg))
current_table$Veg[current_table$Veg=="<NULL>"] = "UNKNOWN"
current_table$area = current_table$count * (res(heritage)[1] * res(heritage)[2])
current_table$area = current_table$area / 10000

current_wide <- pivot_wider(current_table,id_cols = Veg,names_from=Status,values_from=area)
current_wide <- mutate(current_wide, across(everything(), ~replace_na(.x, 0)))


current_wide <- current_wide %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))
current_wide <- current_wide %>% rowwise() %>% mutate(Total =  sum(across(!contains("Veg"))))

write_csv(current_wide,"G:/ft_work/fesm_bmnts_study_30m/FESM_current_year_veg_summary.csv")

