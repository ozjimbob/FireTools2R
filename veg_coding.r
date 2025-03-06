library(tidyverse)
library(terra)

#veg <- vect('G:/ft_work/kos_2023_apr/svtm.gpkg')
#template = rast('G:/ft_work/statewide_veg_parks/rast_done/r_vegmax.tif')
#r <- rasterize(veg,template,field="PCTID",filename="G:/ft_work/kos_2023_apr/r_vegcode.tif")

cc <- rast("G:/ft_work/kos_2023_apr/r_vegcode.tif")


full_lut<-read_csv("G:/ft_work/statewide_veg_parks/rast_done/veg_lut.csv")


max_mat <- cbind(full_lut$PCTID,full_lut$MAX)
min_mat <- cbind(full_lut$PCTID,full_lut$MIN)
form_mat<- cbind(full_lut$PCTID,full_lut$FormID)

cc_max <- classify(cc,max_mat,others=NA,filename="G:/ft_work/kos_2023_apr/r_vegmax.tif",overwrite=TRUE)
cc_min <- classify(cc,min_mat,others=NA,filename="G:/ft_work/kos_2023_apr/r_vegmin.tif",overwrite=TRUE)
cc_form <- classify(cc,form_mat,others=NA,filename="G:/ft_work/kos_2023_apr/r_vegform.tif",overwrite=TRUE)

cc[is.na(cc)]=0
writeRaster(cc,"G:/ft_work/kos_2023_apr/temp/r_vegcode.tif")


