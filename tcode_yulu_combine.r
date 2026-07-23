library(tidyverse)
library(terra)
gc()

#inp <- rast(paste0(rast_temp,c("/r_YULU.tif",
#                               "/rTimeSinceLast.tif",
#                               "/r_vegout.tif",
#                               "/r_YUWT.tif")))
#names(inp)<-c("YULU","TSL","HTS","YUWT")

inp <- rast(paste0(rast_temp,c("/r_YULU.tif",
                               "/rTimeSinceLast.tif",
                               "/r_vegout.tif",
                               "/r_YUWT.tif",
                               "/r_vegcode.tif")))
names(inp)<-c("YULU","TSL","HTS","YUWT","VEG")


#p <- as.polygons(inp, values=TRUE, dissolve=TRUE, na.rm=TRUE)

#df_1 <- as.data.frame(inp, cells=TRUE, na.rm=FALSE) # Too much memory

df <- as.data.frame(inp[[1]], cells=TRUE, na.rm=FALSE)
df_2 <- as.data.frame(inp[[2]], cells=TRUE, na.rm=FALSE)
df$TSL <- df_2$TSL
rm(df_2)
gc()

df_3 <- as.data.frame(inp[[3]], cells=TRUE, na.rm=FALSE)
df$HTS <- df_3$HTS
rm(df_3)
gc()

df_4 <- as.data.frame(inp[[4]], cells=TRUE, na.rm=FALSE)
df$YUWT <- df_4$YUWT
rm(df_4)
gc()

df_5 <- as.data.frame(inp[[5]], cells=TRUE, na.rm=FALSE)
df$VEG <- df_5$VEG
rm(df_5)
gc()


  
df$combo_id <- as.integer(factor(do.call(paste, df[,-1])))

combo <- rast(inp[[1]])
values(combo) <- NA
values(combo)[df$cell] <- df$combo_id
gc()


u <- unique(combo)
out <- list()

ml <- match(u$YULU, df$combo_id)

for(idx in seq_along(u$YULU)){
  tfs <- df[ml[idx],]
  tfs$cell <- NULL
  rownames(tfs)<-NULL
  out[[idx]]=tfs
}
out <- bind_rows(out)


p <- as.polygons(combo, dissolve = TRUE)
names(p) <- "combo_id"

p <- merge(p, out, by = "combo_id", all.x = TRUE)
p$combo_id <- NULL

p_lut <- dplyr::select(v_vegfire_table,VEG,MIN_,MAX_,VEGTEXT,NSWVegClass,NSWFormation)
p <- merge(p,p_lut,by="VEG",all.x=TRUE)
p <- subset(p,!is.na(p$VEG))

HTS_lut <- data.frame(HTS=c(1,2,3,4,5,9),
                      HTS_text =c("NoRegime","TooFrequentlyBurnt","Vulnerable","LongUnburnt","WithinThreshold","Unknown"))

p <- merge(p,HTS_lut,by="HTS",all.x=TRUE)
p$HTS <- NULL
### recode HTS to text

writeVector(p,paste0(rast_temp,"/v_combined_output.gpkg"),overwrite=TRUE)
writeVector(p,paste0(rast_temp,"/v_combined_output.shp"),overwrite=TRUE)

## TODO

## max/min via VEG because they are unique to VEG - # i_vt_veg_lut


