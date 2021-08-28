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




out_ls<-read_csv(paste0(rast_temp,"/metrics_landscape.csv"))
out_cl<-read_csv(paste0(rast_temp,"/metrics_class.csv"))

out_cl$class <- factor(out_cl$class,levels=c("TooFrequentlyBurnt","Vulnerable","LongUnburnt","WithinThreshold"),
                       labels=c("TooFrequentlyBurnt","Vulnerable", "LongUnburnt","WithinThreshold"))

### ls
out_ls <- filter(out_ls,Year>=1980)

out_ls_wide <- out_ls %>% pivot_wider(names_from=function_name,values_from=value,id_cols = Year)

out_ls_mat <- as.matrix(out_ls_wide[,-1])
rownames(out_ls_mat)<-out_ls_wide$Year

library(vegan)
ord <- metaMDS(out_ls_mat)
png("D:/OneDrive/OneDrive - University of Tasmania/FireHub/DPIE Risk Assessment/DPIE_report/figs/ord_lm_heritage.png",
    width=1000,height=800,units="px",res=150,type="cairo")
ordiplot(ord, type = "n", main = "Heritage Threshold Landscape Metrics")
orditorp(ord, display = "sites",pcol="white")
spp.fit <- envfit(ord, out_ls_mat, permutations = 999)
plot(spp.fit, p.max = 0.01, col = "red", cex = 0.7)
dev.off()

# Load config files
source("../config/global_config.r")
source("../config/config_summary_fire.r")
### fire_ks
out_ls <- read_csv(paste0(rast_temp,"/metrics_landscape.csv"))
out_ls <- filter(out_ls,Year>=1980)

out_ls_wide <- out_ls %>% pivot_wider(names_from=function_name,values_from=value,id_cols = Year)

out_ls_mat <- as.matrix(out_ls_wide[,-1])
rownames(out_ls_mat)<-out_ls_wide$Year

library(vegan)
ord <- metaMDS(out_ls_mat)
png("D:/OneDrive/OneDrive - University of Tasmania/FireHub/DPIE Risk Assessment/DPIE_report/figs/ord_lm_fire.png",
    width=1000,height=800,units="px",res=150,type="cairo")

ordiplot(ord, type = "n", main = "Time Since Fire Landscape Metrics")
orditorp(ord, display = "sites",pcol="white")
spp.fit <- envfit(ord, out_ls_mat, permutations = 999)
plot(spp.fit, p.max = 0.01, col = "red", cex = 0.7)
dev.off()