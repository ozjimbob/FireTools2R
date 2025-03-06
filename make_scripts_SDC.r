library(tidyverse)
library(sf)

reg <- read_sf("G:/ft_work/new_eco_scorecard/regions.gpkg")

branch <- read_sf("R:/SET/PlantSci/ForestEcology/FireHub/FireToolsSDC/svtm_test/gpkg/branch.gpkg")
branch <- filter(branch,NAME != "ACT")

st_crs(branch)<-"EPSG:4283"
st_crs(reg)<-"EPSG:3308"
branch <- st_transform(branch,st_crs(reg))

branch$styear = c(1950,1943,1953,1926,1935,1902,1938,1942)

reg_c <- st_centroid(reg)
reg_c <- st_join(reg_c,branch)

library(glue)
### Config scripts
for(i in 1:nrow(reg_c)){
  current_year <- 2022
  outslug <- reg_c$Eco_short[i]
  outyear <- reg_c$styear[i]
template <-glue('fire_folder <- "/mnt/scratch_lustre/FireTools/Inputs/statewide_fire_preprocess"
veg_folder <- "/mnt/scratch_lustre/FireTools/Inputs/statewide_veg_all"
single_year <- "timeseries"
current_year<-{current_year}
start_year<-1990
future_years = 1
rast_temp<-"/mnt/scratch_lustre/FireTools/Output/ecohealth/heritage_{outslug}_30m"
corp_gdb<-"/mnt/scratch_lustre/FireTools/Inputs/regions.gpkg"
i_vt_boundary<-"regions"
f_spatial_unit<-"Eco_short"
d_spatial_unit<-"{outslug}"
fesm_dir="/mnt/scratch_lustre/FireTools/Inputs/statewide_fesm"
sCPU=11
history_start_year <- {outyear}
calculate_metrics=TRUE
')
cat(template,file=paste0("G:/ft_work/new_eco_scorecard/sdc_files/config_",outslug,"_heritage.r"))

template <- glue('#!/bin/bash
#
#SBATCH --job-name=firetools
#SBATCH --cpus-per-task=25
#SBATCH --ntasks=1
#SBATCH --time=124:10:00
#SBATCH --mem-per-cpu=16000
#SBATCH --partition=general

module purge
module restore default

cd ../FireTools2R
export GDAL_PAM_ENABLED=YES
export TMPDIR=/mnt/scratch_lustre/FireTools/tmp
export R_LIBS_USER=/home/gwilliamson/R/x86_64-pc-linux-gnu-library/4.2
export UDUNITS2_LIBS=/mnt/appsource/udunits/udunits-2.2.28/lib

srun /mnt/appsource/R/R-4.2.0_S15SP2/bin/Rscript run_post_heritage_sdc.r /home/gwilliamson/test_submission/ecohealth_sdc/config_{outslug}_heritage.r
')

cat(template,file=paste0("G:/ft_work/new_eco_scorecard/sdc_files/ft_heritage_postprocess_",outslug,".sh"))

template <- glue('#!/bin/bash
#
#SBATCH --job-name=firetools
#SBATCH --cpus-per-task=12
#SBATCH --ntasks=1
#SBATCH --time=124:10:00
#SBATCH --mem-per-cpu=16000
#SBATCH --partition=general

module purge
module restore default

cd ../FireTools2R
export GDAL_PAM_ENABLED=YES
export TMPDIR=/mnt/scratch_lustre/FireTools/tmp
export R_LIBS_USER=/home/gwilliamson/R/x86_64-pc-linux-gnu-library/4.2
export UDUNITS2_LIBS=/mnt/appsource/udunits/udunits-2.2.28/lib

srun /mnt/appsource/R/R-4.2.0_S15SP2/bin/Rscript new_heritage_summary_sdc.r /home/gwilliamson/test_submission/ecohealth_sdc/config_{outslug}_heritage.r
')

cat(template,file=paste0("G:/ft_work/new_eco_scorecard/sdc_files/ft_heritage_summary_",outslug,".sh"))

}

#!/bin/sh

template <- glue('sbatch ft_heritage_summary_{reg_c$Eco_short}.sh
')
template <-paste0(template,collapse="\n")
template <- paste0("#!/bin/bash\n",template)
cat(template,file="G:/ft_work/new_eco_scorecard/sdc_files/run_heritage_summary_ecohealth.sh")

template <- glue('sbatch ft_heritage_postprocess_{reg_c$Eco_short}.sh
')
template <-paste0(template,collapse="\n")
template <- paste0("#!/bin/bash\n",template)
cat(template,file="G:/ft_work/new_eco_scorecard/sdc_files/run_heritage_ecohealth.sh")
