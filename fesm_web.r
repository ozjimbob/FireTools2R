library(terra)

Sys.time()
# Cloud optimized Geotiff online, 10m resolution over state of NSW
# 110,000 x 140,000 cells
# Raw ERDAS file = 15gb
# GeoTIFF = 71 mb
r <- rast("/vsicurl/http://ft.bushfirehub.org/images/merge_fesm.tif")

# Define small region
crex <- ext(c(9523391,9542384,4524069,4541359))
            
# Crop to region
r2 <- crop(r,crex)            
           
# plot
plot(r2)
Sys.time()
