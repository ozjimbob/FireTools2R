# veg logic

g_rasterize <- function(layer,filename,output,attribute="",otype="Int32"){
  if(attribute==""){
    paste0(gdal_rasterize," -burn 1 -l ",layer," -of GTiff ",
         "-te ",rex," -tr ",rres[1]," ",rres[2]," -ot ",otype," -co COMPRESS=PACKBITS ",
         paste0(rast_temp,"/",filename)," ",output)
  }else{
    paste0(gdal_rasterize," -a ",attribute," -l ",layer," -of GTiff ",
           "-te ",rex," -tr ",rres[1]," ",rres[2]," -ot ",otype," -co COMPRESS=PACKBITS ",
           paste0(rast_temp,"/",filename)," ",output)
  }
}

g_polygonize <- function(layer,filename,output,attribute="",otype="Int32"){
  if(attribute==""){
    paste0(gdal_rasterize," -burn 1 -l ",layer," -of GTiff ",
           "-te ",rex," -tr ",rres[1]," ",rres[2]," -ot ",otype," -co COMPRESS=PACKBITS ",
           paste0(rast_temp,"/",filename)," ",output)
  }else{
    paste0(gdal_rasterize," -a ",attribute," -l ",layer," -of GTiff ",
           "-te ",rex," -tr ",rres[1]," ",rres[2]," -ot ",otype," -co COMPRESS=PACKBITS ",
           paste0(rast_temp,"/",filename)," ",output)
  }
}

proccell2 = function(i){
  
  #
  #i = 2873
  #
  st = stack(file_list,quick=TRUE)
  r_vegmin = raster(paste0(rast_temp,"/r_vegmin.tif"),values=FALSE)
  r_vegmax= raster(paste0(rast_temp,"/r_vegmax.tif"),values=FALSE)
  r_timesburnt= raster(paste0(rast_temp,"/rNumTimesBurnt.tif"),values=FALSE)
  r_tsl= raster(paste0(rast_temp,"/rTimeSinceLast.tif"),values=FALSE)
  
  
  st = getValues(st,row=i)
  r_vegmax = getValues(r_vegmax,row=i)
  r_vegmin = getValues(r_vegmin,row=i)
  r_timesburnt = getValues(r_timesburnt,row=i)
  r_tsl = getValues(r_tsl,row=i)
  
  ovec = rep(NA,length(r_vegmax))
  
  calc_status=function(){
    Status = 0
    IntervalStatus = 0 # 1 
    if(MaxThresh == 0 && MinThresh == 0){
      Status = statusNoFireRegime
      return(Status)
    }else{
      if(is.na(FireFrequency)){
        FireFrequency = 0
      }
      #2
      if(MaxThresh == 9999 && MinThresh == 9999){
        # 3
        # Repair NA in FireFreq
        
        if(FireFrequency > 0){
          Status = statusTooFrequentlyBurnt
          return(Status)
        }else{
          Status = statusVulnerable
          return(Status)
        }
      }else{
        #4
        if(FireFrequency == 0){
          #5
          if(TSFF > MaxThresh){
            Status = statusLongUnburnt
            return(Status)
          }else{
            Status = statusUnknown
            return(Status)
          }
        }
      }
    }
    
    
    #6
    if(Status == 0){
      #### A Section
      # Yet list of fire years and diff to intervals
      fint = int_list[IntervalList>0]
      fint = diff(fint)
      IntervalStatus = is_WithinThreshold
      # overburnt tally
      overburnt = 0
      for(this_int in fint){
        #7
       # print(this_int)
        if(this_int < MinThresh){
          #print("Less than threshhold")
          # 8
          if(IntervalStatus == is_WithinThreshold){
            IntervalStatus = is_Vulnerable
            #print("IS Set to vulnerable")
            next
          }else{
            IntervalStatus = is_TooFreq
            overburnt = overburnt + 1
            #print("IS set to too frequently")
            next
          }
        }else if(this_int > 2 * MinThresh){
          #print("Interval within threshold")
          IntervalStatus = is_WithinThreshold
          next
        }else if(IntervalStatus %in% c(is_WithinThreshold,is_Vulnerable)){
          #print("Interval within threshold (poss vuln)")
          IntervalStatus = is_WithinThreshold
        }
        
        
      }
    } else{
      IntervalStatus = is_WithinThreshold
    }
    
    
    ## A section
    # 11
    # fix TSF
    if(is.na(TSF)){TSF = TSFF}
    if(IntervalStatus == is_TooFreq){
      # 12
      if(TSF > 2 * MinThresh){
        Status = statusWithinThreshold
        return(Status)
      } else {
        Status = statusTooFrequentlyBurnt
        return(Status)
      }
    }else if(TSF < MinThresh){
      Status = statusVulnerable #13
      return(Status)
    }else if(TSF > MaxThresh){
      Status = statusLongUnburnt #14
      return(Status)
    } else{
      Status = statusWithinThreshold
      return(Status)
    }
    if(Status==0){
      Status = statusUnknown
    }
    return(Status)
  }
  
  for(j in seq_along(ovec)){
    # Rasters we need
    # r_MaxThresh - derived from veg + lookup table
    # r_MinThresh - derived from veg + lookup table
    # FireFrequency - from fire frequency raster (number of times burnt)
    # TSFF - Time Since First Fire value
    
    # output rasters:
    # r_Status (eg. no fire regime, too frequently burnt, vulnerable, long uburnt, unknown)
    statusNoFireRegime=1
    statusTooFrequentlyBurnt = 2
    statusVulnerable = 3
    statusLongUnburnt = 4
    statusUnknown = 9
    statusWithinThreshold = 5
    
    # r_IntervalStatus (eg. within interval)
    is_WithinThreshold = 1
    is_Vulnerable = 2
    is_TooFreq = 3
    
    # get vectors
    MaxThresh = r_vegmax[j]
    MinThresh = r_vegmin[j]
    
    if(is.na(MaxThresh)){
      ovec[j]=NA
      next
    }
    
    FireFrequency = r_timesburnt[j]
    TSF = r_tsl[j]
    IntervalList = st[j,]
    
    # Set base status and intervalstatus
   
    
    
    
    ovec[j]=calc_status()
    ovec[j][ovec[j]==9999]=NA
  }
  
  
  return(ovec)
}


proccell_fmz = function(i){
  
  r_tsf = raster(paste0(rast_temp,"/rTimeSinceLast.tif"),values=FALSE)
  r_fmz = raster(paste0(rast_temp,"/r_fmz.tif"),values=FALSE)
  
  r_fmz = getValues(r_fmz,row=i)
  r_tsf = getValues(r_tsf,row=i)
  
  op = rep(NA,length(r_fmz))
  

  
  op[r_tsf > r_fmz] = 4 # LongUnburnt
  op[r_tsf <= r_fmz] = 5 # WithinThreshold
  
  # Set 0 intervals to NA eg. for mechanical / land management zones
  op[r_fmz == 0] = NA
  
  return(op)
}

log_it = function(strn){
  cat(paste0(Sys.time()," - ",strn),file=paste0(rast_temp,"/log.txt"),sep="\n",append=TRUE)
  print(paste0(Sys.time()," - ",strn))
}


## FUNCTIONS
# Time Since Last
calc_tsl = function(v){
  last_int = last(which(v>0))
  if(is.na(last_int)){return(NA)}
  #return(current_year - year_list[last_int])
  return(year_list[last_int])
}

# Calc interval
calc_interval = function(v){
  tyears = year_list[v>0]
  return(mean(diff(tyears)))
}

# Calc times burnt
calc_timesburnt = function(v){
  tyears = year_list[v>0]
  if(length(tyears)==0){return(NA)}
  return(length(tyears))
}


polygonizer_win <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile', 
                        pypath=NULL, readpoly=TRUE, quietish=TRUE) {
  # x: an R Raster layer, or the file path to a raster file recognised by GDAL
  # outshape: the path to the output shapefile (if NULL, a temporary file will be created)
  # gdalformat: the desired OGR vector format
  # pypath: the path to gdal_polygonize.py (if NULL, an attempt will be made to determine the location
  # readpoly: should the polygon shapefile be read back into R, and returned by this function? (logical)
  # quietish: should (some) messages be suppressed? (logical)
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  ## The line below has been commented:
  # if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.") 
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists)) 
      stop(sprintf('File already exists: %s', 
                   toString(paste(outshape, c('shp', 'shx', 'dbf'), 
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    f <- tempfile(fileext='.tif')
    bigWrite(x, f)
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  
  ## Now 'python' has to be substituted by OSGeo4W
  #system2('python',
  system2('C:\\OSGeo4W64\\OSGeo4W.bat',
          args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"', 
                        pypath, rastpath, gdalformat, outshape)))
  unlink(f)
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quietish)
    return(shp) 
  }
  return(NULL)
}

## Define the function
polygonizer <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quietish=TRUE) {
  quiet = quietish
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}
# Prepare file output space
prepare <- function(){
  # Prepare filesystem
  dir.create(rast_temp)
  log_it("Created temp directory")
  
  file.remove(list.files(rast_temp,full.names=TRUE))
  log_it("Cleaned old files")
}

bigWrite <- function(r,out){
  s2 <- writeStart(r, filename=out, format='GTiff', overwrite=TRUE)
  tr <- blockSize(r)
  for (i in tr$n:1) {
    print(i)
    v <- getValuesBlock(r, row=tr$row[i], nrows=tr$nrows[i])
    s2 <- writeValues(s2, v, tr$row[i])
  }
  s2 <- writeStop(s2)
}

saveWidgetFix <- function (widget,file,...) {
  ## A wrapper to saveWidget which compensates for arguable BUG in
  ## saveWidget which requires `file` to be in current working
  ## directory.
  wd<-getwd()
  on.exit(setwd(wd))
  outDir<-dirname(file)
  file<-basename(file)
  setwd(outDir)
  saveWidget(widget,file=file,...)
}

# Full union operation
st_or = function(x, y, dim = 2) {
  
  # st_erase to get the remainder of the intersection (taken from ?st_difference)
  st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
  
  # we need st_dump to extract polygons from a potential GEOMETRYCOLLECTION
  st_dump = function(x, dim) {
    dims = sapply(x, st_dimension)
    x[dims == dim, ]
  }
  
  # get overlap via intersection
  overlap = sf::st_intersection(x, y)
  
  # extract polygons (if dimm = 2)
  overlap = overlap[st_dimension(overlap) == dim, ]
  
  gc = which(sapply(seq(nrow(overlap)), function(i) {
    inherits(overlap[i, ], "GEOMETRYCOLLECTION")
  }))
  
  if (length(gc) > 0) {
    dmp = st_dump(overlap, dim = dim)
    overlap = rbind(overlap[-gc, ], dmp)
  }
  
  # get the non-intersecting parts and set missing attribute to NA
  diff1 = st_erase(x, y)
  diff2 = st_erase(y, x)
  
  diff1[, setdiff(names(overlap), names(diff1))] = NA
  diff2[, setdiff(names(overlap), names(diff2))] = NA
  
  cb = rbind(diff1,diff2)
  
  # return combined geometry set with attributes
  return(rbind(overlap, cb))
}
