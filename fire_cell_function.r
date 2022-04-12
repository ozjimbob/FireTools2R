# veg logic
log_it = function(strn){
  cat(paste0(Sys.time()," - ",strn),file=paste0(rast_temp,"/log.txt"),sep="\n",append=TRUE)
  print(paste0("[FT2] ",Sys.time()," - ",strn))
}

c_sfaz = as.numeric(c_sfaz)
current_year = as.numeric(current_year)
ras_res = as.numeric(ras_res)

getFreeMemoryKB <- function() {
  osName <- Sys.info()[["sysname"]]
  if (osName == "Windows") {
    x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
    x <- x[grepl("FreePhysicalMemory", x)]
    x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
    x <- gsub("\r", "", x, fixed = TRUE)
    return(as.integer(x)/1000000)
  } else if (osName == 'Linux') {
    x <- system2('free', args='-k', stdout=TRUE)
    x <- strsplit(x[2], " +")[[1]][4]
    return(as.integer(x)/1000000)
  } else {
    stop("Only supported on Windows and Linux")
  }
}

g_rasterize <- function(layer,filename,output,attribute="",otype="Int32"){
  if(attribute==""){
    paste0(gdal_rasterize," -burn 1 -l ",layer," -of GTiff ",
         "-te ",rex," -tr ",rres[1]," ",rres[2]," -ot ",otype," -co COMPRESS=PACKBITS -co TILED=TRUE -optim VECTOR -q ",
         paste0(rast_temp,"/",filename)," ",output)
  }else{
    paste0(gdal_rasterize," -a ",attribute," -l ",layer," -of GTiff ",
           "-te ",rex," -tr ",rres[1]," ",rres[2]," -ot ",otype," -co COMPRESS=PACKBITS -co TILED=TRUE -optim VECTOR -q ",
           paste0(rast_temp,"/",filename)," ",output)
  }
}


g_polygonize <- function(layer,filename,output,attribute="",otype="Int32"){
  if(attribute==""){
    paste0(gdal_rasterize," -burn 1 -l ",layer," -of GTiff ",
           "-te ",rex," -tr ",rres[1]," ",rres[2]," -ot ",otype," -co COMPRESS=PACKBITS -co TILED=TRUE -optim VECTOR -q ",
           paste0(rast_temp,"/",filename)," ",output)
  }else{
    paste0(gdal_rasterize," -a ",attribute," -l ",layer," -of GTiff ",
           "-te ",rex," -tr ",rres[1]," ",rres[2]," -ot ",otype," -co COMPRESS=PACKBITS -co TILED=TRUE -optim VECTOR -q ",
           paste0(rast_temp,"/",filename)," ",output)
  }
}


proccell2_post_sdc = function(i,cyear=0,the_tmprast){
  
  if(cyear == 0){
    st = stack(file_list,quick=TRUE)
    r_vegmin = raster(paste0(veg_folder,"/r_vegmin.tif"),values=FALSE)
    r_vegmax= raster(paste0(veg_folder,"/r_vegmax.tif"),values=FALSE)
    r_timesburnt= raster(paste0(fire_folder,"/rNumTimesBurnt.tif"),values=FALSE)
    r_tsl= raster(paste0(fire_folder,"/rTimeSinceLast.tif"),values=FALSE)
    
    st = crop(st,the_tmprast)
    r_tsl = crop(r_tsl,the_tmprast)
    r_vegmin = crop(r_vegmin,the_tmprast)
    r_vegmax = crop(r_vegmax,the_tmprast)
    r_timesburnt = crop(r_timesburnt,the_tmprast)
  } else {
    
    reduced_year = all_years[all_years < cyear]
    to_keep = which(int_list %in% reduced_year)
    file_list = file_list[to_keep]
    int_list = int_list[to_keep]
    
    
    st = stack(file_list,quick=TRUE)
    r_vegmin = raster(paste0(veg_folder,"/r_vegmin.tif"),values=FALSE)
    r_vegmax= raster(paste0(veg_folder,"/r_vegmax.tif"),values=FALSE)
    r_timesburnt= raster(paste0(fire_folder,"/rNumTimesBurnt_",cyear,".tif"),values=FALSE)
    r_tsl= raster(paste0(fire_folder,"/rTimeSinceLast_",cyear,".tif"),values=FALSE)
    
    st = crop(st,the_tmprast)
    r_tsl = crop(r_tsl,the_tmprast)
    r_vegmin = crop(r_vegmin,the_tmprast)
    r_vegmax = crop(r_vegmax,the_tmprast)
    r_timesburnt = crop(r_timesburnt,the_tmprast)
    
  }
  
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
  
  for(j in seq_along(ovec)){
    # Rasters we need
    # r_MaxThresh - derived from veg + lookup table
    # r_MinThresh - derived from veg + lookup table
    # FireFrequency - from fire frequency raster (number of times burnt)
    # TSFF - Time Since First Fire value
    
    # output rasters:
    # r_Status (eg. no fire regime, too frequently burnt, vulnerable, long uburnt, unknown)
    
    
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


proccell2_post = function(i,cyear=0){
  
  if(cyear == 0){
    st = stack(file_list,quick=TRUE)
    r_vegmin = raster(paste0(veg_folder,"/r_vegmin.tif"),values=FALSE)
    r_vegmax= raster(paste0(veg_folder,"/r_vegmax.tif"),values=FALSE)
    r_timesburnt= raster(paste0(fire_folder,"/rNumTimesBurnt.tif"),values=FALSE)
    r_tsl= raster(paste0(fire_folder,"/rTimeSinceLast.tif"),values=FALSE)
  } else {
    
    reduced_year = all_years[all_years < cyear]
    to_keep = which(int_list %in% reduced_year)
    file_list = file_list[to_keep]
    int_list = int_list[to_keep]
    
    
    st = stack(file_list,quick=TRUE)
   
    r_vegmin = raster(paste0(veg_folder,"/r_vegmin.tif"),values=FALSE)
    r_vegmax= raster(paste0(veg_folder,"/r_vegmax.tif"),values=FALSE)
    r_timesburnt= raster(paste0(fire_folder,"/rNumTimesBurnt_",cyear,".tif"),values=FALSE)
    r_tsl= raster(paste0(fire_folder,"/rTimeSinceLast_",cyear,".tif"),values=FALSE)
  }
  
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
  
  for(j in seq_along(ovec)){
    # Rasters we need
    # r_MaxThresh - derived from veg + lookup table
    # r_MinThresh - derived from veg + lookup table
    # FireFrequency - from fire frequency raster (number of times burnt)
    # TSFF - Time Since First Fire value
    
    # output rasters:
    # r_Status (eg. no fire regime, too frequently burnt, vulnerable, long uburnt, unknown)
    
    
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



proccell2 = function(i){
  
  
  
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
  
  for(j in seq_along(ovec)){
    # Rasters we need
    # r_MaxThresh - derived from veg + lookup table
    # r_MinThresh - derived from veg + lookup table
    # FireFrequency - from fire frequency raster (number of times burnt)
    # TSFF - Time Since First Fire value
    
    # output rasters:
    # r_Status (eg. no fire regime, too frequently burnt, vulnerable, long uburnt, unknown)
    
    
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

tile_win <- function(infile,outdir,pypath=NULL){
  pypath = "C:/OSGeo4W64/bin/gdal2tiles.py"
  system2('C:\\OSGeo4W64\\OSGeo4W.bat',
        args=(sprintf('"%1$s" "%2$s" -q -f "%3$s" "%4$s.shp"', 
        pypath, infile, outdir)))
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
    cmd = ifelse(.Platform$OS=="windows", "C:/OSGeo4W64/OSGeo4W.bat", "python")
    #pypath <- Sys.which('C:/OSGeo4W64/bin/gdal_polygonize.py')
    pypath = "gdal_polygonize"
  }
  ## The line below has been commented:
  # if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.") 
  owd <- getwd()
  on.exit(setwd(owd))
 setwd("C:/OSGeo4W64/bin")
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
  #system2('C:\\OSGeo4W64\\OSGeo4W.bat',
  #        args=(sprintf('"%1$s" "%2$s" -q -f "%3$s.shp"', 
 #                       pypath, rastpath, outshape)))
 # system2(cmd,
 #         args=(sprintf('"%1$s" "%2$s" -q -f "%3$s.shp"', 
  #                      pypath, rastpath, outshape)))
  tcmd = paste(cmd,pypath,rastpath,paste0(outshape,".shp"))
  system(tcmd)
  unlink(f)
  if (isTRUE(readpoly)) {
    shp <- readOGR(paste0(outshape,".shp"),  verbose=!quietish)
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
    f <- tempfile(fileext='.tif')
    bigWrite(x, f)
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  
  system2('python', args=(sprintf('"%1$s" "%2$s" -q -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}

polygonize_by_name <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                        pypath=NULL, readpoly=TRUE, quietish=TRUE) {
  quiet = quietish
  
  print(paste0("Pre-normal path: ",x))
  rastpath <- normalizePath(x)
  print(paste0("Post-normal path: ",rastpath))
  
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
  

  system2('python', args=(sprintf('"%1$s" "%2$s" -q -f "%3$s" "%4$s.shp"',
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
  #log_it("Write Start")
  s2 <- writeStart(r, filename=out, format='GTiff', options="COMPRESS=LZW", overwrite=TRUE)
  #log_it("Getting Block Size")
  tr <- blockSize(r)
  #log_it(paste0("Looping through blocks:",tr$n))
  for (i in tr$n:1) {
    #log_it(paste("Block: ",i))
    v <- getValuesBlock(r, row=tr$row[i], nrows=tr$nrows[i])
    #log_it("Writing Values")
    s2 <- writeValues(s2, v, tr$row[i])
  }
  #log_it("Stop Write")
  s2 <- writeStop(s2)
 # log_it("Write done")
}

bigWriteBinary <- function(r,out){
  s2 <- writeStart(r, filename=out, format='GTiff', overwrite=TRUE,datatype="INT1U")
  tr <- blockSize(r)
  for (i in tr$n:1) {
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

tile_win <- function(infile,pypath=NULL){
  pypath = "C:/OSGeo4W64/bin/gdal2tiles.py"
  if(!file.exists(paste0(rast_temp,"/tiles"))){
    print("Making Dir")
    dir.create(paste0(rast_temp,"/tiles"))
  }
  
  infile_b = paste0(rast_temp,"/",infile)
  in_pfile = paste0(infile_b,".tif")
  in_cfile = paste0(infile_b,".vrt")
  outfile = paste0(rast_temp,"/tiles/",infile)
  dir.create(outfile)
  cmd="C:/OSGeo4W64/bin/gdal_translate.exe"
  #args=c("-of vrt","-expand rgba",in_pfile,in_cfile)
  #system2(cmd,args,wait=TRUE)
  #system2('C:\\OSGeo4W64\\OSGeo4W.bat',
  #        args=(sprintf('"%1$s" "%2$s" -w none -r near -a 255,255,255,255 -z 10-14 "%3$s"', 
  #                      pypath, normalizePath(in_cfile), normalizePath(outfile))),wait=TRUE)
  #unlink(in_cfile)
}

tile_linux <- function(infile,pypath=NULL){
  pypath = paste0(gdal_path,"gdal2tiles.py")
  if(!file.exists(paste0(rast_temp,"/tiles"))){
    print("Making Dir")
    dir.create(paste0(rast_temp,"/tiles"))
  }
  
  infile_b = paste0(rast_temp,"/",infile)
  in_pfile = paste0(infile_b,".tif")
  in_cfile = paste0(infile_b,".vrt")
  outfile = paste0(rast_temp,"/tiles/",infile)
  dir.create(outfile)
  cmd=paste0(gdal_path,"gdal_translate")
  #args=c("-of vrt","-expand rgba",in_pfile,in_cfile)
  #system2(cmd,args,wait=TRUE)
  #system2('python',
  #        args=(sprintf('"%1$s" "%2$s" -w none -r near -a 255,255,255,255 -z 10-14 "%3$s"', 
  #                      pypath, normalizePath(in_cfile), normalizePath(outfile))),wait=TRUE)
  #unlink(in_cfile)
}

remove_invalid_poly <- function(xx){
  to_fix=c()
  for(i in 1:nrow(xx)){
    mn <- min(sapply(st_geometry(xx)[[i]], function(x) nrow(x[[1]])))
    if(mn < 4){
      to_fix <- c(to_fix,i)
    }
  }
  
  for(fix_list in to_fix){
    aa = st_geometry(xx)[[fix_list]]
    to_remove = c()
    for(i in 1:length(aa)){
      nr <- nrow(aa[[i]][[1]])
      if(nr<4){
        
        to_remove <- c(to_remove,i)
      }
    }
    to_remove <- rev(to_remove)
    for(i in to_remove){
      st_geometry(xx)[[fix_list]][[i]]<-NULL
    }
  }
return(xx)
}


remove_invalid_poly_multi <- function(xx){
  for(i in 1:nrow(xx)){
    if(i %% 100 == 0){print(i)}
    this_geom = st_geometry(xx[i,])[[1]][[1]]
    to_check <- length(this_geom)
    to_fix = c()
    for(j in 1:to_check){
      if(nrow(this_geom[[j]]) <4){
        to_fix = c(to_fix,j)
      }
    }
    for(j in rev(to_fix)){
      st_geometry(xx[i,])[[1]][[1]][[j]] <- NULL
    }
  }
  return(xx)
}



# GDAL function to convert zero cells to NA
zero_raster <- function(x){
  
  log_it("Raster Zero to NA")
  infile = paste0(rast_temp,"/",x)
  tempfile = paste0(rast_temp,"/",x,".tmp")
  gt = Sys.which("gdal_translate")
  if(gt==""){
    gt="C:/OSGeo4W64/bin/gdal_translate.exe"
  }else{
    gt=paste0(gdal_path,"gdal_translate")
  }
  cmd=paste0(gt," -of GTiff -a_nodata 0 -co COMPRESS=LZW ",infile," ",tempfile)
  cout = system(cmd,intern=TRUE)
  log_it(cout)
  unlink(infile)
  file.rename(tempfile,infile)
  
  #gdal_translate -of GTiff -a_nodata 0 -co COMPRESS=LZW rLastYearBurnt_2018.tif test_lyb.tif
  
}


rx_write=function(file,outfile){
  require(foreign)
  require(sp)
  require(raster)
  
  log_it("Define lookup table")
  rtable = data.frame(ID = c(1,2,3,4,5,6,7,8,9,10),
                      Status = c("NoFireRegime",
                                 "TooFrequentlyBurnt",
                                 "Vulnerable",
                                 "LongUnburnt",
                                 "WithinThreshold",
                                 "Recently Treated",
                                 "Monitor OFH In the Field",
                                 "Priority for Assessment and Treatment",
                                 "Unknown",
                                 "Priority for Assessment and Treatment"))
  col_vec = c("#ffffff",
              "#ffffff","#ff0000","#ff6600","#00ffff","#999999","#99FF99","#226622","#00ff00","#cccccc")
  
  log_it("Load input file")
  tr <- raster(paste0(rast_temp,"/",file))
  
  if(exists("mask_tif")){
    log_it("Mask input file")
    tr <- tr * mask_tif
  }
  
  log_it("Ratify input file")
  tr <- ratify(tr)
  
  log_it("Get the levels of the raster table (eg. the numbers)")
  rat <- levels(tr)[[1]]
  
  log_it("Join levels with the text status table") 
  rat <- left_join(rat,rtable)
  
  log_it("Insert levels into the raster")
  levels(tr) <- rat
  
  log_it("Insert color table into raster")
  colortable(tr) <- col_vec
  
  # Write ESRI DB
  
  log_it("Construct ESRI table")
  log_it("Retrieve levels from existing raster table")
  atable = levels(tr)[[1]]
  names(atable)=c("VALUE","CATEGORY")
  
  log_it("Calculating value frequency table")
  x <- raster::freq(tr)
  x <- as.data.frame(x)
  names(x)=c("VALUE","COUNT")
  x$VALUE = as.numeric(as.character(x$VALUE))
  
  log_it("Joining frequency table")
  a2 = left_join(atable,x)
  a2$COUNT[is.na(a2$COUNT)]=0
  a2 = dplyr::select(a2,VALUE,COUNT,CATEGORY)
  
  log_it("Writing ESRI DBF")
  write.dbf(a2,paste0(rast_temp,"/",outfile,".vat.dbf"))
  
  # Fix projection
  log_it("Fix Projection")
  crs(tr) <- CRS('+init=epsg:3308')
  
  log_it("Big Write raster with table")
  bigWrite(tr,paste0(rast_temp,"/",outfile))
  
  log_it("Deleting temp file")
  unlink(paste0(rast_temp,"/",file))
  
  log_it("Cleaning up")
  x <- NULL
  a2 <- NULL
  tr <- NULL
  atable <- NULL
  
  rm(x)
  rm(a2)
  rm(tr)
  rm(atable)
  
  gc()
  
}

# Insert 3308 projection into TIF output
esri_output = function(tfile){
  log_it("Generating ESRI projection")
  infile = paste0(rast_temp,"/",tfile)
  tempfile = paste0(rast_temp,"/",tfile,".tmp")
  gt = Sys.which("gdal_translate")
  if(gt==""){
    gt="C:/OSGeo4W64/bin/gdal_translate.exe"
  }else{
    gt=paste0(gdal_path,"gdal_translate")
  }
  cmd=paste0(gt," ",infile," -a_srs 3308.prj -co COMPRESS=LZW -of GTiff ",tempfile)
  cout = system(cmd,intern=TRUE)
  log_it(cout)
  unlink(infile)
  file.rename(tempfile,infile)
}
