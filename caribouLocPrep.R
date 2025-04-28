defineModule(sim, list(
  name = "caribouLocPrep",
  description = "",
  keywords = c("Caribou","Location",""),
  authors = c(person("Julie", "Tuner", email = "", role = c("aut", "cre")),
              person("Rory", "McInnes", email = "", role = c("aut", "cre")),
              person("Eliot", "McIntire", email = "", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(caribouLocPrep = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "caribouLocPrep.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9002)", "ggplot2", 'data.table', 'readxl', 'fs', 'tidyr',
                  'purrr', 'janitor', 'raster','sf','googledrive', 'sfheaders', 'amt', 'Require',
                  'reproducible', 'move','dplyr', 'terra'),
  parameters = bindrows(
    defineParameter("urlToBCDataFolder", "character",
                    "https://drive.google.com/file/d/1isb5qtXhHEt728F_isunXVnefOIiIgny/view?usp=drive_link", NA, NA,
                    desc = "The URL to a folder where BC data is kept"),
    defineParameter("urlToSKDataFolder", "character",
                    "https://drive.google.com/drive/folders/18jdIk_62S73PRe1ZtX0JXxp1pxxbFPaX", NA, NA,
                    desc = "The URL to a folder where SK data is kept"),
    defineParameter("urlToMBDataFolder", "character",
                    "https://drive.google.com/drive/folders/1hdTIcpiMueewC1DZsJE1gW8R8mdG7CkF", NA, NA,
                    desc = "The URL to a folder where MB data is kept"),
    #should be a movebank url
    defineParameter("NWTMovebankID", "character",
                    "", NA, NA,
                    desc = "The MoveBank study ID where NTW is kept"),
    defineParameter("YTMovebankID", "character",
                    "3023885998", NA, NA,
                    desc = "The MoveBank study ID where YT data is kept"),
    defineParameter("MoveBankUser", "character",
                    "", NA, NA,
                    desc = "The MoveBank user ID to access studies stored on MoveBank"),
    defineParameter("MoveBankPass", "character",
                    "", NA, NA,
                    desc = "The MoveBank user password to access studies stored on MoveBank"),
    defineParameter("jurisdiction", "character",c("BC","SK","MB","YT"),
                    desc = "A list of jurisdictions to run"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "boo", objectClass = "list",
                 desc = "This must be a list of data.table objects with ... ", sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...)
    createsOutput(objectName = "caribouLoc", objectClass = "data.frame", 
                  desc = "Harmonized and cleaned caribou locations of all jurisdictions provided"),
    createsOutput(objectName = "studyareaFullextent", objectClass = "vector",
                  desc = "a single polygon derived from the full extent of caribou locations")
  )
))

doEvent.caribouLocPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # run data harmonization
      sim <- Init(sim)
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  #harmonize the jurisdictions provided and remove erroneous points
  dat.all <- list()
  if ("BC" %in% Par$jurisdiction == TRUE){
    bc<- sim$boo$BC
    bc[,any(duplicated(datetime)), by = id]
    bc <- unique(bc, by = c('id', 'datetime'))
    bc[,Npts := .N, by = .(id)]
    bc <- bc[Npts>2] # remove those with only 1 point
    dat.all[[length(dat.all)+1]] <- (bc[,.(id, jurisdiction = 'bc', pop = Population_Unit, subpop = NA, datetime, x, y)])
  }
  if ("SK" %in% Par$jurisdiction == TRUE){
    sk<- sim$boo$SK
    sk[,any(duplicated(datetime)), by = id]
    sk <- unique(sk, by = c('id', 'datetime'))
    #add in the line to remove those with only 1 point
    dat.all[[length(dat.all)+1]] <- (sk[,.(id, jurisdiction = 'sk', pop = 'SK1', subpop = NA, datetime, x, y)])
  }
  if ("MB" %in% Par$jurisdiction == TRUE){
    mb<- sim$boo$MB
    mb[,any(duplicated(datetime)), by = id]
    mb <- unique(mb, by = c('id', 'datetime'))
    mb[,Npts := .N, by = .(id)]
    mb <- mb[Npts>2] # remove those with only 1  point
    dat.all[[length(dat.all)+1]] <- (mb[,.(id, jurisdiction = 'mb', pop = Range, subpop = NA, datetime, x, y)])
  }
  if ("YT" %in% Par$jurisdiction == TRUE){
    yt<- sim$boo$YT
    yt[,any(duplicated(datetime)), by = id]
    yt <- unique(yt, by = c('id', 'datetime'))
    #add in the line to remove those with only 1 point
    dat.all[[length(dat.all)+1]] <- (yt[,.(id, jurisdiction = 'yt', pop = "Yukon", subpop = NA, datetime, x, y)])
  }
  if ("NT" %in% Par$jurisdiction == TRUE){
    nt<- sim$boo$NT
    ## check fix rates ####
    #check for duplicated time stamps
    nt[,any(duplicated(datetime)), by = id]
    #We have some duplicated time stamps, these need to be removed prior to creating a track.
    nt <- unique(nt, by = c('id', 'datetime'))
    dat.all[[length(dat.all)+1]] <- (nt[,.(id, jurisdiction = 'nwt', pop = area, subpop = habitat, datetime, x, y)])
  }
  dat.bind <- do.call("rbind",dat.all)
  ### remove crazy points in Russia (??) ----
  dat.clean <- dat.bind[complete.cases(x,y, datetime) & between(x, -1665110, 0) &between(y, -98940, 2626920)]
  
  # Save clean data ----
  sim$booALL <- dat.clean
  # create study area and buffered study area ----
  coords<- dat.clean%>%st_as_sf(coords = c('x','y'))%>%
    st_set_crs(st_crs(3978))
  
  ## 100 km buffer around points to get an idea of extent of study area 
  ## A convex hull of all the polygons creates a single polygon for the simulation
  ## This requires a lot of computing power, run the script on a HPC
  studyArea.buff <- st_buffer(coords, dist = 100000)
  studyArea.sf <- st_as_sf(studyArea.buff)
  studyArea.ch <- st_convex_hull(studyArea.sf)
  
  sim$studyareaFullextent <- studyArea.ch
  ## save buffered study area ----
  
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  #the login required to access data on Move Bank for YT and NWT
  loginStored <- movebankLogin(username=Par$MoveBankUser, 
                               password=Par$MoveBankPass)
  
  #run the jurisdictional data prep scripts for the supplied jurisdictions
  if (!suppliedElsewhere("boo", sim = sim)) {
    sim$boo <- list()
    if ("BC" %in% Par$jurisdiction == TRUE) {
      #download just the .gdb files from google drive
      bc_kmb <- prepInputs(url = Par$urlToBCDataFolder,
                           targetFile = "telem_data_request_20211026.gdb.zip",
                           destinationPath = dPath, fun = terra::vect(x = targetFile,layer="KMB_Local_Telemetry_20211026"))
      bc_reg <- prepInputs(url = Par$urlToBCDataFolder,
                           targetFile = "telem_data_request_20211026.gdb.zip",
                           destinationPath = dPath, fun = terra::vect(x = targetFile,layer="Regional_Telemetry_20211026"))
      
      sim$boo[["BC"]] <- prepInputs(destinationPath = dPath,
                                    fun = dataPrep_BC(dPath=dPath, bc_kmb, bc_reg))
    }
    if ("SK" %in% Par$jurisdiction == TRUE){
      #download the spreadsheets of points
      sim$boo[["SK"]] <- prepInputs(url = Par$urlToSKDataFolder,
                                    destinationPath = dPath,
                                    fun = dataPrep_SK(dPath = dPath))
    }
    if ("MB" %in% Par$jurisdiction == TRUE){
      #download the .gdb files (there are many so this prepInputs takes more time)
      sim$boo[["MB"]] <- prepInputs(url = Par$urlToMBDataFolder,
                                    destinationPath = dPath,
                                    archive = NA,
                                    fun = dataPrep_MB(dPath = dPath))
    }
    if ("YT" %in% Par$jurisdiction == TRUE){
      #use movebank to access the data
      print("Access to YT data requires a Move Bank account, ensure you have collaboration rights to the study")
      sim$boo[["YT"]] <- prepInputs(fun = dataPrep_YT(loginStored))
    }
    if ("NT" %in% Par$jurisdiction == TRUE){
      #use movebank to access the data
      print("Access to NT data requires a Move Bank account, ensure you have collaboration rights to the study")
      sim$boo[["NWT"]] <- prepInputs(fun = dataPrep_NT(loginStored))
    }
  }
  return(invisible(sim))
}

spaceless <- function(x) {colnames(x) <- gsub(" ", "", colnames(x));x}

#i think breaking these out into individual scripts would work well and have cleaner module code
dataPrep_SK <- function(dPath =dpath) {
  ### Input data ----
  sk <- dPath
  
  # list of files
  ls_sk <- as.character(dir_ls(path = sk, regexp = "Telemetry"))
  
  temp <- data.table()
  temp <- rbindlist(lapply(seq(1:length(ls_sk)), function(i){
    temp[,.(file= ls_sk[[i]],
            data = list(setDT(read_excel(ls_sk[[i]], sheet = 'TelemetryData', skip = 3))))]
  }))
  
  temp$file
  
  #### gather just needed data ####
  colnames(temp[1]$data[[1]])
  str(temp[1]$data[[1]]$`Sample Date                              (yyyy-mm-dd)`)
  spaceless(temp[1]$data[[1]])
  
  prep_sk <- lapply(ls_sk, function(ll){
    temp[file == ll,.(file,
                      spaceless(data[[1]][,.SD, .SDcols = names(data[[1]]) %like% 'Sample|Latitude|Longitude|UTM|Datum|Individual|Comments & !Sensitive']))]
  })
  #standardize the column names
  colnames(prep_sk[[1]])
  newnames <- c('file','date','time','datum','lat','long','UTMzone','northing','easting','id')
  for (dd in 1:length(ls_sk)) {
    setnames(prep_sk[[dd]], old = colnames(prep_sk[[dd]]), new = newnames)
    prep_sk[[dd]][,`:=`(date = as.IDate(convert_to_date(date)))]
    prep_sk[[dd]][,time:=as.ITime(as.character(time)) ]
    prep_sk[[dd]][,datetime:= as.POSIXct(paste(date,time, sep = ' '))]
    prep_sk[[dd]][,lat:= as.numeric(lat)]
    prep_sk[[dd]][,long:= as.numeric(long)]
  }
  
  #check for complete entries and remove any incomplete data
  dat_sk <- rbindlist(prep_sk)
  dat_sk <- dat_sk[complete.cases(long,lat, datetime)]
  dat_sk <- dat_sk[long<0&lat>0]
  # convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
  crs <- CRS(st_crs(4326)$wkt)
  outcrs <- st_crs(3978)
  sfboo <- st_as_sf(dat_sk, coords = c('long', 'lat'),
                    crs = crs)
  outboo <- st_transform(sfboo, outcrs)
  booSK <- setDT(sfheaders::sf_to_df(outboo, fill = T))
  
  return(booSK)
}

dataPrep_BC <- function(dPath = dPath, bc_kmb, bc_reg) {
  ### Input data ----
  kmb <- bc_kmb
  regional <- bc_reg
  
  ### Prep data ----
  # checking for right formats and grabbing what need
  kmb.dt <- as.data.table(kmb)
  kmb.dt <- kmb.dt[!(is.na(Animal_ID)) & Animal_ID != 'None',.(id = Animal_ID, Region, Population_Unit, 
                                                               datetime = GMT_FixDateTime, Longitude, Latitude)]
  regional.dt <- as.data.table(regional)
  regional.dt[, datetime := FixDateTime + hours(8)]
  regional.dt <- regional.dt[!(is.na(Animal_ID)) & Animal_ID != 'None',.(id = Animal_ID, Region, Population_Unit, 
                                                                         datetime, Longitude, Latitude)]
  dat<- rbind(kmb.dt, regional.dt)
  #check for complete entries and remove any incomplete data
  dat_cleaner <- dat[complete.cases(Longitude,Latitude, datetime)]
  
  
  ### convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
  crs <- st_crs(4326)$wkt
  outcrs <- st_crs(3978)
  
  sfboo <- st_as_sf(dat_cleaner, coords = c('Longitude', 'Latitude'),
                    crs = crs)
  outboo <- st_transform(sfboo, outcrs)
  booBCnc <- setDT(sfheaders::sf_to_df(outboo, fill = T))
  
  
  
  ### EXPLORE ----
  # check if all observations are complete
  all(complete.cases(booBCnc[,.(x,y, datetime)]))
  
  
  # check for duplicated time stamps
  booBCnc[,any(duplicated(datetime)), by = id]
  
  # We have some duplicated time stamps, these need to be removed prior to creating a track.
  DT <- unique(booBCnc, by = c('id', 'datetime'))
  booBC <- DT
  
  return(booBC)
  
}

#no movebank data here
dataPrep_NT <- function(loginStored) {
  ### Input data ----
  browser()
  # NWT dataset names to fill in loop
  dsNames <-c('Dehcho Boreal Woodland Caribou', 'North Slave Boreal Caribou', 
              'Sahtu Boreal Woodland Caribou', 'Sahtu Boreal Woodland Caribou (2020)',
              'South Slave Boreal Woodland Caribou') 
  # make a list of all data from Movebank
  # TODO: update this with prepInputs()
  nt.move <- list()
  for (ds in 1:length(dsNames)) {
    #hh = 1
    nt.move[[ds]]<-getMovebankLocationData(study =paste0( 'GNWT ', dsNames[[ds]]),
                                           login = loginStored, removeDuplicatedTimestamps=TRUE)
  }
  #saveRDS(nwt.move, paste0(raw.nwt, 'NWTmoveDat.RDS'))
  
  # pull just the data
  hab <-c('dehcho', 'north.slave', 'sahtu', 'south.slave') 
  
  
  nt <- rbindlist(lapply(1:length(hab), function(hh){
    nt[,.(area=hab[[hh]], dat=list(setDT(nt.move[[hh]]@data)))]
  })
  )
  
  # gathering just the columns needed
  nt.long <- rbindlist(lapply(1:length(hab), function(hh){
    nt$dat[[hh]][,.(area = hab[[hh]], habitat, id=tag_id, 
                    location_long, location_lat, datetime = timestamp)]
    
  }))
  
  # convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
  crs <- CRS(st_crs(4326)$wkt)
  outcrs <- st_crs(3978)
  
  sfboo <- st_as_sf(nt.long, coords = c('location_long', 'location_lat'),
                    crs = crs)
  outboo <- st_transform(sfboo, outcrs)
  sim$booNT <- setDT(sfheaders::sf_to_df(outboo, fill = T))
  return(booNT)
}

dataPrep_YT <- function(loginStored) {
  ### Input data ----
  dat <- getMovebankLocationData(study="Boreal Caribou - Yukon Collars", login=loginStored, 
                                 removeDuplicatedTimestamps=TRUE)
  ### Prep data ----
  dat <- setDT(dat)
  dat_cleaner <- dat[complete.cases(location.long,location.lat, timestamp)]
  data.table::setnames(dat_cleaner, "timestamp", "datetime")
  ### convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
  crs <- st_crs(4326)$wkt
  outcrs <- st_crs(3978)
  
  sfboo <- st_as_sf(dat_cleaner, coords = c('location.long', 'location.lat'),
                    crs = crs)
  outboo <- st_transform(sfboo, outcrs)
  boo_notclean <- setDT(sfheaders::sf_to_df(outboo, fill = T))
  
  ### standarize names and columns ----
  booYT <- boo_notclean[, .(id = individual.id, datetime, x, y, argos.altitude, gps.fix.type.raw)]
  
  ### EXPLORE ----
  # check if all observations are complete
  booComplete <- all(complete.cases(booYT[,.(x,y, datetime)]))
  return(booYT)
}

dataPrep_MB <- function(dPath = dPath) {
  ### Input data ----
  raw.mb <- dPath
  #unzip all of the .gdb files
  MI_Berens <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb.zip'), "MI_Berens_Caribou_2020APR02_CLEAN_NAD83_Z14")
  BERENS_RND <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb.zip'), "BERENS_RND_GPS_2001_to_2004_complete_NAD83Z14")
  BLDVN <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb.zip'), "BLDVN_2000_to_2014_complete_NAD83Z14")
  ATIKO <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb.zip'), "ATIKO_2000_to_2014_complete_NAD83Z14")
  
  interlake <- st_read(file.path(raw.mb, 'CaribouGPSData_Interlake.gdb.zip'), "Interlake_Total_Jan2021")
  
  MI_NWH <- st_read(file.path(raw.mb, 'CaribouGPSdata_Molson.gdb.zip'), "MI_NWH_Caribou_Telemetry2020APR02_Clean_NAD83")
  charron1 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Molson.gdb.zip'), "CharronLK_GPS_MBHYDRO_Q2_2010_072020")
  charron2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Molson.gdb.zip'), "CharronLK_GPS_MBHYDRO_Q4_2010_022021")
  
  # imperial doesn't have enough data for an SSA, it's just VHF
  MBhydro <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "MBHYDRO_Q2_2010_072020")
  naosap1 <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "Naosap_Reed_Total_NAD83_July2018")
  naosap2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "gps_naosap_2002_06")
  kississing <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "Kississing_Total_NAD83_July2018")
  #imperial <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "Imperial_NAD83_2011")
  naosap3 <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "NaosapReed_MBHYDRO_Q4_2010_022021")
  
  flintstone <- st_read(file.path(raw.mb, 'CaribouGPSdata_Owl_Flintstone.gdb.zip'), "Owl_Flintstone_1995_2018_complete_WGS84")
  
  wimwap1<- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Wim_Wap_GPS_MBHydro_Q2_2010_072020")
  wimwap2 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Wim_Wap_GPS_MBHYDRO_Q4_2010_022021")
  harding1 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Harding_GPS_MBHydro_Q2_2010_072020")
  harding2 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Harding_GPS_MBHYDRO_Q4_2010_022021")
  wheadon1 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Wheadon_GPS_MBHydro_Q2_2010_072020")
  wheadon2 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Wheadon_Total_NAD83_Nov2018_Final")
  wheadon3 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Wheadon_GPS_MBHYDRO_Q4_2010_022021")
  
  bog1<- st_read(file.path(raw.mb, 'CaribouGPSdata_TheBog.gdb.zip'), "TheBog_MH_2010_072020")
  bog2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_TheBog.gdb.zip'), "TheBog_MBHYDRO_Q4_2010_022021")
  
  william <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb.zip'), "William_Lake_Total_NAD83_Dec2019")
  wabowden1 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb.zip'), "Wabowden_GPS_MH_Q2_2010_072020")
  wabowden2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb.zip'), "Wabowden_UHF_relocations_2009to2012_NAD83")
  wabowden3 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb.zip'), "Wabowden_GPS_MBHYDRO_Q4_2010_022021")
  
  ### Prep data ----
  ### convert to NAD83/Canada Atlas Lambert (need units to be m and consistent across)
  outcrs <- st_crs(3978)
  
  out_MI_Berens <- st_transform(MI_Berens, outcrs)
  out_BERENS_RND <- st_transform(BERENS_RND, outcrs)
  out_BLDVN <- st_transform(BLDVN, outcrs)
  out_ATIKO <- st_transform(ATIKO, outcrs)
  
  out_interlake <- st_transform(interlake, outcrs)
  
  out_MI_NWH <- st_transform(MI_NWH, outcrs)
  out_charron1 <- st_transform(charron1, outcrs)
  out_charron2 <- st_transform(charron2, outcrs)
  
  out_MBhydro <- st_transform(MBhydro, outcrs)
  out_naosap1 <- st_transform(naosap1, outcrs)
  out_naosap2 <- st_transform(naosap2, outcrs)
  out_naosap3 <- st_transform(naosap3, outcrs)
  out_kississing <- st_transform(kississing, outcrs)
  #out_imperial <- st_transform(imperial, outcrs)
  
  out_flintstone <- st_transform(flintstone, outcrs)
  
  out_wimwap1 <- st_transform(wimwap1, outcrs)
  out_wimwap2 <- st_transform(wimwap2, outcrs)
  out_harding1 <- st_transform(harding1, outcrs)
  out_harding2 <- st_transform(harding2, outcrs)
  out_wheadon1 <- st_transform(wheadon1, outcrs)
  out_wheadon2 <- st_transform(wheadon2, outcrs)
  out_wheadon3 <- st_transform(wheadon3, outcrs)
  
  out_bog1 <- st_transform(bog1, outcrs)
  out_bog2 <- st_transform(bog2, outcrs)
  
  out_william <- st_transform(william, outcrs)
  out_wabowden1 <- st_transform(wabowden1, outcrs)
  out_wabowden2 <- st_transform(wabowden2, outcrs)
  out_wabowden3 <- st_transform(wabowden3, outcrs)
  
  
  # checking for right formats and grabbing what need
  DT_MI_Berens <- setDT(sfheaders::sf_to_df(out_MI_Berens, fill = T))
  DT_MI_Berens <- DT_MI_Berens[,.(id = Animal_ID, Fix_Status, Range,
                                  datetime = as.POSIXct(paste(paste(GMT_Year, GMT_Month, GMT_Day, sep = '-'),
                                                              paste(GMT_Hour, GMT_Minute, sep = ':'), sep = ' '),
                                                        tz = 'gmt', format = '%Y-%m-%d %H:%M'),
                                  x, y)]
  # TODO what is time tz for BERENS_RND, BLDVN?
  DT_BERENS_RND <- setDT(sfheaders::sf_to_df(out_BERENS_RND, fill = T))
  DT_BERENS_RND <- DT_BERENS_RND[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                                    datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'), 
                                                                TIME, sep = ' '), 
                                                          format = '%Y-%m-%d %H:%M:%OS'),
                                    x, y)]
  DT_BLDVN <- setDT(sfheaders::sf_to_df(out_BLDVN, fill = T))
  DT_BLDVN <-DT_BLDVN[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                         datetime = as.POSIXct(paste(gsub( " .*$", "", DATE ), 
                                                     TIME, sep = ' '), 
                                               format = '%Y-%m-%d %H:%M:%OS'),
                         x, y)]
  DT_ATIKO <- setDT(sfheaders::sf_to_df(out_ATIKO, fill = T))
  DT_ATIKO <- DT_ATIKO[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                          datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'), 
                                                      TIME, sep = ' '), 
                                                format = '%Y-%m-%d %H:%M:%OS'),
                          x, y)]
  
  
  DT_interlake <- setDT(sfheaders::sf_to_df(out_interlake, fill = T))
  DT_interlake <- DT_interlake[,.(id = Animal_ID, Fix_Status, Range,
                                  datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                              paste(Hour, Minute, Second, sep = ':'), sep = ' '), 
                                                        format = '%Y-%m-%d %H:%M:%OS', tz = 'America/Chicago'),
                                  x, y)]
  
  DT_MI_NWH <- setDT(sfheaders::sf_to_df(out_MI_NWH, fill = T))
  DT_MI_NWH <- DT_MI_NWH[,.(id = Animal_ID, Fix_Status, Range = RANGE,
                            datetime = as.POSIXct(paste(paste(GMT_Year, GMT_Month, GMT_Day, sep = '-'),
                                                        paste(GMT_Hour, GMT_Minute, sep = ':'), sep = ' '),
                                                  tz = 'gmt', format = '%Y-%m-%d %H:%M'),
                            x, y)]
  DT_charron1 <- setDT(sfheaders::sf_to_df(out_charron1, fill = T))
  # what is the tz for Charron 1 and 2?
  DT_charron1 <- DT_charron1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
  DT_charron2 <- setDT(sfheaders::sf_to_df(out_charron2, fill = T))
  DT_charron2 <- DT_charron2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
  
  DT_MBhydro <- setDT(sfheaders::sf_to_df(out_MBhydro, fill = T))
  DT_MBhydro <- DT_MBhydro[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_naosap1 <- setDT(sfheaders::sf_to_df(out_naosap1, fill = T))
  DT_naosap1 <- DT_naosap1[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                              datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'), 
                                                          paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_naosap2 <- setDT(sfheaders::sf_to_df(out_naosap2, fill = T))
  DT_naosap2 <- DT_naosap2[,.(id = UNIQUE_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                              datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'), 
                                                          paste(HOUR, MINUTE, SECOND, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_naosap3 <- setDT(sfheaders::sf_to_df(out_naosap3, fill = T))
  DT_naosap3 <- DT_naosap3[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_kississing <- setDT(sfheaders::sf_to_df(kississing, fill = T))
  DT_kississing <- DT_kississing[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                                    datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'), 
                                                                paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '), 
                                                          format = '%Y-%m-%d %H:%M:%OS'),
                                    x=-1*x, y)]
  # Don't use imperial, not enough info for SSA
  #DT_imperial <- setDT(sfheaders::sf_to_df(out_imperial, fill = T))
  
  DT_flintstone <- setDT(sfheaders::sf_to_df(out_flintstone, fill = T))
  DT_flintstone <- DT_flintstone[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range,
                                    datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'), 
                                                                TIME, sep = ' '), 
                                                          format = '%Y-%m-%d %H:%M:%OS'),
                                    x, y)]
  
  DT_wimwap1 <- setDT(sfheaders::sf_to_df(out_wimwap1, fill = T))
  DT_wimwap1 <- DT_wimwap1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_wimwap2 <- setDT(sfheaders::sf_to_df(out_wimwap2, fill = T))
  DT_wimwap2 <- DT_wimwap2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_harding1 <- setDT(sfheaders::sf_to_df(out_harding1, fill = T))
  DT_harding1 <- DT_harding1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
  DT_harding2 <- setDT(sfheaders::sf_to_df(harding2, fill = T))
  DT_harding2 <- DT_harding2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
  DT_wheadon1 <- setDT(sfheaders::sf_to_df(out_wheadon1, fill = T))
  DT_wheadon1 <- DT_wheadon1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
  DT_wheadon2 <- setDT(sfheaders::sf_to_df(out_wheadon2, fill = T))
  DT_wheadon2 <- DT_wheadon2[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                                datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'), 
                                                            paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '), 
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x=x, y)]
  DT_wheadon3 <- setDT(sfheaders::sf_to_df(out_wheadon3, fill = T))
  DT_wheadon3 <- DT_wheadon3[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
  
  DT_bog1 <- setDT(sfheaders::sf_to_df(out_bog1, fill = T))
  DT_bog1 <- DT_bog1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                        datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                    paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                              format = '%Y-%m-%d %H:%M:%OS'),
                        x, y)]
  DT_bog2 <- setDT(sfheaders::sf_to_df(out_bog2, fill = T))
  DT_bog2 <- DT_bog2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                        datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                    paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                              format = '%Y-%m-%d %H:%M:%OS'),
                        x, y)]
  
  DT_william <- setDT(sfheaders::sf_to_df(william, fill = T))
  DT_william <- DT_william[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                              datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'), 
                                                          paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '), 
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x=-1 *x, y)]
  DT_wabowden1 <- setDT(sfheaders::sf_to_df(out_wabowden1, fill = T))
  DT_wabowden1 <- DT_wabowden1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                  datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'), 
                                                              paste(Hour, 00, 00, sep = ':'), sep = ' '), 
                                                        format = '%Y-%m-%d %H:%M:%OS'),
                                  x, y)]
  DT_wabowden2 <- setDT(sfheaders::sf_to_df(out_wabowden2, fill = T))
  DT_wabowden2 <- DT_wabowden2[,.(id = Unique_id, Fix_Status = NAV, Range = Population,
                                  datetime = as.POSIXct(paste(paste(Year, sprintf("%02d", Month), sprintf("%02d", Day), sep = '-'), 
                                                              paste(sprintf("%02d", Hour), sprintf("%02d", Minute), sprintf("%02d", Second), sep = ':'), sep = ' '), 
                                                        format = '%Y-%m-%d %H:%M:%OS'),
                                  x, y)]
  DT_wabowden3 <- setDT(sfheaders::sf_to_df(out_wabowden3, fill = T))
  DT_wabowden3 <- DT_wabowden3[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                  datetime = as.POSIXct(paste(paste(Year, sprintf("%02d", Month), sprintf("%02d", Day), sep = '-'), 
                                                              paste(sprintf("%02d", Hour), 00, 00, sep = ':'), sep = ' '), 
                                                        format = '%Y-%m-%d %H:%M:%OS'),
                                  x, y)]
  
  mb.dt <- rbind(DT_ATIKO, DT_BERENS_RND, DT_BLDVN, DT_bog1, DT_bog2, DT_charron1, DT_charron2,
                 DT_flintstone, DT_harding1, DT_harding2, DT_interlake, DT_kississing, DT_MBhydro,
                 DT_MBhydro, DT_MI_Berens, DT_MI_NWH, DT_naosap1, DT_naosap2, DT_naosap3,
                 DT_wabowden1, DT_wabowden2, DT_wabowden3, DT_wheadon1, DT_wheadon2, DT_wheadon3,
                 DT_william, DT_wimwap1, DT_wimwap2)
  
  # right now quick and dirty way to deal with outliers
  dat_cleaner <- mb.dt[complete.cases(x,y, datetime) & x < 0]
  booMB <- dat_cleaner
  
  return(booMB)
}

#add ON