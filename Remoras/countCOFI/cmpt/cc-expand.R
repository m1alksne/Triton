#########################################################
############### CountCOFI  Expansion ####################
#########################################################
# STEP 1: enter the date you would like to compile and expand
ccdate = 20230703
#########################################################
# STEP 2: select all (ctrl + a)
# STEP 3: run the code (control + enter on windows)
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
cc.expand <- function(ccdate,wd="G:/"){
  #wd <-  "/Users/eric.keen/Dropbox/"  # Mac
  wd <-  "L:/"  # Dell
  readpath = "Shared drives/MBARC_All/CalCOFI/data/countCOFIfiles_2016-2023/2023-07" ; readpath
  writepath = "Shared drives/MBARC_All/CalCOFI/data/countCOFIfiles_2016-2023/2023-07/expanded"
  setwd(paste0(wd,readpath))
  lf <- list.files() ; lf
  dates <- substr(lf,4,11) ; dates
  match <- which(dates==ccdate) ; match
  cc <- data.frame()
  for(m in 1:length(match)){
    filename <- lf[match[m]]
    setwd(paste0(wd,readpath))
    ccm <- read.csv(filename,stringsAsFactors=FALSE)
    cc <- rbind(cc,ccm)
  }
  names(cc)[1] <- "ev" ; head(cc)
  #######################################################
  #######################################################
  ####  Prep detail columns
  #######################################################
  details <- cc[,7:ncol(cc)] ; head(details)

  #######################################################
  #######################################################
  ####  Create Index row
  #######################################################
  cc$EID <- 1:nrow(cc) # (works for PBSmapping package too)
  #######################################################
  #######################################################
  ####  Prep master columns
  #######################################################
  ev <- as.character(cc$ev) ; ev
  when <- strftime(cc$date,format="%Y-%m-%d %H:%M:%S") ; when
  X <- round(as.numeric(as.character(cc$X)),digits=8) ; X
  Y <- round(as.numeric(as.character(cc$Y)),digits=8) ; Y
  spd <- sprintf("%4.f",round(as.numeric(as.character(cc$spd)),digits=1)) ; spd
  hdg <- sprintf("%3.f",round(as.numeric(as.character(cc$hdg)))) ; hdg

  #######################################################
  #######################################################
  ####  Prep EFFORT
  #######################################################
  cceff <- cc[cc$ev=="EFF",] ; cceff
  starts <- cceff$EID ; starts# set up starting cues for periods of same effort
  ends <- c((starts[2:length(starts)]-1),nrow(cc)) ; ends # setup ending cues
  cues <- data.frame(starts,ends) ; cues
  cues <- cues[cues$starts!=cues$ends,] ; cues
  eff <- trn <- port <- star <- cruise <- vessel <- rep(NA,times=nrow(cc))
  for(i in 1:nrow(cues)){
    from <- cues$starts[i]
    to <- cues$ends[i]
    eff[from:to] <- format(as.character(cceff$X1[cceff$EID==from]),width=2)
    trn[from:to] <- format(as.character(cceff$X2[cceff$EID==from]),width=2)
    port[from:to] <- format(as.character(cceff$X3[cceff$EID==from]),width=3)
    star[from:to] <- format(as.character(cceff$X4[cceff$EID==from]),width=3)
    cruise[from:to] <- as.character(cceff$X5[cceff$EID==from])
    vessel[from:to] <- as.character(cceff$X6[cceff$EID==from])
  }
  eff
  trn
  port
  star
  cruise
  vessel
  #######################################################
  #######################################################
  ####  Prep CONDITIONS
  #######################################################
  ccsea <- cc[cc$ev=="SEA",] ; head(ccsea)
  starts <- ccsea$EID # set up starting cues for periods of same effort
  ends <- c((starts[2:length(starts)]-1),nrow(cc)) # setup ending cues
  cues <- data.frame(starts,ends) ; cues

  qual <- vis <- precip <- cloud <- glareL <- glareR <- glareS <- wind.dir <- wind.spd <- bft <- swell <- rep(NA,times=nrow(cc))
  for(i in 1:nrow(cues)){
    from <- cues$starts[i]
    to <- cues$ends[i]
    qual[from:to] <- format(as.character(ccsea$X1[ccsea$EID==from]),width=2)
    vis[from:to] <- format(as.character(ccsea$X2[ccsea$EID==from]),width=3)
    precip[from:to] <- format(as.character(ccsea$X3[ccsea$EID==from]),width=3)
    glareS[from:to] <- format(as.character(ccsea$X7[ccsea$EID==from]),width=2)
    wind.dir[from:to] <- format(as.character(ccsea$X8[ccsea$EID==from]),width=3)
    wind.spd[from:to] <- format(as.character(ccsea$X9[ccsea$EID==from]),width=4)
    bft[from:to] <- format(as.character(ccsea$X10[ccsea$EID==from]),width=1)
    swell[from:to] <- format(as.character(ccsea$X11[ccsea$EID==from]),width=3)
  }
  qual
  vis
  precip
  glareS
  wind.dir
  wind.spd
  bft
  swell
  
  ## INTERPOLATE VALUES for CLOUD and GLARE
  if(nrow(cues)>1){  
    # Set up interpolation cues
    cloudfrom <- ccsea$X4[ccsea$EID==cues$starts] ; cloudfrom
    cloudto <- c(cloudfrom[2:length(cloudfrom)],cloudfrom[length(cloudfrom)]) ; cloudto

    gLfrom <- ccsea$X5[ccsea$EID==cues$starts] 
    gLto <- c(gLfrom[2:length(gLfrom)],gLfrom[length(gLfrom)])
  
    gRfrom <- ccsea$X6[ccsea$EID==cues$starts]
    gRto <- c(gRfrom[2:length(gRfrom)],gRfrom[length(gRfrom)])

    # Perform interpolation
    for(i in 1:nrow(cues)){
      from <- cues$starts[i]
      to <- cues$ends[i]   
      places <- (to - from) + 1
      ##################################
      # CLOUD
      cldfrom <- as.numeric(cloudfrom[i])
      cldto <- as.numeric(cloudto[i])
      if(is.na(cldto)){
        if(is.na(cldfrom)){cldseg <- rep(NA,times=places)}else{cldseg <- rep(cldfrom,times=places)}
      }else{
        if(is.na(cldfrom)){cldseg <- rep(NA,times=places)}else{cldseg <- seq(cldfrom,cldto,length=places)}
      }
      ##################################
      # GLARE L
      Lfrom <- as.numeric(gLfrom[i])
      Lto <- as.numeric(gLto[i])
      if(is.na(Lto)){
        if(is.na(Lfrom)){Lseg <- rep(NA,times=places)}else{Lseg <- rep(Lfrom,times=places)}
      }else{
        if(is.na(Lfrom)){Lseg <- rep(NA,times=places)}else{Lseg <- seq(Lfrom,Lto,length=places)}
      }  
      ##################################
      # GLARE R
      Rfrom <- as.numeric(gRfrom[i])
      Rto <- as.numeric(gRto[i])
      if(is.na(Rto)){
        if(is.na(Rfrom)){Rseg <- rep(NA,times=places)}else{Rseg <- rep(Rfrom,times=places)}
      }else{
        if(is.na(Rfrom)){Rseg <- rep(NA,times=places)}else{Rseg <- seq(Rfrom,Rto,length=places)}
      }
      
      cloud[from:to] <- round(as.numeric(cldseg))
      glareL[from:to] <- round(as.numeric(Lseg))
      glareR[from:to] <- round(as.numeric(Rseg))
    }
  }else{
    # If there aren't two SEA entries to interpolate, just save values without interpolation
    for(i in 1:nrow(cues)){
      from <- cues$starts[i]
      to <- cues$ends[i]  
      cloud[from:to] <- round(as.numeric(ccsea$X3[ccsea$EID==from]))
      glareL[from:to] <- round(as.numeric(ccsea$X6[ccsea$EID==from]))
      glareR[from:to] <- round(as.numeric(ccsea$X7[ccsea$EID==from])) 
    }
  }
  cloud
  glareL
  glareR
  #######################################################
  #######################################################
  ####  Compile final master
  #######################################################
  EID <- format(cc$EID,width=5) ; EID
  cloud <- format(cloud,width=3) ; cloud
  glareL <- format(glareL,width=3) ; glareL
  glareR <- format(glareR,width=3) ; glareR

  ccx <- data.frame(
    EID,
    X,
    Y,
    ev,
    when,
    spd,
    hdg,
    cruise=gsub(" ","",cruise),
    vessel=gsub(" ","",vessel),
    eff,
    trn,
    port,
    star,
    qual,
    vis,
    precip,
    cloud,
    glareL,
    glareR,
    glareS,
    wind.dir,
    wind.spd,
    bft,
    swell,
    stringsAsFactors=FALSE
    )
  head(ccx)
  #######################################################
  #######################################################
  ####  BIND MASTER and DETAILS TOGETHER
  #######################################################
  ccx <- cbind(ccx,details) ; head(ccx)

  #######################################################
  #######################################################
  #### FINAL FORMATTING
  #######################################################
  ccx$ev <- as.character(ccx$ev) ; ccx$ev
  ccx$eff <- gsub(" ","",ccx$eff) ; unique(ccx$eff)
  ccx$trn <- as.numeric(ccx$trn) ; unique(ccx$trn)
  
  # Subset to good lat/long readings
  ccx$X <- as.numeric(as.character(ccx$X)) ; ccx$X
  ccx$Y <- as.numeric(as.character(ccx$Y)) ; ccx$Y
  
  gpsna <- which(is.na(ccx$X) | is.na(ccx$Y)) ; gpsna
  gpsbad <- which(ccx$X > -100 | ccx$X < -140 | ccx$Y > 60 | ccx$Y < 20) ; gpsbad
  gpsproblems <- sort(c(gpsna,gpsbad)); gpsproblems
  
  print("***GPS problems were found at the following time stamps and have been given a value of NA!***")
  print(data.frame(GPS.BAD=(as.character(ccx$when[gpsproblems])),
                   EVENT=(as.character(ccx$ev[gpsproblems])),
                   LAT=round(ccx$Y[gpsproblems],digits=5),
                   LONG=round(ccx$X[gpsproblems],digits=5)))
  print("***GPS problems are defined as position fixes that are outside of this range: Longitudes -140 and -100, Latitudes 20 and 60!***")
  print("***To fix, go into raw data output, find the timestamp, and revise GPS entries. Then re-run this expansion routine!***")
  nrow(ccx) ; ccx <- ccx[-gpsproblems,] ; nrow(ccx)
  
  #######################################################
  #######################################################
  ####  WRITE TO FILE
  #######################################################
  setwd(paste0(wd,writepath))
  newfile <- paste0("CC-",ccdate,".txt") ; newfile
  write.csv(ccx,file=newfile,quote=FALSE,row.names=FALSE)
  print(paste0("Expansion complete! ",newfile," has been written to ",writepath," !!!!"))

} # end of function
#########################################################
#########################################################
cc.expand(ccdate)
#cc.expand(ccdate,wd="/Users/eric.keen/")
#########################################################
#########################################################