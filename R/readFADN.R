#' @title readFADN
#' @description
#' reads a harmonized FADN and EOBS(ECA&D) data. (Link To be provided to download locations)
#'
#' @return data frame.
#' @author Abhijeet Mishra

#' @param start First year for which data needs to be read in from FADN files
#' @param end last year for which data needs to be read in from FADN files
#' @param int_rate interest rate+100 percent. Here, 1.01 stands for 1 percent interest rate. Can be changed for sensitivity analysis.
#' @param data_ver the data version for FADN data.
#' @import fastmatch gdata MASS dplyr zoo base gplots corrplot lattice plm stargazer xlsx ggplot2 gridExtra
#' @examples
#' \dontrun{
#' x <- readFADN(start=1990, end=2009, int_rate=1.01,data_ver="20150722")
#' }

# Before starting the function:
# If new data is avaiable, change the start and end years accordingly
# int_rate is the interest rate+100 percent. Here, 1.01 stands for 1 percent interest rate. Can be changed for sensitivity analysis.
# data_ver is the data version for FADN data.

  # Core function
readFADN <- function(start=1990, end=2009, int_rate=1.01,data_ver="20150722"){
  library(fastmatch)
  library(gdata)
  library(MASS)     #It is ABSOLUTELY NECESSARY to load MASS package before dplyr.
  library(dplyr)    #Loading dplyr before activating MASS package causes errors in using select() function.
  library(zoo)
  library(base)
  library(gplots)
  library(corrplot)
  library(lattice)
  library(plm)
  library(stargazer)
  library(xlsx)
  library(ggplot2)
  library(gridExtra)

    start.time <- Sys.time() #To time the function run.

    cat(paste0("++++++++++++++++++++ STARTING read process : FADN Data (Belgium) for the year ",start," ++++++++++++++++++++\n\n"))

    cat(paste0("A factor of ",int_rate,"e",end-start," will be used for adjusting financial indicators of ",start," to ",end," values.\n\n"))

    file <- paste0(getwd(),"/AGRI_RICA_",data_ver,"/BEL",start,".csv")

    cat(paste0("A factor of ",int_rate,"e",end-start," will be used for adjusting financial indicators of ",start," to ",end," values.\n\n"))

    file <- paste0(getwd(),"/AGRI_RICA_",data_ver,"/BEL",start,".csv")

    cat(paste0("reading File from the location --> \n",file,"\n\n"))
    data <- read.csv(file,header = T)

    ## Variables Needed
    ####################

    N3<-fmatch("NUTS3",names(data)) # NUTS3 Levels

    farm_income_p_FWU<-fmatch("SE430",names(data)) # Dependent Variables

    ID<-fmatch("ID",names(data)) # Farm ID

    EconSize<-fmatch("SE005",names(data)) # Economic Size (European units)

    TotalOutput <- fmatch("SE131",names(data)) # Total Output

    Total_Specific_Cost <- fmatch("SE281",names(data))

    Electricity_Cost <- fmatch("F79",names(data))

    Water_Cost <- fmatch("F81",names(data))

    UAA_Owned <- fmatch("B48",names(data))


    ## Livestock Unit Variables
    #=====

    Total_Livestock <- fmatch("DTOTLU",names(data))


    ## Capital Variables
    #=====

    Avg_Farm_Capital <- fmatch("SE510",names(data))

    Land_Value <- fmatch("G95CV",names(data))

    Total_Assets <- fmatch("SE436",names(data))

    # Subsidies
    #=====

    Total_Subsidy <- fmatch("SE605",names(data))

    #Farm Type
    #=====

    Type_of_Farming <- fmatch("TF14",names(data))

    # Area under different activities
    # ===

    AA_Columns <- matchcols(data, with=c("K","AA"),method = "and")

    AA <- select(data, one_of(AA_Columns))

    AA$K139AA <- AA$K139A*0.01 #K139AA = Area Under Mushroom in Ares (Conversion to Ha : 1 are = 0.01ha)

    AA <- rowSums(AA)

    head(AA)

    AUIrr <- data$A40 #Area Under Irrigation

    ## -------------------------------------------------------
    # Only keeping desired values
    data <- data [,c(Avg_Farm_Capital, EconSize, Electricity_Cost, farm_income_p_FWU, ID, Land_Value, N3, Total_Assets, Total_Livestock, Total_Specific_Cost, Total_Subsidy, TotalOutput, Type_of_Farming, Water_Cost,UAA_Owned) ]

    data <- data.frame(data,AA,AUIrr)
    ## -------------------------------------------------------

    ## Weather Stations Data with NUTS3 values and Station IDs
    ##########################################################
    WeatherStation_NUTS<-read.table("BELGIUM_WEATHER_NUTS3_ALL.txt", header=TRUE, sep="\t")
    colnames(WeatherStation_NUTS) <- c("Station","NUTS3","SOUID")

    ## Specifying the "code" for data sets of Belgium in ECA&D/EOBS data
    ####################################################################

    belgium_EOBS <- c("000017","000934","000935","000937","000938",
                      "000940","000943","000944","000945","000946",
                      "000949","000950","000952","000954","002178","002179") #These are the file name indicators for Belgian weather stations.

    ## Need TEMPERATURE (tg) values from master weather station data now
    #####################################################################

    readTG <- function(){
      core <- getwd()
      out=NULL
      for(i in 1:length(belgium_EOBS)){
        DataFolder <- "/ECA_indexTG/"
        directoy <- paste0(core,DataFolder)
        file <- paste0(directoy,"indexTG",belgium_EOBS[i],".txt")
        dat <- read.table(file, skip = 30,sep = "")
        dat <- data.frame(dat)
        out <- rbind(out,dat)
      }
      return(out)
    }

    BEL_TG <- readTG()

    # Rename Columns
    colnames(BEL_TG) <- c("SOUID","YEAR","ADV","WHYDV","SHYDV","Annual","SPR_MAM","SUM_JJA","AUT_SON","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

    BEL_TG <- BEL_TG[,-22] #Remove NA Column in end

    # Identify NA values
    BEL_TG[BEL_TG== -999999] <- NA

    #Convert to degC from master file which is in 0.01 degC
    BEL_TG[,3:21] <- BEL_TG[,3:21]/100
    BEL_TG <- BEL_TG[,c(1,2,3)]

    ## Need PRECIPITATION (RR) values from master weather station data now
    #####################################################################

    readRR <- function(){
      core <- getwd()
      out=NULL
      for(i in 1:length(belgium_EOBS)){
        DataFolder <- "/ECA_indexRR/"
        directoy <- paste0(core,DataFolder)
        file <- paste0(directoy,"indexRR",belgium_EOBS[i],".txt")
        dat <- read.table(file, skip = 30,sep = "")
        dat <- data.frame(dat)
        out <- rbind(out,dat)
      }
      return(out)
    }

    BEL_RR <- readRR()

    # Rename Columns
    colnames(BEL_RR) <- c("SOUID","YEAR","ADV","WHYDV","SHYDV","Annual","SPR_MAM","SUM_JJA","AUT_SON","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

    BEL_RR <- BEL_RR[,-22] #Remove NA Column in end

    # Identify NA values
    BEL_RR[BEL_RR== -999999] <- NA

    #Convert to mm from master file which is in 0.01 mm
    BEL_RR[,3:21] <- BEL_RR[,3:21]/100

    # Keeping only required values
    BEL_RR <- BEL_RR[,c(1,2,3)]

    # Change NUTS3 Values to character values
    data$NUTS3 <- as.character(data$NUTS3)
    WeatherStation_NUTS$NUTS3 <- as.character(WeatherStation_NUTS$NUTS3)

    ## Need Extreme Temp Range (ETR) values from master data now
    ############################################################

    readETR <- function(){
      core <- getwd()
      out=NULL
      for(i in 1:length(belgium_EOBS)){
        DataFolder <- "/ECA_indexETR/"
        directoy <- paste0(core,DataFolder)
        file <- paste0(directoy,"indexETR",belgium_EOBS[i],".txt")
        dat <- read.table(file, skip = 30,sep = "")
        dat <- data.frame(dat)
        out <- rbind(out,dat)
      }
      return(out)
    }

    BEL_ETR <- readETR()

    # Rename Columns
    colnames(BEL_ETR) <- c("SOUID","YEAR","ADV","WHYDV","SHYDV","Annual","SPR_MAM","SUM_JJA","AUT_SON","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

    BEL_ETR <- BEL_ETR[,-22] #Remove NA Column in end

    # Identify NA values
    BEL_ETR[BEL_ETR== -999999] <- NA

    #Convert to degC from master file which is in 0.01 degC
    BEL_ETR[,3:21] <- BEL_ETR[,3:21]/100

    # Keeping only required values
    BEL_ETR <- BEL_ETR[,c(1,2,3)]

    ## Need Warm Days values from master data now
    ##############################################
    readTG90p <- function(){
      core <- getwd()
      out=NULL
      for(i in 1:length(belgium_EOBS)){
        DataFolder <- "/ECA_indexTG90p/"
        directoy <- paste0(core,DataFolder)
        file <- paste0(directoy,"indexTG90p",belgium_EOBS[i],".txt")
        dat <- read.table(file, skip = 30,sep = "")
        dat <- data.frame(dat)
        out <- rbind(out,dat)
      }
      return(out)
    }

    BEL_TG90p <- readTG90p()

    # Rename Columns
    colnames(BEL_TG90p) <- c("SOUID","YEAR","ADV","WHYDV","SHYDV","Annual","SPR_MAM","SUM_JJA","AUT_SON","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

    BEL_TG90p <- BEL_TG90p[,-22] #Remove NA Column in end

    # Identify NA values
    BEL_TG90p[BEL_TG90p== -999999] <- NA

    #Convert to days from master file which is in 0.01 days
    BEL_TG90p[,3:21] <- BEL_TG90p[,3:21]/100

    # Keeping only required values
    BEL_TG90p <- BEL_TG90p[,c(1,2,3)]

    ## Need high precipitation days values from master data now
    ############################################################
    readR10mm <- function(){
      core <- getwd()
      out=NULL
      for(i in 1:length(belgium_EOBS)){
        DataFolder <- "/ECA_indexR10mm/"
        directoy <- paste0(core,DataFolder)
        file <- paste0(directoy,"indexR10mm",belgium_EOBS[i],".txt")
        dat <- read.table(file, skip = 30,sep = "")
        dat <- data.frame(dat)
        out <- rbind(out,dat)
      }
      return(out)
    }

    BEL_R10mm <- readR10mm()

    # Rename Columns
    colnames(BEL_R10mm) <- c("SOUID","YEAR","ADV","WHYDV","SHYDV","Annual","SPR_MAM","SUM_JJA","AUT_SON","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

    BEL_R10mm <- BEL_R10mm[,-22] #Remove NA Column in end

    # Identify NA values
    BEL_R10mm[BEL_R10mm== -999999] <- NA

    #Convert to days from master file which is in 0.01 days
    BEL_R10mm[,3:21] <- BEL_R10mm[,3:21]/100

    # Keeping only required values
    BEL_R10mm <- BEL_R10mm[,c(1,2,3)]

    ## Need SUMMER DAYS values from master data now
    ###############################################
    readSU <- function(){
      core <- getwd()
      out=NULL
      for(i in 1:length(belgium_EOBS)){
        DataFolder <- "/ECA_indexSU/"
        directoy <- paste0(core,DataFolder)
        file <- paste0(directoy,"indexSU",belgium_EOBS[i],".txt")
        dat <- read.table(file, skip = 30,sep = "")
        dat <- data.frame(dat)
        out <- rbind(out,dat)
      }
      return(out)
    }

    BEL_SU <- readSU()

    # Rename Columns
    colnames(BEL_SU) <- c("SOUID","YEAR","ADV","WHYDV","SHYDV","Annual","SPR_MAM","SUM_JJA","AUT_SON","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

    BEL_SU <- BEL_SU[,-22] #Remove NA Column in end

    # Identify NA values
    BEL_SU[BEL_SU== -999999] <- NA

    #Convert to days from master file which is in 0.01 days
    BEL_SU[,3:21] <- BEL_SU[,3:21]/100

    # Keeping only required values
    BEL_SU <- BEL_SU[,c(1,2,3)]

    ########################### FURTHER ADJUSTMENTS #############################

    # Keeping values only where Farmer owned some land
    data <- subset(data, data$B48 != 0)

    data$G95CV_p_ha <- data$G95CV/data$B48

    ## Bring SOUID (Station ID) to FADN Data in BEL
    ###############################################
    EndColumn <- ncol(data)
    for (i in 1:nrow(WeatherStation_NUTS))
    {
      for(j in 1:nrow(data))
      {
        if (data[j,'NUTS3'] == WeatherStation_NUTS[i,2])
        {
          data[j,EndColumn+1] <- WeatherStation_NUTS[i,3]
        }
      }
    }

    names(data)[ncol(data)] <- 'SOUID'

    # For tempearture, Now we only need start year (start as in when this function starts)
    BEL_TG <-BEL_TG[(BEL_TG$YEAR==start),]

    # For precipitation, Now we only need start year (start as in when this function starts)
    BEL_RR <-BEL_RR[(BEL_RR$YEAR==start),]

    # For Extreme temp range, Now we only need start year (start as in when this function starts)
    BEL_ETR <-BEL_ETR[(BEL_ETR$YEAR==start),]

    # For warm days, Now we only need start year (start as in when this function starts)
    BEL_TG90p <-BEL_TG90p[(BEL_TG90p$YEAR==start),]

    # For high precipitation days, Now we only need start year (start as in when this function starts)
    BEL_R10mm <-BEL_R10mm[(BEL_R10mm$YEAR==start),]

    # For Sunny days, Now we only need start year (start as in when this function starts)
    BEL_SU <-BEL_SU[(BEL_SU$YEAR==start),]

    ## Bring Temperature (TG) values to FADN Data
    #############################################

    EndColumn_TG <- ncol(data)
    for (i in 1:nrow(BEL_TG))
    {
      for (j in 1:nrow(data))
      {
        if(data[j,'SOUID'] == BEL_TG[i,1])
        {
          data[j,EndColumn_TG+1] <- BEL_TG[i,3]
        }
      }
    }

    ## Bring Precipitation (RR) values to FADN Data
    ###############################################
    EndColumn_RR <- ncol(data)
    for (i in 1:nrow(BEL_RR))
    {
      for (j in 1:nrow(data))
      {
        if(data[j,'SOUID'] == BEL_RR[i,1]) #Still checking SOUID
        {
          data[j,EndColumn_RR+1] <- BEL_RR[i,3]
        }
      }
    }

    ## Bring ETR values to FADN Data
    ################################
    EndColumn_ETR <- ncol(data)
    for (i in 1:nrow(BEL_ETR))
    {
      for (j in 1:nrow(data))
      {
        if(data[j,'SOUID'] == BEL_ETR[i,1]) #Still checking SOUID
        {
          data[j,EndColumn_ETR+1] <- BEL_ETR[i,3]
        }
      }
    }

    ## Bring R10mm values to FADN Data
    ##################################

    EndColumn_R10mm <- ncol(data)
    for (i in 1:nrow(BEL_R10mm))
    {
      for (j in 1:nrow(data))
      {
        if(data[j,'SOUID'] == BEL_R10mm[i,1])
        {
          data[j,EndColumn_R10mm+1] <- BEL_R10mm[i,3]
        }
      }
    }

    ## Bring TG90p values to FADN Data
    ##################################

    EndColumn_TG90p <- ncol(data)
    for (i in 1:nrow(BEL_TG90p))
    {
      for (j in 1:nrow(data))
      {
        if(data[j,'SOUID'] == BEL_TG90p[i,1])
        {
          data[j,EndColumn_TG90p+1] <- BEL_TG90p[i,3]
        }
      }
    }

    ## Bring SU values to FADN Data
    ###############################

    EndColumn_SU <- ncol(data)
    for (i in 1:nrow(BEL_SU))
    {
      for (j in 1:nrow(data))
      {
        if(data[j,'SOUID'] == BEL_SU[i,1])
        {
          data[j,EndColumn_SU+1] <- BEL_SU[i,3]
        }
      }
    }

    # Definition of weather indicators available on:
    # http://www.ecad.eu/indicesextremes/indicesdictionary.php

    names(data)[EndColumn_TG+1] <- 'TG_Annual'
    names(data)[EndColumn_RR+1] <- 'RR_Annual'
    names(data)[EndColumn_ETR+1] <- 'ETR_Annual'
    names(data)[EndColumn_R10mm+1] <- 'R10mm_Annual'
    names(data)[EndColumn_TG90p+1] <- 'TG90p_Annual'
    names(data)[EndColumn_SU+1] <- 'SU_Annual'

    #Change NA to averages

    data$TG_Annual <- na.aggregate(data$TG_Annual)
    data$RR_Annual <- na.aggregate(data$RR_Annual)
    data$ETR_Annual <- na.aggregate(data$ETR_Annual)
    data$R10mm_Annual <- na.aggregate(data$R10mm_Annual)
    data$TG90p_Annual <- na.aggregate(data$TG90p_Annual)
    data$SU_Annual <- na.aggregate(data$SU_Annual)

    data$TG_Annual_Squared <- data$TG_Annual*data$TG_Annual
    data$RR_Annual_Squared <- data$RR_Annual*data$RR_Annual

    #Converting financial variables in current values

    data$SE510 <- data$SE510*(int_rate^(end-start))
    data$F79 <- data$F79*(int_rate^(end-start))
    data$SE436 <- data$SE436*(int_rate^(end-start))
    data$SE281 <- data$SE281*(int_rate^(end-start))
    data$SE605 <- data$SE605*(int_rate^(end-start))
    data$SE131 <- data$SE131*(int_rate^(end-start))
    data$F81 <- data$F81*(int_rate^(end-start))

    data$SE510 <- data$SE510*(1.01^(end-start))
    data$F79 <- data$F79*(1.01^(end-start))
    data$SE436 <- data$SE436*(1.01^(end-start))
    data$SE281 <- data$SE281*(1.01^(end-start))
    data$SE605 <- data$SE605*(1.01^(end-start))
    data$SE131 <- data$SE131*(1.01^(end-start))
    data$F81 <- data$F81*(1.01^(end-start))


    data$YEAR <- start

    end.time <- Sys.time()
    time.taken <- round(end.time - start.time,digits = 1)
    cat(paste0("Step completed in ",time.taken," seconds.\n\n"))
    cat(paste0("++++++++++++++++++++ FINISHED read process : FADN Data (Belgium) for the year ",start," ++++++++++++++++++++\n\n\n\n"))
    return(data)
}
