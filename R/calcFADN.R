#' @title calcFADN
#' @description
#' Extracts results from readFADN()
#'
#' @return NULL
#' @author Abhijeet Mishra

#' @examples
#' \dontrun{
#' calcFADN
#' }
#'
calcFADN <- function(){

  BELGIUM <- rbind(readFADN(start = 1990),readFADN(start = 1991),readFADN(start = 1992),readFADN(start = 1993),readFADN(start = 1994),
                   readFADN(start = 1995),readFADN(start = 1996),readFADN(start = 1997),readFADN(start = 1998),readFADN(start = 1999),
                   readFADN(start = 2000),readFADN(start = 2001),readFADN(start = 2002),readFADN(start = 2003),readFADN(start = 2004),
                   readFADN(start = 2005),readFADN(start = 2006),readFADN(start = 2007),readFADN(start = 2008),readFADN(start = 2009))

  summary(BELGIUM) #Still NAs in the data

  #Converting NAs to means
  BELGIUM$TG_Annual <- na.aggregate(BELGIUM$TG_Annual)
  BELGIUM$TG_Annual_Squared <- na.aggregate(BELGIUM$TG_Annual_Squared)
  BELGIUM$RR_Annual <- na.aggregate(BELGIUM$RR_Annual)
  BELGIUM$ETR_Annual <- na.aggregate(BELGIUM$ETR_Annual)
  BELGIUM$R10mm_Annual <- na.aggregate(BELGIUM$R10mm_Annual)
  BELGIUM$TG90p_Annual <- na.aggregate(BELGIUM$TG90p_Annual)
  BELGIUM$SU_Annual <- na.aggregate(BELGIUM$SU_Annual)

  summary(BELGIUM) #No NAs in the data

  #Exploring Data

  backup <- BELGIUM #Keeping a copy of data just in case

  # In case you need to delete all files in Env except for the files you desire and the "analysis" function:
  # Just run the following code fragment to remove unncecessary files

  # save_it <- c("analysis", "backup")
  #   x <- ls()
  #   x <- x[! x %in% save_it]
  #   rm(list = x)
  # rm(x)
  # BELGIUM <- backup


  BELGIUM <- subset(BELGIUM, BELGIUM$SE430>0) #keeping only positive farm income values
  BELGIUM <- subset(BELGIUM, BELGIUM$SE605>0) #Keeping only positive farm subsidies
  BELGIUM <- subset(BELGIUM, BELGIUM$SE131>0) #Keeping only positive total output

  theme_set(theme_grey(base_size = 18))
  # Farm Incomes: Heterogeneity across YEARS and Regions
  # plotmeans draw a 95% confidence interval around the means
  x <- BELGIUM
  col <-ncol(x)
  x[grep("BE2",x$NUTS3),col+1] <- "Flanders"
  x[grep("BE3",x$NUTS3),col+1] <- "Wallonia"
  colnames(x)[col+1] <- "Region"
  x <- subset(x,!(x$SE430 > quantile(x$SE430, probs=c(.01, .99))[2] | x$SE430 < quantile(x$SE430, probs=c(.01, .91))[1]) )
  ggplot(x,aes(YEAR,SE430,colour = Region)) + geom_smooth(span = 0.3) + facet_wrap(~NUTS3) + ggtitle("Farm family income across different regions in Belgium (1990-2009)") + labs(x="Year",y="Average income in 2009 Euro")

  ## Weather related indicators
  ##############################

  # Annual Temperature
  x <- BELGIUM
  col <-ncol(x)
  x[grep("BE2",x$NUTS3),col+1] <- "Flanders"
  x[grep("BE3",x$NUTS3),col+1] <- "Wallonia"
  colnames(x)[col+1] <- "Region"
  ggplot(x, aes(x=YEAR, y=TG_Annual, colour=Region), shape=Region) + geom_point() + geom_smooth(span = 1.5) + ggtitle("Mean annual temperatures in Belgium (1990-2009)") + labs(x="Year",y="Temperature (degree Celcius)")
  p1 <- ggplot(x, aes(x=YEAR, y=TG_Annual, colour=Region), shape=Region) + geom_point() + geom_smooth(span = 1.5) + ggtitle("Mean annual temperatures in Belgium (1990-2009)") + labs(x="Year",y="Temperature (degree Celcius)")

  # Annual Precipitation
  x <- BELGIUM
  col <-ncol(x)
  x[grep("BE2",x$NUTS3),col+1] <- "Flanders"
  x[grep("BE3",x$NUTS3),col+1] <- "Wallonia"
  colnames(x)[col+1] <- "Region"
  ggplot(x, aes(x=YEAR, y=RR_Annual, colour=Region, shape=Region)) +  geom_point() + geom_smooth(span = 1.5) + ggtitle("Mean annual precipitation in Belgium (1990-2009)") + labs(x="Year",y="Precipitation (mm)")
  p2 <- ggplot(x, aes(x=YEAR, y=RR_Annual, colour=Region, shape=Region)) +  geom_point() + geom_smooth(span = 1.5) + ggtitle("Mean annual precipitation in Belgium (1990-2009)") + labs(x="Year",y="Precipitation (mm)")

  #Plot on one frame
  gridExtra::grid.arrange(p1, p2, ncol=2)

  theme_set(theme_grey(base_size = 15))
  # ETR
  x <- BELGIUM
  col <-ncol(x)
  x[grep("BE2",x$NUTS3),col+1] <- "Flanders"
  x[grep("BE3",x$NUTS3),col+1] <- "Wallonia"
  colnames(x)[col+1] <- "Region"
  p3 <- ggplot(x, aes(x=YEAR, y=ETR_Annual, colour=Region, shape=Region)) + geom_point() + geom_smooth(span = 1.5) + ggtitle("Extreme temperature range in Belgium (1990-2009)") + labs(x="Year",y="ETR (degree Celcius)")

  # Warm Days
  x <- BELGIUM
  col <-ncol(x)
  x[grep("BE2",x$NUTS3),col+1] <- "Flanders"
  x[grep("BE3",x$NUTS3),col+1] <- "Wallonia"
  colnames(x)[col+1] <- "Region"
  p4 <- ggplot(x, aes(x=YEAR, y=TG90p_Annual, colour=Region, shape=Region)) + geom_point() + geom_smooth(span = 1.5) + ggtitle("Number of warm days in Belgium (1990-2009)") + labs(x="Year",y="no. of days")

  # Heavy precipitation
  x <- BELGIUM
  col <-ncol(x)
  x[grep("BE2",x$NUTS3),col+1] <- "Flanders"
  x[grep("BE3",x$NUTS3),col+1] <- "Wallonia"
  colnames(x)[col+1] <- "Region"
  p5 <- ggplot(x, aes(x=YEAR, y=R10mm_Annual, colour=Region, shape=Region)) + geom_point() + geom_smooth(span = 1.5) + ggtitle("Number of warm days in Belgium (1990-2009)") + labs(x="Year",y="no. of extreme rain days")

  #Plot on one frame
  gridExtra::grid.arrange(p3, p4, p5,ncol=3)

  #reset the theme
  theme_set(theme_grey(base_size = 18))

  # x <- BELGIUM
  # col <-ncol(x)
  # x[grep("BE2",x$NUTS3),col+1] <- "Flanders"
  # x[grep("BE3",x$NUTS3),col+1] <- "Wallonia"
  # colnames(x)[col+1] <- "Region"
  # x <- subset(x, x$TF14 %in% c(14,20,41,50,70,80))
  # ggplot(x,aes(YEAR,SE430,colour = Region)) + geom_smooth(span = 0.3) + facet_wrap(~TF14) + ggtitle("Farm family income across different regions in Belgium (1990-2009)") + labs(x="Year",y="Average income in 2009 Euro")
  # z <- ggplot(x,aes(YEAR,SE430,colour = Region)) + geom_smooth(span = 0.3) + facet_wrap(~TF14) + ggtitle("Farm family income across different regions in Belgium (1990-2009)") + labs(x="Year",y="Average income in 2009 Euro")
  # cowplot::ggdraw(cowplot::add_sub(z, "Farm codes: \n14: Other field crop; 20: Horticulture; 41:Dairy \n50: Granivore; 70: Mixed livestock; 80: Mixed crop & livestock"),)
  #

  ## Mean income across years according to farm types
  ggp <- list()
  for (i in c(14,20,41,45,50,70,80)){
    x <- subset(BELGIUM, BELGIUM$TF14==i)
    x <- subset(x,!(x$SE430 > quantile(x$SE430, probs=c(.01, .99))[2] | x$SE430 < quantile(x$SE430, probs=c(.01, .91))[1]) )
    col <-ncol(x)
    x[grep("BE2",x$NUTS3),col+1] <- "Flanders"
    x[grep("BE3",x$NUTS3),col+1] <- "Wallonia"
    colnames(x)[col+1] <- "Region"
    print(ggplot(x,aes(YEAR,SE430,colour = Region)) + geom_smooth(span = 0.3) + ggtitle(paste0("Mean income in farm type TF",i)) + labs(x="Year",y="Average farm income (in 2009 Euro)"))
    ggpobject <- ggplot(x,aes(YEAR,SE430,colour = Region)) + geom_smooth(span = 0.3) + ggtitle(paste0("Mean income in farm type TF",i)) + labs(x="Year",y="Average farm income (in 2009 Euro)")
    name <- paste0("TC",i)
    ggp[[name]] <- ggpobject
  }

  #Write results in a file
  folder <- "Farm Income (Specialist)"
  if (!dir.exists(folder)){
    dir.create(folder)
  }

  pdf(paste0(getwd(),"/",folder,"/",folder,".pdf"),width = 11.7,height = 8.3,fonts =  "Times")
  for (i in 1:length(ggp)) {
    print(ggp[[i]])
  }
  dev.off()

  rm(x)
  rm(i)
  rm(col)
  rm(ggp)
  rm(ggpobject)
  rm(folder)
  rm(name)

  # Check for correlations
  # ======================
  folder <- "Correlation Plots"
  if (!dir.exists(folder)){
    dir.create(folder)
  }

  for (i in c(14,20,41,45,50,70,80)) {
    x <- subset(BELGIUM, BELGIUM$TF14==i)
    x <- subset(x,!(x$SE430 > quantile(x$SE430, probs=c(.01, .99))[2] | x$SE430 < quantile(x$SE430, probs=c(.01, .91))[1]) )
    # First extract indexes of only numerical columns
    BELCorr <- x[,c("SE510","SE005","F79","SE430","SE436","DTOTLU","SE281","SE605","SE131","F81","B48","AA","AUIrr","TG_Annual","RR_Annual","ETR_Annual","R10mm_Annual","SU_Annual")]
    nums <- sapply(BELCorr, is.numeric) #Output is a logical Vector
    colnames(BELCorr)<-c("Avg. farm capital","Economic size","Electricity","Farm income","Total assets","Total livestock",
                         "Specific costs","Subsidies","Output","Water","UAA owned","Agricultural area",
                         "Irrigated area","Temperature (mean/year)","Rainfall (mean/year)","ETR","R10","SU")
    #Now Subset and save the correlations to an object
    B_Corr <- cor(BELCorr[,nums])
    # Plot it
    png(height=1200, width=1500, pointsize=15, file=paste0(getwd(),"/",folder,"/TF",i,".png"))
    corrplot(B_Corr,method = "number",order = "hclust",addrect = 5,mar=c(0,0,1,0),title = paste0("Coorelation Plot of Farm Code TF",i),type = "lower")
    dev.off()
  }

  rm(nums)
  rm(BELCorr)
  rm(B_Corr)
  rm(i)
  rm(x)
  rm(folder)

  ## Specify the regression formula
  formula <- SE430 ~ SE510 + SE005 + F79 + SE436 + DTOTLU +  SE281  +  SE605  +  SE131 + F81  +  AA   +  AUIrr  + B48 + TG_Annual +  TG_Annual_Squared + log10(TG90p_Annual) + ETR_Annual + RR_Annual + RR_Annual_Squared + log10(R10mm_Annual) + log10(SU_Annual)

  #TF14 FADN Classification code summary:
  #=====================================
  #14: OtFC (Other Field Crops)
  #20: HORTI (Horticulture)
  #41: MILK (Dairy)
  #45: CATTLE (Cattle)
  #50: GRANI (Granivores)
  #70: MIXEDLS (Mixed Livestock)
  #80: MIXEDCrLS (Mixed Crop and Livestock)

  ## OLS estimates

  pb <- winProgressBar(title = "OLS estimates", min = 0, max = 70, width = 300)
  OLS_List <- list()
  for (i in c(14,20,41,45,50,70,80)){
    Sys.sleep(0.1)
    setWinProgressBar(pb, i, title=paste("OLS estimates calculated for farm TF",i))
    x <- subset(BELGIUM, BELGIUM$TF14==i)
    x <- subset(x,!(x$SE430 > quantile(x$SE430, probs=c(.01, .99))[2] | x$SE430 < quantile(x$SE430, probs=c(.01, .91))[1]) )
    ols <- lm(formula, data = x)
    name <- paste0('TFC_',i) #TFC = FADN TF14 clalssification code. TFC_14 contains data for OtFC (Other Field Crops)....
    OLS_List[[name]] <- ols
  }
  close(pb)


  #################### PANEL DATA TEST ########################

  ## Fixed effects: n entity-specific intercepts (using plm)

  pb <- winProgressBar(title = "One way individual fixed effects", min = 0, max = 70, width = 300)
  fixef_List <- list()
  for (i in c(14,20,41,45,50,70,80)){
    Sys.sleep(0.1)
    setWinProgressBar(pb, i, title=paste("Fixef for farm TF",i))
    x <- subset(BELGIUM, BELGIUM$TF14==i)
    x <- subset(x,!(x$SE430 > quantile(x$SE430, probs=c(.01, .99))[2] | x$SE430 < quantile(x$SE430, probs=c(.01, .91))[1]) )
    fixef <- plm(formula, data = x, index = c("ID", "YEAR"), model="within", effect = "individual")
    name <- paste0('TFC_',i) #TFC = FADN TF14 clalssification code. TFC_14 contains data for OtFC (Other Field Crops)....
    fixef_List[[name]] <- fixef
  }
  close(pb)
  ###### OLS OR FIXED : pFtest ######

  for (i in c(14,20,41,45,50,70,80)){
    name <- paste0('TFC_',i)
    x <- pFtest(fixef_List[[name]],OLS_List[[name]])
    pval <- x$p.value
    cat(paste0("p value for pFtest between fixed effects model and OLS estimates for TFC_",i," is ",pval,".\n"))
    if(pval<0.05){
      cat(paste0("As p value is less than 0.05, FIXED effects model is suitable for TFC_",i,".\n\n"))
    } else {
      cat(paste0("As p value is greater than 0.05, OLS estimates are suitable for TFC_",i,".\n\n"))
    }
  }

  ## Random effects model

  random_List <- list()
  for (i in c(14,20,41,45,50,70,80)){
    x <- subset(BELGIUM, BELGIUM$TF14==i)
    x <- subset(x,!(x$SE430 > quantile(x$SE430, probs=c(.01, .99))[2] | x$SE430 < quantile(x$SE430, probs=c(.01, .91))[1]) )
    cat(paste0("\nRandom effects model being run for farm type TF",i,".\nWriting results in the list.\n"))
    random <- plm(formula, data = x, index = c("ID", "YEAR"), model="random", effect = "individual")
    name <- paste0('TFC_',i) #TFC = FADN TF14 clalssification code. TFC_14 contains data for OtFC (Other Field Crops)....
    random_List[[name]] <- random
  }


  ## Fixed or Random: Hausman test

  # To decide between fixed or random effects we can run a Hausman test
  # In HAUSMAN test, the null hypothesis is that the preferred model is random effects vs. the alternative the fixed effects.
  # It basically tests whether the unique errors (ui) are correlated with the regressors, the null hypothesis is they are not.

  #phtest to choose between random or fixed effects
  for (i in c(14,20,41,45,50,70,80)){
    name <- paste0('TFC_',i)
    x <- phtest(fixef_List[[name]],random_List[[name]])
    pval <- x$p.value
    cat(paste0("p value for Hausman between fixed effects model and random effects model for TFC_",i," is ",pval,".\n"))
    if(pval<0.05){
      cat(paste0("As p value is less than 0.05, FIXED effects model is suitable for TFC_",i,".\n\n"))
    } else {
      cat(paste0("As p value is greater than 0.05, RANDOM effects model is suitable for TFC_",i,".\n\n"))
    }
  }

  rm(i)
  rm(ols)
  rm(fixef)
  rm(random)
  rm(name)
  rm(pval)
  rm(x)
  rm(pb)

  BELTemp <- BELGIUM[,c("SE510","SE005","F79","SE430","G95CV","SE436","DTOTLU","SE281","SE605","SE131","F81","B48","AA","AUIrr","TG_Annual","RR_Annual","ETR_Annual","R10mm_Annual","TG90p_Annual", "SU_Annual")]
  stargazer(BELTemp, out="Belgium Descriptors.htm",digits = 1)
  rm(BELTemp)

  #TF14 FADN Classification code summary:
  #=====================================
  #14: OtFC (Other Field Crops)
  #20: HORTI (Horticulture)
  #41: MILK (Dairy)
  #45: CATTLE (Cattle)
  #50: GRANI (Granivores)
  #70: MIXEDLS (Mixed Livestock)
  #80: MIXEDCrLS (Mixed Crop and Livestock)

  stargazer(OLS_List[[1]],OLS_List[[2]],OLS_List[[3]],OLS_List[[4]],OLS_List[[5]],OLS_List[[6]],OLS_List[[7]],
            type= "html", stystyle = "default", digits = 1,
            no.space = T, single.row = T ,dep.var.labels=c("Farm family income (Euro)"),
            column.labels = c("Other Field Crops (TFC14)","Horticulture (TFC20)","Milk (TFC41)",
                              "Cattle (TFC45)","Granivores (TFC50)","Mixed LS (TFC70)","Mixed CrLS (TFC80)"),
            covariate.labels = c("Average farm capital (Euro)","Economic size (ESU)","Electricity","Total Assets ",
                                 "Total Livestock (LU)", "Crop Specific Inputs", "Total Subsidies", "Total Crop Output",
                                 "Water", "Area Under Agriculture (ha)", "Area Under Irrigation (ha)", "UAA Owned (ha)",
                                 "Annual mean temperature (degC)", "Annual mean temperature Sq. (degC)",
                                 "No. of extreme temperature days (log)", "Extreme temperature range (degC)",
                                 "Annual mean precipitation (mm)", "Annual mean precipitation Sq. (mm)", "No. of extreme rainfall days (log)","No. of Summer days (log)")
            , out="OLS Estimates.htm")

  stargazer(fixef_List[[1]],fixef_List[[2]],fixef_List[[3]],fixef_List[[4]],fixef_List[[5]],fixef_List[[6]],fixef_List[[7]],
            type= "html", stystyle = "default", digits = 1,
            no.space = T, single.row = T ,dep.var.labels=c("Farm family income (Euro)"),
            column.labels = c("Other Field Crops (TFC14)","Horticulture (TFC20)","Milk (TFC41)",
                              "Cattle (TFC45)","Granivores (TFC50)","Mixed LS (TFC70)","Mixed CrLS (TFC80)"),
            covariate.labels = c("Average farm capital (Euro)","Economic size (ESU)","Electricity","Total Assets ",
                                 "Total Livestock (LU)", "Crop Specific Inputs", "Total Subsidies", "Total Crop Output",
                                 "Water", "Area Under Agriculture (ha)", "Area Under Irrigation (ha)", "UAA Owned (ha)",
                                 "Annual mean temperature (degC)", "Annual mean temperature Sq. (degC)",
                                 "No. of extreme temperature days (log)", "Extreme temperature range (degC)",
                                 "Annual mean precipitation (mm)", "Annual mean precipitation Sq. (mm)", "No. of extreme rainfall days (log)","No. of Summer days (log)")
            , out="fixef Models.htm")


  ## Plot results of panel model
  ggp <- list()
  for (i in c(14,20,41,45,50,70,80)){
    df <- subset(BELGIUM,BELGIUM$TF14==i)
    df <- subset(df,!(df$SE430 > quantile(df$SE430, probs=c(.01, .99))[2] | df$SE430 < quantile(df$SE430, probs=c(.01, .91))[1]) )
    df$predicted <- as.numeric(fixef_List[[paste0("TFC_",i)]]$model[[1]] - fixef_List[[paste0("TFC_",i)]]$residuals)
    g <- ggplot(df)
    g <- g + geom_point(aes(x=SE430, y = predicted), size = 2, colour = "blue")
    print(g + geom_smooth(data=df, aes(x=predicted, y=SE430), size = 1.5, colour = "red", se = TRUE, stat = "smooth") + ggtitle(paste0("One-way individual fixed effects model Performance in farm type TF",i)) + labs(x="Farm income (Euro)",y="Predicted value from model (Euro)"))
    ggpobject <- g + geom_smooth(data=df, aes(x=predicted, y=SE430), size = 1.5, colour = "red", se = TRUE, stat = "smooth") + ggtitle(paste0("One-way individual fixed effects model Performance in farm type TF",i)) + labs(x="Farm income (Euro)",y="Predicted value from model (Euro)")
    name <- paste0("TC",i)
    ggp[[name]] <- ggpobject
  }

  #Write results in a file
  folder <- "Panel Model Performance Plots"
  if (!dir.exists(folder)){
    dir.create(folder)
  }

  pdf(paste0(getwd(),"/",folder,"/",folder,".pdf"),width = 11.7,height = 8.3,fonts =  "Times")
  for (i in 1:length(ggp)) {
    print(ggp[[i]])
  }
  dev.off()

  ## Plot results of OLS estimates
  ggp_ols <- list()
  for (i in c(14,20,41,45,50,70,80)){
    df <- subset(BELGIUM,BELGIUM$TF14==i)
    df <- subset(df,!(df$SE430 > quantile(df$SE430, probs=c(.01, .99))[2] | df$SE430 < quantile(df$SE430, probs=c(.01, .91))[1]) )
    df$predicted <- stats::predict(OLS_List[[paste0("TFC_",i)]], newdata=df)
    g <- ggplot(df)
    g <- g + geom_point(aes(x=SE430, y = predicted), size = 2, colour = "blue")
    print(g + geom_smooth(data=df, aes(x=predicted, y=SE430), size = 1.5, colour = "red", se = TRUE, stat = "smooth") + ggtitle(paste0("OLS predictors performance in farm type TF",i)) + labs(x="Farm income (Euro)",y="Predicted value from model (Euro)"))
    ggpobject <- g + geom_smooth(data=df, aes(x=predicted, y=SE430), size = 1.5, colour = "red", se = TRUE, stat = "smooth") + ggtitle(paste0("OLS predictors performance in farm type TF",i)) + labs(x="Farm income (Euro)",y="Predicted value from model (Euro)")
    name <- paste0("TC",i)
    ggp_ols[[name]] <- ggpobject
  }

  #Write results in a file
  folder <- "OLS Performance Plots"
  if (!dir.exists(folder)){
    dir.create(folder)
  }

  pdf(paste0(getwd(),"/",folder,"/",folder,".pdf"),width = 11.7,height = 8.3,fonts =  "Times")
  for (i in 1:length(ggp_ols)) {
    print(ggp_ols[[i]])
  }
  dev.off()

  rm(g)
  rm(folder)
  rm(ggp)
  rm(ggp_ols)
  rm(ggpobject)
  rm(i)
  rm(name)
  rm(df)

}
