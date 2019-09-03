##Data Mining and Business Analytics in R
#Johannes Ledolter
#webage with data sets and R-code
browseURL('http://www.biz.uiowa.edu/faculty/jledolter/DataMining')

#BB - 8/14/19

#Brief Summary of Chapter and Code examples

#Load libraries
library(tidyverse)

#CH1 - Introduction ----
  
  #Lessons from chapter one
  cat('Lessons from Chapter 1
      - The problem of overfitting models is of central importance 
      in data mining. Number of variables it takes to predict should be limited.
      Using more than necessary makes the model poor predictors on future data sets.
      Always have a training set (~75%) and a test set (~25%) 
      to check if the model holds up
      
      -Steps of the data mining process!!!
      1. Effeicient data storage and data preprocessing steps are critical
      2. Appropriate response variables and number of variables should be selected
      3. Data needs to be screened for outliers. Missing values need to be handled
      4. Data needs to be split into train and test sets
      5. Before fancy models are applied Bradley do basic summaries and visuals of data
          and scatter plots... of data
      6. Summary involves mean, percentiles, ... and more advanced summaries like 
          principle component analysis
      7. Appropriate models should be selected such as regression, trees, knn
      8. The findings from these models need to be confirmed, typically on
          the test set
      9. One must implement the insights gained
      -plan, do, check, act
      
      -Factors that came together recently that make data mining perfect for solving
        problems
      1. more and more relevant data is being collected
      2. data is being warehoused and is ready to mined
      3. computer storage and power are cheaper every day, good software
          like R is available.
      4. Customers are interested in keeping good customers. and even getting rid
          of bad ones')

#CH2 - Processing the Information and Getting to Know Your Data ----

  #Lessons from Ch2
  cat('Lessons from Ch2
      - ')

  #Example 2.1
  #Data: 2006 birth data set (427323 records, 13 variables)
  
    #install packages/load packages
    library(lattice)
    library(nutshell)
    
    #load data
    data(births2006.smpl)
    
    #take a peak at the data
    births2006.smpl[1:5,]
    
    dim(births2006.smpl)
    
    #Lets explore the data a little
    births.dow = table(births2006.smpl$DOB_WK)
    births.dow
    barchart(births.dow, ylab = 'Day of the Week', col = 'black')
    
    #Less births are on the weekends
    #The hypothesis is there are less c-sections on the weekends
    #lets test this with a two-way classification: day of the week and method of delivery
    dob.dm.tbl = table(WK = births2006.smpl$DOB_WK,
                       MM = births2006.smpl$DMETH_REC)
    
      #remove unknown column
      dob.dm.tbl = dob.dm.tbl[,-2]
     
      #make bar chart from this table
      trellis.device()
      barchart(dob.dm.tbl, ylab = 'Day of the Week')
      barchart(dob.dm.tbl, horizontal = FALSE, groups = FALSE,
               xlab = 'Day of the Week', col = 'black')
      
    #While it is true both methods decrease on the weekends.
    #C-sections decreases ~50% and vagnial by ~25-30%
    #Our hypothesis was correct in saying c-sections decrease the most on weekends
    
    ##
    
    #Compare birth weights based on number of offspring
    histogram(~DBWT|DPLURAL, data = births2006.smpl, layout = c(1,5),
              col = 'black')
    
    densityplot(~DBWT|DPLURAL, data = births2006.smpl, layout = c(1,5),
                col = 'black', plot.points = F)
    
    #compare birth weights based on method
    histogram(~DBWT|DMETH_REC, data = births2006.smpl, layout = c(1,3),
              col = 'black')
    
    densityplot(~DBWT, groups = DPLURAL, data = births2006.smpl,
                plot.points = F)
    
    #try a dotplot but be warned a histogram or density plot is better
    #when dealing with large number of values. THe dots run into each other
    dotplot(~DBWT|DPLURAL, data = births2006.smpl, layout = c(1,5),
            plot.points = F, col = 'black')
    
    ##
    
    #Scatter plot of birth weight first weight gain
    xyplot(DBWT~WTGAIN, data = births2006.smpl, col = 'black')
    
      #stratify on number of offspring
      xyplot(DBWT~WTGAIN|DPLURAL, data = births2006.smpl, col = 'black',
             layout = c(1,5))
      
      #Better look smooth scatter, where density of points is scaled to color (alpha)
      smoothScatter(births2006.smpl$WTGAIN, births2006.smpl$DBWT)
      
    ##
      
    #Boxplot to show birth weight against APGAR score
    boxplot(DBWT~APGAR5, data = births2006.smpl, ylab = 'DBWT',
            xlab = 'APGAR5')
    
    bwplot(DBWT~factor(APGAR5)|factor(SEX), data = births2006.smpl,
           xlab = 'APGAR5')
    
    #Boxplot to show birth weight against day of the week
    boxplot(DBWT~DOB_WK, data = births2006.smpl, ylab = 'DBWT',
            xlab = "Day of the Week")
    
    bwplot(DBWT~factor(DOB_WK), data = births2006.smpl,
           xlab = 'Day of the Week')

    ##
    
    #Calculate Birth weight as a function of multiple births for males and females seperatly
    fac = factor(births2006.smpl$DPLURAL)
    res = births2006.smpl$DBWT    
    t4 = tapply(res, fac, mean, na.rm = T)
    t4      

    t5 = tapply(births2006.smpl$DBWT, list(births2006.smpl$DPLURAL, births2006.smpl$SEX),
                FUN = mean, na.rm = T)      
    t5
    
    #Visualize these with barplots
    barplot(t4, ylab = 'DBWT')
    barplot(t5, beside = T, ylab = 'DBWT')

    ##
    
    #create cross-classification of weight gain and estimated gestation periiod
    #divide variables into 11 nonoverlapping groups
    t5 = table(births2006.smpl$ESTGEST)
    t5
    
      #99 is code for unknown gestation period so we will remove it from analysis
      new = births2006.smpl[births2006.smpl$ESTGEST != 99,]
      t51 = table(new$ESTGEST)    
      t51      
      
      #display this data as levelplot
      t6 = tapply(new$DBWT, INDEX = list(cut(new$WTGAIN, breaks = 10), 
                  cut(new$ESTGEST, breaks = 10)), FUN = mean, na.rm = T)
      t6
      levelplot(t6, scales = list(x = list(rot = 90)))
      
      #display this data as contourplot
      contourplot(t6, scales = list(x = list(rot = 90)))
      
  ##Example 2.2 Alumni Donations
  #load libraries and data
  library(lattice)
  don <- read.csv('contribution.csv')
  
    ##look at distribution of data
    barchart(table(don$Class.Year), horizontal = F,
             xlab = 'class year', col = 'black')
    
    don$TGiving <- don$FY00Giving + don$FY01Giving + don$FY02Giving + 
      don$FY03Giving + don$FY04Giving

    mean(don$TGiving)
    
    sd(don$TGiving)
    
    quantile(don$TGiving, probs = seq(0,1,0.05))

    quantile(don$TGiving, probs = seq(0.95, 1, 0.01))
    
    hist(don$TGiving)
    hist(don$TGiving[don$TGiving!=0][don$TGiving[don$TGiving!=0] <= 1000])
    boxplot(don$TGiving, horizontal = T, xlab = 'Total Contribution')
    boxplot(don$TGiving, outline = F, horizontal = T, xlab = 'Total Contribution')

    ##Explore relationship of alumni contributions among the 5 years
    data <- data.frame(don$FY04Giving, don$FY03Giving, don$FY02Giving, don$FY01Giving, don$FY00Giving)
    correlation <- cor(data)
    plot(data)
    library(ellipse)
    plotcorr(correlation)      

    ##explore contributions with mosaic plots
    don$TGivingIND <- cut(don$TGiving, c(-1, 0.5, 10000000), labels = F) - 1
    
    mosaicplot(factor(don$Gender) ~ factor(don$TGivingIND))
    mosaicplot(factor(don$Marital.Status) ~ factor(don$TGivingIND))
    mosaicplot(factor(don$AttendenceEvent) ~ factor(don$TGivingIND))
    t2 <- table(factor(don$Marital.Status), factor(don$TGivingIND),
                factor(don$AttendenceEvent))
    mosaicplot(t2[,,1])
    mosaicplot(t2[,,2])
    
    ##
    
  ##Example 3: Orange Juice
    
    ##load lattice for plotting
    library(lattice)
    
    ##look at OJ sales per store per brand on a weekly basis
    oj <- read.csv('oj.csv')
    oj$store <- factor(oj$store)
    oj[1:2,]
    
    t1 <- tapply(oj$logmove, oj$brand, FUN = mean, na.rm = T)
    t1      
    
    t2 <- tapply(oj$logmove, INDEX = list(oj$brand, oj$week), FUN = mean, na.rm = T)      
    t2
    
    plot(t2[1,], type = 'l', xlab = 'week', ylab = 'dominicks', ylim = c(7,12))
    plot(t2[2,], type = 'l', xlab = 'week', ylab = 'minute.maid', ylim = c(7,12))
    plot(t2[3,], type = 'l', xlab = 'week', ylab = 'tropicana', ylim = c(7,12))
    
    ##clean up plots with lattice
    logmove <- c(t2[1,], t2[2,], t2[3,])
    week1 <- c(40:160)      
    week <- c(week1, week1, week1)
    brand1 <- rep(1, 121)
    brand2 <- rep(2, 121)      
    brand3 <- rep(3, 121)      
    brand <- c(brand1, brand2, brand3)      

    xyplot(logmove ~ week|factor(brand), type = 'l', layout = c(1,3),
           col = 'black')      
    
    boxplot(logmove ~ brand, data = oj)
    histogram(~logmove|brand, data = oj, layout = c(1,3))
    densityplot(~logmove|brand, data = oj, layout = c(1,3),
                plot.points = F)
    densityplot(~logmove, groups = brand, data = oj, plot.points = F)
    
    ##Previous plots ignore effects of price and advertisment
    ##look at the effects of these variables on sales
    xyplot(logmove~price, data = oj, col = 'black')
    xyplot(logmove~price|brand, data = oj, col = 'black',
           layout = c(1,3))
    
    smoothScatter(oj$price, oj$logmove)
    densityplot(~logmove, groups = feat, data = oj, plot.points = F)
    xyplot(logmove~price, groups = feat, data = oj) #clearly advertising increases sales
    
    ##consider sales at just one store
    oj1 <- oj[oj$store == 5,]
    xyplot(logmove ~ week|brand, data = oj1, type = 'l', layout = c(1,3),
           col = 'black')
    xyplot(logmove ~ price|brand, data = oj1, col = 'black', layout = c(1,3))
    densityplot(~logmove|brand, groups = feat, data = oj1, plot.points = F)
    xyplot(logmove ~ price|brand, groups = feat, data = oj1)
    
    ##do poorer areas react to price changes more than wealthy areas
    #store 62 - wealthy, store 75 - poor
    t21 <- tapply(oj$INCOME, oj$store, FUN = mean, na.rm = T)
    t21[t21 == max(t21)] #62
    t21[t21 == min(t21)] #75
    
    oj1 <- oj[oj$store == 62,]
    oj2 <- oj[oj$store == 75,]      
    oj3 <- rbind(oj1, oj2)
    
    xyplot(logmove~price|store, data = oj3) #hard to compare lets add advertising groups
    xyplot(logmove~price|store, groups = feat, data = oj3)
    
      #wealthy store
      mhigh <- lm(logmove~price, data = oj1) #lm finds best fit line
      summary(mhigh)
      plot(logmove~price, data = oj1, xlim = c(0,4), ylim = c(0,13))
      abline(mhigh)
      
      #poor store
      mlow <- lm(logmove ~ price, data = oj2)
      summary(mlow)      
      plot(logmove~price, data = oj2, xlim = c(0,4), ylim = c(0,13))      
      abline(mlow)      
      
      #poorer areas respond more to price changes than wealthy areas
      
#CH3 - Standard Linear Regression ----
      

  ##Lessons from chapter 3
      cat('1. In standard linear regression 
                the response variable y is continous
          y = f(x1, x2, ..., xk) + e = a + b1x1 + b2x2 + ... + bkxk + e,
          ')
      
      
      
      
      ###Make random sequences with at least 2 aromatics and 2 acidcs
      aa_vector <- c('A','L','V','I','M','W','Y','F',
                     'S','T','N','Q','C','G','P','R','H','K','D','E')
      seq_list <- list()
      for(i in 1:1000){
      set_pos <- sample(1:20, 4, replace = F)
      seq <- sample(aa_vector, 20, replace = T)
      seq[set_pos[1:2]] <- 'W'
      seq[set_pos[3:4]] <- 'D'
      seq <- paste(seq, collapse = '')
      seq_list[[i]] <- seq
      }

      
      
      

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      