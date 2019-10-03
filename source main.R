##Source Main

#Use this script to build libraries and start analysis
#so the directory will be at the top level of the repository

#Load Libraries
library(tidyverse)

#Build old design and random Libraries
source("R Scripts/Build_old_design_and_random_libraries.R")

  #Two designed sequences in random library. Remove them here
  ro <- grep('WDWDWD', random_library$sequence)
  WD_seqs <- random_library[ro,]
  random_library <- random_library[-ro,]

#Build new design library
#source("R Scripts/Build_2019_design_library.R")
  
#Load final AA library
library_sets_list <- readRDS("Final_Robject_AA_library_v3.rds")
  
#Load custom functions
source('R scripts/Custom_functions.R')



