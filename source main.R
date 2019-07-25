##Source Main

#Use this script to build libraries and start analysis
#so the directory will be at the top level of the repository

#Load Libraries
library(tidyverse)

#Build old design and random Libraries
source("R Scripts/Build_old_design_and_random_libraries.R")

#Build new design library
source("R Scripts/Build_2019_design_library.R")

