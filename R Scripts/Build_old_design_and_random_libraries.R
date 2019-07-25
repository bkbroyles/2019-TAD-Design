##Source Main

#build libraries and load packages for rest of the analysis

#load libraries
library(tidyverse)

#get master data set and split up into design and random libraries
master <- read_rds('master data set/master.rds')

#master contains two lists: design library and random library
  #with three dataframes under each list. These are Charles features and metadata. 
  #I just need sequence and estimate (functionality score)

#get random library first
random <- master$random$dataset %>% select(target, estimate)
random_sequences <- master$random$target_metadata %>% select(target, sequence)

  #remove 'sq=' from random$target and then join two df by target
  random$target <- gsub('sq=', '', random$target)
  
  random_library <- inner_join(random, random_sequences, "target")
  
  #remove na estimates and na sequences. NA sequences are stop codons. An analysis was performed in Ravarani et al
  #to place a threshold for functional (live) vs non-functional (die) at estimate > -0.13. This was based on estimate
  #of stop codon sequences which should be non-functional a threshold that satisfies low false positive with still enough
  #living sequences to analyze
  random_library <- random_library %>% filter(!is.na(estimate)) %>% 
    filter(!is.na(sequence))
  
  #adding binary_stop (estimate > -0.13)
  random_library <- random_library %>% 
    mutate(
      binary_stop = ifelse(estimate > -0.13, 'live', 'die') %>% as.factor()
    ) %>% select(sequence, estimate, binary_stop)
  
#get design library, I am also going to add set and ref_tad since they
  #are useful in analyzing design library
design_library <- master$design$dataset %>% select(target, estimate)

design_data <- master$design$target_metadata
#I am not sure which peices of the metadata I will use so I might as well keep it all


  #remove 'sq=' and remove NAs
  design_library$target <- gsub('sq=', '', design_library$target)
  
  design_library <- design_library %>% filter(!is.na(estimate)) %>% 
    filter(!is.na(target))
  
  #add binary_stop (this time threshold is -0.15)
  design_library <- design_library %>% 
    mutate(
      binary_stop = ifelse(estimate > -0.15, 'live', 'die')
    )
  
  colnames(design_library)[1] <- 'sequence'
  
  #add meta data
  design_library <- inner_join(design_library, design_data, by = "sequence")
  
#remove unecessary variables to keep environment clean
  rm(master, random, random_sequences, design_data)
  
