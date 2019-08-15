###Custom Functions 

#a collection of functions that will save some time coding the same transformations over and over

##build_aa_class----
#return a list of amino acid classes and the amino acids that fall under that class
#can be a vector of amino acids or a regular expression friendly format. Change output format with regex = T or F
build_aa_class <- function(regex = T){
  aa_class <- list()
  aa_class[['aliphatic']] <- c('A', 'L', 'V', 'I', 'M')
  aa_class[['aromatic']] <- c('W', 'Y', 'F')
  aa_class[['polar']] <- c('S', 'T', 'N', 'Q')
  aa_class[['special']] <- c('C', 'G', 'P')
  aa_class[['basic']] <- c('R', 'H', 'K')
  aa_class[['acidic']] <- c('D', 'E')
  
  #if regex = T I want these to be a regex that captures the whole class
  #if regex = F i just want a list of which amino acids fall under each class
  if(regex == T){
    for (i in 1:length(aa_class)) {
      aa_class[[i]] <- paste(aa_class[[i]], collapse = '') %>% 
        paste('[', ., ']', sep = '')
    }
  }
  
  return(aa_class)
}

##build_class_array----
#turn amino acid sequence into one_hot_encodding based on amino acid class at each position
build_class_array <- function(lib){
  #aa_class is a list of regexes for each amino acid class
  aa_class <- build_aa_class(regex = T)
  
  #make an empty array to fill
  class_array <- array(data = 0, dim = c(nrow(lib),
                                         max(nchar(lib$sequence)),
                                         length(aa_class)))
  
  #fill the empty array
  for (i in 1:length(aa_class)) {
    for(j in 1:nrow(lib)){
      seq <- strsplit(lib$sequence[j], '')[[1]]
      pos <- grep(aa_class[[i]], seq)
      class_array[j, pos, i] <- 1
    }
  }
  
  #return class array
  return(class_array)
}

##build_train_split----
build_train_split <- function(lib){
  
  #set seed so results are reproducible
  set.seed(123)
  
  #size of train set will be based on how many lives i Include
  #find lives and sample 80% of the rows and sample an equal amount of dying sequences
  live_ro <- which(lib$binary_stop == 'live')
  
  #how many sequences is ~80%
  sample_size <- (length(live_ro) * 0.8) %>% 
    round()
  
  #get live and die samples. Train will be the samples, test will be without the sampled data
  live_sample <- sample(live_ro, sample_size)
  
  die_sample <- which(lib$binary_stop == 'die') %>% 
    sample(., sample_size)
  
  train_sample <- lib[c(live_sample, die_sample),]
  
  test_sample <- lib[-c(live_sample, die_sample),]
  
  #both of these should be shuffled.
  shuffle <- sample(1:nrow(train_sample), nrow(train_sample))
  train_sample <- train_sample[shuffle,]
  
  shuffle <- sample(1:nrow(test_sample), nrow(test_sample))  
  test_sample <- test_sample[shuffle,]  
  
  #return the two sets as a list
  split_data <- list(train_sample, test_sample)
  return(split_data)
}

##build_lollipop----
#makes lollipops graphs
build_lollipop <- function(lib){
  live <- lib %>% filter(binary_stop == 'live')
  die <- lib %>% filter(binary_stop == 'die')
  
  live <- strsplit(live$sequence, '') %>% unlist() %>% table()
  die <- strsplit(die$sequence, '') %>% unlist() %>% table()
  
  df <- tibble(live, die, aa = names(live)) %>%
    mutate(
      live_freq = live/sum(live),
      die_freq = die/sum(die),
      enrich = log2(live_freq/die_freq)
      )
  
  return(df)
}

hold <- build_lollipop(random_library %>% filter(nchar(sequence) == 3))



