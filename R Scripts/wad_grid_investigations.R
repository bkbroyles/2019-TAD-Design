##Investigate WAD_grid
#sequences with only W A and D amino acids from design set grid

#get grid set
lib <- design_library %>% filter(set == 'grid')

  #find sequences with only WAD
  ro <- grep('[^WAD]', lib$sequence)
  
  #remove matches and get simplified data frame
  lib <- lib[-ro,] %>% select(sequence, estimate, binary_stop)
  
  #add W count, D count, and A count
  lib$W_count <- 0
  lib$D_count <- 0
  lib$A_count <- 0
  
    #add data
    lib$W_count <- lapply(lib$sequence, function(x){
      seq <- strsplit(x,'')[[1]]
      count <- grep('W', seq) %>% length()
      return(count)
    }) %>% unlist()
    
    lib$D_count <- lapply(lib$sequence, function(x){
      seq <- strsplit(x,'')[[1]]
      count <- grep('D', seq) %>% length()
      return(count)
    }) %>% unlist()
    
    lib$A_count <- lapply(lib$sequence, function(x){
      seq <- strsplit(x,'')[[1]]
      count <- grep('A', seq) %>% length()
      return(count)
    }) %>% unlist()
    
  #add groups based composition
    lib$index <- lib %>% group_by(A_count, W_count, D_count) %>% 
      group_indices()
    
##Find live_percent vs length-----
    
    ##No correlation between length and live_percent here in WAD grid

#Add length
lib$length <- nchar(lib$sequence)

#get live percent for each length
lens <- unique(lib$length)

len_data <- tibble(lengths = lens, live_count = 0, die_count = 0)
for (i in 1:length(lens)) {
  df <- lib %>% filter(length == lens[i])
  df <- table(df$binary_stop)
  len_data$live_count[i] <- df['live']
  len_data$die_count[i] <- df['die']
}    

len_data <- len_data %>% mutate(
  live_percent = live_count/(live_count + die_count) * 100
)

ggplot(len_data, aes(lengths, live_percent))+
  geom_bar(stat = 'identity')

##Find aro_aci ratio vs live percent----
lib$aro_acid_ratio <- lib$W_count/lib$D_count

ratios <- unique(lib$aro_acid_ratio)   

ratio_data <- tibble(ratio = ratios, live_count = 0, die_count = 0)
for (i in 1:length(ratios)) {
  df <- lib %>% filter(aro_acid_ratio == ratios[i])
  df <- table(df$binary_stop)
  ratio_data$live_count[i] <- df['live']
  ratio_data$die_count[i] <- df['die']
} 

ratio_data <- ratio_data %>% mutate(
  live_percent = live_count/(live_count + die_count) * 100
)

ggplot(ratio_data[c(1:10, 12),], aes(ratio, live_percent))+
  geom_bar(stat = 'identity')

##Find heterogenous vs homogenous sequences by chunks
#for this I will remove the A so I just have W and D pushed up to each other
lib$module <- gsub('A', '', lib$sequence)    

#group by module
lib$module_group <- lib %>% group_by(module) %>% group_indices()

#Get live percent per module
#126 modules
module_data <- tibble(hold = 1:126)
module_data$module <- ''

for (i in 1:126) {
    df <- lib %>% filter(module_group == i)
    module_data$module[i] <- df$module[1]
}

  #add live and die count
  module_data$live_count <- 0
  module_data$die_count <- 0

  for (i in 1:126) {
    df <- lib %>% filter(module_group == i)
    df <- table(df$binary_stop)
    module_data$live_count[i] <- df['live']
    module_data$die_count[i] <- df['die']
  }
  
 ro <- which(is.na(module_data$live_count))
 module_data$live_count[ro] <- 0
 
 ro <- which(is.na(module_data$die_count))
 module_data$die_count[ro] <- 0
 
 #add live percent
 module_data$live_percent <- module_data$live_count/(module_data$live_count + module_data$die_count) * 100

hold <-  lib %>% filter(module_group == 82)

#count W and D chunks in each module
chunk_counter <- function(x){
  #if no module then 0 chunks
  if (nchar(x) == 0) {
    return(0)
  } 
  #if module is only 1 character then 1 chunk
  else if(nchar(x) == 1){
    return(1)
  } 
  #if module is longer than 1 character check it for multiple chunks like so
  else
  seq <- strsplit(x, '')[[1]]
  letter <- seq[1] #get the first letter, and see how long it matches the following letters
  chunks <- 1
  for (i in 2:length(seq)) {
    #when letter does not match add a chunk and change the letter to match
  if (letter != seq[i]) {
    letter <- seq[i]
    chunks <- chunks + 1
  }
  }
  return(chunks)
}


lapply(library_2019$combinatorial$module, chunk_counter) %>% unlist() %>% min()

ho <- c('a', 'b','c', 'd','e','f','g','h')

hold <- expand.grid(ho, ho, stringsAsFactors = F) %>% as_tibble()

hold <- paste(hold$Var1, hold$Var2, sep ='')

for(i in 1:nrow(hold)){
  lib$hold <- str_count(library_2019$combinatorial, combos[i])
  colnames(lib)[which(colnames(lib)=='hold')] <- paste('new name', i)
}


seq <- 'WWWWWWWWWWW'

match <- '(?=WWWWWW)'

grep(match, seq, perl = T)



 