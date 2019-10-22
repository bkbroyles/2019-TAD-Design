###Analysis functions

#homogeneous vs heterogenous
test_sample <- tibble(sequence = c('GGGGGWWDDDWDWWW',    #5
                                   'GGGWWWWWWWWDDDW',    #3
                                   'GGGGGGGGWDWDWDWD',   #8
                                   'WDWDWDWDWDWDWDWD'),  #16
                      binary_stop = c('live', 'die', 'live', 'die'))

lib <- library_sets_list$random_WD20
lib$modules <- 0
for (i in 1:nrow(lib)) {
  #grab new sequences for each i
  seq <- lib$sequence[i]
  
  #remove G_linker
  seq <- gsub('G', '', seq)
  
  #cut right here... if sequence only have one letter it only has one module and will break my code
  if(nchar(seq) == 1){
    lib$modules[i] <- 1
  } else {
  #split seq into positions
  seq <- strsplit(seq, '')[[1]]
  
  #save pos 1 aa
  check_aa <- seq[1]
  
  #set modules to 1
  module <- 1
  
  #check if current aa matches last one, if not we are in a new module
  for (j in 2:length(seq)) {
    current_aa <- seq[j]
    if (current_aa != check_aa) {
      module <- module + 1
    }
    check_aa <- current_aa
  }
  lib$modules[i] <- module
  }
}

 