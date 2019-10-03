codon_tibble <- tibble(aa = c('a', 'b', 'c'),
                       codon = c('aaa', 'bbb', 'ccc'))

seqs <- c('aab', 'abc', 'ccbabababa', 'bbc')

seq_tibble <- tibble(aa_seq = seqs, nuc_seq = '')

for (i in 1:nrow(seq_tibble)) {
  #make a vector for each aa sequence
  seq <- strsplit(seq_tibble$aa_seq[i], '')[[1]]
  
  #just initialize a new nucleotide sequence for each aa sequence
  new_seq <- ''
  
  for(j in 1:length(seq)){
    #match aa letter with codon, and return as a character vector
    new_codon <- codon_tibble %>%
      filter(aa == seq[j]) %>%
      select(codon) %>% 
      as.character()
    #pastes all the codons as one nucleotide sequence
    new_seq <- paste(new_seq, new_codon, sep = '')
  }
  #save nucleotide sequence
  seq_tibble$nuc_seq[i] <- new_seq
}

