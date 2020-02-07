#this is a script to change from anino acid sequences to nucleotide sequence
#making a codon chart 
tibble(aa=c("W", 'Y', 'F'),
       codon_1= c('a', 'b', 'c'),
       codon_2=c('aa','bb','cc'))


Codon_table<-tibble(aa= c('F','S','Y','C','L','P','H','R','I','T','N','V','A','D','G','E','K','M','Q', 'W'),
                    codon_1=c('TTT','TCT','TAT','TGT','CTT','CCT','CAT','CGT','ATT','ACT','AAT','GTT','GCT','GAT','GGT','GAA','AAA','ATG','CAA', 'TGG'),
                    codon_2= c('TTC','TCC','TAC','TGC','CTC','CCC','CAC','CGC','ATC','ACC','AAC','GTC','GCC','GAC','GGC','GAG','AAG','ATG','CAG', 'TGG'))


#making a funciton so that we dont have to keep retyping the 
# CodontoNuc(WHatever$sequence)

CodontoNuc<-function(df){
holdthetibble<-tibble(aa_seq= df, nuc_seq = '')



for (i in 1:nrow(holdthetibble)) {
  #make a vector for each aa sequence
  seq <- strsplit(holdthetibble$aa_seq[i], '')[[1]]
  
  #just initialize a new nucleotide sequence for each aa sequence
  new_seq <- ''
  
  for(j in 1:length(seq)){
    #match aa letter with codon, and return as a character vector
    new_codon <- Codon_table %>%
      filter(aa == seq[j]) %>%
      select(codon_1) %>% 
      as.character()
    #pastes all the codons as one nucleotide sequence
    new_seq <- paste(new_seq, new_codon, sep = '')
  }
#save nucleotide sequence
holdthetibble$nuc_seq[i] <- new_seq
}
return(holdthetibble)
}

library_2019$

library_2019_sequences<-list()

library_2019_sequences$W_first<-CodontoNuc(library_2019$W_first$sequence)

library_2019_sequences$combinatorial<- CodontoNuc(library_2019$combinatorial$sequence)

library_2019_sequences$D_first<-CodontoNuc(library_2019$D_first$sequence)

library_2019_sequences$set_g_spacing<- CodontoNuc(library_2019$set_g_spacing$sequence)

library_2019_sequences$g_in<-CodontoNuc(library_2019$g_in$sequence)

library_2019_sequences$g_out<-CodontoNuc(library_2019$g_out$sequence)

library_2019_sequences$terminal_wd<-CodontoNuc(library_2019$temrinal_wd$sequence)

library_2019_sequences$internal_wd<-CodontoNuc(library_2019$internal_wd$sequence)




