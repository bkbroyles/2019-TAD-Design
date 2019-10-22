#making a tibble for the new histone binding domain sequences 
library(tidyverse)


HistoneBindingMotifHold<-tibble(sequence=c('EEDDEDEEDDDFKE', 'QSDTEDEVDSDFDI', 'EEIQDEEDDDDYVE', 'DGDLDLDDSEDFTV', 'ASDESEEEVSEYEA', 'YEPTEEECEWKPDEEDEISEELK', 'YEPTEEECEWKPDEEDEVSEELK', 'YEPTEEECEWKPDEEDEISEELK','EPTEEECEWKPDEEDEISEELKE','EPTEEECEWKPDEEDEVSEELKE',
                  'EPTEEECEWKPDEEDEISEELKE', 'EDDGDDYVE','EEEVSEYEA', 'EEDGKEYEQ'), 
       name=c('DEF/Y', 'DEF/Y', 'DEF/Y', 'DEF/Y', 'DEF/Y', 'Nap1L1 Hs_a', 'Nap1L1 Mm_a', 'Nap1L1 Bt_a', 'Nap1L1 Hs_b', 'Nap1L1 Mm_b', 'Nap1L1 Bt_b', 'Anp32E Hs', 'Spt16 Sc', 'YL1 Dm'))



HistoneBindingMotif<-tibble(sequence=c('EEDDEDEEDDDFKE', 'QSDTEDEVDSDFDI', 'EEIQDEEDDDDYVE', 'DGDLDLDDSEDFTV', 'ASDESEEEVSEYEA', 'YEPTEEECEWKPDEEDEISEELK', 'YEPTEEECEWKPDEEDEVSEELK', 'YEPTEEECEWKPDEEDEISEELK','EPTEEECEWKPDEEDEISEELKE','EPTEEECEWKPDEEDEVSEELKE',
                                           'EPTEEECEWKPDEEDEISEELKE', 'EDDGDDYVE','EEEVSEYEA', 'EEDGKEYEQ'), 
                                name=c('DEF/Y', 'DEF/Y', 'DEF/Y', 'DEF/Y', 'DEF/Y', 'Nap1L1 Hs_a', 'Nap1L1 Mm_a', 'Nap1L1 Bt_a', 'Nap1L1 Hs_b', 'Nap1L1 Mm_b', 'Nap1L1 Bt_b', 'Anp32E Hs', 'Spt16 Sc', 'YL1 Dm'))



#running this through GEEZ to fill out every sequence to 20 AA in length
HistoneBindingMotif$sequence <- ''
for(i in 1:nrow(HistoneBindingMotifHold)){
  len_factor <- nchar(HistoneBindingMotifHold$sequence[i])
  if(len_factor < 20){
  geez <- paste(rep('G', 20 - len_factor), collapse = '')
  HistoneBindingMotif$sequence[i] <- paste(geez, HistoneBindingMotifHold$sequence[i], sep = '') 
  } else {
    HistoneBindingMotif$sequence[i] <- HistoneBindingMotifHold$sequence[i]
  }
  
}


saveRDS(HistoneBindingMotif, file = 'HistoneBindingMotifSequences')
