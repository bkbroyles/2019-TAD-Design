#I am turning all the natural motifs into tibbles 
library_2019$p53_2rep<-tibble(sequence=library_2019$p53_2rep,
                              set= 'p53_2rep')


names(library_2019)

library_2019$Rela2_2rep<-tibble(sequence=library_2019$Rela2_2rep,
                                set= 'Rela2_2rep')


set<-'CREBZF_2rep'
library_2019[[set]]<- tibble(sequence= library_2019[[set]],
                              set= set)

set<-'AR_2rep'
library_2019[[set]]<- tibble(sequence= library_2019[[set]],
                             set= set)
set<-'EKLF_2rep'
library_2019[[set]]<- tibble(sequence= library_2019[[set]],
                             set= set)
set<-'ANAC013_2rep'
library_2019[[set]]<- tibble(sequence= library_2019[[set]],
                             set= set)





















