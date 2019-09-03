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


#make library amplication sequence 
amplificationseq1<-'CCCGCACCAGGTGCCGAATGGCGCGCCA'
amplificationseq2<-'GCGATCCTAGGGCGATCAGGC'
stopcodons<-'TAAGTAGCTGA'

#this is the random WD sequences at length 20 
rbinom(20, 1, 0.5)
hold<-list()
for ( i in 1:50){
  hold[[i]]<-rbinom(20, 1, 0.5)
}



Whold<-list()
for (i in 1:length(hold)){
  Whold[[i]]<-gsub('0', 'W', hold[[i]])
}

WDhold<-list()
for (i in 1:length(Whold)) {
  WDhold[[i]]<-gsub('1', 'D', Whold[[i]])
}


















