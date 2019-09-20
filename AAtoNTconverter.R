#this is a script to change from anino acid sequences to nucleotide sequence
#making a codon chart 
tibble(aa=c("W", 'Y', 'F'),
       codon_1= c('a', 'b', 'c'),
       codon_2=c('aa','bb','cc'))


Codon_table<-tibble(aa= c('F','S','Y','C','L','P','H','R','I','T','N','V','A','D','G','E','K','M','Q'),
                    codon_1=c('TTT','TCT','TAT','TGT','CTT','CCT','CAT','CGT','ATT','ACT','AAT','GTT','GCT','GAT','GGT','GAA','AAA','ATG','CAA'),
                    codon_2= c('TTC','TCC','TAC','TGC','CTC','CCC','CAC','CGC','ATC','ACC','AAC','GTC','GCC','GAC','GGC','GAG','AAG','ATG','CAG'))

hold_a<-strsplit(library_2019$combinatorial$sequence,'')


NThold<-list()

for( i in 1:length(Codon_table$aa)){
  NThold[[i]]<-gsub(Codon_table$aa, Codon_table$codon_1, hold_a)
}



