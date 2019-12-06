#I am trying to make a script which would find pallendormnes within the DNA sequences 
#going to make a test sequence to try this on 
#library(Biostrings)


#test<-("WDWDWDWDWDWDWDWDWD")
#test<-CodontoNuc(test)
#hold<-BString(test$nuc_seq)
#pallendromehold<-findPalindromes(hold, min.armlength = 2, min.looplength = 0)


#library_2019$combinatorial$sequence

#LibCombinatorial<-BString(library_2019$combinatorial$sequence)
#the input for biostrings must be a single string at a time, run a for loop 

#LibCombinatorial <- list()
#for(i in 1:nrow(library_2019$combinatorial)){
 # LibCombinatorial[i]<- BString(library_2019$combinatorial$sequence[i])
#}


#biohold<-list()
#biohold<-map(library_2019$combinatorial$sequence, BString)


#pallendrometest<-findPalindromes(biohold, min.armlength = 2, min.looplength = 0)
#cannot run a list through findpallendromes, will need to run it one at a time 
#pallendromehold<-list()
#for (i in 1:length(biohold)) {
 # pallendromehold[i]<-findPalindromes(biohold[[i]], min.armlength = 2, min.looplength = 0)
#}

##this needs to be converted to NT 
ugggggh<-CodontoNuc(library_2019$combinatorial$sequence)

biohold<-map(ugggggh$nuc_seq, DNAString)

pallendromehold<-list()
for (i in 1:length(biohold)) {
 pallendromehold[i]<-findPalindromes(biohold[[i]], min.armlength = 5, min.looplength = 0)
}


testing<-pallendromehold[[1]]
