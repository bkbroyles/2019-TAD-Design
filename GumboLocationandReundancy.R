#redundancy in random library 
randomhold<-subset(random_library, select= c('sequence', 'estimate', 'binary_stop'))
randomhold$length<-0
randomhold$length<-nchar(randomhold$sequence)
#above is creating a library copy to work with 
#now I am going to create the tripeptides 
A<-'WWW'
B<-'WWD'
C<-'WDD'
D<-'DDD'
E<-'DDW'
F<-'DWW'
G<-'DWD'
H<-'WDW'

TPhold<-c(A, B, C, D, E, F, G, H)

WWWhold<- grep(A, randomhold$sequence)

randomhold$WWW_count<-str_count(randomhold$sequence, A)
randomhold$WWD_count<-str_count(randomhold$sequence, B)
randomhold$WDD_count<-str_count(randomhold$sequence, C)
randomhold$DDD_count<-str_count(randomhold$sequence, D)
randomhold$DDW_count<-str_count(randomhold$sequence, E)
randomhold$DWW_count<-str_count(randomhold$sequence, F)
randomhold$DWD_count<-str_count(randomhold$sequence, G)
randomhold$WDW_count<-str_count(randomhold$sequence, H)
#the code works and I have made a feature based on the presence of each tripeptide 
#the problem with running this on the random library is that there are other amino acids besides just WD
##therefor I am essentially just looking for different motifs within the random lib (not good ones either)
###as a proof of concept I would need to create a tripeptide for each combinatnion of 20^3
###so there would be 8000 possible tripeptide combinations 

#shifting gears. attempting to find the distance from the end and the beginging 
randomhold$motif<-str_detect(randomhold$sequence, '[WYF].{0,3}[DE]|[DE].{0,3}[WYF]', negate= FALSE)
#that found the sequences whihc have the motif 
#making a data set of just the true sequences 
randomlocation<-randomhold[which(randomhold$motif=="TRUE"),]
#now I am going to try to find the location of the motif 
#this tells us where the motif begins and where it ends
randomlocation$location<-str_locate_all(pattern= '[WYF].{0,3}[DE]|[DE].{0,3}[WYF]', randomlocation$sequence)

qplot(randomlocation$location,geom='histogram')

lapply(strsplit(randomlocation$sequence, ''), function(x) which(randomlocation$sequence == '[WYF].{0,3}[DE]|[DE].{0,3}[WYF]'))
