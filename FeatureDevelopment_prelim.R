
#this is exporting the library as an excel
lapply(library_2019, function(x) write.table( data.frame(x), 'test.csv'  , append= T, sep=',' ))
#this worked but then I had to copy everything into its own seperate page 
#brad's worked better 








#now I am trying to run these ideas from the new library on the design library 

designhold<-subset(design_library, select = c('sequence', 'estimate', 'binary_stop'))
designhold$length<-0
designhold$length<- nchar(designhold$sequence)
library(stringr)
#now that I have length I will find the count of each amino acids 
designhold$W_count<-0
designhold$A_count<-0
designhold$R_count<-0
designhold$K_count<-0
designhold$D_count<-0
#now count
designhold$W_count<-str_count(designhold$sequence, 'W')
designhold$A_count<-str_count(designhold$sequence, 'A')
designhold$R_count<-str_count(designhold$sequence, 'R')
designhold$K_count<-str_count(designhold$sequence, 'K')
designhold$D_count<-str_count(designhold$sequence, 'D')

#so now I have the count of each amino acids in the sequences 

livehold<-designhold[which(designhold$binary_stop=='live'),]
diehold<-designhold[which(designhold$binary_stop=='die'),]

#now I am going to compare the live percent based off length of the sequence and see if 
#there is any any correlaiton between length and live 
Onehold<-designhold[which(designhold$length== 1),]
Twohold<-designhold[which(designhold$length== 2),]
Threehold<-designhold[which(designhold$length==3),]
Fourhold<-designhold[which(designhold$length==4),]
Fivehold<-designhold[which(designhold$length==5),]
Elevenhold<-designhold[which(designhold$length==11),]
#I could fill this out to 11 spaces 
#Because these are the same length I should be able to just find the live and die percent for each set 
Onehold[which(Onehold$binary_stop=='live'),]
Onehold[which(Onehold$binary_stop=='die'),]
#there are no sequences with the length one in this lib

#there are sequences with length 11 tho 
elevenlive<-nrow(Elevenhold[which(Elevenhold$binary_stop=='live'),])
elevendie<- nrow(Elevenhold[which(Elevenhold$binary_stop=='die'),])
elevenpercentlive<-(elevenlive/elevendie)*100
#this worked to create the percent live at a specific position 


#this may actually not be needed 
#what if I just plotted lenght vs the %live 

#I am going to try to find the number of times each lenght occurs in the living and dying pools 
ggplot(data= livehold, aes(livehold$length))+
  geom_histogram()
#now i am going to do the same thing with the dead pool 
ggplot(data=diehold, aes(diehold$length))+
  geom_histogram()
#note: these graphs are really really bad, this is becasue of the different sets within
##the design lib, we could run these on the combinatorial space alone 

#so now I am going to try to address the redundancy feature 
#look at the onenote doc for guidance 

#all the sequences which I am looking for are within the combinatorial space 
library_2019$combinatorial
#these are the eight tripeptides which we are going to use to describe the sequences 
A<-'WWW'
B<-'WWD'
C<-'WDD'
D<-'DDD'
E<-'DDW'
F<-'DWW'
G<-'DWD'
H<-'WDW'

TPhold<-c(A, B, C, D, E, F, G, H)

#this is just creating a tibble for me to work in 
lib2019hold<-subset(library_2019$combinatorial, select='sequence')


#this finds sequences which have the specific tripeptide from her we can find the live percent 
#based on the occurance of any tripeptide 
Ahold<-grep(A, lib2019hold$sequence)

#trying to do this as a for loop 


lib2019hold$WWW_count<-str_count(lib2019hold$sequence, A)
lib2019hold$WWD_count<-str_count(lib2019hold$sequence, B)
lib2019hold$WDD_count<-str_count(lib2019hold$sequence, C)
lib2019hold$DDD_count<-str_count(lib2019hold$sequence, D)
lib2019hold$DDW_count<-str_count(lib2019hold$sequence, E)
lib2019hold$DWW_count<-str_count(lib2019hold$sequence, F)
lib2019hold$DWD_count<-str_count(lib2019hold$sequence, G)
lib2019hold$WDW_count<-str_count(lib2019hold$sequence, H)

#I am going to try using grep because this wil allow me to find the occurances within 
#different reading frames 
grep(H, lib2019hold$sequence, perl = TRUE)




#now I am going to try to create the tripeptide pairs 

TP_pair<-apply(expand.grid(TPhold,TPhold), 1, paste, collapse= '')

dfhold<-tibble(combos= TP_pair, count= 0)
for ( i in length(TP_pair)){
  dfhold$count[i]<-str_count()
}



for(i in 1:length(TP_pair)){
  lib2019hold$hold <- str_count(lib2019hold$sequence, TP_pair[i])
  colnames(lib2019hold)[which(colnames(lib2019hold)=='hold')] <- 
    paste(TP_pair[i], '_count', sep = '')
}







































