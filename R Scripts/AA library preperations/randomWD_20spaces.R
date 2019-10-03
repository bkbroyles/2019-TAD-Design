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

for (i in 1:length(WDhold)) {
  WDhold[[i]] <- paste(WDhold[[i]], collapse = '')
}
i <- 1

hold <- tibble(sequence = unlist(WDhold),
       set = 'random_WD20')

library_2019$random_WD20 <- hold













