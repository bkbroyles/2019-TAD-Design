#Load Libraries
library(ggplot2)

##Need a function to generate data for classes
##I need to have a count for each class
#aromatic, special, polar, basic, acidic, aliphatic----
lib <- random_library

#make aa class
aliphatic <-  c('A','L','V','I','M')
aromatic <-  c('W','Y','F')
polar <-  c('S','T','N','Q')
special <-  c('C','G','P')
basic <-  c('R','K','H')
acidic <-  c('D','E')
W <- 'W'
F. <- 'F'
Y <- 'Y'

#add class count columns
class_count <- function(x,y){
  seq <- strsplit(x, '')[[1]]
  count <- which(seq %in% y) %>% length()
  return(count)
}

lib$aliphatic <- lapply(lib$sequence, class_count, y = aliphatic) %>% 
                      unlist()

lib$aromatic <- lapply(lib$sequence, class_count, y = aromatic) %>% 
  unlist()

lib$polar <- lapply(lib$sequence, class_count, y = polar) %>% 
  unlist()

lib$special <- lapply(lib$sequence, class_count, y = special) %>% 
  unlist()

lib$basic <- lapply(lib$sequence, class_count, y = basic) %>% 
  unlist()

lib$acidic <- lapply(lib$sequence, class_count, y = acidic) %>% 
  unlist()

lib$W <- lapply(lib$sequence, class_count, y = W)
lib$F. <- lapply(lib$sequence, class_count, y = F.)
lib$Y <- lapply(lib$sequence, class_count, y = Y)

##Build class x class data----
live_table <- function(x){
  df <- table(x$binary_stop)
  return(c(df['live'], df['die']))
}

live_percent <- function(x){
  x['live']/(x['live'] + x['die']) * 100
}

hold <- matrix(0, nrow = 8, ncol = 3)

#Two classes to compare
class1 <- 'acidic'
#class2 <- 'acidic'

for (i in 0:7) {
  df <- lib %>% filter(
    acidic > i
    #acidic > i
  )
    hold[i + 1,] <- c(
      live_table(df),
      live_table(df) %>% live_percent()
    )
}



#I need to turn hold into a tibble
df <- hold %>% as_tibble()

colnames(df) <- c('live_count',
                  'die_count',
                  'live_percent')

  #add a character column for >0, >1, >2 for x axis
  df$aa_count <- paste(rep('>', 8), 0:7, sep = '')
  
  
  
#make a nice ggplot
ggplot(df, aes(aa_count, live_percent))+
  geom_bar(stat = 'identity')+
  coord_cartesian(ylim = c(0,50))+
  labs(title = paste(class1, 'and', class2, sep = ' ')
         )+
  xlab('Amino Acid Class Count')+
  ylab('Percent Living')














