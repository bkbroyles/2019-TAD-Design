##Right now I have a clean library, but I need to add additional sets
#I will load in individual library sets, replace and add the ones I need to 
#and then make a final set again with new barcodes

##load in final library set.... do not overwrite cause I don't have nice scripts to regenerate
library(tidyverse)
library(xlsx)

#load library sets
lib_2019 <- list()
for (i in 2:27) {
lib_2019[[i]] <- read.xlsx("Final_AA_Library_2019_v2.xlsx" , sheetIndex = i)
}

lib_2019[[1]] <- read.xlsx("Final_AA_Library_2019_v2.xlsx" , sheetIndex = 1)
read.xlsx("Final_AA_Library_2019_v2.xlsx" , sheetIndex = 7)

#okay so something is going on, and I can't load my sequences back into R
#it goes from a 630 kb excel file to over 3mb in the first 6 sets,
  #it will be too big, and number seven wont load either
  #I will do the cleaning with some manual excel work. For now just make new sets
known_tads <- read.csv('cnjr_known_tads.csv') %>% 
  as_tibble() %>% 
  mutate(
    tad = sequence %>% as.character()
  ) %>% 
  select(
    name, tad, length_aa
  )

#add G to make all length 20, and remove Gcn4 for being too long

known_tads$sequence <- lapply(known_tads$tad, function(x){
          len <- nchar(x)
          if(len < 20){
          geez <- rep('G', 20 - len) %>% paste(collapse = '')
          seq <- paste(geez, x, sep = '')
          return(seq)
          } else
            return(x)
        }) %>% unlist()

known_tads_add_set <- known_tads %>% 
  filter(length_aa < 21) %>% 
  select(name, sequence)

##I lost script to generate motifs
#now i know how andrew feels
#i will rewrite them here

#just kidding found it
##Get 100 single copies and 100 double copies
#p53 [ALVIMWYF]..[ALVIMWYF][ALVIMWYF]
reggie1 <- c('A','L','V','I','M','W','Y','F')
aa_vector <- c('A', 'L', 'V', 'I', 'M',
               'W', 'F', 'Y',
               'S', 'T', 'N', 'Q',
               'C', 'G', 'P',
               'R', 'H', 'K',
               'D', 'E')


seq <- matrix(data = '', nrow = 100, ncol = 5) 

for(i in 1:100){
  seq[i, c(1,4,5)] <- sample(reggie1, 3, replace = T)
  seq[i, 2:3] <- sample(aa_vector, 2, replace = T)
}

motifs <- seq %>% as_tibble(.name_repair = 'unique') %>% 
  mutate(
    seq = paste(...1,...2,...3,...4,...5,sep = '')
  )

motif_list <- list()
motif_list$p53 <- motifs$seq

#Rela2  - [ALVIMWYF][ALVIMWYF]..[ALVIMWYF]..[ALVIMWYF]
reggie1 <- c('A','L','V','I','M','W','Y','F')
aa_vector <- c('A', 'L', 'V', 'I', 'M',
               'W', 'F', 'Y',
               'S', 'T', 'N', 'Q',
               'C', 'G', 'P',
               'R', 'H', 'K',
               'D', 'E')

seq <- matrix(data = '', nrow = 100, ncol = 8)

for (i in 1:100) {
  seq[i, c(1,2,5,8)] <- sample(reggie1, 4, replace = T)
  seq[i, c(3,4,6,7)] <- sample(aa_vector, 4, replace = T)
}

motifs <- seq %>% as_tibble(.name_repair = 'unique') %>% 
  mutate(
    seq = paste(...1,...2,...3,...4,...5,...6,...7,...8, sep = '')
  )

motif_list$Rela2 <- motifs$seq

#CREBZF - D[VILM][VILM][RKDEQNHSTYC][RKDEQNHSTYC][VILM][VILFWYM]
reggie1 <- c('V', 'I', 'L', 'M')
reggie2 <- c('R', 'K', 'D', 'E', 'Q', 'N',
             'S', 'T', 'Y', 'C', 'H')
reggie3 <- c('V', 'I', 'L', 'F', 'W', 'Y', 'M')

seq <- matrix(data = '', nrow = 100, ncol = 7)

for (i in 1:100) {
  seq[i, 1] <- 'D'
  seq[i, c(2,3,6)] <- sample(reggie1, 3, replace = T)
  seq[i, c(4,5)] <- sample(reggie2, 2, replace = T)
  seq[i, 7] <- sample(reggie3, 1)
}

motifs <- seq %>% as_tibble(.name_repair = 'unique') %>% 
  mutate(
    seq = paste(...1,...2,...3,...4,...5,...6,...7, sep = '')
  )

motif_list$CREBZF <- motifs$seq

#AR - F..LF
seq <- matrix(data = '', nrow = 100, ncol = 5)
for (i in 1:100) {
  seq[i, c(1,5)] <- 'F'
  seq[i, c(2,3)] <- sample(aa_vector, 2, replace = T)
  seq[i, 4] <- 'L'
}

motifs <- seq %>% as_tibble(.name_repair = 'unique') %>% 
  mutate(
    seq = paste(...1,...2,...3,...4,...5, sep = '')
  )

motif_list$AR <- motifs$seq

#EKLF - [RKDEQNHSTYC][ALVIMWYF][ALVIMWYF]..[ALVIMWYF]..[RKDEQNHSTYC][RKDEQNHSTYC]
reggie1 <- c('R', 'K', 'D', 'E', 'Q', 'N',
             'S', 'T', 'Y', 'C', 'H')
reggie2 <- c('A','L','V','I','M','W','Y','F')

seq <- matrix(data = '', nrow = 100, ncol = 10)

for (i in 1:100) {
  seq[i, c(1,9,10)] <- sample(reggie1, 3, replace = T)
  seq[i, c(2,3,6)] <- sample(reggie2, 3, replace = T)
  seq[i, c(4,5,7,8)] <- sample(aa_vector, 4, replace = T)
}

motifs <- seq %>% as_tibble(.name_repair = 'unique') %>% 
  mutate(
    seq = paste(...1,...2,...3,...4,...5,...6,...7,...8,...9,...10, sep = '')
  )

motif_list$EKLF <- motifs$seq


#ANAC013 - [DE].{1,2}[YF].{1,4}[DE]L
reggie1 <- c('D', 'E')
reggie2 <- c('Y', 'F')
size1 <- c(1, 2)
size2 <- c(1,2,3,4)

seq <- list()
for (i in 1:100) {
  section1 <- sample(reggie1, 1)
  section2 <- sample(aa_vector, size = sample(size1, 1),
                     replace = T)
  section2 <- paste(section2, collapse = '')
  section3 <- sample(reggie2, 1)
  section4 <- sample(aa_vector, size = sample(size2, 1), 
                     replace = T)
  section4 <- paste(section4, collapse = '')
  section5 <- sample(reggie1, 1)
  section6 <- 'L'
  seq[[i]] <- paste(section1, section2, section3, section4, section5, section6, sep = '')
}

motif_list$ANAC013 <- seq %>% unlist()

  #6 subsets of list (make single and double repeats)

#build sequences and add to library
p53_1rep_add_set <- paste('GGGGGGGGGGGGGGG', motif_list$p53, sep = '')
p53_1rep_add_set <- tibble(name = 'p53', sequence = p53_1rep_add_set)

p53_2rep_add_set <- paste('GGGGGGGGGG', motif_list$p53, motif_list$p53, sep = '')
p53_2rep_add_set <- tibble(name = 'p53x2', sequence = p53_2rep_add_set)


Rela2_1rep_add_set <- paste('GGGGGGGGGGGG', motif_list$Rela2, sep = '')
Rela2_1rep_add_set <- tibble(name = 'Rela2', sequence = Rela2_1rep_add_set)

Rela2_2rep_add_set <- paste('GGGG', motif_list$Rela2, motif_list$Rela2, sep = '')
Rela2_2rep_add_set <- tibble(name = 'Rela2x2', sequence = Rela2_2rep_add_set)


CREBZF_1rep_add_set <- paste('GGGGGGGGGGGGG', motif_list$CREBZF, sep = '')
CREBZF_1rep_add_set <- tibble(name = 'CREBZF', sequence = CREBZF_1rep_add_set)

CREBZF_2rep_add_set <- paste('GGGGGG', motif_list$CREBZF, motif_list$CREBZF, sep = '')
CREBZF_2rep_add_set <- tibble(name = 'CREBZFx2', sequence = CREBZF_2rep_add_set)


AR_1rep_add_set <- paste('GGGGGGGGGGGGGGG', motif_list$AR, sep = '')
AR_1rep_add_set <- tibble(name = 'AR', sequence = AR_1rep_add_set)

AR_2rep_add_set <- paste('GGGGGGGGGG', motif_list$AR, motif_list$AR, sep = '')
AR_2rep_add_set <- tibble(name = 'ARx2', sequence = AR_2rep_add_set)


EKLF_1rep_add_set <- paste('GGGGGGGGGG', motif_list$EKLF, sep = '')
EKLF_1rep_add_set <- tibble(name = 'EKLF', sequence = EKLF_1rep_add_set)

EKLF_2rep_add_set <- paste(motif_list$EKLF, motif_list$EKLF, sep = '')
EKLF_2rep_add_set <- tibble(name = 'EKLFx2', sequence = EKLF_2rep_add_set)

#anac013 has variable length so I need to calculate
seqs <- list()
for (i in 1:100) {
  seq <- motif_list$ANAC013[i]
  len <- nchar(seq)
  geez <- rep('G', 20 - len) %>% paste(collapse = '')
  seqs[[i]] <- paste(geez, seq, sep = '')
}
ANAC013_1rep_add_set <- unlist(seqs)
ANAC013_1rep_add_set <- tibble(name = 'ANAC013', sequence = ANAC013_1rep_add_set)

seqs <- list()
for (i in 1:100) {
  seq <- motif_list$ANAC013[i]
  len <- nchar(seq) * 2
  geez <- rep('G', 20 - len) %>% paste(collapse = '')
  seqs[[i]] <- paste(geez, seq, seq, sep = '')
}
ANAC013_2rep_add_set <- unlist(seqs)
ANAC013_2rep_add_set <- tibble(name = 'ANAC013x2', sequence = ANAC013_2rep_add_set)

###Make two more sets 
  #WD for ten spaces, equal amount of R spaced one G away except for Dx10
  #sample the previous set to fill out library (I will have to double some sets and calculate how much room I have left)
lib <- c('W', 'D')
hold <- expand.grid(lib, lib, lib, lib, lib,
            lib, lib, lib, lib, lib,
            stringsAsFactors = F) %>% as_tibble()

seq <- list()
for (i in 1:nrow(hold)) {
seq[[i]] <- paste(hold[i,1:10], collapse = '')
}
seq <- unlist(seq)
hold_set <- tibble(motif = seq) %>% 
  mutate(
    D_count = str_count(motif, 'D')
  )

seq <- list()
for (i in 1:nrow(hold_set)) {
if(hold_set$D_count[i] < 10){
count <- hold_set$D_count[i]
rs <- rep('R', count) %>% paste(collapse = '')
geez <- rep('G', 9 - nchar(rs)) %>% paste(collapse = '')
seq[[i]] <- paste(geez, rs, 'G', hold_set$motif[i], sep = '')
} else {
  seq[[i]] <- paste('RRRRRRRRRR', hold_set$motif[i], sep = '')
}
}


t <- 0
for(i in 1:12){
  t <- t + 2^i
}
t



equal_R_D_add_set <- tibble(sequence = unlist(seq))

#set up code to sample and replace with K
size <- 300
ro <- sample(1:1024, size)
seqs <- equal_R_D_add_set$sequence[ro] %>% gsub('R', 'K', .)
equal_K_D_add_set <- tibble(sequence = seqs)







#save the addsets as individual
sets <- ls()
sets <- sets[grep('add_set', sets)]
for (i in 1:length(sets)) {
  df <- get(sets[i])
  name <- paste(sets[i], '.csv', sep = '')
  write.csv(df, file = name)
}
get(sets[1])












