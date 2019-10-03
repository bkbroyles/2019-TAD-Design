##Find how big the space is for each natural motif

#p53 - [ALVIMWYF]..[ALVIMWYF][ALVIMWYF]
8 * 20 * 20 * 8 * 8
      #204,000 sequences

#RelA_2 - [ALVIMWYF][ALVIMWYF]..[ALVIMWYF]..[ALVIMWYF]
8 * 8 * 20 * 20 * 8 * 20 * 20 * 8
      #655,360,000 sequences

#CREBZF - D[VILM][VILM][RKDEQNHSTYC][RKDEQNHSTYC][VILM][VILFWYM]
1 * 4 * 4 * 11 * 11 * 4 * 7
      #54,208 sequences

#AR - F..LF
1 * 20 * 20 * 1 * 1
      #400 sequences

#ANAC013 - [DE].{1,2}[YF].{1,4}[DE]L
2 * 20 * 2 * 20 * 2 +
2 * 20 * 2 * 20 * 20 * 2 +
2 * 20 * 2 * 20 * 20 * 20 * 2 + 
2 * 20 * 2 * 20 * 20 * 20 * 20 * 2 +
2 * 20 * 20 * 2 * 20 * 2 +
2 * 20 * 20 * 2 * 20 * 20 * 2 +
2 * 20 * 20 * 2 * 20 * 20 * 20 * 2 +
2 * 20 * 20 * 2 * 20 * 20 * 20 * 20 * 2
      #565,891,200

#EKLF - [RKDEQNHSTYC][ALVIMWYF][ALVIMWYF]..[ALVIMWYF]..[RKDEQNHSTYC][RKDEQNHSTYC]
11 * 8 * 8 * 20 * 20 * 8 * 20 * 20 * 11 * 11
      #109,035,520,000

#WxxLF
20 * 20
      #400 squences



##Get 50 single copies and 50 double copies
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

saveRDS(motif_list, file = 'natural_motif_samples')

nchar(seq %>% unlist()) %>% table()









