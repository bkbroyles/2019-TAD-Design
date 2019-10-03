##Breds clean 2019
set <- 'p53_1rep'
library_2019$p53_1rep <- tibble(sequence = library_2019$p53_1rep,
                           set = 'p53_1rep')

set <- 'Rela2_1rep'
library_2019[[set]] <- tibble(sequence = library_2019[[set]],
                                set = set)

set <- 'CREBZF_1rep'
library_2019[[set]] <- tibble(sequence = library_2019[[set]],
                              set = set)

set <- 'AR_1rep'
library_2019[[set]] <- tibble(sequence = library_2019[[set]],
                              set = set)

set <- 'EKLF_1rep'
library_2019[[set]] <- tibble(sequence = library_2019[[set]],
                              set = set)

set <- 'ANAC013_1rep'
library_2019[[set]] <- tibble(sequence = library_2019[[set]],
                              set = set)

###Run Gumbo Clean to fill in 2reps before proceding
names(library_2019)

#I need to combine these into a final full library tibble (seq, set)
#1 combinatorial * extend to 12
#2-11 question sets ** double
#12_23 - motif 
#24 known tads ** double
#25 Umayrs Sequences




umayr <- c('MWNDELNWDMFDELWE',
'WNDEMNWDEYTLMWND',
'FDENLWDMWETLWDNWD',
'WDFEWDEFDWEDWDWDE',
'NWDMFDELTWMDLWDFDE',
'WDEMNFELWEDEWLMDEW',
'MWDFDELNWDEFENLWDEF',
'FDEWYDEFWDWEFYWEDYF',
'GWISWMLNDEPGTFDMNWD',
'IPWMEDGFDMLNWLDELWME')

nchar(umayr)
#add g to make len 20
for (i in 1:length(umayr)) {
  len <- nchar(umayr[i])
  geez <- 20 - len
  geez <- rep('G', geez) %>% paste(collapse = '')
  seq <- paste(geez, umayr[i], sep = '')
  umayr[i] <- seq
}
umayr <- tibble(sequence = umayr, set = 'Umayr')

#add umayr to library
library_2019$Umayr_set <- umayr

letters <- c('W', 'D')
len12 <- expand.grid(letters, letters, letters, letters,
            letters, letters, letters, letters,
            letters, letters, letters, letters, stringsAsFactors = F) %>% as_tibble()

len12$module = ''
len12$sequence = ''
for (i in 1:nrow(len12)) {
  len12$module[i] <- len12[i,1:12] %>% paste(collapse = '')
  len12$sequence[i] <- paste('GGGGGGGG', len12$module[i], sep = '')
}

len12 <- len12 %>% select(module, sequence)

#add to combinatorial
#library_2019$combinatorial <- rbind(library_2019$combinatorial, len12)

#I need to combine these into a final full library tibble (seq, set)
#1 combinatorial * extend to 12 ** done
#2-11 question sets ** double
#12_23 - motif 
#24 known tads ** double
#25 Umayrs Sequences
full_library <- library_2019$combinatorial %>% select(sequence) %>% 
  mutate(set = 'combinatorial')

#for 2-11 double and then add
for (i in 2:11) {
hold <- rbind(library_2019[[i]], library_2019[[i]]) %>% select(sequence) %>% 
  mutate(set = names(library_2019[i]))

full_library <- rbind(full_library, hold)
}

#12-23
for (i in 12:23) {
  full_library <- rbind(full_library, library_2019[[i]])
}

#24 known tads double
hold <- library_2019[[24]] %>% transmute(
  sequence = seq,
  set = paste('tad_', name, sep = '')
)

full_library <- rbind(full_library, hold)

#25 umayr sequences
full_library <- rbind(full_library, library_2019[[25]])

#26 random WD for 20 segments
full_library <- rbind(full_library, library_2019[[26]])

#library is done. add barcodes and put into excel sheet
#I need 9277 unique barcodes are they 20 aa or 20 nucleotides. I dont know. I am going to finish library export so 
  #I dont have to rerun these messy scripts
library_2019$full_library <- full_library

##export final library
#Export library as excel where each set is its own sheet----
library(xlsx)

for(i in 1:length(library_2019)){
  write.xlsx(library_2019[[i]], file = 'Final_AA_Library_2019_v2.xlsx', sheetName = names(library_2019)[i],
             append = T)
}

#also just save a smaller file with just full library
write.xlsx(library_2019$full_library, file = 'Full_AA_library_v2.xlsx')

#Use append = T in write.xlsx() to add on to this file in the future

#didn't work. FUll library doesn't have len12 wd sequences but has a space for them
full_library[5000,]

#save library as rds
saveRDS(library_2019, 'library_2019_r_data.rds')


library_2019 <- readRDS('library_2019_r_data.rds')

#combinatorial is messed up. i added blank sequences to len 12. lets remove the len 12 sequences and add the full ones
#and then make the same change in the full library version of combinatorial
nchar(library_2019$combinatorial$module[4094])
hold <- library_2019$combinatorial
hold <- hold[1:4094,]
hold <- rbind(hold, len12)

library_2019$combinatorial <- hold

#clean up full
hold <- library_2019$full_library
hold[4095:8190,] <- library_2019$combinatorial[4095:8190,] %>% select(sequence) %>% mutate(set = 'combinatorial')
library_2019$full_library <- hold

#ready for export

#add DNA bar code
dna_barcode <- list()
for (i in 1:10077) {
seq <- sample(1:4, 20, replace = T) %>% as.character() %>% paste(collapse = '')
seq <- gsub(1, 'A', seq)
seq <- gsub(2, 'T', seq)
seq <- gsub(3, 'G', seq)
seq <- gsub(4, 'C', seq)
dna_barcode[[i]] <- seq
}
dna_barcode <- tibble(barcode = dna_barcode %>% unlist())
unique(dna_barcode$barcode) %>% length()
full_library$dna_barcode <- dna_barcode$barcode


library_2019$full_library <- full_library

#stupid library is broken
#fix len12 glinkwds
library_2019$combinatorial[5000,]
nrow(library_2019$combinatorial)
#8190
full_library$sequence[1:8190] <- library_2019$combinatorial$sequence

library_2019$full_library <- full_library











log(43/38)


log(38/9.5)/10




