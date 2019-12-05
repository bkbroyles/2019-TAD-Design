##Edit 2019 library per Dr. Erkine email 10/17/19

#taking motif sets from 100 sequences each, to 50
#removing another ~200 or there about from combinatorial 
#then finalizing another library
dir()
aa_library <- readRDS("Final_Robject_AA_library_v3.rds")

#get the names of motif sets
names(aa_library)

motif_sets <- names(aa_library)[grep('rep', names(aa_library))]

aa_library[[motif_sets[2]]]
#remove last 50 sequences from each set
for (i in motif_sets) {
  df <- aa_library[[i]]
  df <- df[1:50,]
  aa_library[[i]] <- df
}


#all the motifs_sets are length 50 and the 1rep matches the 2rep in motif

#get the length of the current library
  #add up all the set lengths
length(aa_library)
length_counter <- 0
for(i in 1:length(aa_library)){
  df <- aa_library[[i]]
  if(i >= 27){
    hold <- length(df)
  }else{
  hold <- nrow(df)
  }
  length_counter <- length_counter + hold
  print(length_counter)
}

##As of now library has 11183 amino acids
#andrew has 14 to add
#disuki has 1000 to add
#I need to add 10 stop codons
#and I need to add 1 more natural tad

#that is a total of 12208
11183 + 14 + 1000 + 10 + 1

#except I forgot some sets are to be doubled so lets get a new total
# I have it written down as 13,054 with doubled sets, disuki and stops
#so add 1 for natural tad and 14 from andrew
13054 + 1 + 14
#no its 13069 but I removed 600 from motif sets
13069-600

#so we have 12469 sequences, we need to lose 69 from wd12



#removing 69 from wd12
combinatorial <- aa_library[[1]]
rows <- which(nchar(combinatorial$module) == 12)
wd11 <- which(nchar(combinatorial$module) < 12)

wd11 <- combinatorial[wd11,] %>% as_tibble()
wd12 <- combinatorial[rows,] %>% as_tibble()

wd12_size <- nrow(wd12)
remove_rows <- sample(1:wd12_size, 69, replace = F)
wd12 <- wd12[-remove_rows,]

  #paste combinatorial back together
  combinatorial <- rbind(wd11, wd12)
  aa_library[[1]] <- combinatorial
  
  #also save the 69 sequences that will be left out just to have
  lost_wd12 <- wd12[remove_rows,]
  saveRDS(lost_wd12, '69_sequences_removed from combinatorial.rds')
  
#add andrews histone binding sets
histone_binding <- readRDS(file = "R Scripts/AA library preperations/HistoneBindingMotifSequences.rds")

#fixing names for DEF/Y
histone_binding$name[1:5] <- c('Chz1_S.c.', 
                               'YL1_H.s',
                               'Anp32e_H.s',
                               'Swr1_S.c',
                               'Spt16_S.c')

aa_library[[30]] <- histone_binding
names(aa_library)[[30]] <- 'histone_binding_set'


#save the library into a new file name v4
#library(xlsx)

#for(i in 1:length(aa_library)){
# write.xlsx(aa_library[[i]], file = 'Library_2019_v5.xlsx', sheetName = names(aa_library)[i],
#             append = T)
#}

#save aa_library
#saveRDS(aa_library, file = 'aa_library_v5.rds')




