##grab natural tads from random library
hold<-design_library %>% filter(set == 'tad_set_1')

#get ref seq from each tad set
table(design_library$set)

#should be 29 tads
ref_list <- list()
for (i in 1:29) {
  name <- paste('tad_set_', i, sep = '')
  hold <- design_library %>% filter(set == name)
  ref_list[[i]] <- hold$ref_seq[1]
}

known_tad <- tibble(tad = unlist(ref_list)) %>% 
  mutate(
    length = nchar(tad)
  )

known_tad$name <- ''
known_tad$name[27] <- 'Pdr1'
print(known_tad$tad)

known_tad <- known_tad %>% filter(name != '')
saveRDS(known_tad, file = 'natural_tad_sequences')
