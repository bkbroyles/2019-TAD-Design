##adding few more supplemenatry sets
#load libraries
library(tidyverse)

#need aavector
aa_vector <- c('A', 'L', 'V', 'I', 'M',
               'W', 'F', 'Y',
               'S', 'T', 'N', 'Q',
               'C', 'G', 'P',
               'R', 'H', 'K',
               'D', 'E')

df <- expand.grid(aa_vector, aa_vector) %>% 
  as_tibble() %>% 
  mutate(
    combo = paste(Var1, Var2, sep = '')
  )

#rep combo to make full 20 aa sequence
df$sequence <- ''
for (i in 1:nrow(df)) {
  df$sequence[i] <- rep(df$combo[i], 10) %>% 
    paste(collapse = '')
}

dipep_repeats <- df$sequence
saveRDS(dipep_repeats, file = 'dipep_repeat_sequences')




