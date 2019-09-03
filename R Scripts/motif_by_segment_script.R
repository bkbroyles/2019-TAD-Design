lib <- random_library

lib <- lib %>% filter(nchar(sequence) == 20)


front_half <- list()
for(i in 1:nrow(lib)){
  front_half[[i]] <- strsplit(lib$sequence[i], '')[[1]][1:10] %>% paste(collapse = '')
}

split_df <- tibble(front_half = front_half %>% unlist())

back_half <- list()
for(i in 1:nrow(lib)){
  back_half[[i]] <- strsplit(lib$sequence[i], '')[[1]][11:20] %>% paste(collapse = '')
}

split_df <- tibble(front_half = front_half %>% unlist(),
                   back_half = back_half %>% unlist(),
                   binary_stop = lib$binary_stop, 
                   estimate = lib$estimate)

#regex these sequences
reggie <- '(?=[WYF].{0,4}[DE]|[DE].{0,4}[WYF])'
reggie <- '[RHK]'
reggie <- '[WYF].{0,4}[DE]'
reggie <- '[DE].{0,4}[WYF]'

ro <- grep(reggie, split_df$back_half, perl = T)

table(split_df$binary_stop[ro])

#add aromatic and acidic counts for front and back
split_df <- split_df %>% 
  mutate(
    front_aro = str_count(front_half, pattern = '[WYF]'),
    front_acid = str_count(front_half, pattern = '[DE]'),
    back_aro = str_count(back_half, '[WYF]'),
    back_acid = str_count(back_half, '[DE]')
  )



hold <- split_df %>% filter(back_aro == 5)
table(hold$binary_stop)

summary(split_df$back_aro)

#split sequences into 4 sections of 5 aas
lib <- random_library %>% filter(nchar(sequence) == 20)

split_df <- tibble(sequence = lib$sequence,
                   section_1 = '',
                   section_2 = '',
                   section_3 = '',
                   section_4 = '')

for(i in 1:nrow(split_df)){
split <- strsplit(lib$sequence[i], '')[[1]]
split_df$section_1[i] <- split[1:5] %>% paste(collapse = '')
split_df$section_2[i] <- split[6:10] %>% paste(collapse = '')
split_df$section_3[i] <- split[11:15] %>% paste(collapse = '')
split_df$section_4[i] <- split[16:20] %>% paste(collapse = '')
}

#look in each of these sections for motifs and live percent
split_df$match_1 <- 0
ro <- grep('[^RHK]{5}', split_df$section_1)
split_df$match_1[ro] <- 1

split_df$match_2 <- 0
ro <- grep('[^RHK]{5}', split_df$section_2)
split_df$match_2[ro] <- 1

split_df$match_3 <- 0
ro <- grep('[^RHK]{5}', split_df$section_3)
split_df$match_3[ro] <- 1

split_df$match_4 <- 0
ro <- grep('[^RHK]{5}', split_df$section_4)
split_df$match_4[ro] <- 1

split_df$binary_stop <- lib$binary_stop

##find live percent of each combo
combos <- expand.grid(0:1, 0:1, 0:1, 0:1) %>% as_tibble()
colnames(combos) <- c('match_1', 'match_2', 'match_3', 'match_4')

combos$live_count <- 0
combos$die_count <- 0

for(i in 1:nrow(combos)){
  df <- split_df %>% filter(match_1 == combos$match_1[i]) %>% 
                     filter(match_2 == combos$match_2[i]) %>% 
                     filter(match_3 == combos$match_3[i]) %>% 
                     filter(match_4 == combos$match_4[i])
  combos$live_count[i] <- df %>% filter(binary_stop == 'live') %>% nrow()
  combos$die_count[i] <- df %>% filter(binary_stop == 'die') %>% nrow()
}

combos$live_percent <- combos$live_count/(combos$live_count + combos$die_count) * 100

#
#count aromatics instead of regex
lib <- random_library %>% filter(nchar(sequence) == 20)

split_df <- tibble(sequence = lib$sequence,
                   section_1 = '',
                   section_2 = '',
                   section_3 = '',
                   section_4 = '',
                   binary_stop = lib$binary_stop)

for(i in 1:nrow(split_df)){
  split <- strsplit(lib$sequence[i], '')[[1]]
  split_df$section_1[i] <- split[1:5] %>% paste(collapse = '')
  split_df$section_2[i] <- split[6:10] %>% paste(collapse = '')
  split_df$section_3[i] <- split[11:15] %>% paste(collapse = '')
  split_df$section_4[i] <- split[16:20] %>% paste(collapse = '')
}

##find live percent of each combo
split_df$aro_1 <- 0
split_df$aro_2 <- 0
split_df$aro_3 <- 0
split_df$aro_4 <- 0
for(i in 1:nrow(split_df)){
  split_df$aro_1[i] <- str_count(split_df$section_1[i], '[WYF]')
  split_df$aro_2[i] <- str_count(split_df$section_2[i], '[WYF]')
  split_df$aro_3[i] <- str_count(split_df$section_3[i], '[WYF]')
  split_df$aro_4[i] <- str_count(split_df$section_4[i], '[WYF]')
}

##get combos of aromatics per section
combos2 <- expand.grid(0:4,0:4,0:4,0:4) %>% as_tibble()
colnames(combos2) <- c('aro_1', 'aro_2', 'aro_3', 'aro_4')

combos$live_count <- 0
combos$die_count <- 0

for(i in 1:nrow(combos2)){
  df <- split_df %>% filter(aro_1 == combos2$aro_1[i]) %>% 
    filter(aro_2 == combos2$aro_2[i]) %>% 
    filter(aro_3 == combos2$aro_3[i]) %>% 
    filter(aro_4 == combos2$aro_4[i])
  combos2$live_count[i] <- df %>% filter(binary_stop == 'live') %>% nrow()
  combos2$die_count[i] <- df %>% filter(binary_stop == 'die') %>% nrow()
}

combos2$live_percent <- combos2$live_count/(combos2$live_count + combos2$die_count) * 100

#remove times where there were no matches
combos2 <- combos2 %>% mutate(
              count = live_count + die_count
            ) %>% filter(count > 0)


##NO sequences lived with 4 aromatics in a section can 4 aromatics in row live
ro <- grep('[WYF]{5}', random_library$sequence)
df <- random_library[ro,]
table(df$binary_stop)
#1 aromatic in a row - 582 live, 38736 die - 1.5% live
#2 aromatic in a row - 129 live, 4546 die - 2.8% live
#3 aromatic in a row - 11 live, 345 die - 3.1 % live
#4 aromatic in a row - 1 live, 22 die - 4.3% live
#5 aromatic in a row - 0 live, 2 die - 0% live

quantile(combos2$count)



ro <- grep('[^RHK]', random_library$sequence)
df <- random_library[ro,]
table(df$binary_stop)


####################
##SPlit_df
split_df$aro_1 <- str_count(split_df$section_1, '[WYF]')
split_df$acid_1 <- str_count(split_df$section_1, '[DE]')

split_df$aro_2 <- str_count(split_df$section_2, '[WYF]')
split_df$acid_2 <- str_count(split_df$section_2, '[DE]')

split_df$aro_3 <- str_count(split_df$section_3, '[WYF]')
split_df$acid_3 <- str_count(split_df$section_3, '[DE]')

split_df$aro_4 <- str_count(split_df$section_4, '[WYF]')
split_df$acid_4 <- str_count(split_df$section_4, '[DE]')

#aro is 0,1, or 2 which is 2 or more
ro <- which(split_df$aro_1 > 2)
split_df$aro_1[ro] <- 2

ro <- which(split_df$aro_2 > 2)
split_df$aro_2[ro] <- 2

ro <- which(split_df$aro_3 > 2)
split_df$aro_3[ro] <- 2

ro <- which(split_df$aro_4 > 2)
split_df$aro_4[ro] <- 2

#get combos
combos <- expand.grid(0:2, 0:2, 0:2, 0:2) %>% as_tibble()

combos$live_count <- 0
combos$die_count <- 0

for(i in 1:nrow(combos)){
  df <- split_df %>% 
    filter(aro_1 == combos$Var1[i]) %>% 
    filter(aro_2 == combos$Var2[i]) %>% 
    filter(aro_3 == combos$Var3[i]) %>% 
    filter(aro_4 == combos$Var4[i])
  combos$live_count[i] <- df %>% filter(binary_stop == 'live') %>% nrow()
  combos$die_count[i] <- df %>% filter(binary_stop == 'die') %>% nrow()
}

combos$live_percent <- combos$live_count/(combos$live_count + combos$die_count) * 100

##DO SAME FOR ACIDIC
#aro is 0,1, or 2 which is 2 or more
ro <- which(split_df$acid_1 > 2)
split_df$acid_1[ro] <- 2

ro <- which(split_df$acid_2 > 2)
split_df$acid_2[ro] <- 2

ro <- which(split_df$acid_3 > 2)
split_df$acid_3[ro] <- 2

ro <- which(split_df$acid_4 > 2)
split_df$acid_4[ro] <- 2

#get combos
combos <- expand.grid(0:2, 0:2, 0:2, 0:2) %>% as_tibble()

combos$live_count <- 0
combos$die_count <- 0

for(i in 1:nrow(combos)){
  df <- split_df %>% 
    filter(acid_1 == combos$Var1[i]) %>% 
    filter(acid_2 == combos$Var2[i]) %>% 
    filter(acid_3 == combos$Var3[i]) %>% 
    filter(acid_4 == combos$Var4[i])
  combos$live_count[i] <- df %>% filter(binary_stop == 'live') %>% nrow()
  combos$die_count[i] <- df %>% filter(binary_stop == 'die') %>% nrow()
}

combos$live_percent <- combos$live_count/(combos$live_count + combos$die_count) * 100

set.seed(123)
hold <- tibble(row = 1:12000, barcode = '')
for(i in 1:12000){
hold$barcode[i] <- sample(c('A', 'T', 'G', 'C'), 20, replace = T) %>% paste(collapse = '')
}










