###Aromatic and Acidic Ratio for living sequences

#add aromatic number and acidic number to random library sequences----
lib <- random_library

lib$aromatic_number <-lapply(lib$sequence, function(x){
                                seq <- strsplit(x, '')[[1]]
                                count <- grep('[WYF]', seq) %>% length()
                                return(count)
                                }) %>% unlist()

#Remove sequences with aromatic_number == 10 they were designed sequences and should not be part of the random seqeunce set
lib <- lib %>% filter(aromatic_number < 10)

lib$acidic_number <- lapply(lib$sequence, function(x){
                                seq <- strsplit(x, '')[[1]]
                                count <- grep('[DE]', seq) %>% length()
                                return(count)
                                }) %>% unlist()

lib$ratio <- lib$aromatic_number/lib$acidic_number


ratios <- tibble(ratio = unique(lib$ratio))
ratios$live_percent <- 0
ratios$count <- 0
for (i in 1:nrow(ratios)) {
  df <- lib %>% filter(ratio == ratios$ratio[i])
  live <- nrow(df %>% filter(binary_stop == 'live'))
  total <- nrow(df)
  ratios$live_percent[i] <- live/total * 100
  ratios$count[i] <- total
}

#remove NA and INF
ratios <- ratios %>% filter(!is.na(live_percent))
ratios <- ratios[-4,]

ggplot(ratios, aes(ratio, live_percent))+
  geom_bar(stat = 'identity')+
  labs(title = 'Random Library: aromatic acidic ratio vs live_percent')+
  xlab('Aromatic_number/Acidic_number')

######################################################################################

#same analysis for design library----
lib <- design_library

lib$aromatic_number <-lapply(lib$sequence, function(x){
  seq <- strsplit(x, '')[[1]]
  count <- grep('[WYF]', seq) %>% length()
  return(count)
}) %>% unlist()

lib$acidic_number <- lapply(lib$sequence, function(x){
  seq <- strsplit(x, '')[[1]]
  count <- grep('[DE]', seq) %>% length()
  return(count)
}) %>% unlist()

lib$ratio <- lib$aromatic_number/lib$acidic_number

ratios <- tibble(ratio = unique(lib$ratio))
ratios$live_percent <- 0
ratios$count <- 0
for (i in 1:nrow(ratios)) {
  df <- lib %>% filter(ratio == ratios$ratio[i])
  live <- nrow(df %>% filter(binary_stop == 'live'))
  total <- nrow(df)
  ratios$live_percent[i] <- live/total * 100
  ratios$count[i] <- total
}

#remove NA and INF
ratios <- ratios %>% filter(!is.na(live_percent))
ratios <- ratios[-6,]

ggplot(ratios, aes(ratio, live_percent))+
  geom_bar(stat = 'identity')+
  labs(title = 'Design Library:aromatic acidic ratio vs live_percent')+
  xlab('Aromatic_number/Acidic_number')


#same analysis for WARK grid----
lib <- design_library %>% filter(set == 'grid')

lib$aromatic_number <-lapply(lib$sequence, function(x){
  seq <- strsplit(x, '')[[1]]
  count <- grep('[WYF]', seq) %>% length()
  return(count)
}) %>% unlist()

lib$acidic_number <- lapply(lib$sequence, function(x){
  seq <- strsplit(x, '')[[1]]
  count <- grep('[DE]', seq) %>% length()
  return(count)
}) %>% unlist()

lib$ratio <- lib$aromatic_number/lib$acidic_number

ratios <- tibble(ratio = unique(lib$ratio))
ratios$live_percent <- 0
ratios$count <- 0
for (i in 1:nrow(ratios)) {
  df <- lib %>% filter(ratio == ratios$ratio[i])
  live <- nrow(df %>% filter(binary_stop == 'live'))
  total <- nrow(df)
  ratios$live_percent[i] <- live/total * 100
  ratios$count[i] <- total
}

#remove NA and INF
ratios <- ratios %>% filter(!is.na(live_percent))
ratios <- ratios[-6,]

ggplot(ratios, aes(ratio, live_percent))+
  geom_bar(stat = 'identity')+
  labs(title = 'WARK Grid: aromatic acidic ratio vs live_percent')+
  xlab('Aromatic_number/Acidic_number')
