##aro_aci motifs. Does positioning or distance from n terminus matter
####Need to make a regex that can return position of multiple matches
reggie <- '[RHK]'

#going to make a table and chart for each length
#open length list
length_tables <- list()
legnth_plots <- list()

for(j in 2:20){
lib <- random_library %>% filter(nchar(sequence) == j)

#need to add to live count vs die count
pos_table <- tibble(pos = 1:20, 
                    live_count = 0,
                    die_count = 0)

#subset lib so only sequences with this regex will be returned
ro <- grep(reggie, lib$sequence, perl = T)
lib <- lib[ro,]

for (i in 1:nrow(lib)) {
  seq <- lib$sequence[i]
  seq <- strsplit(seq,'')[[1]] %>% 
    rev() %>% 
    paste(collapse = '')
  
  pos <- gregexpr(reggie, seq, perl = T)[[1]] %>% 
    as.vector()

  if(lib$binary_stop[i] == 'live'){
    pos_table$live_count[pos] <- pos_table$live_count[pos] + 1
  } else {
    pos_table$die_count[pos] <- pos_table$die_count[pos] + 1
  }
}

#generate live Percent
pos_table <- pos_table %>% mutate(
  live_percent = live_count/(live_count + die_count) * 100
)

p <- ggplot(pos_table, aes(pos, live_percent))+
  geom_bar(stat = 'identity')

length_tables[[j]] <- pos_table
}


###make a plot for each length


length_tables[[i]] %>% 
  ggplot(aes(pos, live_percent))+
  geom_bar(stat = 'identity')+
  coord_cartesian(ylim = c(0,25), xlim = c(1,20))

df <- lib %>% filter(nchar(sequence) == i)
table(df$binary_stop)

i <- i + 1
i <- 7
i <- 5

#for word report delete later
length_tables[[3]]
ro <- grep(reggie, random_library$sequence, perl = T)
lib <- random_library[ro,]

df <- lib %>% filter(nchar(sequence) == 3)


table(df$binary_stop)
ggplot(df, aes(binary_stop))+
  geom_bar()

length_tables


##graph increasing frequency
#need number of matches and whether we lived or died
freq_table <- tibble(freq = 0:20, live_count = 0, die_count = 0)

reggie <- '(?=[WYF].{0,4}[DE]|[DE].{0.4}[WYF])'

for (i in 1:nrow(random_library)) {
  matches <- gregexpr(reggie, random_library$sequence[i], perl = T)[[1]] %>% as.vector()
  if(matches != -1){
    if(random_library$binary_stop[i] == 'live'){
      freq_table$live_count[length(matches)] <- freq_table$live_count[length(matches)] + 1
    }
    if(random_library$binary_stop[i] == 'die'){
      freq_table$die_count[length(matches)] <- freq_table$die_count[length(matches)] + 1
    }
  }
}

#add live percent
freq_table <- freq_table %>% mutate(
  live_percent = live_count/(live_count + die_count) * 100
)

ggplot(freq_table, aes(freq, live_percent))+
  geom_bar(stat = 'identity')+
  coord_cartesian(ylim = c(0,30))






#could i do a look ahead regex from forward and reverse to get all WD combos
##need a test sequence
seq <- 'GGWDDGGWDWD'

#i want to say there are 8 aromatic







































