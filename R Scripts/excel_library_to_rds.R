##Load final aa library back into R
library(openxlsx)
library(tidyverse)

#make a list of sets from final aa library, right now there are 29 sets
library_sets_list <- list()
set_number <- 29

for (i in 1:set_number) {
  library_sets_list[[i]] <- openxlsx::read.xlsx('AA Library/Final_AA_Library_2019_v3.xlsx', sheet = i)
}

names(library_sets_list) <- c("combinatorial", "W_first", "D_first", "set_g_spacing", "g_in", 
                              "g_out", "terminal_WD", "internal_WD", "oneR", "twoR", "threeR",
                              "Umayr_set", "random_WD20", "dipep", "ANAC013_1rep", "ANAC013_2rep",
                              "AR_1rep", "AR_2rep", "CREBZF_1rep", "CREBZF_2rep", "EKLF_1rep", 
                              "EKLF_2rep", "P53_1rep", "P53_2rep", "Rela2_1rep", "Rela2_2rep",
                              "Equal_K_and_D", "Equal_R_and_D", "Known_tads")

#Remove first column from every tibble because it is not needed
#for (i in 1:length(library_sets_list)) {
#  df <- library_sets_list[[i]]
#  df <- df[,-1]
#  library_sets_list[[i]] <- df
#}

#save set list as an RDS object
#saveRDS(library_sets_list, "Final_Robject_AA_library_v3.rds")





