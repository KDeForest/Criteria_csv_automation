#libraries
library(tidyverse);library(dplyr);library(tibble)

#read in data
a <- read_csv("criteria_scores_ex.csv", col_names =FALSE) %>% rownames_to_column()

f <- as.character

#list of criteria scores
Rating <- f(c(3,2,1))
DQ <- f(c(2,1,1))
Weight <- f(c(1,2,3))

#update scores in loop
for(i in 1:length(Rating)){
  
  b <- rows_update(a, tibble(rowname = f(5:7), X2 = Rating[i])) 
  c <- rows_update(b, tibble(rowname = f(5:8), X3 = DQ[i])) 
  d <- rows_update(c, tibble(rowname = f(5:8), X4 = Weight[i])) %>%
    select(-rowname)
 #write to csv 
  write.table( d, file = paste0("criteria_scores_",i,".csv"), sep=",",  col.names=FALSE, row.names = FALSE)
}



