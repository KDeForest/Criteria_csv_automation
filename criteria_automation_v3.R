#libraries
library(tidyverse);library(dplyr);library(tibble)

#DATA
#a <- read_csv("criteria_scores_ex.csv", col_names =FALSE) %>% rownames_to_column()
crit <- read_csv("Criteria_ex1.csv")

#BUILD ROWS FOR ADDITIONAL TABLE
hab_name <- "dolphin"

r1 <- c("HABITAT NAME", `hab_name`, " ", " ", "CRITERIA TYPE")
space <- c("", "", "", "", "")
r3 <- c("HABITAT RESILIENCE ATTRIBUTES","", "", "", "")
r4 <- c("HABITAT STRESSOR OVERLAP PROPERTIES","", "", "", "")
top <- rbind(r1,space,r3)

#SET UP FOR LOOP  

## Create df without NAs in scenario column so we can grab the number of scenarios
scenarios <- crit %>% 
  filter(!is.na(scenario))

## This dictates how many loops run based on the number of scenarios in the input table
runs <- c(1:max(scenarios$scenario))

## Create a df of just the critera names to later bind to the criteria tables
names <- crit[1,]

## Create boolean lists for columns needed for each table
res <- grepl("score|resilience", colnames(crit)) 
stres <- grepl("score|stressor1", colnames(crit)) 

#START LOOP FOR TRANSPOSING DATA

for(i in 1:length(runs)){
  #transpose matrix for resilience criteria
  res_crit <- crit[crit$scenario == i,][2:5,] #uses scenario column to subset dataframe
  res_crit <- dplyr::bind_rows(names, res_crit) #adds column descriptions back in after subset
  res_crit <- res_crit[res]
  res_crit <- t(res_crit)
  
  #transpose matrix for stressor criteria
  stress_crit <- crit[crit$scenario == i,][2:5,] 
  stress_crit <- dplyr::bind_rows(names, stress_crit) 
  stress_crit <- stress_crit[stres]
  stress_crit <- t(stress_crit)
  
  #bind all and write to csv
  crit_tables <- rbind(top,res_crit,space,r4,stress_crit)
  write.table(crit_tables, paste0("criteria_scores_",i,".csv"), append = FALSE, sep = ",",
              row.names = FALSE, col.names = FALSE)
  #figure out how to incorporate spatial data names and add stressor names if necessary
  
}

