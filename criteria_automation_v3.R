#libraries
library(tidyverse)

#DATA
crit <- read_csv("Criteria_ex1.csv")

#BUILD DF FOR ADDITIONAL TABLE ROWS
hab_name <- "sim" #figure out where to pull this info from to automate

r1 <- c("HABITAT NAME", `hab_name`, " ", " ", "CRITERIA TYPE")
space <- c("", "", "", "", "")
r3 <- c("HABITAT RESILIENCE ATTRIBUTES","", "", "", "")
r4 <- c("HABITAT STRESSOR OVERLAP PROPERTIES","", "", "", "")
top <- rbind(r1,space,r3)

#SET UP FOR LOOP 

##Create a function for error checking - this was adapted from a stackoverflow response by Ronak Shah
checkFunction <- function() {
  user_input <- readline("Are you sure you want to create the csv with missing criteria? (y/n)  ")
  if(user_input != 'y') stop('Exiting since you did not select y')
  print("Continuing since you selected y.")
}

## Create df without NAs in scenario column to use in loop
df <- crit %>% 
  filter(!is.na(scenario))

## This dictates how many loops run based on the number of scenarios in the input table
runs <- unique(df$scenario)

## Create a df of just the critera names to later bind to the criteria tables
names <- crit[1,]

## Create boolean lists for columns needed for each table
res <- grepl("score|resilience", colnames(crit)) 
stres <- grepl("score|stressor1", colnames(crit)) #need to include ability have multiple stressors

#START LOOP FOR TRANSPOSING DATA

for(i in 1:length(runs)){
  #Transpose resilience criteria
  res_crit <- df[df$scenario == i,] #uses scenario column to subset dataframe
  #Error check for criteria with NA values 
  if(any(is.na(res_crit)) == TRUE){
    findNAs <- as.data.frame(cbind(lapply(lapply(res_crit, is.na), sum))) #Find the number of nas for each column
    colna <- rownames(subset(findNAs, findNAs$V1 != 0)) #Get names for rows that have NAs 
    print(paste("ERROR - The following column in scenario", i, "contains NAs:", colna))#Need to implement this in a better way
    #Check to see if the user would like to continue building the tables without the missing criteria
    checkFunction()
    print("Creating tables without missing criteria")
  } 
  res_crit <- dplyr::bind_rows(names, res_crit) #Adds column descriptions back in after subset
  res_crit <- res_crit[res]
  res_crit <- res_crit[ , colSums(is.na(res_crit)) == 0] #Update table to exclude missing criteria
  res_crit <- t(res_crit)
  
  #transpose stressor criteria
  
  stress_crit <- df[df$scenario == i,]
  stress_crit <- dplyr::bind_rows(names, stress_crit)
  stress_crit <- stress_crit[stres]
  stress_crit <- stress_crit[ , colSums(is.na(stress_crit)) == 0]
  stress_crit <- t(stress_crit)
  
  #bind to top
  crit_tables <- rbind(top,res_crit,space,r4,stress_crit)
  write.table(crit_tables, paste0("criteria_scores_",i,".csv"), append = FALSE, sep = ",",
              row.names = FALSE, col.names = FALSE)
  
  #figure out how to incorporate spatial data names and add stressor names if necessary
  
}


