###############################################################################
#################################### Map & Aircraft Generator for ATC-LAB #####
###############################################################################
library(tidyverse)
source("R/0-ac_map_generation_functions.R")

callsigns <- read.csv('callsigns.csv', header = TRUE, sep = ",")$CS

###############################################################################
############################################ Enter experimental variables #####
###############################################################################
n_participants <- 32
conds <- factor(c("AUTO", "MANUAL"))
sessions <- 2
participant_number <- 1:n_participants
nPairs=10
failure_rate = 0.1


for (p in participant_number){
  for (session in 1:sessions){

    auto_exp_var_df <- create_exp_var_df(nPairs=nPairs, callsigns=callsigns)
    manual_exp_var_df <- create_exp_var_df(nPairs=nPairs, callsigns=callsigns)
    
    #Randomly pick auto fail trials, 
    #match corresponding manual trials on all stimulus properties
   num_fails <- failure_rate* length(auto_exp_var_df$DOMS)
   failtrials <- get_auto_fails(auto_exp_var_df, num_fails)
   
   #Index which trials are auto failures
   auto_exp_var_df$failtrial <- FALSE
   auto_exp_var_df$failtrial[auto_exp_var_df$presOrder %in% 
                     rbind(failtrials$fail_conflicts,
                           failtrials$fail_nonconflicts)] <- TRUE
   
   manual_exp_var_df <- add_matched_auto_fails_reshuffle(
                              manual_exp_var_df,
                              auto_exp_var_df,
                              failtrials)

    ##for all auto trials ship matching rows to manual except for stimulus

    auto_sim_input_df <- create_sim_input_df(exp_var_df = auto_exp_var_df,
                                        aspectRatio = 0.625, x_dim=180)
    
    manual_sim_input_df <- create_sim_input_df(exp_var_df =  manual_exp_var_df,
                                             aspectRatio = 0.625, x_dim=180)
    
    auto_maps_and_ac <- create_xml_ac_and_maps(
                        "auto", auto_exp_var_df, auto_sim_input_df)
    
    manual_maps_and_ac <- create_xml_ac_and_maps(
                        "manual", manual_exp_var_df, manual_sim_input_df)
    
    writeLines(auto_maps_and_ac, paste('components/atc_09_maps_and_ac_p', 
                                  participant_number[p], '_', 's', 
                                  session, '_', "AUTO", 
                                  '.txt', sep = ''), sep = '\n\n')
    
    writeLines(manual_maps_and_ac, paste('components/atc_09_maps_and_ac_p', 
                                       participant_number[p], '_', 's', 
                                       session, '_', "MANUAL", 
                                       '.txt', sep = ''), sep = '\n\n')
  }
  cat(paste("p", p, " ", sep=""))
}


