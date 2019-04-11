library(tidyverse)
source("R/0-ac_map_generation_functions.R")
callsigns <- read.csv('components/callsigns.csv', header = TRUE, sep = ",")$CS

###############################################################################
############################################ Enter experimental variables #####
###############################################################################
n_participants <- 32
conds <- factor(c("AUTO", "MANUAL"))
sessions <- 2
participant_number <- 1:n_participants
#nPairs per block
nPairs=600
#rate of automation failure
failure_rate = 0.1


for (p in participant_number){
  for (session in 1:sessions){
    
    #create initial data frames 
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
   
     #pick the manual trials with presOrder matching the auto failures
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
    #writes exp var df to csv, sim input to csv, maps and ac to txt files
    write_exp_data('manual', manual_exp_var_df, manual_sim_input_df,
              manual_maps_and_ac, p, session)
    
    write_exp_data('auto', auto_exp_var_df, auto_sim_input_df,
              auto_maps_and_ac, p, session)
  }
  cat(paste("p", p, " ", sep=""))
}

#Create training

training_exp_var_df <- create_exp_var_df(nPairs=40, callsigns=callsigns)
training_sim_input_df <- create_sim_input_df(exp_var_df = training_exp_var_df,
                                         aspectRatio = 0.625, x_dim=180)
training_maps_and_ac <- create_xml_ac_and_maps(
  "training", training_exp_var_df, training_sim_input_df)

write_exp_data('training', training_exp_var_df, training_sim_input_df,
               training_maps_and_ac, p = "_ALL", session=0)

