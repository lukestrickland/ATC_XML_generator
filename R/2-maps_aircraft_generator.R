###############################################################################
#################################### Map & Aircraft Generator for ATC-LAB #####
###############################################################################
rm(list = ls())

source("aircraft_map_generation_functions.R")

callsigns <- read.csv('callsigns.csv', header = TRUE, sep = ",")$CS

###############################################################################
############################################ Enter experimental variables #####
###############################################################################
n_participants <- 2
n_conds <- 1
participant_number <- 1:n_participants
nPairs=240


session=1

for (p in participant_number){
  for (c in 1:n_conds){

    exp_var_df <- create_exp_var_df(nPairs=240, callsigns=callsigns)
    sim_input_df <- create_sim_input_df(exp_var_df = exp_var_df,
                                        aspectRatio = 0.625, x_dim=180)
    maps_and_ac <- create_xml_ac_and_maps("auto", exp_var_df, sim_input_df)
    
    writeLines(maps_and_ac, paste('atc_09_maps_and_ac_p', 
                                  participant_number[p], '_', 's', 
                                  session, '_', 'auto', '_', 
                                  '1', '.xml', sep = ''), 
                                                      sep = '\n\n')
  }
}


