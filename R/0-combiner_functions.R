read_trial_csv <- function(csvname, training=FALSE){
  trial_df <- read.csv(csvname)
  trial_df$pair1 <- paste("\t", trial_df$pair1, sep="")
  trial_df$xml_TP<- paste("\t", trial_df$xml_TP, sep="")
  trial_vec <- paste(trial_df$xml_trials, trial_df$pair1, trial_df$xml_TP)
  if(training) trial_vec <- paste(trial_vec, trial_df$feedback)
  paste(trial_vec, collapse="")
}


create_xml_script <- function(ppt, cond, sess, preamble, mapaircraft_preamble, 
                              display_parameters, response_key_set, clock_position,
                              display_general_instructions, begin_block_message, post){
  
  instructions_cond <- toString(readtext(file = paste(
    'instructions/atc_02_instructions_condition_', cond ,'.txt', sep = "")))
  
  instructions <- paste(instructions_cond, '\n\n', instructions_key)
  
  maps <- toString(readtext(file = paste(
    'components/atc_09_maps_and_ac_p', ppt, '_s', sess, '_', cond, '.txt', sep = "")))
  
  trials <- read_trial_csv(paste('components/xml_trials_', cond, '.csv', sep=""))
  
  paste(preamble, instructions,
        mapaircraft_preamble,display_parameters,
        response_key_set,
        clock_position, maps, display_general_instructions,
        begin_block_message,
        trials, post)
  
}