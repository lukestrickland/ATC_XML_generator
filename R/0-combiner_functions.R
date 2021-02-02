#read csv with xml containing trial syntax,
#paste together.

read_trial_csv <- function(csvname, training=FALSE){
  trial_df <- read.csv(csvname)
  trial_df$pair1 <- paste("\t", trial_df$pair1, sep="")
  trial_df$xml_TP<- paste("\t", trial_df$xml_TP, sep="")
  trial_vec <- paste(trial_df$xml_trials, trial_df$pair1, trial_df$xml_TP)
  if(training) trial_vec <- paste(trial_vec, trial_df$feedback)
  paste(trial_vec, collapse="")
}

#inputs:all the ingredients necessary for creating xml script
#outputs: string of complete XML script

create_xml_script <- function(ppt, cond, sess, preamble, instructions_key,
                              instructions_gen, mapaircraft_preamble, scoring, 
                              response_key_set,
                              display_general_instructions, begin_block_message, post){
  
  clock_position <- toString(readtext(file = paste(
    'instructions/atc_08_clock_position','_', cond, '.txt', sep = "")))
  
  display_parameters <- toString(readtext(file = paste(
    'instructions/atc_06_display_parameters','_', cond, '.txt', sep = "")))
                      
  instructions_cond <- toString(readtext(file = paste(
    'instructions/atc_02_instructions_condition_', cond ,'.txt', sep = "")))
  
  instructions <- paste(instructions_cond, '\n\n', instructions_key, 
                        '\n\n', instructions_gen)
  
  maps <- toString(readtext(file = paste(
    'components/atc_09_maps_and_ac_p', ppt, '_s', sess, '_', cond, '.txt', sep = "")))
  
  trials <- read_trial_csv(paste('components/xml_trials_', cond, '.csv', sep=""))
  
  paste(preamble, instructions,
        mapaircraft_preamble,display_parameters, scoring,
        response_key_set,
        clock_position, maps, display_general_instructions,
        begin_block_message,
        trials, post)
  
}