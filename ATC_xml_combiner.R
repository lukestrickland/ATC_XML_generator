rm(list = ls())
library('readtext')
library('stringr')


n_participants <- 2
participant_number <- c(1:n_participants)
# p <- 1
# c <- 1
# k <- 1
key_set <- c(1,2,3,4)
exp_cond <- c("A","B","C","D","E")

for(p in 1:length(participant_number)){
for(c in 1:length(exp_cond)){
for(k in 1:length(key_set)){
  
  instructions <- c()
  display_block_info <- c()
  display_redirect_info <- c()
  maps <- c()
  trials <- c()
  if (exp_cond[c] == 'A'){
    instructions <- toString(readtext(file = paste('atc_02_instructions_condition_A.txt', sep = "")))
    display_block_info <- toString(readtext(file = paste('atc_11_display_block_info_A.txt', sep = "")))
    display_redirect_info <- toString(readtext(file = paste('atc_12_display_redirect_info_A.txt', sep = "")))
    maps <- toString(readtext(file = paste('atc_09_maps_P', participant_number[p], '_CONTROL_A.txt', sep = "")))
    trials <- toString(readtext(file = paste('atc_17_trials_32_A.txt', sep = "")))
  } else if (exp_cond[c] == 'B'){
    instructions <- toString(readtext(file = paste('atc_02_instructions_condition_B.txt', sep = "")))
    display_block_info <- toString(readtext(file = paste('atc_11_display_block_info_B.txt', sep = "")))
    display_redirect_info <- toString(readtext(file = paste('atc_12_display_redirect_info_B.txt', sep = "")))
    maps <- toString(readtext(file = paste('atc_09_maps_P', participant_number[p], '_PM_B.txt', sep = "")))
    trials <- toString(readtext(file = paste('atc_17_trials_48_B.txt', sep = "")))
  } else if (exp_cond[c] == 'C'){
    instructions <- toString(readtext(file = paste('atc_02_instructions_condition_C.txt', sep = "")))
    display_block_info <- toString(readtext(file = paste('atc_11_display_block_info_C.txt', sep = "")))
    display_redirect_info <- toString(readtext(file = paste('atc_12_display_redirect_info_C.txt', sep = "")))
    maps <- toString(readtext(file = paste('atc_09_maps_P', participant_number[p], '_PM_C.txt', sep = "")))
    trials <- toString(readtext(file = paste('atc_17_trials_48_C.txt', sep = "")))
  } else if (exp_cond[c] == 'D'){
    instructions <- toString(readtext(file = paste('atc_02_instructions_condition_D.txt', sep = "")))
    display_block_info <- toString(readtext(file = paste('atc_11_display_block_info_D.txt', sep = "")))
    display_redirect_info <- toString(readtext(file = paste('atc_12_display_redirect_info_D.txt', sep = "")))
    maps <- toString(readtext(file = paste('atc_09_maps_P', participant_number[p], '_PM_D.txt', sep = "")))
    trials <- toString(readtext(file = paste('atc_17_trials_48_D.txt', sep = "")))
  } else if (exp_cond[c] == 'E'){
    instructions <- toString(readtext(file = paste('atc_02_instructions_condition_E.txt', sep = "")))
    display_block_info <- toString(readtext(file = paste('atc_11_display_block_info_E.txt', sep = "")))
    display_redirect_info <- toString(readtext(file = paste('atc_12_display_redirect_info_E.txt', sep = "")))
    maps <- toString(readtext(file = paste('atc_09_maps_P', participant_number[p], '_PM_E.txt', sep = "")))
    trials <- toString(readtext(file = paste('atc_17_trials_48_E.txt', sep = "")))
  }
  
  
  response_key_set <- c()
  PM_key_reminder <- c()
  if (key_set[k] == 1){
    response_key_set <- toString(readtext(file = paste('atc_05_responseKeySet1.txt', sep = "")))
    PM_key_reminder <- toString(readtext(file = paste('atc_14_pm_key_1.txt', sep = "")))
  } else if (key_set[k] == 2){
    response_key_set <- toString(readtext(file = paste('atc_05_responseKeySet2.txt', sep = "")))
    PM_key_reminder <- toString(readtext(file = paste('atc_14_pm_key_2.txt', sep = "")))
  } else if (key_set[k] == 3){
    response_key_set <- toString(readtext(file = paste('atc_05_responseKeySet3.txt', sep = "")))
    PM_key_reminder <- toString(readtext(file = paste('atc_14_pm_key_3.txt', sep = "")))
  } else if (key_set[k] == 4){
    response_key_set <- toString(readtext(file = paste('atc_05_responseKeySet4.txt', sep = "")))
    PM_key_reminder <- toString(readtext(file = paste('atc_14_pm_key_4.txt', sep = "")))
  }
  
  preamble <- toString(readtext(file = paste("atc_01_preamble.txt", sep = "")))
  filler_task_inputs <- toString(readtext(file = paste('atc_03_filler_task_inputs.txt', sep = "")))
  aircraft_parameters <- toString(readtext(file = paste('atc_04_aircraft_parameters.txt', sep = "")))
  display_parameters <- toString(readtext(file = paste('atc_06_display_parameters.txt', sep = "")))
  scoring_system <- toString(readtext(file = paste('atc_07_scoring_system_neutral.txt', sep = "")))
  clock_position <- toString(readtext(file = paste('atc_08_clock_position.txt', sep = "")))
  display_general_instructions <- toString(readtext(file = paste('atc_10_display_general_instructions.txt', sep = "")))
  display_response_key_info <- toString(readtext(file = paste('atc_13_display_response_key_info.txt', sep = "")))
  display_filler_task <- toString(readtext(file = paste('atc_15_display_filler_task.txt', sep = "")))
  begin_block_message <- toString(readtext(file = paste('atc_16_begin_block_message.txt', sep = "")))
  post_task_screen <- toString(readtext(file = paste('atc_18_post-task_screen.txt', sep = "")))
  
  
  # Compile scripts
  script <- c()
  if (exp_cond[c] == 'A'){
    script <- c(preamble,
                instructions,
                filler_task_inputs,
                aircraft_parameters,
                response_key_set,
                display_parameters,
                scoring_system,
                clock_position,
                maps,
                display_general_instructions,
                display_block_info,
                display_redirect_info,
                display_response_key_info,
                display_filler_task,
                begin_block_message,
                trials,
                post_task_screen)
  } else {
    script <- c(preamble,
                instructions,
                filler_task_inputs,
                aircraft_parameters,
                response_key_set,
                display_parameters,
                scoring_system,
                clock_position,
                maps,
                display_general_instructions,
                display_block_info,
                display_redirect_info,
                display_response_key_info,
                PM_key_reminder,
                display_filler_task,
                begin_block_message,
                trials,
                post_task_screen)
    
  }
  
  # Checks
  script
  
  # Write to .xml script for ATC-Lab
  writeLines(script, paste('ATC_A6_P', participant_number[p], '_', exp_cond[c], '_', 'keyset', '_', key_set[k], '.xml', sep = ''), sep = '\n\n')
  
}
}
}
