library('readtext')
library('stringr')

source('R/0-combiner_functions.R')

n_participants <- 100
participant_number <- c(1:n_participants)

key_set <- c(1,1, 2, 2)

cond <- list(c("AUTO", "MANUAL", "MANUAL", "AUTO"),
             c( "MANUAL", "AUTO", "AUTO", "MANUAL"),
             c("AUTO", "MANUAL", "MANUAL", "AUTO"),
             c( "MANUAL", "AUTO", "AUTO", "MANUAL")
             )

for (ppt in participant_number){
  
  counterbalance <- (participant_number[ppt] %% 4) + 1
  cb_key <- key_set[counterbalance]
  
  
  preamble <- toString(readtext(file = "instructions/atc_01_preamble.txt"))
  
  instructions_key <- toString(readtext(file = paste(
    'instructions/atc_03_instructions_responsekeys_', cb_key,'.txt', sep = "")))
  
  response_key_set <- toString(readtext(file = 
                                          paste('instructions/atc_05_responseKeySet_',
                                                  cb_key, '.txt', sep = "")))
  
  mapaircraft_preamble <- toString(readtext(file = paste(
    'instructions/atc_04_aircraft_parameters', '.txt', sep = "")))
  
  display_parameters <- toString(readtext(file = 'instructions/atc_06_display_parameters.txt'))
  
  scoring <- toString(readtext(file = 
                                 'instructions/atc_07_scoring_system_neutral.txt'))
  
  clock_position <- toString(readtext(file = 
                                        'instructions/atc_08_clock_position.txt'))
  
  display_general_instructions <- toString(readtext(file = 
                                                      'instructions/atc_10_display_general_instructions.txt'))
  
  begin_block_message <- toString(
    readtext(file = paste('instructions/atc_11_begin_block_message.txt', sep = "")))
  
  post <- toString(readtext(file = paste(
    'instructions/atc_12_post-task_screen','.txt', sep = "")))
  
  for (sess in 1:2) {
    if (sess == 1) {
      blk1 <- cond[[counterbalance]][1]
      blk2 <- cond[[counterbalance]][2]
    } else {
      blk1 <- cond[[counterbalance]][3]
      blk2 <- cond[[counterbalance]][4]
    }
    
    xml_script_blk1 <- create_xml_script(blk1, sess, preamble,
                                         mapaircraft_preamble, display_parameters,
                                         response_key_set, clock_position, 
                                         display_general_instructions, 
                                         begin_block_message, post)
    
    
    writeLines(xml_script_blk1, paste('XML/ppt', ppt,'_sess', sess,
                                      '_cond_', blk1, '.xml', sep = ''), sep = '\n\n')
    
    xml_script_blk2 <- create_xml_script(blk2, sess, preamble,
                                         mapaircraft_preamble, display_parameters,
                                         response_key_set, clock_position, 
                                         display_general_instructions, 
                                         begin_block_message, post)
    
    writeLines(xml_script_blk1, paste('XML/ppt', ppt,'_sess', sess,
                                      '_cond_', blk2, '.xml', sep = ''), sep = '\n\n')
  }
}
