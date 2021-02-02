library('readtext')
library('stringr')

source('R/0-combiner_functions.R')

###Read in all the generic XML that 
#doesn't change over participants
preamble <-
  toString(readtext(file = "instructions/atc_01_preamble.txt"))

instructions_gen <-
  toString(readtext(file = 'instructions/atc_04_0_genericinstructions.txt'))

mapaircraft_preamble <- toString(readtext(
  file = 'instructions/atc_04_aircraft_parameters.txt'))

display_parameters_training <- toString(readtext(file = paste(
  'instructions/atc_06_display_parameters','_', 'MANUAL', '.txt', sep = "")))

scoring <- toString(readtext(file = 
                               'instructions/atc_07_scoring_system_neutral.txt'))

clock_position_training <- toString(readtext(file = paste(
  'instructions/atc_08_clock_position','_', 'MANUAL', '.txt', sep = "")))

display_general_instructions <- toString(readtext(file = 
                                                    'instructions/atc_10_display_general_instructions.txt'))

begin_block_message <- toString(
  readtext(file = paste('instructions/atc_11_begin_block_message.txt', sep = "")))

post <- toString(readtext(file = paste(
  'instructions/atc_12_post-task_screen','.txt', sep = "")))

instructions_train <-
  toString(readtext(file = 'instructions/atc_training_instructions.txt'))

conflict_feedback <-
  toString(readtext(file = "instructions/atc_training_conflict_feedback.txt"))

training_maps <- toString(readtext(file = 'components/atc_09_maps_and_ac_p_ALL_s0_TRAINING.txt'))

training_trials <- read_trial_csv('components/xml_trials_TRAINING.csv', training=TRUE)

##Set up loops to generate the rest of the XML separately for each participant

n_participants <- 32
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
  
  instructions_key <- toString(readtext(file = paste(
    'instructions/atc_03_instructions_responsekeys_', cb_key,'.txt', sep = "")))
  
  response_key_set <- toString(readtext(file = 
                                          paste('instructions/atc_05_responseKeySet_',
                                                  cb_key, '.txt', sep = "")))
  for (sess in 1:2) {
    if (sess == 1) {
      blk1 <- cond[[counterbalance]][1]
      blk2 <- cond[[counterbalance]][2]
    } else {
      blk1 <- cond[[counterbalance]][3]
      blk2 <- cond[[counterbalance]][4]
    }
    
    xml_script_blk1 <- create_xml_script(ppt= ppt, cond = blk1, sess = sess, 
                                         preamble= preamble, 
                                         instructions_key = instructions_key,
                                         instructions_gen = instructions_gen,
                                         mapaircraft_preamble = mapaircraft_preamble, 
                                         scoring = scoring,
                                         response_key_set = response_key_set, 
                                         display_general_instructions = display_general_instructions, 
                                         begin_block_message = begin_block_message, 
                                         post=post)
    
    
    writeLines(xml_script_blk1, paste('XML/ppt', ppt,'_sess', sess,
                                      '_block1_cond_', blk1, '.xml', sep = ''), sep = '\n\n')
    
    xml_script_blk2 <- create_xml_script(ppt=ppt, cond = blk2, sess = sess, 
                                         preamble = preamble,
                                         instructions_key = instructions_key,
                                         instructions_gen = instructions_gen, 
                                         mapaircraft_preamble = mapaircraft_preamble, 
                                         scoring = scoring,
                                         response_key_set = response_key_set, 
                                         display_general_instructions = display_general_instructions, 
                                         begin_block_message = begin_block_message, 
                                         post = post)
    
    writeLines(xml_script_blk2, paste('XML/ppt', ppt,'_sess', sess,
                                      '_block2_cond_', blk2, '.xml', sep = ''), sep = '\n\n')
  }
  
  instructions_train_ppt <- paste(instructions_train, '\n\n', instructions_key, '\n\n',
                        instructions_gen, '\n\n',
                        conflict_feedback )
  
  training_XML<- paste(preamble, instructions_train_ppt,
                       mapaircraft_preamble,display_parameters_training, scoring,
                       response_key_set,
                       clock_position_training, training_maps, display_general_instructions,
                       begin_block_message,
                       training_trials, post)
  
  writeLines(training_XML, paste("XML/ppt", ppt, "_training", ".xml", sep=""), sep = '\n\n')
  
  cat(ppt)
}




