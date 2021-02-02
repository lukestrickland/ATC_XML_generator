#generates csv with all trial procedures
source("R/0-trials_generator_functions.R")

conds <- c("MANUAL", "AUTO")
nTrials=600

#to paste a block break in at the appropriate place
block_break <- "\n <atc:phase atc:idx='displayblockBreak'> 
\t<atc:instruction atc:idxref='blockBreak'/>
</atc:phase>\n\n"

#which trial should the break be after
break_trials <- 300

for (cond in conds) {
  trials <- create_trials(condition=cond, timePressure= 8, 
                          trafficLoad =1, nTrials= nTrials)
  trials[break_trials, 3] <-
    paste(trials[break_trials, 3], block_break)
  write.csv(trials,
            paste('components/xml_trials_',
                  cond, '.csv', sep = ''),
            row.names = FALSE)
}

#Read in training exp_vars (which have conflict status of each trial)
#create training feedback using training exp_vars
training_trials <- create_trials(condition= 'TRAINING', timePressure=8,
                                 trafficLoad =1, nTrials= 40)
training_data <- read.csv("data/exp_vars_p_ALL_s0_TRAINING.csv")
conflict_status <-
  as.character(training_data$conflict_status[order(training_data$presOrder)])

#NOTE NO LONGER NEED OLD FEEDBACK ANYMORE BECAUSE TASK IS NOW PROGRAMMED
# TO GIVE FEEDBACK
# feedback <- c()
# 
# for (i in 1:length(conflict_status)){
#   feedback[i] <- paste("\n<!-- Display Feedback --> \n<atc:phase atc:idx='training_trial_", 
#                        i, "_feedback'> \n", "<atc:instruction atc:idxref='training", 
#                        simpleCap(conflict_status[i]),
#                        "Feedback'/> \n", "</atc:phase>\n\n", sep="")
# }
# 
# training_trials$feedback <- feedback

#Add feedback at end 

training_trials <- rbind(
  training_trials,
  c(
    "<atc:phase atc:idx='manfB'>\n",
    "<atc:scoreFeedback atc:idxref='feedback_manual'/>\n",
    "</atc:phase>"
  )
)

write.csv(training_trials,
         'components/xml_trials_TRAINING.csv', row.names = FALSE)

