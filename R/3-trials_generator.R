###############################################################################
############################################ Trials Generator for ATC-LAB #####
###############################################################################
source("R/0-trials_generator_functions.R")
###############################################################################
############################################ Enter experimental variables #####
###############################################################################
conds <- c("MANUAL", "AUTO")

nTrials=600

#to paste a block break in at the appropriate place
block_break <- "\n <atc:phase atc:idx='displayblockBreak'> 
\t<atc:instruction atc:idxref='blockBreak'/>
</atc:phase>\n\n"

break_trials <- 300

for (cond in conds) {
  trials <- create_trials(cond, 8, 1, nTrials)
  trials[break_trials, 3] <-
    paste(trials[break_trials, 3], block_break)
  write.csv(trials,
            paste('components/xml_trials_',
                  cond, '.csv', sep = ''),
            row.names = FALSE)
}

#Read in training exp_vars
#create training feedback using training exp_vars
training_trials <- create_trials('TRAINING', 8, 1, 40)
training_data <- read.csv("data/exp_vars_p_ALL_s0_TRAINING.csv")
conflict_status <-
  as.character(training_data$conflict_status[order(training_data$presOrder)])

feedback <- c()

for (i in 1:length(conflict_status)){
  feedback[i] <- paste("\n<!-- Display Feedback --> \n<atc:phase atc:idx='training_trial_", 
                       i, "_feedback'> \n", "<atc:instruction atc:idxref='training", 
                       simpleCap(conflict_status[i]),
                       "Feedback'/> \n", "</atc:phase>\n\n", sep="")
}

training_trials$feedback <- feedback

write.csv(training_trials,
         'components/xml_trials_TRAINING.csv', row.names = FALSE)

