###############################################################################
############################################ Trials Generator for ATC-LAB #####
###############################################################################
rm(list = ls())
setwd("~/atc.sim")

# library(XML)


###############################################################################
############################################ Enter experimental variables #####
###############################################################################
condition <- 'd'
block <- 'control'
timePressure <- 4  # Time Pressure (seconds)
trafficLoad <- 1
nTrials <- 84  # N Control Trials (84)
# nTrials <- 240  # N PM Trials (240)

TP <- timePressure*1000
nPairs <- trafficLoad*nTrials
trialNumber <- rep(1:nTrials, nTrials)
trialNumberWithin <- rep(1:nTrials, each = trafficLoad)
pairNumber <- 1:nPairs
condition <- rep(condition, nPairs)
trafficLoadNumber <- rep(1:trafficLoad, nTrials)

xml_trials <- c()

for (i in 1:nTrials){
    xml_trials[i] <- paste(
    '<!-- Condition ', condition[i], ' Trial ', trialNumber[i], ' -->', '\n',
    '<atc:phase atc:idx=', '\'', 'cond_', condition[i], '_trial_', trialNumber[i], '\'', '>', '\n',
    '<atc:instruction atc:idxref=', '\'', 'interTrialInfo', '\'', '/>', '\n',
    '<atc:block_trial atc:waitTimeOut=', '\'', 'true', '\'', '>', '\n\n', sep = '')
}

xml_trials <- as.data.frame.character(xml_trials)
xml_trials

xml_load <- c()

for (j in 1:nPairs){
xml_load[j] <- paste(
    '<atc:trial atc:idx=', '\'',  'cond_', condition[j], '_trial_', trialNumberWithin[j], '-', trafficLoadNumber[j], '\'',
    ' atc:sky=', '\'', 'sky', pairNumber[j], '\'', ' atc:param=', '\'', 'default', '\'', ' atc:map=', '\'', 'map', pairNumber[j], '\'', ' atc:ui=', '\'', 'ui01',  '\'', '>', '\n',
    '\t', '<atc:keyEvent atc:type=', '\'', 'terminateTrial', '\'', '>', 'q', '</atc:keyEvent>',
    '</atc:trial>', '\n\n', sep = '')
}

xml_load

xml_load_split <- split(xml_load, ceiling(seq_along(xml_load)/trafficLoad))
xml_load_split <- as.data.frame(xml_load_split)
xml_load_split.T <- as.data.frame(t(xml_load_split))
xml_load_split.T

xml_TP <- c()

xml_TP <- paste(
    '<atc:timeEvent atc:timeUnit=', '\'', 'milliSeconds', '\'', '>', TP, '</atc:timeEvent>', '\n\n',
    '</atc:block_trial>', '\n',
    '</atc:phase>', '\n\n', sep = '')

xml_TP <- rep(xml_TP, nTrials)
xml_TP

trials <- cbind(xml_trials, xml_load_split.T, xml_TP)

# Name columns depending on traffic load
if (trafficLoad == 2){
    colnames(trials) <- c("trial", "pair1", "pair2", "TP")
} else if (trafficLoad == 4){
    colnames(trials) <- c("trial", "pair1", "pair2", "pair3", "pair4", "TP")
} else if (trafficLoad == 5){
  colnames(trials) <- c("trial", "pair1", "pair2", "pair3", "pair4", "pair5", "TP")
}

write.csv(trials, paste('xml_trials_', condition[1], '_', block, '.csv', sep = ''), row.names = FALSE)


