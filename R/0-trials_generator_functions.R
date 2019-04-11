
#inputs: condition, timepressure, load, ntrials
#outputs: a data frame full of the necessary
#XML syntax for each trial
create_trials <- function (condition, timePressure, trafficLoad, nTrials){
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
      '<atc:block_trial atc:waitTimeOut=', '\'', 'false', '\'', '>', '\n\n', sep = '')
  }
  xml_trials <- as.data.frame.character(xml_trials)
  
  xml_load <- c()
  for (j in 1:nPairs){
    xml_load[j] <- paste(
      '<atc:trial atc:idx=', '\'',  'cond_', condition[j], '_trial_', trialNumberWithin[j], '-', 
      trafficLoadNumber[j], '\'',' atc:sky=', '\'', 'sky', pairNumber[j], '\'', 
      ' atc:param=', '\'', 'default', '\'', ' atc:map=', '\'', 'map', pairNumber[j],
      '\'', ' atc:ui=', '\'', 'ui01',  '\'', '>', '\n','\t', '<atc:keyEvent atc:type=', 
      '\'', 'terminateTrial', '\'', '>', 'q', '</atc:keyEvent>',
      '</atc:trial>', '\n\n', sep = '')
  }
  xml_load_split <- split(xml_load, ceiling(seq_along(xml_load)/trafficLoad))
  xml_load_split <- as.data.frame(xml_load_split)
  xml_load_split.T <- as.data.frame(t(xml_load_split))
  
  xml_TP <- paste(
    '<atc:timeEvent atc:timeUnit=', '\'', 'milliSeconds', '\'', '>', TP, '</atc:timeEvent>', '\n\n',
    '</atc:block_trial>', '\n',
    '</atc:phase>', '\n\n', sep = '')
  xml_TP <- rep(xml_TP, nTrials)
  
  trials <- cbind(xml_trials, xml_load_split.T, xml_TP, stringsAsFactors=FALSE)
  pair_cols <- grep("V", colnames(trials))
  colnames(trials)[pair_cols] <- paste("pair", pair_cols-1, sep="")
  trials
}

#capitalizes the first letter of a string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
