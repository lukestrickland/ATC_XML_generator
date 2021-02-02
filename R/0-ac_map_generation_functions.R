#helper

degtorad = function (deg) {
  (deg*pi/180)
}

##Creates all the basic experimental variables
# required for the sim
#Inputs: experimental options
#outputs: a df full of experimental variables

create_exp_var_df <- function(nPairs, callsigns){

#get as many callsigns as needed
  callsign_pool <- as.character(sample(callsigns, 2*nPairs, replace = FALSE))
 
#sample for DOMS - currently trials are quite easy
#so that participants don't rely on automation - too easy?
  doms_lower <- runif(nPairs*0.5, 0, 1.5)
  doms_upper <- runif(nPairs*0.5, 8.5, 10)
  DOMS <- round(c(doms_lower, doms_upper), 1)

#use DOMS whether to categorize as conflict/nonconflict
  conflict_status <- as.character(
    factor(DOMS>5, labels = c("conflict", "nonconflict")))

  tibble(     #randomise presOrder so stimuli will be presented randomly
                presOrder= sample(1:nPairs, replace = FALSE), 
              #number the pairs (why?)  
                pairNumber=1:nPairs, ac1_type=rep('B737', nPairs),
              #sample callsigns for the aircraft
                ac1_cs = callsign_pool[1:nPairs], ac2_type = rep('B737', nPairs),
                ac2_cs = callsign_pool[(nPairs+1):length(callsign_pool)],
              #flight levels
                ac1_fl = rep(370, nPairs), ac2_fl = rep(370, nPairs),
              #perpendicular flight paths
                angle = rep(90, nPairs),
              #sample speeds
                ac1_speed = sample(400:700, nPairs, replace = TRUE),
                ac2_speed = sample(400:700, nPairs, replace = TRUE),
                DOMS, 
              #sample TTMS
                TTMS = sample(120:210, nPairs, replace = TRUE), 
              #OOP - alternating 1s and 2s (will be randomized because aircraft
              #are presented by presOrder)
                OOP= rep(c(1, 2), 0.5*nPairs),
              #set stimulus as conflict_status in case we need it (Don't know what it does)
                conflict_status, stimulus=conflict_status)
}


#inputs: a df full of stimulus properties for the 
#automated conditions, the number of total automation
#failures
#outputs: a data frame that contains presOrders
#for randomly selected automation failures 
#for conflicts and non-conflict trials

get_auto_fails <- function (auto_exp_var_df, num_fails) {
  
  if ((length(auto_exp_var_df$presOrder) %% 2 != 0) |
      (num_fails %% 2 != 0)) {
    stop("Can't assign equal conflicts/nonconflicts")
  }
  
  fail_conflicts <- sample(auto_exp_var_df$presOrder[auto_exp_var_df$conflict_status ==
                                                       "conflict"],
                           num_fails/2, replace = F)
  
  fail_nonconflicts <- sample(auto_exp_var_df$presOrder[auto_exp_var_df$conflict_status ==
                                                          "nonconflict"],
                              num_fails/2, replace = F)
  
  tibble(fail_conflicts, fail_nonconflicts)
  
}

#todo make make_manual function
#inputs:callsign, auto_exp_var_df
#outputs: a manual df which is a shuffled version
#of auto df (except for the fail trials which are pegged on order)
#also has different callsigns

make_manual <- function(auto_exp_var_df, callsigns){
  manual_exp_var_df <- auto_exp_var_df
  
  #Shuffle presentation order (except for fail trials)
  all_possible_presOrders <- 1:length(manual_exp_var_df$presOrder) 
  
  nonfail_presOrders <- 
    all_possible_presOrders[!(
      all_possible_presOrders %in% manual_exp_var_df$presOrder[manual_exp_var_df$failtrial]
    )]
  
  manual_exp_var_df$presOrder[!manual_exp_var_df$failtrial] <- sample(nonfail_presOrders,
                                                                      length(nonfail_presOrders),
                                                                      replace=F)
  #Add new callsigns
  nPairs <- length(manual_exp_var_df$pairNumber)
  callsign_pool <-
    as.character(sample(callsigns, 2 * nPairs, replace = FALSE))
  manual_exp_var_df$ac1_cs <- callsign_pool[1:nPairs]
  manual_exp_var_df$ac2_cs <- callsign_pool[(nPairs+1):length(callsign_pool)]
  
  manual_exp_var_df
  
}


#OLD WAY: match a manual df with an auto one
#the difference between make_manual is this one
#takes a manual df with a whole other list of aircraft pairs
#inputs: the exp_var_df for the manual condition, the df 
#for the automated condition, and a data frame containing
#conflict and non-conflict failtrials


#outputs: a different manual data frame of experimental variables.
#a bunch of trials are matched to the automation failures from auto
#condition. The rest of the trials are the same as they were 
#but with a shuffled presentation order (to make sure they do not
#conflict with presetnation order of matches to auto failures)
# 
# add_matched_auto_fails_reshuffle <- function (manual_exp_var_df, 
#                                               auto_exp_var_df,
#                                               failtrials) {
#   
#   columns_to_replace <-  c("presOrder", "ac1_type" ,  "ac2_type",
#                            "ac1_fl", "ac2_fl", "angle", "ac1_speed",
#                            "ac2_speed", "DOMS", "TTMS", "OOP", "conflict_status",
#                            "stimulus", "failtrial")
#  
#   manual_exp_var_df$failtrial = FALSE
#   #Replace first (num conflict fails) of manual trials with
#   #the auto fail trials and same for nonconflicts
#   manual_exp_var_df[manual_exp_var_df$conflict_status=='conflict',
#                     ][1:length(failtrials$fail_conflicts),
#                       columns_to_replace] <-
#     auto_exp_var_df[auto_exp_var_df$presOrder %in% failtrials$fail_conflicts,
#                     columns_to_replace]
#   
#   manual_exp_var_df[manual_exp_var_df$conflict_status=='nonconflict',
#                     ][1:length(failtrials$fail_nonconflicts),
#                       columns_to_replace] <-
#     auto_exp_var_df[auto_exp_var_df$presOrder %in% failtrials$fail_nonconflicts,
#                     columns_to_replace]
#   
#   #randomly shuffle pres-order of all stimuli not matched to failures, making 
#   #sure that they do not conflict with the presOrder of the manual trials
#   #matched to auto failures (which were matched to the auto failure presOrders
#   #in the step above)
#   
#   all_possible_presOrders <- 1:length(manual_exp_var_df$presOrder) 
#   nonfail_presOrders <- 
#     all_possible_presOrders[!(
#       all_possible_presOrders %in% manual_exp_var_df$presOrder[manual_exp_var_df$failtrial]
#     )]
#   
#   manual_exp_var_df$presOrder[!manual_exp_var_df$failtrial] <- sample(nonfail_presOrders,
#                                                                       length(nonfail_presOrders),
#                                                                       replace=F)
#   manual_exp_var_df
#   
# }

make_auto_aids <- function(stimuli, failtrials) {
  aids <- stimuli
  aids[stimuli=="conflict" & failtrials] <- "nonconflict"
  aids[stimuli=="nonconflict" & failtrials] <- "conflict"
  as.character(factor(
    aids,
    levels = c("conflict", "nonconflict"),
    labels = c("CONFLICT", "NON-CONF")
  ))
}

#Creates inputs for the sim
#inputs: experimental variable df, aspect ratio, x dimension
#outputs: a data frame containing all the x, y coordinates of the 
#aircraft include x dim, y dim of the screen,
# starting coordinates of the aircraft (cartesian),
#starting coordinates of the aircraft (in pixels)
#and ending coordinates (given bc atc sim requires although they
#will never actually reach them)

create_sim_input_df <- function(exp_var_df, aspectRatio, x_dim){
  #define consequential variables
  y_dim <- x_dim*aspectRatio
  #convert aircraft speed
  v1m_sec <- exp_var_df$ac1_speed/60/60
  v2m_sec <- exp_var_df$ac2_speed/60/60
  #ATC math implemented by p.lindsay
  #now verified to be correct
  radians <- exp_var_df$angle*pi/180
  A <- v1m_sec^2 + v2m_sec^2 - 2*v1m_sec*v2m_sec*cos(radians)
  Y <- v1m_sec*v2m_sec*sin(radians)
  W <- v2m_sec - v1m_sec*cos(radians)
  absM <- exp_var_df$DOMS*sqrt(A)/Y
  #For exp_var_df$OOP of 1 set absM to negative, for exp_var_df$OOP of 2 positive
  M <- (-1)^exp_var_df$OOP * absM
  #TCOP calculations verified in lab notes
  TCOP1 <- exp_var_df$TTMS - (v2m_sec*M*W)/A
  TCOP2 <- TCOP1 + M
  # Radial distances (relative distance of each aircraft from the intersection point 
   #required to produce the specified spatial and temporal properties of the event. )
  Dist1 <- TCOP1*v1m_sec
  Dist2 <- TCOP2*v2m_sec
  # randomly sample polar angles for the first aircraft path
  #then add angle to determine the second
  theta1 <- degtorad(sample(0:360, length(exp_var_df$presOrder), replace = TRUE))
  theta2 <- theta1 + degtorad(exp_var_df$angle)

  # Cartesian coordinates AC1
  x1 <- (0.5*x_dim) + cos(theta1)*Dist1
  y1 <- (0.5*y_dim) + sin(theta1)*Dist1
  
  # Cartesian coordinates AC2
  x2 <- (0.5*x_dim) + cos(theta2)*Dist2
  y2 <- (0.5*y_dim) + sin(theta2)*Dist2
  
  # Route lines AC1
  # Coordinates scaled to screen size with screenDiagonalRadius
  screenDiagonalRadius <- 0.5*sqrt(x_dim^2 + y_dim^2)
  x1start <- (0.5*x_dim) + cos(theta1)*screenDiagonalRadius
  y1start <- (0.5*y_dim) + sin(theta1)*screenDiagonalRadius
  x1end <- x_dim - x1start
  y1end <- y_dim - y1start
  
  # Route lines AC2
  x2start <- (0.5*x_dim) + cos(theta2)*screenDiagonalRadius
  y2start <- (0.5*y_dim) + sin(theta2)*screenDiagonalRadius
  x2end <- x_dim - x2start
  y2end <- y_dim - y2start
  
  tibble(x_dim, y_dim, x1, x2, y1, y2, x1start, x2start, y1start, y2start, x1end,
                      x2end, y1end, y2end)

}


#Inputs:experimental variables df, sim inputs df, condition
#Outputs: an array of strings with xml maps and xml ac
create_xml_ac_and_maps <- function (condition, exp_var_df, sim_input_df) {
  ##assign variables from exp_var_df
  ac1_fl <- exp_var_df$ac1_fl
  ac2_fl <- exp_var_df$ac2_fl
  ac1_type <- exp_var_df$ac1_type
  ac2_type <- exp_var_df$ac2_type
  ac1_speed <- exp_var_df$ac1_speed
  ac2_speed <- exp_var_df$ac2_speed
  ac1_cs <- exp_var_df$ac1_cs
  ac2_cs <- exp_var_df$ac2_cs
  stimulus <- exp_var_df$stimulus
  presOrder <- exp_var_df$presOrder
  autorec <- exp_var_df$autorec
  
  #another recommendation column for 
  #Aaron's new recommendation variable
  #write nothing if not auto condition
  if (condition=="auto"){
 
    recommendations <-
      as.character(factor(
        autorec,
        levels = c("CONFLICT", "NON-CONF"),
        labels = c("conflict", "nonconflict")
      ))
    rec <-
      paste('<atc:recommendation>',
            recommendations,
            '</atc:recommendation>\n',
            sep = "")
  } else {
    rec <- rep('', length(exp_var_df$autorec))
    
  }
  
  #assign variables from sim_input_df
  x1 <- sim_input_df$x1
  x2 <- sim_input_df$x2
  y1 <- sim_input_df$y1
  y2 <- sim_input_df$y2
  x1start <- sim_input_df$x1start
  x2start <- sim_input_df$x2start
  x1end <- sim_input_df$x1end
  x2end <- sim_input_df$x2end
  y1start <- sim_input_df$y1start
  y2start <- sim_input_df$y2start
  y1end <- sim_input_df$y1end
  y2end <- sim_input_df$y2end
  x_dim <- sim_input_df$x_dim
  y_dim <- sim_input_df$y_dim
  
  xml_ac <- c()
  xml_maps <- c()
  
  for (i in 1:length(presOrder)){
    xml_ac[i] <- paste(
      '<!-- ', toupper(condition), ' Pair ', presOrder[i], ': Aircraft 1 -->', '\n',
      '<atc:sky atc:idx=', '\'', 'sky', presOrder[i], '\'', '>', '\n',
      '<atc:aircraft atc:type=', '\'', ac1_type[i], '\'', ' atc:idx=', '\'', ac1_cs[i], '\'', '>', '\n',
      '<atc:start>', 0, '</atc:start>', '\n',
      '<atc:altitude>', ac1_fl[i], '00', '</atc:altitude>', '\n',
      '<atc:velocity>', ac1_speed[i], '</atc:velocity>', '\n',
      '<atc:flightpath>',
      '<atc:point atc:x=', '\'', round(x1[i], digits = 3), '\'', ' atc:y=', '\'', round(y1[i], digits = 3), '\'', '>', '\n',
      '<atc:altitude>', ac1_fl[i], '00', '</atc:altitude>', '</atc:point>', '\n',
      '<atc:point atc:x=', '\'', 0.5*x_dim[i], '\'', ' atc:y=', '\'', 0.5*y_dim[i], '\'', '>', '\n',
      '<atc:altitude>', ac1_fl[i], '00', '</atc:altitude>', '</atc:point>', '\n',
      '<atc:point atc:x=', '\'', round(x1end[i], digits = 3), '\'', ' atc:y=', '\'', round(y1end[i], digits = 3), '\'', '>', '\n',
      '<atc:altitude>', ac1_fl[i], '00', '</atc:altitude>', '</atc:point>', '\n',
      '</atc:flightpath> \n', 
      '<atc:autorecommendation>', autorec[i], '</atc:autorecommendation> \n', 
      '</atc:aircraft>', '\n\n',
      
      '<!-- ', toupper(condition), ' Pair ', presOrder[i], ': Aircraft 2 -->', '\n',
      '<atc:aircraft atc:type=', '\'', ac2_type[i], '\'', ' atc:idx=', '\'', ac2_cs[i], '\'', '>', '\n',
      '<atc:start>', 0, '</atc:start>', '\n',
      '<atc:altitude>', ac2_fl[i], '00', '</atc:altitude>', '\n',
      '<atc:velocity>', ac2_speed[i], '</atc:velocity>', '\n',
      '<atc:flightpath>',
      '<atc:point atc:x=', '\'', round(x2[i], digits = 3), '\'', ' atc:y=', '\'', round(y2[i], digits = 3), '\'', '>', '\n',
      '<atc:altitude>', ac2_fl[i] ,'00', '</atc:altitude>', '</atc:point>', '\n',
      '<atc:point atc:x=', '\'', 0.5*x_dim[i], '\'', ' atc:y=', '\'', 0.5*y_dim[i], '\'', '>', '\n',
      '<atc:altitude>', ac2_fl[i] ,'00', '</atc:altitude>', '</atc:point>', '\n',
      '<atc:point atc:x=', '\'', round(x2end[i], digits = 3), '\'', ' atc:y=', '\'', round(y2end[i], digits = 3), '\'', '>', '\n',
      '<atc:altitude>', ac2_fl[i] ,'00', '</atc:altitude>', '</atc:point>', '\n',
      '</atc:flightpath> \n', 
      '<atc:autorecommendation>', autorec[i], '</atc:autorecommendation> \n', 
      '</atc:aircraft>', '\n',
      '<atc:aircraftstatus>', '\n',
      '<atc:aircraft>', ac1_cs[i], '</atc:aircraft>', '\n',
      '<atc:aircraft>', ac2_cs[i], '</atc:aircraft>', '\n',
      '<atc:status>', stimulus[i], '</atc:status>', '\n',
      '<atc:finaltime>', 100, '</atc:finaltime>', '\n',
           rec[i], 
      '</atc:aircraftstatus>',
      '</atc:sky>', '\n', sep = '')
    
    xml_maps[i] <- paste(
      '<!-- Map ', presOrder[i], ' -->', '\n',
      '<atc:map atc:idx=', '\'', 'map', presOrder[i], '\'', '>', '\n\n',
      '<!-- Map Dimensions -->', '\n',
      '<atc:region atc:x=', '\'', 0, '\'', ' atc:y=', '\'', 0, '\'', ' atc:x_dim=', '\'', x_dim[i], 
      '\'', ' atc:y_dim=', '\'', y_dim[i], '\'', '/>', '\n\n',
      
      '<!-- Start and End Points -->', '\n',
      '<atc:location atc:idx=', '\'', 'startAC1trial', presOrder[i], '\'', ' atc:x=', '\'', 
      round(x1start[i], digits = 3), '\'', ' atc:y=', '\'', round(y1start[i], digits = 3), '\'', ' atc:visible=', '\'', 'off', '\'', '/>', '\n',
      '<atc:location atc:idx=', '\'', 'endAC1trial', presOrder[i], '\'', ' atc:x=', '\'', round(x1end[i], digits = 3), '\'', ' atc:y=', '\'', 
      round(y1end[i], digits = 3), '\'', ' atc:visible=', '\'', 'off', '\'', '/>', '\n',
      '<atc:location atc:idx=', '\'', 'startAC2trial', presOrder[i], '\'', ' atc:x=', '\'', round(x2start[i], digits = 3), '\'', ' atc:y=', '\'', 
      round(y2start[i], digits = 3), '\'', ' atc:visible=', '\'', 'off', '\'', '/>', '\n',
      '<atc:location atc:idx=', '\'', 'endAC2trial', presOrder[i], '\'', ' atc:x=', '\'', round(x2end[i], digits = 3), '\'', ' atc:y=', '\'', 
      round(y2end[i], digits = 3), '\'', ' atc:visible=', '\'', 'off', '\'', '/>', '\n\n',
      
      '<!-- Crossing Point -->', '\n',
      '<atc:location atc:idx=', '\'', 'crossingPointTrial', presOrder[i], '\'', ' atc:x=', '\'', 
      0.5*x_dim[i], '\'', ' atc:y=', '\'', 0.5*y_dim[i], '\'', ' atc:visible=', '\'', 'off', '\'', '/>', '\n\n',
      
      '<!-- Map ', presOrder[i], ' Route Lines -->', '\n',
      '<atc:route atc:idx=', '\'', 'routeAC1trial', presOrder[i], '\'', '>', '\n',
      '<atc:pointref atc:location=', '\'', 'startAC1trial', presOrder[i], '\'', '/>', '\n',
      '<atc:pointref atc:location=', '\'', 'endAC1trial', presOrder[i], '\'', '/>', '\n',
      '</atc:route>', '\n\n',
      
      '<atc:route atc:idx=', '\'', 'routeAC2trial', presOrder[i], '\'', '>', '\n',
      '<atc:pointref atc:location=', '\'', 'startAC2trial', presOrder[i], '\'', '/>', '\n',
      '<atc:pointref atc:location=', '\'', 'endAC2trial', presOrder[i], '\'', '/>', '\n',
      '</atc:route>', '\n\n',
      
      '<!-- Circular Sector -->', '\n',
      '<atc:sector atc:status=', '\'', 'active', '\'', ' atc:idx=', '\'', 'circleTrial', presOrder[i], '\'', '>', '\n',
      '<atc:arc atc:r=', '\'', 0.5*y_dim[i], '\'', ' atc:x=', '\'', 0.5*x_dim[i], '\'', ' atc:y=', '\'', 0.5*y_dim[i], '\'', '/>', '\n',
      '</atc:sector>', '\n',
      '</atc:map>', '\n', sep = '')
    
  }
  cbind(xml_maps, xml_ac)
}



#inputs:condition, df of experimental variables, df of
#sim input variables string, array of maps and aircraft
#for each pair

#writes the data frames to dfs and the maps and ac to 
#a text file

write_exp_data <- function(condition, exp_var_df, sim_input_df,
                           maps_and_ac, p, session){
  
  write.csv(sim_input_df, paste('data/sim_inputs_p', 
                                p, '_', 's', 
                                session, '_', toupper(condition), 
                                '.csv', sep = ''))
  
  write.csv(exp_var_df, paste('data/exp_vars', '_p', 
                              p, '_', 's', 
                              session, '_', toupper(condition), 
                              '.csv', sep = ''))
  
  writeLines(maps_and_ac, paste('components/atc_09_maps_and_ac_p', 
                                p, '_', 's', 
                                session, '_', toupper(condition), 
                                '.txt', sep = ''), sep = '\n\n')
  
}


