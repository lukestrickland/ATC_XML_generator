create_exp_var_df <- function(condition, session, nPairs, callsigns){
  pairNumber <- 1:nPairs
  presOrder <- sample(1:nPairs, replace = FALSE)
  callsign_pool <- as.character(sample(callsigns, 2*nPairs, replace = FALSE))
  ac1_type <- rep('B737', nPairs)
  ac1_cs <- callsign_pool[1:nPairs]
  ac1_cs
  ac2_type <- rep('B737', nPairs)
  ac2_cs <- callsign_pool[(nPairs+1):length(callsign_pool)]
  ac2_cs
  ac1_fl <- rep(370, nPairs)
  ac2_fl <- rep(370, nPairs)
  angle <- rep(90, nPairs)
  ac1_speed <- sample(400:450, nPairs, replace = TRUE)
  ac2_speed <- sample(400:450, nPairs, replace = TRUE)
  ac2_pm_speed <- ac2_speed
  doms_lower <- runif(nPairs*0.5, 0, 3)
  doms_upper <- runif(nPairs*0.5, 7, 10)
  DOMS <- round(c(doms_lower, doms_upper), 1)
  TTMS <- sample(120:210, nPairs, replace = TRUE)
  OOP <- rep(c(1, 2), 0.5*nPairs)
  conflict_status <- factor(DOMS>5, labels = c("conflict", "non-conflict"))
  stimulus <- conflict_status
  pairNumber <- 1:nPairs
  presOrder <- sample(1:nPairs, replace = FALSE)
  as.tibble(cbind(presOrder, pairNumber, ac1_type, ac1_cs, ac2_type, ac2_cs,
                  ac1_fl, ac2_fl, ac1_speed, ac2_speed, angle, DOMS, TTMS, OOP,
                  conflict_status, stimulus))
}


create_sim_input_df <- function(exp_var_df, aspectRatio, x_dim){
  #exp_var_dfs back to numeric where necessary
  ac1_speed <- as.numeric(exp_var_df$ac1_speed)
  ac2_speed <- as.numeric(exp_var_df$ac2_speed)
  angle <-  as.numeric(exp_var_df$angle)
  DOMS <-  as.numeric(exp_var_df$DOMS)
  OOP <-  as.numeric(exp_var_df$OOP)
  TTMS <- as.numeric(exp_var_df$TTMS)
  
  #define consequential variables
  y_dim <- x_dim*aspectRatio
  x_dim_vec <- rep(x_dim, nPairs)
  y_dim_vec <- rep(y_dim, nPairs)
  
  #crazy ATC math implemented by p.lindsay
  v1m_sec <- ac1_speed/60/60
  v2m_sec <- ac2_speed/60/60
  radians <- angle*pi/180
  A <- v1m_sec^2 + v2m_sec^2 - 2*v1m_sec*v2m_sec*cos(radians)
  Y <- v1m_sec*v2m_sec*sin(radians)
  W <- v2m_sec - v1m_sec*cos(radians)
  absM <- DOMS*sqrt(A)/Y
  #For OOP of 1 set absM to negative, for OOP of 2 positive
  M <- (-1)^OOP * absM

  TCOP1 <- TTMS - (v1m_sec*M*W)/A #################################### New TCOP1
  TCOP2 <- TTMS - (v2m_sec*M*W)/A #################################### New TCOP2
  
  # Radial distances
  Dist1 <- TCOP1*v1m_sec
  Dist2 <- TCOP2*v2m_sec
  
  # Polar angles
  theta1 <- sample(0:360, nPairs, replace = TRUE)
  theta2 <- theta1 + angle
  
  # Coordinates on unit circle
  cosTheta1 <- cos(theta1*pi/180)
  sinTheta1 <- sin(theta1*pi/180)
  cosTheta2 <- cos(theta2*pi/180)
  sinTheta2 <- sin(theta2*pi/180)
  
  screenDiagonalRadius <- 0.5*sqrt(x_dim^2 + y_dim^2)
  
  # Coordinates scaled to screen size
  cosTheta1Radius <- cosTheta1*screenDiagonalRadius
  sinTheta1Radius <- sinTheta1*screenDiagonalRadius
  cosTheta2Radius <- cosTheta2*screenDiagonalRadius
  sinTheta2Radius <- sinTheta2*screenDiagonalRadius
  
  # Cartesian coordinates AC1
  x1 <- (0.5*x_dim) + cosTheta1*Dist1
  y1 <- (0.5*y_dim) + sinTheta1*Dist1
  
  # Cartesian coordinates AC2
  x2 <- (0.5*x_dim) + cosTheta2*Dist2
  y2 <- (0.5*y_dim) + sinTheta2*Dist2
  
  # Route lines AC1
  x1start <- (0.5*x_dim) + cosTheta1*screenDiagonalRadius
  y1start <- (0.5*y_dim) + sinTheta1*screenDiagonalRadius
  x1end <- x_dim - x1start
  y1end <- y_dim - y1start
  gradient1 <- (y1end - y1start)/(x1end - x1start)
  
  # Route lines AC2
  x2start <- (0.5*x_dim) + cosTheta2*screenDiagonalRadius
  y2start <- (0.5*y_dim) + sinTheta2*screenDiagonalRadius
  x2end <- x_dim - x2start
  y2end <- y_dim - y2start
  gradient2 <- (y2end - y2start)/(x2end - x2start)
  
  as.tibble(cbind(x_dim_vec, y_dim_vec, x1, x2, y1, y2, x1start, x2start, y1start, y2start, x1end,
                      x2end, y2end, y1end, y2end, gradient1, gradient2))
  
}


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
  
  
  x_dim_vec <- sim_input_df$x_dim_vec
  y_dim_vec <- sim_input_df$y_dim_vec
  
  
  xml_ac <- c()
  xml_maps <- c()
  
  for (i in 1:nPairs){
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
      '<atc:point atc:x=', '\'', 0.5*x_dim_vec[i], '\'', ' atc:y=', '\'', 0.5*y_dim_vec[i], '\'', '>', '\n',
      '<atc:altitude>', ac1_fl[i], '00', '</atc:altitude>', '</atc:point>', '\n',
      '<atc:point atc:x=', '\'', round(x1end[i], digits = 3), '\'', ' atc:y=', '\'', round(y1end[i], digits = 3), '\'', '>', '\n',
      '<atc:altitude>', ac1_fl[i], '00', '</atc:altitude>', '</atc:point>', '\n',
      '</atc:flightpath>', '</atc:aircraft>', '\n\n',
      
      '<!-- ', toupper(condition), ' Pair ', presOrder[i], ': Aircraft 2 -->', '\n',
      '<atc:aircraft atc:type=', '\'', ac2_type[i], '\'', ' atc:idx=', '\'', ac2_cs[i], '\'', '>', '\n',
      '<atc:start>', 0, '</atc:start>', '\n',
      '<atc:altitude>', ac2_fl[i], '00', '</atc:altitude>', '\n',
      '<atc:velocity>', ac2_speed[i], '</atc:velocity>', '\n',
      '<atc:flightpath>',
      '<atc:point atc:x=', '\'', round(x2[i], digits = 3), '\'', ' atc:y=', '\'', round(y2[i], digits = 3), '\'', '>', '\n',
      '<atc:altitude>', ac2_fl[i] ,'00', '</atc:altitude>', '</atc:point>', '\n',
      '<atc:point atc:x=', '\'', 0.5*x_dim_vec[i], '\'', ' atc:y=', '\'', 0.5*y_dim_vec[i], '\'', '>', '\n',
      '<atc:altitude>', ac2_fl[i] ,'00', '</atc:altitude>', '</atc:point>', '\n',
      '<atc:point atc:x=', '\'', round(x2end[i], digits = 3), '\'', ' atc:y=', '\'', round(y2end[i], digits = 3), '\'', '>', '\n',
      '<atc:altitude>', ac2_fl[i] ,'00', '</atc:altitude>', '</atc:point>', '\n',
      '</atc:flightpath>', '</atc:aircraft>', '\n',
      '<atc:aircraftstatus>', '\n',
      '<atc:aircraft>', ac1_cs[i], '</atc:aircraft>', '\n',
      '<atc:aircraft>', ac2_cs[i], '</atc:aircraft>', '\n',
      '<atc:status>', stimulus[i], '</atc:status>', '\n',
      '<atc:finaltime>', 100, '</atc:finaltime>', '\n',
      '</atc:aircraftstatus>',
      '</atc:sky>', '\n', sep = '')
    
    xml_maps[i] <- paste(
      '<!-- Map ', presOrder[i], ' -->', '\n',
      '<atc:map atc:idx=', '\'', 'map', presOrder[i], '\'', '>', '\n\n',
      '<!-- Map Dimensions -->', '\n',
      '<atc:region atc:x=', '\'', 0, '\'', ' atc:y=', '\'', 0, '\'', ' atc:x_dim=', '\'', x_dim_vec[i], '\'', ' atc:y_dim=', '\'', y_dim_vec[i], '\'', '/>', '\n\n',
      
      '<!-- Start and End Points -->', '\n',
      '<atc:location atc:idx=', '\'', 'startAC1trial', presOrder[i], '\'', ' atc:x=', '\'', round(x1start[i], digits = 3), '\'', ' atc:y=', '\'', round(y1start[i], digits = 3), '\'', ' atc:visible=', '\'', 'off', '\'', '/>', '\n',
      '<atc:location atc:idx=', '\'', 'endAC1trial', presOrder[i], '\'', ' atc:x=', '\'', round(x1end[i], digits = 3), '\'', ' atc:y=', '\'', round(y1end[i], digits = 3), '\'', ' atc:visible=', '\'', 'off', '\'', '/>', '\n',
      '<atc:location atc:idx=', '\'', 'startAC2trial', presOrder[i], '\'', ' atc:x=', '\'', round(x2start[i], digits = 3), '\'', ' atc:y=', '\'', round(y2start[i], digits = 3), '\'', ' atc:visible=', '\'', 'off', '\'', '/>', '\n',
      '<atc:location atc:idx=', '\'', 'endAC2trial', presOrder[i], '\'', ' atc:x=', '\'', round(x2end[i], digits = 3), '\'', ' atc:y=', '\'', round(y2end[i], digits = 3), '\'', ' atc:visible=', '\'', 'off', '\'', '/>', '\n\n',
      
      '<!-- Crossing Point -->', '\n',
      '<atc:location atc:idx=', '\'', 'crossingPointTrial', presOrder[i], '\'', ' atc:x=', '\'', 0.5*x_dim_vec[i], '\'', ' atc:y=', '\'', 0.5*y_dim_vec[i], '\'', ' atc:visible=', '\'', 'off', '\'', '/>', '\n\n',
      
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
      '<atc:arc atc:r=', '\'', 0.5*y_dim_vec[i], '\'', ' atc:x=', '\'', 0.5*x_dim_vec[i], '\'', ' atc:y=', '\'', 0.5*y_dim_vec[i], '\'', '/>', '\n',
      '</atc:sector>', '\n',
      '</atc:map>', '\n', sep = '')
    
  }
  cbind(xml_maps, xml_ac)
}
