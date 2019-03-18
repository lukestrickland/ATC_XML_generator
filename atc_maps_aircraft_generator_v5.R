###############################################################################
#################################### Map & Aircraft Generator for ATC-LAB #####
###############################################################################
rm(list = ls())
setwd("~/atc.sim")

callsigns_control <- read.csv('callsigns_control.csv', header = TRUE, sep = ",")
callsigns_control <- callsigns_control[,5]

callsigns_pm <- read.csv('callsigns_pm.csv', header = TRUE, sep = ",")
callsigns_pm <- callsigns_pm[,5]


###############################################################################
############################################ Enter experimental variables #####
###############################################################################
n_participants <- 1
n_conds <- 4

participant_number <- c(1:n_participants)

for (p in participant_number){
    for (c in 1:n_conds){

exp_condition <- c('A', 'B', 'C', 'D')
condition <- 'PM' # 'PM' or 'control'
session <- 1
nPairs <- 240
nPairs_plus1 <- nPairs + 1
pm_rate <- 0.2

aspectRatio <- 0.625 # 16:10
x_dim <- 180
y_dim <- x_dim*aspectRatio

x_dim_vec <- rep(x_dim, nPairs)
y_dim_vec <- rep(y_dim, nPairs)

exp_cond_seed <- c()
if (exp_condition[c] == 'A'){
    exp_cond_seed <- 150
} else if (exp_condition[c] == 'B'){
    exp_cond_seed <- 250
} else if (exp_condition[c] == 'C'){
    exp_cond_seed <- 350
} else if (exp_condition[c] == 'D'){
    exp_cond_seed <- 450
}

cond_seed <- c()
if (condition == 'control'){
    cond_seed <- 1000
} else if (condition == 'PM'){
    cond_seed <- 2000
}

session_seed <- c()
if (session == 1){
    session_seed <- 10000
} else if (session == 2){
    session_seed <- 20000
}

seed <- participant_number[p] + exp_cond_seed + cond_seed + session_seed
set.seed(seed)

pairNumber <- 1:nPairs
presOrder <- sample(1:nPairs, replace = FALSE)
pm_number <- pm_rate*nPairs


callsign_pool <- as.character(sample(callsigns_control, 2*nPairs, replace = FALSE))

ac1_type <- rep('B737', nPairs)
ac1_cs <- callsign_pool[1:nPairs]
ac1_cs
ac2_type <- rep('B737', nPairs)
ac2_cs <- callsign_pool[nPairs_plus1:length(callsign_pool)]
ac2_cs
ac2_pm_cs <- as.character(sample(callsigns_pm, nPairs, replace = FALSE))
ac2_pm_cs

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

###############################################################################
###############################################################################
conflict_status <- c()

for (i in 1:length(DOMS)){
    if (DOMS[i] < 5){
        conflict_status[i] <- 'conflict'
    } else if (DOMS[i] > 5){
        conflict_status[i] <- 'nonconflict'
    }
}

conflict_status

###############################################################################
###############################################################################
pm_pairs <- sample(pairNumber, pm_number, replace = FALSE)
pm_status <- rep(0, nPairs)
pm_status

for (i in 1:nPairs){
    if (condition == 'PM'){
    pm_status[pm_pairs] <- 1
    } else if (condition == 'control')
    pm_status[pm_pairs] <- 0
}

pm_status

stimulus <- c()

for (i in 1:length(pm_status)){
    if (pm_status[i] == 1){
        stimulus[i] <- 'other'
    } else if (pm_status[i] == 0){
        stimulus[i] <- conflict_status[i]
    }
}

stimulus

for (i in 1:length(pm_status)){
    if (pm_status[i] == 1){
        ac2_cs[i] <- ac2_pm_cs[i]
    } else if (pm_status[i] == 0){
        ac2_cs[i] <- ac2_cs[i]
    }
}

ac2_cs
cbind(ac2_cs, stimulus)

exp_vars <- as.data.frame(cbind(presOrder, pairNumber, ac1_type, ac1_cs, ac2_type, ac2_cs,
                                ac1_fl, ac2_fl, ac1_speed, ac2_speed, angle, DOMS, TTMS, OOP,
                                conflict_status, pm_status, stimulus))
head(exp_vars)
tail(exp_vars)

write.csv(exp_vars, paste('atc_exp_vars_p', participant_number[p], '_', 's', session, '_', condition, '_', exp_condition[c],  '.csv', sep = ''), row.names = FALSE)

###############################################################################
######################################## Calculate simulator input values #####
###############################################################################

v1m_sec <- ac1_speed/60/60
v2m_sec <- ac2_speed/60/60

radians <- angle*pi/180

A <- v1m_sec^2 + v2m_sec^2 - 2*v1m_sec*v2m_sec*cos(radians)
Y <- v1m_sec*v2m_sec*sin(radians)
W <- v2m_sec - v1m_sec*cos(radians)
absM <- DOMS*sqrt(A)/Y
M <- c()
for (i in 1:length(OOP)){
    if (OOP[i] == 1){
        M[i] <- -absM[i]
    } else if (OOP[i] == 2){
        M[i] <- absM[i]
    }
}

TCOP1 <- TTMS - (v2m_sec*M*W)/A
TCOP2 <- TCOP1 + M

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


###############################################################################
###################################################### Create XML scripts #####
###############################################################################

xml_ac <- c()

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
    '<atc:status>', exp_vars$stimulus[i], '</atc:status>', '\n',
    '<atc:finaltime>', 100, '</atc:finaltime>', '\n',
    '</atc:aircraftstatus>',
    '</atc:sky>', '\n', sep = '')
}

# writeLines(xml_ac, paste('atc_ac_', 's', session, '_', exp_condition[c], '_', condition, '_p', participant_number[p], '.xml', sep = ''), sep = '\n\n')


xml_maps <- c()

for (i in 1:nPairs){
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

# writeLines(xml_maps, paste('atc_maps_', 's', session, '_', exp_condition[c], '_', condition, '_p', participant_number[p], '.xml', sep = ''), sep = '\n\n')


maps_and_ac <- cbind(xml_maps, xml_ac)

writeLines(maps_and_ac, paste('atc_09_maps_and_ac_p', participant_number[p], '_', 's', session, '_', condition, '_', exp_condition[c], '.xml', sep = ''), sep = '\n\n')

    }
}


