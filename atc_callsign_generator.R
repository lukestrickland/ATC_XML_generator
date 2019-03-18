##########################################
### Control and PM call-sign generator ###
##########################################
rm(list = ls())

alpha1 <- c(rep('A', 26), rep('B', 26), rep('C', 26), rep('D', 26),
            rep('E', 26), rep('F', 26), rep('G', 26), rep('H', 26),
            rep('I', 26), rep('J', 26), rep('K', 26), rep('L', 26),
            rep('M', 26), rep('N', 26), rep('O', 26), rep('P', 26),
            rep('Q', 26), rep('R', 26), rep('S', 26), rep('T', 26),
            rep('U', 26), rep('V', 26), rep('W', 26), rep('X', 26),
            rep('Y', 26), rep('Z', 26))
alpha2 <- rep(LETTERS[1:26], 26)
alpha3 <- rep(LETTERS[26:1], 26)
alpha_control <- cbind(alpha1, alpha2, alpha3)
head(alpha_control)

for (i in 1:length(alpha1)){
    if (alpha_control[i,1] == alpha_control[i,2]){
        alpha_control[i,1:3] <- NA}
    else if (alpha_control[i,1] == alpha_control[i,3]){
        alpha_control[i,1:3] <- NA}
    else if (alpha_control[i,2] == alpha_control[i,3]){
        alpha_control[i,1:3] <- NA}
}

cs_alpha_control <- as.data.frame(na.omit(alpha_control))
cs_alpha_control
cs_num_control <- sample(100:999, length(cs_alpha_control[,1]), replace = FALSE)
cs_num_control
callsign_control <- cbind(cs_alpha_control, cs_num_control)
callsign_control

alpha_pm <- cbind(alpha1, alpha2, alpha1)
head(alpha_pm)

for (i in 1:length(alpha1)){
    if (alpha_pm[i,1] == alpha_pm[i,2]){
        alpha_pm[i,1:3] <- NA}
    else if (alpha_pm[i,2] == alpha_pm[i,3]){
        alpha_pm[i,1:3] <- NA}
}

cs_alpha_pm <- as.data.frame(na.omit(alpha_pm))
cs_alpha_pm
cs_num_pm <- sample(100:999, length(cs_alpha_pm[,1]), replace = FALSE)
cs_num_pm
callsign_pm <- cbind(cs_alpha_pm, cs_num_pm)
callsign_pm

write.csv(callsign_control, file = "callsigns_control.csv", row.names = FALSE)
write.csv(callsign_pm, file = "callsigns_pm.csv", row.names = FALSE)
