##########################################
### Control and PM call-sign generator ###
##########################################
library(tidyverse)
rm(list = ls())

alpha1 <- rep(LETTERS, each=26*26)
alpha2 <- rep(rep(LETTERS, each=26), 26)
alpha3 <- rep(LETTERS, 26*26)
alpha <- data.frame(cbind(alpha1, alpha2, alpha3))

  
cs_num <- sample(100:999, length(alpha[,1]), replace = TRUE)
cs_num

callsign <- cbind(alpha, cs_num)
callsign <- callsign %>% mutate(CS = paste(alpha1, alpha2,alpha3, cs_num, sep=""))



write.csv(callsign, file = "callsigns.csv", row.names = FALSE)