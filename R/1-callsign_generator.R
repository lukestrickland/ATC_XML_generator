##########################################
### Control and PM call-sign generator ###
##########################################

library(tidyverse)

alpha1 <- rep(LETTERS, each=26*26)
alpha2 <- rep(rep(LETTERS, each=26), 26)
alpha3 <- rep(LETTERS, 26*26)
alpha <- data.frame(cbind(alpha1, alpha2, alpha3))

  
cs_num <- sample(100:999, length(alpha[,1]), replace = TRUE)

callsign <- cbind(alpha, cs_num)
callsign <- callsign %>% mutate(CS = paste(alpha1, alpha2,alpha3, cs_num, sep=""))


write.csv(callsign, file = "components/callsigns.csv", row.names = FALSE)
