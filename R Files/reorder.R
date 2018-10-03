setwd("/Users/tpliu/Desktop/Projects/Conjoint_Analysis_and_Political_Heuristics/Data Version 2")

t11online <- read.csv("1_1online.csv", header=TRUE)
t12online <- read.csv("1_2online.csv", header=TRUE)
t13online <- read.csv("1_3online.csv", header=TRUE)
t21online <- read.csv("2_1online.csv", header=TRUE)
t22online <- read.csv("2_2online.csv", header=TRUE)
t23online <- read.csv("2_3online.csv", header=TRUE)
t31online <- read.csv("3_1online.csv", header=TRUE)
t32online <- read.csv("3_2online.csv", header=TRUE)
t33online <- read.csv("3_3online.csv", header=TRUE)

## Exclude IP "155.245.155.242" first due to it appears twice
which(t11online$IPAddress=="155.245.155.242") ## 59 & 104
which(t12online$IPAddress=="155.245.155.242") ## 59 & 104
which(t13online$IPAddress=="155.245.155.242") ## 57 & 104
which(t21online$IPAddress=="155.245.155.242") ## 57 & 105
which(t22online$IPAddress=="155.245.155.242") ## 57 & 105
which(t23online$IPAddress=="155.245.155.242") ## 57 & 105
which(t31online$IPAddress=="155.245.155.242") ## 57 & 106
which(t32online$IPAddress=="155.245.155.242") ## 56 & 106
which(t33online$IPAddress=="155.245.155.242") ## 56 & 107

t11oltemp <- t11online[-c(59, 104),]
t12oltemp <- t12online[-c(59, 104),]
t13oltemp <- t13online[-c(57, 104),]
t21oltemp <- t21online[-c(57, 105),]
t22oltemp <- t22online[-c(57, 105),]
t23oltemp <- t23online[-c(57, 105),]
t31oltemp <- t31online[-c(57, 106),]
t32oltemp <- t32online[-c(56, 106),]
t33oltemp <- t33online[-c(56, 107),]

##
online <- matrix()

for(i in 1:nrow(t11oltemp)){
    IP <- t11oltemp$IPAddress[i]
    ## Searching for the same IP from different survey
    IP12 <- which(t12oltemp$IPAddress==IP)
    IP13 <- which(t13oltemp$IPAddress==IP)
    IP21 <- which(t21oltemp$IPAddress==IP)
    IP22 <- which(t22oltemp$IPAddress==IP)
    IP23 <- which(t23oltemp$IPAddress==IP)
    IP31 <- which(t31oltemp$IPAddress==IP)
    IP32 <- which(t32oltemp$IPAddress==IP)
    IP33 <- which(t33oltemp$IPAddress==IP)
    ## combining surveys by IP
    ## First 3 surveys
    respondent1 <- rbind(t11oltemp[i,], t12oltemp[IP12,], t13oltemp[IP13,])
    ## Second 3 surveys
    respondent2 <- rbind(t21oltemp[IP21,],t22oltemp[IP22,], t23oltemp[IP23,])
    ## Third 3 surveys
    respondent3 <- rbind(t31oltemp[IP31,],t32oltemp[IP32,], t33oltemp[IP33,])
    ## combining respondents
    online <- rbind(online, respondent)
}
test <- rbind(t11oltemp[1,], t12oltemp[IP12,], t13oltemp[IP13,])
