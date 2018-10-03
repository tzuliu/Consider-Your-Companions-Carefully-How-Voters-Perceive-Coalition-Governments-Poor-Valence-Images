setwd("/Users/tpliu/Desktop/Projects/Conjoint_Analysis_and_Political_Heuristics/Data Version 2")

t12 <- read.csv("1_2.csv", header=TRUE)
t12n <- t12[197:nrow(t12),]
t13 <- read.csv("1_3.csv", header=TRUE)
t13n <- t13[197:nrow(t13),]
t21 <- read.csv("2_1.csv", header=TRUE)
t21n <- t21[197:nrow(t21),]
t22 <- read.csv("2_2.csv", header=TRUE)
t22n <- t22[197:nrow(t22),]
t23 <- read.csv("2_3.csv", header=TRUE)
t23n <- t23[197:nrow(t23),]
t31 <- read.csv("3_1.csv", header=TRUE)
t31n <- t31[197:nrow(t31),]
t32 <- read.csv("3_2.csv", header=TRUE)
t32n <- t32[197:nrow(t32),]
t33 <- read.csv("3_3.csv", header=TRUE)
t33n <- t33[197:nrow(t33),]
t11n <- read.csv("1_1_nnn.csv", header=TRUE)
t11 <- read.csv("1_1_O.csv", header=TRUE)
t12o <- t12[1:196,]
t13o <- t13[1:196,]
t21o <- t21[1:196,]
t22o <- t22[1:196,]
t23o <- t23[1:196,]
t31o <- t31[1:196,]
t32o <- t32[1:196,]
t33o <- t33[1:196,]

##
setdiff(t11$IPAddress, t12o$IPAddress)
setdiff(t12o$IPAddress, t11$IPAddress)
setdiff(t11$IPAddress, t13o$IPAddress)
setdiff(t13o$IPAddress, t11$IPAddress)
setdiff(t11$IPAddress, t21o$IPAddress)
setdiff(t21o$IPAddress, t11$IPAddress)
setdiff(t11$IPAddress, t22o$IPAddress)
setdiff(t22o$IPAddress, t11$IPAddress)
setdiff(t11$IPAddress, t23o$IPAddress)
setdiff(t23o$IPAddress, t11$IPAddress)
setdiff(t11$IPAddress, t31o$IPAddress)
setdiff(t31o$IPAddress, t11$IPAddress)
setdiff(t11$IPAddress, t32o$IPAddress)
setdiff(t32o$IPAddress, t11$IPAddress)
setdiff(t11$IPAddress, t33o$IPAddress)
setdiff(t33o$IPAddress, t11$IPAddress)

## Which IP of 1_2 is not in 1_3 and vice versa
setdiff(t12n$IPAddress, t13n$IPAddress) ## no
setdiff(t13n$IPAddress, t12n$IPAddress) ## no

## Which IP of 1_2 & 1_3 is not in 2_1 and vice versa
setdiff(t12n$IPAddress, t21n$IPAddress) ## "155.245.46.34"
setdiff(t13n$IPAddress, t21n$IPAddress) ## "155.245.46.34"
setdiff(t21n$IPAddress, t12n$IPAddress) ## no
setdiff(t21n$IPAddress, t13n$IPAddress) ## no

## Search for incomplete respondent
which(t12n$IPAddress=="155.245.46.34") ## 39
which(t13n$IPAddress=="155.245.46.34") ## 37

## rearrange new Data
t12c <- t12n[-39,]
t13c <- t13n[-37,]

## Which IP of 1_2 & 1_3 & 2_1 is not in 2_2 and vice versa
setdiff(t12c$IPAddress, t22n$IPAddress) ## "2.223.33.220"
setdiff(t13c$IPAddress, t22n$IPAddress) ## "2.223.33.220"
setdiff(t21n$IPAddress, t22n$IPAddress) ## "2.223.33.220"
setdiff(t22n$IPAddress, t12c$IPAddress) ## no
setdiff(t22n$IPAddress, t13c$IPAddress) ## no
setdiff(t22n$IPAddress, t21n$IPAddress) ## no

## Search for incomplete respondent
which(t12c$IPAddress=="2.223.33.220") ## 118
which(t13c$IPAddress=="2.223.33.220") ## 118
which(t21n$IPAddress=="2.223.33.220") ## 119

## rearrange new Data
t12c <- t12c[-118,]
t13c <- t13c[-118,]
t21c <- t21n[-119,]

## Which IP of 1_2 & 1_3 & 2_1 & 2_2 is not in 2_3 and vice versa
setdiff(t12c$IPAddress, t23n$IPAddress) ## no
setdiff(t13c$IPAddress, t23n$IPAddress) ## no
setdiff(t21c$IPAddress, t23n$IPAddress) ## no
setdiff(t22n$IPAddress, t23n$IPAddress) ## no
setdiff(t23n$IPAddress, t12c$IPAddress) ## no
setdiff(t23n$IPAddress, t13c$IPAddress) ## no
setdiff(t23n$IPAddress, t21c$IPAddress) ## no
setdiff(t23n$IPAddress, t22n$IPAddress) ## no

## rearrange new Data
t22c <- t22n

## Which IP of 1_2 & 1_3 & 2_1 & 2_2 & 2_3 is not in 3_1 and vice versa
setdiff(t12c$IPAddress, t31n$IPAddress) ## "37.205.58.146"
setdiff(t13c$IPAddress, t31n$IPAddress) ## "37.205.58.146"
setdiff(t21c$IPAddress, t31n$IPAddress) ## "37.205.58.146"
setdiff(t22c$IPAddress, t31n$IPAddress) ## "37.205.58.146"
setdiff(t23n$IPAddress, t31n$IPAddress) ## "37.205.58.146"
setdiff(t31n$IPAddress, t12c$IPAddress) ## "94.118.6.109"
setdiff(t31n$IPAddress, t13c$IPAddress) ## "94.118.6.109"
setdiff(t31n$IPAddress, t21c$IPAddress) ## "94.118.6.109"
setdiff(t31n$IPAddress, t22c$IPAddress) ## "94.118.6.109"
setdiff(t31n$IPAddress, t23n$IPAddress) ## "94.118.6.109"

## Search for incomplete respondent
which(t12c$IPAddress=="37.205.58.146") ## 32
which(t13c$IPAddress=="37.205.58.146") ## 30
which(t21c$IPAddress=="37.205.58.146") ## 32
which(t22c$IPAddress=="37.205.58.146") ## 35
which(t23n$IPAddress=="37.205.58.146") ## 35
which(t31n$IPAddress=="94.118.6.109") ## 106

## rearrange new Data
t12c <- t12c[-32,]
t13c <- t13c[-30,]
t21c <- t21c[-32,]
t22c <- t22c[-35,]
t23c <- t23n[-35,]
t31c <- t31n[-106,]

## Which IP of 1_2 & 1_3 & 2_1 & 2_2 & 2_3 & 3_1 is not in 3_2 and vice versa
setdiff(t12c$IPAddress, t32n$IPAddress) ## "82.132.246.208"
setdiff(t13c$IPAddress, t32n$IPAddress) ## "82.132.246.208"
setdiff(t21c$IPAddress, t32n$IPAddress) ## "82.132.246.208"
setdiff(t22c$IPAddress, t32n$IPAddress) ## "82.132.246.208"
setdiff(t23c$IPAddress, t32n$IPAddress) ## "82.132.246.208"
setdiff(t31c$IPAddress, t32n$IPAddress) ## "82.132.246.208"
setdiff(t32n$IPAddress, t12c$IPAddress) ## "94.118.6.109"
setdiff(t32n$IPAddress, t13c$IPAddress) ## "94.118.6.109"
setdiff(t32n$IPAddress, t21c$IPAddress) ## "94.118.6.109"
setdiff(t32n$IPAddress, t22c$IPAddress) ## "94.118.6.109"
setdiff(t32n$IPAddress, t23c$IPAddress) ## "94.118.6.109"
setdiff(t32n$IPAddress, t31c$IPAddress) ## "94.118.6.109"

## Search for incomplete respondent
which(t12c$IPAddress=="82.132.246.208") ## 11
which(t13c$IPAddress=="82.132.246.208") ## 10
which(t21c$IPAddress=="82.132.246.208") ## 10
which(t22c$IPAddress=="82.132.246.208") ## 10
which(t23c$IPAddress=="82.132.246.208") ## 10
which(t31c$IPAddress=="82.132.246.208") ## 10
which(t32n$IPAddress=="94.118.6.109") ## 105

## rearrange new Data
t12c <- t12c[-11,]
t13c <- t13c[-10,]
t21c <- t21c[-10,]
t22c <- t22c[-10,]
t23c <- t23c[-10,]
t31c <- t31c[-10,]
t32c <- t32n[-105,]

## Which IP of 1_2 & 1_3 & 2_1 & 2_2 & 2_3 & 3_1 & 3_2 is not in 3_3 and vice versa
setdiff(t12c$IPAddress, t33n$IPAddress) ## "2.96.126.200"
setdiff(t13c$IPAddress, t33n$IPAddress) ## "2.96.126.200"
setdiff(t21c$IPAddress, t33n$IPAddress) ## "2.96.126.200"
setdiff(t22c$IPAddress, t33n$IPAddress) ## "2.96.126.200"
setdiff(t23c$IPAddress, t33n$IPAddress) ## "2.96.126.200"
setdiff(t31c$IPAddress, t33n$IPAddress) ## "2.96.126.200"
setdiff(t32c$IPAddress, t33n$IPAddress) ## "2.96.126.200"
setdiff(t33n$IPAddress, t12c$IPAddress) ## "94.118.6.109"
setdiff(t33n$IPAddress, t13c$IPAddress) ## "94.118.6.109"
setdiff(t33n$IPAddress, t21c$IPAddress) ## "94.118.6.109"
setdiff(t33n$IPAddress, t22c$IPAddress) ## "94.118.6.109"
setdiff(t33n$IPAddress, t23c$IPAddress) ## "94.118.6.109"
setdiff(t33n$IPAddress, t31c$IPAddress) ## "94.118.6.109"
setdiff(t33n$IPAddress, t32c$IPAddress) ## "94.118.6.109"

## Search for incomplete respondent
which(t12c$IPAddress=="2.96.126.200") ## 57
which(t13c$IPAddress=="2.96.126.200") ## 55
which(t21c$IPAddress=="2.96.126.200") ## 55
which(t22c$IPAddress=="2.96.126.200") ## 55
which(t23c$IPAddress=="2.96.126.200") ## 55
which(t31c$IPAddress=="2.96.126.200") ## 55
which(t32c$IPAddress=="2.96.126.200") ## 55
which(t33n$IPAddress=="94.118.6.109") ## 104

## rearrange new Data
t12c <- t12c[-57,]
t13c <- t13c[-55,]
t21c <- t21c[-55,]
t22c <- t22c[-55,]
t23c <- t23c[-55,]
t31c <- t31c[-55,]
t32c <- t32c[-55,]
t33c <- t33n[-104,]

##
d1112 <- setdiff(t11n$IPAddress, t12c$IPAddress)
d1113 <- setdiff(t11n$IPAddress, t13c$IPAddress)
d1121 <- setdiff(t11n$IPAddress, t21c$IPAddress)
d1122 <- setdiff(t11n$IPAddress, t22c$IPAddress)
d1123 <- setdiff(t11n$IPAddress, t23c$IPAddress)
d1131 <- setdiff(t11n$IPAddress, t31c$IPAddress)
d1132 <- setdiff(t11n$IPAddress, t32c$IPAddress)
d1133 <- setdiff(t11n$IPAddress, t33c$IPAddress)


for(i in 1:length(d1112)){
 aa <- which(t11n$IPAddress==d1112[i])
 t11n <- t11n[-aa,]
}


nd1112 <- setdiff(t12c$IPAddress, t11n$IPAddress)
nd1113 <- setdiff(t13c$IPAddress, t11n$IPAddress)
nd1121 <- setdiff(t21c$IPAddress, t11n$IPAddress)
nd1122 <- setdiff(t22c$IPAddress, t11n$IPAddress)
nd1123 <- setdiff(t23c$IPAddress, t11n$IPAddress)
nd1131 <- setdiff(t31c$IPAddress, t11n$IPAddress)
nd1132 <- setdiff(t32c$IPAddress, t11n$IPAddress)
nd1133 <- setdiff(t33c$IPAddress, t11n$IPAddress)

nd1112;nd1113;nd1121;nd1122;nd1123;nd1131;nd1132;nd1133

which(t12c$IPAddress=="109.144.216.28") ## 39
which(t13c$IPAddress=="109.144.216.28") ## 37
which(t21c$IPAddress=="109.144.216.28") ## 37
which(t22c$IPAddress=="109.144.216.28") ## 37
which(t23c$IPAddress=="109.144.216.28") ## 38
which(t31c$IPAddress=="109.144.216.28") ## 39
which(t32c$IPAddress=="109.144.216.28") ## 40
which(t33c$IPAddress=="109.144.216.28") ## 40

t12c <- t12c[-39,]
t13c <- t13c[-37,]
t21c <- t21c[-37,]
t22c <- t22c[-37,]
t23c <- t23c[-38,]
t31c <- t31c[-39,]
t32c <- t32c[-40,]
t33c <- t33c[-40,]

setdiff(t11n$IPAddress, t12c$IPAddress)
setdiff(t11n$IPAddress, t13c$IPAddress)
setdiff(t11n$IPAddress, t21c$IPAddress)
setdiff(t11n$IPAddress, t22c$IPAddress)
setdiff(t11n$IPAddress, t23c$IPAddress)
setdiff(t11n$IPAddress, t31c$IPAddress)
setdiff(t11n$IPAddress, t32c$IPAddress)
setdiff(t11n$IPAddress, t33c$IPAddress)

setdiff(t12c$IPAddress, t11n$IPAddress)
setdiff(t13c$IPAddress, t11n$IPAddress)
setdiff(t22c$IPAddress, t11n$IPAddress)
setdiff(t23c$IPAddress, t11n$IPAddress)
setdiff(t31c$IPAddress, t11n$IPAddress)
setdiff(t32c$IPAddress, t11n$IPAddress)
setdiff(t33c$IPAddress, t11n$IPAddress)
t11c <- t11n

## output csv files
write.csv(t11c, file="1_1online.csv")
write.csv(t12c, file="1_2online.csv")
write.csv(t13c, file="1_3online.csv")
write.csv(t21c, file="2_1online.csv")
write.csv(t22c, file="2_2online.csv")
write.csv(t23c, file="2_3online.csv")
write.csv(t31c, file="3_1online.csv")
write.csv(t32c, file="3_2online.csv")
write.csv(t33c, file="3_3online.csv")

##
which(t13c$IPAddress=="155.245.155.242")
which(table(t12c$IPAddress)>1)
