######################
####   EYP1113L   ####
#### Evaluación 1 ####
######################

###################
#####  Lunes  #####
###################
ruta = file.choose()
library(rio)
bd = import(ruta)
table(bd$CNT)
bd.3 = bd[bd$CNT == "CAN" | bd$CNT == "BRA" | bd$CNT == "QCI",]
bd.Chile = bd[bd$CNT == "CHL",]

#P1
cor(bd.3$READ,bd.3$MATH) #0.8782
cor(bd.3$READ,bd.3$SCIE) #0.9265
cor(bd.3$SCIE,bd.3$MATH) #0.9320

#P2
#SCIE y MATH

#P3
mean(bd.3$READ[bd.3$CNT == "CAN"]);mean(bd.3$READ[bd.3$CNT == "BRA"]);mean(bd.3$READ[bd.3$CNT == "QCI"])
mean(bd.3$MATH[bd.3$CNT == "CAN"]);mean(bd.3$MATH[bd.3$CNT == "BRA"]);mean(bd.3$MATH[bd.3$CNT == "QCI"])
mean(bd.3$SCIE[bd.3$CNT == "CAN"]);mean(bd.3$SCIE[bd.3$CNT == "BRA"]);mean(bd.3$SCIE[bd.3$CNT == "QCI"])
#READ = QCI; MATH = QCI; SCIE = QCI

#P4
#READ = BRA; MATH = BRA; SCIE = BRA

#P5
#CHL, FALSE o F, muc, sdc

#P6
#a) plot()

#P7
pnorm(700, mean = mean(bd.3$MATH[bd.3$CNT == "CAN"]), sd = sd(bd.3$MATH[bd.3$CNT == "CAN"])) -
  pnorm(600, mean = mean(bd.3$MATH[bd.3$CNT == "CAN"]), sd = sd(bd.3$MATH[bd.3$CNT == "CAN"]))
#0.1132
pnorm(700, mean = mean(bd.3$MATH[bd.3$CNT == "BRA"]), sd = sd(bd.3$MATH[bd.3$CNT == "BRA"])) -
  pnorm(600, mean = mean(bd.3$MATH[bd.3$CNT == "BRA"]), sd = sd(bd.3$MATH[bd.3$CNT == "BRA"]))
#0.0034
pnorm(700, mean = mean(bd.3$MATH[bd.3$CNT == "QCI"]), sd = sd(bd.3$MATH[bd.3$CNT == "QCI"])) -
  pnorm(600, mean = mean(bd.3$MATH[bd.3$CNT == "QCI"]), sd = sd(bd.3$MATH[bd.3$CNT == "QCI"]))
#0.3829

#P8
(mean(bd.Chile$READ) - mean(bd.3$READ[bd.3$CNT == "QCI"]))/(sd(bd.Chile$READ)/sqrt(nrow(bd.Chile)))
qt(0.05, nrow(bd.Chile)-1)
#Menor



####################
#####  Martes  #####
####################
ruta = file.choose()
library(rio)
bd = import(ruta)
table(bd$CNT)
bd.3 = bd[bd$CNT == "AUS" | bd$CNT == "COL" | bd$CNT == "JPN",]
bd.Chile = bd[bd$CNT == "CHL",]

#P1
cor(bd.3$READ,bd.3$MATH) #0.8690
cor(bd.3$READ,bd.3$SCIE) #0.9167
cor(bd.3$SCIE,bd.3$MATH) #0.9217

#P2
#SCIE y MATH

#P3
mean(bd.3$READ[bd.3$CNT == "AUS"]);mean(bd.3$READ[bd.3$CNT == "COL"]);mean(bd.3$READ[bd.3$CNT == "JPN"])
mean(bd.3$MATH[bd.3$CNT == "AUS"]);mean(bd.3$MATH[bd.3$CNT == "COL"]);mean(bd.3$MATH[bd.3$CNT == "JPN"])
mean(bd.3$SCIE[bd.3$CNT == "AUS"]);mean(bd.3$SCIE[bd.3$CNT == "COL"]);mean(bd.3$SCIE[bd.3$CNT == "JPN"])
#READ = AUS; MATH = JPN; SCIE = JPN

#P4
#READ = COL; MATH = COL; SCIE = COL

#P5
#CHL, FALSE o F, muc, sdc

#P6
#a) plot()

#P7
pnorm(700, mean = mean(bd.3$SCIE[bd.3$CNT == "AUS"]), sd = sd(bd.3$SCIE[bd.3$CNT == "AUS"])) -
  pnorm(600, mean = mean(bd.3$SCIE[bd.3$CNT == "AUS"]), sd = sd(bd.3$SCIE[bd.3$CNT == "AUS"]))
#0.1422
pnorm(700, mean = mean(bd.3$SCIE[bd.3$CNT == "COL"]), sd = sd(bd.3$SCIE[bd.3$CNT == "COL"])) -
  pnorm(600, mean = mean(bd.3$SCIE[bd.3$CNT == "COL"]), sd = sd(bd.3$SCIE[bd.3$CNT == "COL"]))
#0.0099
pnorm(700, mean = mean(bd.3$SCIE[bd.3$CNT == "JPN"]), sd = sd(bd.3$SCIE[bd.3$CNT == "JPN"])) -
  pnorm(600, mean = mean(bd.3$SCIE[bd.3$CNT == "JPN"]), sd = sd(bd.3$SCIE[bd.3$CNT == "JPN"]))
#0.1785

#P8
(mean(bd.Chile$SCIE) - mean(bd.3$SCIE[bd.3$CNT == "JPN"]))/(sd(bd.Chile$SCIE)/sqrt(nrow(bd.Chile)))
qt(0.05, nrow(bd.Chile)-1)
#Menor



#######################
#####  Miércoles  #####
#######################
ruta = file.choose()
library(rio)
bd = import(ruta)
table(bd$CNT)
bd.3 = bd[bd$CNT == "BEL" | bd$CNT == "CHE" | bd$CNT == "URY",]
bd.Chile = bd[bd$CNT == "CHL",]

#P1
cor(bd.3$READ,bd.3$MATH) #0.8776
cor(bd.3$READ,bd.3$SCIE) #0.9255
cor(bd.3$SCIE,bd.3$MATH) #0.9268

#P2
#SCIE y MATH

#P3
median(bd.3$READ[bd.3$CNT == "BEL"]);median(bd.3$READ[bd.3$CNT == "CHE"]);median(bd.3$READ[bd.3$CNT == "URY"])
median(bd.3$MATH[bd.3$CNT == "BEL"]);median(bd.3$MATH[bd.3$CNT == "CHE"]);median(bd.3$MATH[bd.3$CNT == "URY"])
median(bd.3$SCIE[bd.3$CNT == "BEL"]);median(bd.3$SCIE[bd.3$CNT == "CHE"]);median(bd.3$SCIE[bd.3$CNT == "URY"])
#READ = BEL; MATH = BEL; SCIE = BEL

#P4
#READ = URY; MATH = URY; SCIE = URY

#P5
#CHL, FALSE o F, muc, sdc

#P6
#a) plot()

#P7
pnorm(700, mean = mean(bd.3$READ[bd.3$CNT == "BEL"]), sd = sd(bd.3$READ[bd.3$CNT == "BEL"])) -
  pnorm(600, mean = mean(bd.3$READ[bd.3$CNT == "BEL"]), sd = sd(bd.3$READ[bd.3$CNT == "BEL"]))
#0.1271
pnorm(700, mean = mean(bd.3$READ[bd.3$CNT == "URY"]), sd = sd(bd.3$READ[bd.3$CNT == "URY"])) -
  pnorm(600, mean = mean(bd.3$READ[bd.3$CNT == "URY"]), sd = sd(bd.3$READ[bd.3$CNT == "URY"]))
#0.0279
pnorm(700, mean = mean(bd.3$READ[bd.3$CNT == "CHE"]), sd = sd(bd.3$READ[bd.3$CNT == "CHE"])) -
  pnorm(600, mean = mean(bd.3$READ[bd.3$CNT == "CHE"]), sd = sd(bd.3$READ[bd.3$CNT == "CHE"]))
#0.1093

#P8
(mean(bd.Chile$READ) - mean(bd.3$READ[bd.3$CNT == "BEL"]))/(sd(bd.Chile$READ)/sqrt(nrow(bd.Chile)))
qt(0.05, nrow(bd.Chile)-1)
#Menor



####################
#####  Jueves  #####
####################
ruta = file.choose()
library(rio)
bd = import(ruta)
table(bd$CNT)
bd.3 = bd[bd$CNT == "FIN" | bd$CNT == "FRA" | bd$CNT == "DEU",]
bd.Chile = bd[bd$CNT == "CHL",]

#P1
cor(bd.3$READ,bd.3$MATH) #0.8899
cor(bd.3$READ,bd.3$SCIE) #0.9345
cor(bd.3$SCIE,bd.3$MATH) #0.9141

#P2
#SCIE y MATH

#P3
median(bd.3$READ[bd.3$CNT == "FIN"]);median(bd.3$READ[bd.3$CNT == "FRA"]);median(bd.3$READ[bd.3$CNT == "DEU"])
median(bd.3$MATH[bd.3$CNT == "FIN"]);median(bd.3$MATH[bd.3$CNT == "FRA"]);median(bd.3$MATH[bd.3$CNT == "DEU"])
median(bd.3$SCIE[bd.3$CNT == "FIN"]);median(bd.3$SCIE[bd.3$CNT == "FRA"]);median(bd.3$SCIE[bd.3$CNT == "DEU"])
#READ = FIN; MATH = DEU; SCIE = FIN

#P4
#READ = FRA; MATH = FRA; SCIE = FRA

#P5
#CHL, FALSE o F, muc, sdc

#P6
#a) plot()

#P7
pnorm(600, mean = mean(bd.3$READ[bd.3$CNT == "FIN"]), sd = sd(bd.3$READ[bd.3$CNT == "FIN"])) -
  pnorm(400, mean = mean(bd.3$READ[bd.3$CNT == "FIN"]), sd = sd(bd.3$READ[bd.3$CNT == "FIN"]))
#0.7015
pnorm(600, mean = mean(bd.3$READ[bd.3$CNT == "FRA"]), sd = sd(bd.3$READ[bd.3$CNT == "FRA"])) -
  pnorm(400, mean = mean(bd.3$READ[bd.3$CNT == "FRA"]), sd = sd(bd.3$READ[bd.3$CNT == "FRA"]))
#0.6647
pnorm(600, mean = mean(bd.3$READ[bd.3$CNT == "DEU"]), sd = sd(bd.3$READ[bd.3$CNT == "DEU"])) -
  pnorm(400, mean = mean(bd.3$READ[bd.3$CNT == "DEU"]), sd = sd(bd.3$READ[bd.3$CNT == "DEU"]))
#0.66596

#P8
(mean(bd.Chile$READ) - mean(bd.3$READ[bd.3$CNT == "FIN"]))/(sd(bd.Chile$READ)/sqrt(nrow(bd.Chile)))
qt(0.025, nrow(bd.Chile)-1)
qt(0.975, nrow(bd.Chile)-1)
#A la zquierda

### FIN.