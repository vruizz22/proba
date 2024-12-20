##############################
###    Solución  Guía 2    ###
###      Laboratorio       ###
### Intro a la estadística ###
##############################

#PREGUNTA 1
#alpha = 2, beta = 6
frecuencias <- c(1, 2.5, 2.8, 1.5, 1.1, 0.8, 0.2, 0.1)*100
casillas = c(rep(0.05, frecuencias[1]), rep(0.15, frecuencias[2]),
          rep(0.25, frecuencias[3]), rep(0.35, frecuencias[4]),
          rep(0.45, frecuencias[5]), rep(0.55, frecuencias[6]),
          rep(0.65, frecuencias[7]), rep(0.75, frecuencias[8]))
hist(casillas, breaks = 5, freq = F, xlim = c(0,1), ylim = c(0,3))
x = seq(0, 1, length = 1000)
lines(x, dbeta(x,2,7), lwd = 2)
#alpha min = 1.7, max = 2.5
#beta min = 4.5, max = 7

#PREGUNTA 5
X = rgamma(300, 6, 1)
Y = rgamma(300, 2, 1)
BETA = X/(X+Y)
hist(BETA, freq = F)
lines(seq(0,1,length=1000), dbeta(seq(0,1,length=1000), 6, 2))

### FIN.
