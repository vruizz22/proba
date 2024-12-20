#####################
###   EYP1113L  ###
###    Lab4    ###
#################

####Motivacion

#Distribución Poisson
lambda = 4
x.pois = rpois(70, lambda)
plot(table(x.pois)/70, xlim = c(0,13), xaxt = "n", ylim = c(0,0.25))
axis(1, at = 0:13, labels = 0:13)
x1 = 0:13
plot(x1, dpois(x1, lambda), type = "h", lwd = 2, ylim = c(0,0.25))

#Distribución Normal
mu = 0
sigma = 1
x.norm = rnorm(70, mu, sigma)
hist(x.norm, xlim = c(-3,3), breaks = 10, freq = F, ylim = c(0,0.5))
x2 = seq(-3,3, length = 1000)
plot(x2, dnorm(x2, mu, sigma), type = "l", lwd = 2, ylim = c(0,0.5))

#Distribución Gamma
alpha = 2
beta = 5
x.gamma = rgamma(70, alpha, beta)
hist(x.gamma, xlim = c(0,2), breaks = 10, freq = F, ylim = c(0,2))
x3 = seq(0,2, length = 1000)
plot(x3, dgamma(x3, alpha,beta), type = "l", lwd = 2, ylim = c(0,2))

#### Ejemplo de altura
#install.packages('gcookbook')
library(gcookbook)
data("heightweight")
head(heightweight)
hist(heightweight$heightIn)
mu <- mean(heightweight$heightIn)
sigma <- sd(heightweight$heightIn)
hist(heightweight$heightIn, freq= F)
x.seq = seq(40, 80, length = 1000)
lines(x.seq, dnorm(x.seq, 62, 10), type = "l", lwd = 2, col = "darkred")
lines(x.seq, dnorm(x.seq, 62, 5), type = "l", lwd = 2, col = "darkblue")
lines(x.seq, dnorm(x.seq, 60, 5), type = "l", lwd = 2, col = "darkgreen")
mu = mean(heightweight$heightIn)
sigma = sd(heightweight$heightIn)
lines(x.seq, dnorm(x.seq, mu, sigma), type = "l", lwd = 2, col = "purple")

# Algunas probabilidades
pnorm(55, mean = mu, sd = sigma) #P(Altura <= 55 pulgadas)
1 - pnorm(65, mean = mu, sd = sigma) #P(Altura > 65 pulgadas)
pnorm(65, mean = mu, sd = sigma) - pnorm(55, mean = mu, sd = sigma) #P(55 pulgadas < Altura <= 65 pulgadas)

# Cuantiles
qnorm(0.4, mean = mu, sd = sigma) #40% mide menos de 60 pulgadas
qnorm(0.8, mean = mu, sd = sigma) #80% mide menos de 64 pulgadas
# o bien, 20% mide mas de 64 pulgadas

### Ejemplo de llamadas
ruta  <-  file.choose()
library(rio)
bd <-  import(ruta)
head(bd)
mean(bd$llamadas)
plot(table(bd$llamadas)/nrow(bd), type = "h", lwd = 2)

lambda = mean(bd$llamadas)
lines(0:15+0.1, dpois(0:15, lambda), type = 'h', lwd = 2, col = 'darkred')

# Algunas probabilidades
ppois(6, lambda) - ppois(3, lambda) #P(3 < llamadas <= 6) = P(4 <= llamadas <= 6)

# Cuantiles
qpois(0.4, lambda) #Un 40% de las horas tiene 5 o menos llamadas
qpois(0.8, lambda) #En un 80% de las horas hay 7 llamadas o menos
# o bien, 20% percibe más de 7 llamadas

#### Ejemplo: Uso de funciones
dexp(1, rate = 3)
pexp(1.5, rate = 3)
qexp(0.5, rate = 3)
hist(rexp(1000, rate = 3))


### FIN.

