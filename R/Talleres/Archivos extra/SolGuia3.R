##### Pregunta 1
n = 3000
distancia = rnorm(n, mean = 100, sd = 5)
tiempo = rgamma(n, 4, 0.2)
velocidad = distancia/tiempo
#a
hist(velocidad)
#b
Prob.10 = cumsum(velocidad < 10)/c(1:n)
plot(Prob.10, type = "l", ylim = c(0,1))
#es suficiente con n = 500 aprox
#c
Prob.20 = cumsum(velocidad > 20)/c(1:n)
plot(Prob.20, type = "l", ylim = c(0,0.2))
#n = 1500 pareciera estar bien
#d
mean.vel = cumsum(velocidad)/c(1:n)
plot(mean.vel, type = "l")
#n = 1500


##### Pregunta 5
conteos = c(1, 2, 4, 8, 7, 4, 1)
#a
plot(0:6, conteos/sum(conteos), type = "h", lwd = 2, xlim = c(0,8))

#b
sum.x = sum(conteos*(0:6))
n = sum(conteos)
l.pois = function(lambda){
  -(log(lambda)*sum.x - n*lambda)
}
EMV.pois = optim(par = 3, fn = l.pois)$par
EMV.pois

#c
lines(0:8+0.1, dpois(0:8, EMV.pois), type = "h", lwd = 2, col = "darkblue")

#d
1 - ppois(3, EMV.pois)

#e
#la varianza poisson es igual a la esperanza
EMV.pois - qt(0.975, df=n-1)*sqrt(EMV.pois/n)
EMV.pois + qt(0.975, df=n-1)*sqrt(EMV.pois/n)

##### Pregunta 6
n = 50
geom.data = rgeom(n, prob = 0.3)
#a
l.geom = function(theta){
  -(n*log(theta) + log(1-theta)*sum(geom.data))
}
grid = seq(0.00001, 0.99999, length = 1000)
plot(grid, l.geom(grid), type = "l", lwd = 2)
#b
EMV.geom = optim(par = 0.5, fn = l.geom)$par
EMV.geom
#c
abs(EMV.geom - 0.3)

##### Pregunta 7
#a
EMVs.geom = rep(0, 1000)
for(i in 1:1000){
  geom.data = rgeom(100, prob = 0.3)
  l.geom = function(theta){
    -(100*log(theta) + log(1-theta)*sum(geom.data))
  }
  EMVs.geom[i] = optim(par = 0.5, fn = l.geom)$par
}
#b
hist(EMVs.geom, freq = F)
grid = seq(0.00001, 0.99999, length = 1000)
lines(grid, dnorm(grid, mean = 0.3, sd = sd(EMVs.geom)), lwd = 2)

##### Pregunta 8
#a
n = 500
data.gam = rgamma(n, 2, 2)
sum.logx = sum(log(data.gam))
sum.x = sum(data.gam)
l.gam = function(theta){
  alpha = theta[1]
  beta = theta[2]
  -(n*alpha*log(beta) - n*log(gamma(alpha)) + (alpha-1)*sum.logx - beta*sum.x)
}
#b
EMV.theta = optim(par = c(1,1), fn = l.gam)$par
#c
abs(EMV.theta-c(2,2))
#d
abs(EMV.theta[1]/EMV.theta[2]- mean(data.gam))
abs(EMV.theta[1]/EMV.theta[2]- 1)

##### Pregunta 9
#a
n = 1500
X = rnorm(n, 0, sqrt(15))
Y = rnorm(n, 0, sqrt(8))
#b
D = sqrt(X^2+Y^2)
hist(D)
#c
mean(D>5)
plot(1:n,cumsum(D>5)/c(1:n), type = "l", ylim = c(0.1,0.5)) #Justificación de n<1500.

### FIN.
