#####################
###   EYP1113L  ###
###    Lab9    ###
#################
rm(list = ls())

##### Función de log-verosimilitud
### Distribución Poisson(lambda)
n = 50
pois = rpois(n, lambda = 2)
l.pois = function(lambda){
  -(log(lambda)*sum(pois) - n*lambda - sum(log(factorial(pois))))
}
grid = seq(0.00001, 10, length = 1000)
plot(grid, -l.pois(grid), type = "l", lwd = 2)
optim(par = 3, fn = l.pois)$par
EMV.pois = optim(par = 3, fn = l.pois)$par
EMV.pois - 2


##### Intervalos de confianza
#### Visualización distribución t-student
grid = seq(-4,4, length = 1000)
plot(grid, dnorm(grid, 0, 1), type = "l", lwd = 2)
lines(grid, dt(grid, df = 2), lwd = 2, col = "red")
lines(grid, dt(grid, df = 10), lwd = 2, col = "green")
lines(grid, dt(grid, df = 25), lwd = 2, col = "blue")

#### Test de hipótesis
#Ejemplo
#a
n1 = 100
n2 = 80
x1 = rgamma(n1,2,2)
x2 = rgamma(n2,4,3)

#b
hist(x1,freq = F,col = rgb(0,1,0,0.8))
hist(x2,freq = F,col = rgb(0,0,1,0.5), add = T)

#c
t.test(x1, x2, alternative = "two.sided", mu = 0, var.equal = F,
       paired = F, conf.level = 0.95)
t.test(x1, x2, alternative = "two.sided", mu = 0, var.equal = T,
       paired = F, conf.level = 0.95)
curve(dt(x,178), lwd = 2, col = "darkgreen", xlim = c(-3,3))
abline(v = -4.19, col = "blue", lwd = 2)

?var.test
var.test(x1, x2, ratio = 1, alternative = "two.sided", conf.level = 0.95)
curve(df(x,99,79), lwd = 2, col = "darkgreen", xlim = c(0,2))
abline(v = 0.686, col = "blue", lwd = 2)


### FIN.