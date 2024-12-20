#####################
###   EYP1113L  ###
###    Lab7    ###
#################

### Lectura de datos
library(gcookbook)
data("heightweight")
head(heightweight)
library(dplyr)
bd = select(heightweight, heightIn, weightLb)

#### Análisis descriptivo
par(mfrow = c(1,2))
hist(bd$heightIn, freq = F)
hist(bd$weightLb, freq = F)
par(mfrow = c(1,1))
plot(bd$heightIn,bd$weightLb, pch = 20)
#Promedios y desviaciones estándar
mean(bd$heightIn);mean(bd$weightLb)
sd(bd$heightIn);sd(bd$weightLb)
#Correlación
cor(bd$heightIn,bd$weightLb)

#Probabilidades muestrales
#Probabilidad de que altura sea mayor a 60
mean(bd$heightIn > 60)
#Probabilidad de que peso sea mayor a 100
mean(bd$weightLb > 100)
#Probabilidad de que altura sea mayor a 60, dado que peso es mayor a 100
mean(bd$heightIn[bd$weightLb > 100] > 60)
#Probabilidad de que peso sea mayor a 100, dado que altura es mayor a 60
mean(bd$weightLb[bd$heightIn > 60] > 100)
#Probabilidad de que altura sea mayor a 60 y peso mayor a 100
mean(bd$heightIn > 60 & bd$weightLb > 100)


##### Distribución Normal bivariada
library(mvtnorm)
#Cómo luce la normal bivariada estándar
x.axis = seq(-3, 3, length = 50)
y.axis = seq(-3, 3, length = 50)
d2norm = function(x,y){
  z = numeric(length(x))
  mu = c(0,0)
  cov = matrix(c(1,0,0,1), ncol = 2)
  for(i in 1:length(x)){
    z[i] = dmvnorm(c(x[i],y[i]), mean = mu, sigma = cov)
  }
  return(z)
}
z.axis = outer(x.axis, y.axis, FUN = d2norm)
persp(x.axis, y.axis, z.axis, theta = 100, phi = 20, expand = 1, shade = 0.1, col = "orange")
image(x.axis, y.axis, z.axis)
contour(x.axis, y.axis, z.axis, add = T, col = "darkred", lwd = 1)
#Otra forma
#library(plotly)
#plot_ly(x=x.axis, y=y.axis, z=z.axis, type = "surface")
#plot_ly(x=x.axis, y=y.axis, z=z.axis, type = "contour")


##### Análisis de datos bajo normalidad
#Checkeo de normalidad
par(mfrow = c(1,2))
hist(bd$heightIn, freq= F)
muh = mean(bd$heightIn)
sdh = sd(bd$heightIn)
x.seq = seq(40, 190, length = 2000)
lines(x.seq, dnorm(x.seq, muh, sdh), type = "l", lwd = 2, col = "purple")

hist(bd$weightLb, freq= F)
muw = mean(bd$weightLb)
sdw = sd(bd$weightLb)
lines(x.seq, dnorm(x.seq, muw, sdw), type = "l", lwd = 2, col = "purple")

par(mfrow = c(1,1))
cor_hw = cor(bd)
plot((bd$heightIn-muh)/sdh,(bd$weightLb-muw)/sdw, pch = 20)
x.axis = seq(-3, 3, length = 50)
y.axis = seq(-3, 3, length = 50)
d2norm = function(x,y){
  z = numeric(length(x))
  for(i in 1:length(x)){
    z[i] = dmvnorm(c(x[i],y[i]), mean = c(0,0), sigma = cor_hw)
  }
  return(z)
}
z.axis = outer(x.axis, y.axis, FUN = d2norm)
image(x.axis, y.axis, z.axis)
lines((bd$heightIn-muh)/sdh,(bd$weightLb-muw)/sdw, type = "p", pch = 20,
      col = rgb(0,0,0.5,0.5))
contour(x.axis, y.axis, z.axis, add = T, col = "darkred", lwd = 1)

### Pareciera ser que la cantidad de datos no es suficiente para decidir
### si se cumple o no la normalidad bivariada de estas dos variables.
### Supondremos que sí.

#### Probabilidades
#Probabilidad de que altura sea mayor a 60
?pnorm
1 - pnorm(60, mean = muh, sd = sdh)
mean(bd$heightIn > 60)
#Probabilidad de que peso sea mayor a 100
1 - pnorm(100, mean = muw, sd = sdw)
mean(bd$weightLb > 100)
#Probabilidad de que altura sea mayor a 60, dado que peso es igual a 100
mu_h.w = muh + cor_hw[1,2]*(sdh/sdw)*(100 - muw)
sigma_h.w = sqrt(sdh^2*(1 - cor_hw[1,2]^2))
1 - pnorm(60, mean = mu_h.w, sd = sigma_h.w)

#Probabilidad de que peso sea mayor a 100, dado que altura es igual a 60
mu_w.h = muw + cor_hw[1,2]*(sdw/sdh)*(60 - muh)
sigma_w.h = sqrt(sdw^2*(1 - cor_hw[1,2]^2))
1 - pnorm(100, mean = mu_w.h, sd = sigma_w.h)

#Probabilidad de que altura sea mayor a 60 y peso mayor a 100
pmvnorm(lower = c(60,100), upper = Inf, mean = c(muh,muw),
        sigma = cov(bd))[1]
mean(bd$heightIn > 60 & bd$weightLb > 100)



### FIN.
