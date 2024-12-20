
#### Ciclo for
v = numeric(10)
for(index in 1:10){
  v[index] = runif(1)
}
v

suma = 0
for(i in 1:10){
  suma = suma + v[i]
}
suma == sum(v)

#### Distribuciones muestrales
#### Ejemplo
#Simulación de datos
n = 50
mu = 5
sigma = 4
muestra = rnorm(n, mu, sigma)
hist(muestra, freq = F)

#Repetición del proceso
promedios = numeric(1000)
sumas = numeric(1000)
for(i in 1:1000){
  X.norm = rnorm(n, mu, sigma)
  promedios[i] = mean(X.norm)
  sumas[i] = (1/sigma^2) * sum((X.norm-mu)^2)
}

#Histogramas y curvas
hist(promedios, freq = F)
x1 = seq(3,7, length = 1000)
lines(x1, dnorm(x1,mu,sigma/sqrt(n)),lwd = 2)
hist(sumas, freq = F)
x2 = seq(20,100, length = 1000)
lines(x2, dchisq(x2, n),lwd = 2)

#Y para la tstudent?
tstudent = numeric(1000)
for(i in 1:1000){
  X.norm = rnorm(n, mu, sigma)
  tstudent[i] = (mean(X.norm) - mu)/sqrt(var(X.norm)/n)
}
hist(tstudent, freq = F)
x3 = seq(-4, 4, length = 1000)
lines(x3, dt(x3, n-1),lwd = 2)

#Distribución Fisher
m = 30
theta = 5
tau = 5
ratio = numeric(1000)
for(i in 1:1000){
  X1.norm = rnorm(n, mu, sigma)
  X2.norm = rnorm(m, theta, tau)
  ratio[i] = (var(X1.norm)/sigma^2)/(var(X2.norm)/tau^2)
}
hist(ratio, freq = F)
x4 = seq(0, 4, length = 1000)
lines(x4, df(x4, n-1, m-1),lwd = 2)


#### Aplicación

#Lectura de datos
library(rio)
ruta = file.choose()
datos = import(ruta)
head(datos)
#Filtrado según interés
library(dplyr)
altoCaldera = filter(datos, centro == "Caldera")$alto
altoMontt = filter(datos, centro == "Puerto Montt")$alto

#El alto distribuye Normal?
n = length(altoCaldera)
hist(altoCaldera, freq = F) #Check
hist(altoMontt, freq = F) #Check

#¿Es mu = 3.2, 3.4, 3.6 o 3.8?
x = seq(-3,3, length = 1000)
plot(x, dt(x, n-1), type = "l", lwd = 2) #Se supone que de aquí viene NormProm
mu = 3.6
NormProm1 = (mean(altoCaldera) - mu)/sqrt(var(altoCaldera)/n)
NormProm2 = (mean(altoMontt) - mu)/sqrt(var(altoMontt)/n)
abline(v = c(NormProm1,NormProm2), lwd = 2, col = c("red","blue"))

#Comparando Caldera vs PtoMontt
hist(altoCaldera, freq = F)
hist(altoMontt, freq = F, add = T, col = rgb(1,0,0,0.4))
ratio0 = (var(altoCaldera))/(var(altoMontt)) #supone sigma = tau
x = seq(0, 2, length = 1000)
plot(x, df(x, n-1, m-1), type = "l", lwd = 2) #Se supone que de aquí viene NormProm
abline(v = ratio0, lwd = 2, col = "red")

#Comentario: Así como comparamos varianzas, podríamos haber comparado
#            las medias. Nos pareció que eran muy parecidas, pero no lo
#            corroboramos. ¿Cómo sería esto?


### FIN.