#####################
###   EYP1113L  ###
###    Lab8    ###
#################
rm(list = ls())
library(rio)
# Variables aleatorias iid --------------------------
# Tiempos de fallas, archivo 'maquinas.xlsx'
ruta  <-  file.choose()
bd <-  import(ruta)
head(bd)
nrow(bd)
lambda <- 1.5
X <- bd$maquina1
Y <- bd$maquina2
# Análisis descriptivo
summary(bd)
# Visualización de las dos variables
par(mfrow = c(1, 2))
hist(X, main = "Maquina 1",
     xlab = "Tiempo hasta la falla", breaks = 30, freq = FALSE, col = "lightblue")
curve(dexp(x, rate = lambda), add = TRUE, col = "red", lwd = 3)

hist(Y, main = "Maquina 2",
     xlab = "Tiempo hasta la falla", breaks = 30, freq = FALSE, col = "lightgreen")
curve(dexp(x, rate = lambda), add = TRUE, col = "red", lwd = 3)
dev.off()

# Comparación conjunta
plot(X, Y, main = "Gráfico de Dispersión de X1 y X2",
     xlab = "X1", ylab = "X2", col = rgb(0, 0, 1, 0.5), pch = 16)
# Pero ¿qúe pasa cuando ambas máquinas dejan de funcionar?
# vamos a la presentación...



# Suma de variables aleatorias --------------------------
# Z = X+Y ~ Gamma(2, lambda)
Z <- X + Y
alpha <- 2
# Paso 2: Visualizar la distribución del tiempo de funcionamiento
hist(Z, breaks = 15, main = 'Histograma de tiempos',
     xlab = 'Tiempos de falla del proceso',
     ylab = 'Densidad',
     col = "lightblue", freq = F,
     ylim = c(0,.6))
curve(dgamma(x, shape = alpha, rate = lambda),
      add = T, col = 'red', lwd = 3)
# vamos a la presentación...



# Funciones de variables aleatorias ---------------------
# W = g(Z) ==> W = log(Z)
W <- log(Z)
# Cálculo de la densidad usando el método del jacobiano
# g-1(W) = Z ==> exp(W) = Z
# La densidad de W = log(Z)
# f_W(w) = f_Z(g-1(W)) * |d/dw g-1(W)|
# f_W(w) = f_Z(exp(W)) * |d/dw exp(W)|
# f_W(w) = f_Z(exp(W)) * exp(W)

hist(W, main = "W = log(X)",
     xlab = 'log(tiempos)',
     ylab = 'Densidad',
     col = "lightgreen", freq = F,
     ylim = c(0,.6), xlim = c(-3,3))
curve(dgamma(exp(x), shape = alpha, rate = lambda)* exp(x),
      col = 'red', lwd = 3, add = T)


# TLC ---------------------------------------------------
n <- 2
hist(Z, breaks = 15, main = 'Histograma de tiempos',
     xlab = 'Tiempos de falla de ambas máquinas',
     ylab = 'Densidad',
     col = "lightblue", freq = F,
     ylim = c(0,.6))
curve(dgamma(x, shape = alpha, rate = lambda),
      add = T, col = 'red', lwd = 3)
# aplicamos TCL
curve(dnorm(x, mean = n/lambda, sd = sqrt(sqrt(n)/lambda)),
      col = 'blue', lwd = 3, add = T)

# Simulamos 25 variables aleatoria exponenciales i.i.d
# con parámetro lambda = 1.5
set.seed(1010)
n <- 25
lambda <-  1.5
Xn <- matrix(NA, nrow = 200, ncol = n)
for(j in 1:n){
  Xn[,j] <- rexp(200, rate = lambda)
}
Xn[1:5,1:5]
#Graficamos la variable X1, X10, X16 y X22, para verificar que son exponenciales
par(mfrow= c(2,2))
hist(Xn[,1], freq = F, main = 'Histograma de X1')
hist(Xn[,10], freq = F, main = 'Histograma de X10')
hist(Xn[,16], freq = F, main = 'Histograma de X16')
hist(Xn[,22], freq = F, main = 'Histograma de X22')
dev.off()
# Aplicamos el TCL
# X1+X2+X3+....+X25
?apply
Sn = rowSums(Xn) #X[,1] + X[,2] + ... + X[,25]
hist(Sn, main = 'Histograma de Sn', freq = F,
     breaks = 10,
     ylim = c(0, .16))
curve(dgamma(x , shape = n, rate = lambda), add = T,
      col = 'red', lwd= 3)
curve(dnorm(x, mean = n/lambda, sd = sqrt(n/lambda^2)),
      add = T, col = 'blue', lwd = 3)
legend('topleft', legend = c('Gamma','Normal'),
       col = c('red','blue'), lwd = 3)


# Simulamos 200 variables aleatoria exponenciales i.i.d
# con parámetro lambda = 1.5
set.seed(1010)
n <- 200
lambda <-  1.5
Xn <- matrix(NA, nrow = 200, ncol = n)
for(j in 1:n){
  Xn[,j] <- rexp(200, rate = lambda)
}
# Aplicamos el TCL
# X1+X2+X3+....+X200
Sn <- apply(Xn, MARGIN = 1, FUN = sum)
hist(Sn, main = 'Histograma de Sn', freq = F,
     breaks = 10,
     ylim = c(0, .25))
curve(dgamma(x , shape = n, rate = lambda), add = T,
      col = 'red', lwd= 3)
curve(dnorm(x, mean = n/lambda, sd = sqrt(n/lambda^2)),
      add = T, col = 'blue', lwd = 3)
legend('topleft', legend = c('Gamma','Normal'),
       col = c('red','blue'), lwd = 3, cex = 0.7)

### FIN.
