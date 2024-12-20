#####################
###   EYP1113L  ###
###    Lab10    ###
#################
rm(list=ls())
library(rio)
library(nortest)
### Lectura de datos 'valor_casas_macul_nunoa.xlsx'
ruta  <-  file.choose()
bd <-  import(ruta)
head(bd)

(n <- length(bd$Valor_UF))
# Modelo de regresión lineal simple --------------------------------------------
## a)
plot(bd$Superficie_Construida_M2, bd$Valor_UF,
     xlab= 'Superficie construida', ylab = 'Valor UF',
     cex = 1.5, pch = 20)
## b)
cor(bd$Superficie_Construida_M2, bd$Valor_UF)

## c)
mod <- lm(Valor_UF ~ Superficie_Construida_M2 , data = bd)
mod

plot(bd$Superficie_Construida_M2, bd$Valor_UF,
     xlab= 'Superficie construida', ylab = 'Valor UF',
     cex = 1.5, pch = 20)
abline(mod, col = 'red', lwd = 2)

## d)
names(mod)
mod$coefficients
# Interpretacion intercepto beta0
mod$coefficients[1]
# Si la superficie construida fuera cero (lo que podría no ser plausible en un
# contexto práctico), el valor de la propiedad en UF sería aproximadamente
# 1899.62 UF.

# Interpretacion coeficiente estimado beta1
mod$coefficients[2]
# Por cada metro cuadrado adicional construido el valor de la casa en
# unidades de fomento aumenta en 50.79 UF

# Significancia ---------------------------------------------------------------
summary(mod)
alpha <- 0.05
qt(1-alpha/2, df = n-2)
# Se rech H0: beta_1=0. Se tiene evidencia estadística para rechazar H0,
# así, el predictor de la superficie construida en metros cuadrados es
# significativo para el modelo.

# Supuestos ------------------------------------------------------------------
e <- mod$residuals
## Residuos estandarizados
r <- rstandard(mod)
## Residuos studentizados
t <- rstudent(mod)

y_gorro <- mod$fitted.values
# Supuesto 1. Linealidad del predictor
# Supuesto 2. Homocedasticidad
par(mfrow = c(2,2))
plot(bd$Superficie_Construida_M2, r, pch=19, cex=1
     , main = 'Res. estandarizados vs Predictor')
abline(h=0, col = 'red', lty = 2)
plot(y_gorro, r, pch=19, cex=1,
     main = 'Res. estandarizados vs Ajustados')
abline(h=0, col = 'red', lty = 2)

plot(bd$Superficie_Construida_M2, t, pch=19, cex=1,
     main = 'Res. studentizados vs Predictor')
abline(h=0, col = 'red', lty = 2)
plot(y_gorro, t, pch=19, cex=1,
     main = 'Res. studentizados vs Ajustados')
abline(h=0, col = 'red', lty = 2)
dev.off()
# Supuesto 3. Normalidad
par(mfrow= c(1,2))
hist(r)
qqnorm(r, pch=19, cex=0.6)
qqline(r, col = 'red', lty = 1)

# H0: Provienen de una distribución Normal
# H1: No provienen de una distribución Normal
lillie.test(e) # Rech H0
# No se rech H0: no se tiene evidencia estadística para decir que lo errores
# no provienen de una distribución normal

# Supuesto 4. Identicamente distribuidos
# Supuesto 5. Independencia
alpha <- 0.05
t_est <- qt(1-alpha/2,n-2) # n grande se asemeja más al Z
Z_est <- qnorm(1-alpha/2)

par(mfrow=c(1,2))
plot(r, pch=19, cex=1, ylim = c(-2.5,2.5),
     main = 'Residuos estandarizados')
abline(h=0, col = 'red', lty = 2)
abline(h=Z_est, col = 'blue', lty =3)
abline(h=-Z_est, col = 'blue', lty =3)

plot(t, pch=19, cex=1, ylim = c(-2.5,2.5),
     main = 'Residuos studentizados')
abline(h=0, col = 'red', lty = 2)
abline(h=t_est, col = 'blue', lty =3)
abline(h=-t_est, col = 'blue', lty =3)
dev.off()


lmtest::dwtest(mod)
# Se Rech H0: hay correlación, como el p-valor es menor a 0.05, (5% de significancia),
# así, existe evidencia estadística para decir que los datos
# tienen algún grado de correlación.

# Predicción ------------------------------------------------------------------
x0 <- 200
predict(mod,data.frame(Superficie_Construida_M2=x0), interval = "prediction")

pred <- predict(mod, interval = 'prediction')
head(pred)


# Intervalos de confianza para la predicción ----------------------------------
plot(bd$Superficie_Construida_M2, bd$Valor_UF, pch=20, cex = 1.5)
abline(mod, col= 'red')
lines(sort(bd$Superficie_Construida_M2),
      pred[,2][order(bd$Superficie_Construida_M2)], lty = 2, col= 'red')
lines(sort(bd$Superficie_Construida_M2),
      pred[,3][order(bd$Superficie_Construida_M2)], lty = 2, col= 'red')


# Modelo de regresión lineal múltiple -----------------------------------------
mod1 <- lm(Valor_UF ~ Superficie_Construida_M2+Comuna , data = bd)
mod1
summary(mod1)
# Interpretacion intercepto beta0
mod1$coefficients[1]
# Si la superficie construida y la comuna fuera cero (lo que podría no ser plausible en un
# contexto práctico), el valor de la propiedad en UF sería aproximadamente
# 1560.024 UF para la comuna de referencia que en este caso es Macul.

# Interpretacion coeficiente estimado beta1
mod1$coefficients[2]
# Por cada metro cuadrado adicional construido el valor de la casa en
# unidades de fomento aumenta en 37.61 UF manteniendo constante
# a la comuna.

# Interpretacion coeficiente estimado beta2
mod1$coefficients[3]
# una propiedad en Ñuñoa tiene un precio estimado 4144.88 UF más alto
# que en la comuna de Macul, manteniendo constante la superficie construida.

# Ambas covariables son significativas para el modelo con el 5%.
