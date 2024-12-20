#### Guía de Regresion lineal

### Pregunta 1
#Datos
data(mtcars)
head(mtcars)

# Ajuste del modelo 1
m1 = lm(mpg ~ wt, data = mtcars)
summary(m1)

#Ajuste del modelo 2
m2 = lm(mpg ~ wt + hp, data = mtcars)
summary(m2)

#Ajuste del modelo 3
m3 = lm(mpg ~ wt + hp + cyl, data = mtcars)
summary(m3)

### Pregunta 2
n = 500
#a
x1 = rgamma(n,2,2)
x2 = rnorm(n)
x3 = 0.7*x1+0.3*x2
#b
epsilon = rnorm(n,0,5)
#c
y = 3 + 10*x3 + epsilon
#d
m1 = lm(y~x1)
summary(m1) #Significativo
m2 = lm(y~x2)
summary(m2) #Significativo
m3 = lm(y~x1+x3) 
summary(m3) #x1 pierde significancia

### Pregunta 3
library(rio)
ruta = file.choose()
datos = import(ruta)
head(datos)
m1 = lm(pesot ~ centro, data = datos)
summary(m1)

#peso medio de caldera es 388 aprox
#peso medio de Chiloé es de 388+3.3=391.3 aprox
#peso medio de Coquimbo es de 386 aprox
#peso medio de Puerto Montt es de 353 aprox

#Ningún coeficiente es significativamente distinto de 0, por ende,
# no hay diferencias en los pesos de los centros. Quizás, Puerto Montt es
# el que más se diferencia de Caldera. Sin embargo, los datos no son suficientes
# para determinar si tal diferencia es realmente significativa.

### FIN.