####################
###   EYP1113   ###
###    Lab 6   ###
#################
library(gcookbook)
data(heightweight)

n <- nrow(heightweight)
summary(heightweight$heightIn)
# Categorización del peso
heightweight$height_category <- cut(heightweight$heightIn,
                                    breaks = c(-Inf, 59, 65, Inf),
                                    labels = c("Bajo", "Media", "Alto"))
head(heightweight)

### Distribucion marginal
table(heightweight$sex)
table(heightweight$sex)/n
tabla_sex = table(heightweight$sex)/n
table(heightweight$height_category)
table(heightweight$height_category)/n
tabla_heigth = table(heightweight$height_category)/n
#Probabilidad de ser mujer
tabla_sex[1]
#Probabilidad de ser alto
tabla_heigth[3]

### Distribución conjunta
table(heightweight$sex, heightweight$height_category)
table(heightweight$sex, heightweight$height_category)/n
tabla_conjunta = table(heightweight$sex, heightweight$height_category)/n

# Probabilidad de que sea una persona alta y mujer
tabla_conjunta[1,3]

# Distribución conidicional
# Distribución condicional de sexo dado que es de altura baja
tabla_condicional_bajo = tabla_conjunta[,"Bajo"]/tabla_heigth["Bajo"]
tabla_condicional_bajo

### Otra forma de obtener las tablas
#Tabla conjunta (prop.table)
tabla_conjunta = prop.table(table(heightweight$sex, heightweight$height_category))
#Tabla marginal (margin.table)
tabla_sex = margin.table(tabla_conjunta, 1)
tabla_height = margin.table(tabla_conjunta, 2)
#Tabla condicional
tabla_condicional_bajo = tabla_conjunta[,"Bajo"]/tabla_height["Bajo"]


#### Aplicación
library(rio)
ruta =  file.choose()
bd =  import(ruta)
head(bd)
summary(bd)

Y = bd$llamadas_recibidas
XdY = bd$compras_realizadas

joint_table = table(Y, XdY)/nrow(bd) #tabla MUESTRAL

# P(Y=5, XdY = 4)
joint_table["5","4"] #muestral
dbinom(4, 5, p = 0.6)*dpois(5, lambda = 6) #Poblacional

# P(Y = 6)
table(Y)["6"]/nrow(bd) #muestral
dpois(6, lambda = 6) #poblacional

# P(Y = 2 | XdY = 0)
joint_table["2","0"]/(table(XdY)["0"]/nrow(bd))
dbinom(0,2, p = 0.6)*dpois(2, lambda = 6)/dpois(0, 6*0.6) #Denominador obtenido por porbabilidades totales


# Gráfica de la distribución conjunta
#Cuantía conjunta
pxy = function(x,y){
  dbinom(x, size = y, prob = 0.6)*dpois(y, lambda = 6)
}

# El dominio del gráfico
X_values = as.numeric(rownames(joint_table)) # Valores de X
Y_values = as.numeric(colnames(joint_table))  # Valores de Y
z = outer(X_values, Y_values, pxy) #tabla conjunta POBLACIONAL
Z_values = as.numeric(z) #Tabla a vector
# Creamos la matriz de coordenadas para el gráfico
coords = expand.grid(X = X_values, Y = Y_values)
coords$Z = as.vector(Z_values)
head(coords, n = 15)
#install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(coords$X, coords$Y, coords$Z,
              type = "h",  # Tipo de gráfico, con líneas desde el eje XY hasta Z
              lwd = 2,
              pch = '',  # Forma de los puntos
              xlab = "Número de Llamadas (Y)",
              ylab = "Número de Compras (XdY)",
              zlab = "Distribución Conjunta",
              main = "Distribución Conjunta: X (Llamadas) vs Y (Compras)",
              highlight.3d=TRUE,
              angle=45)

### FIN.
