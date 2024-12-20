#%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%      EYP1113L     #%%%
#%%% Lab3 - Aplicación #%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Lectura de datos
#install.packages("rio") # si es que no está instalado
library(rio) # Cargamos la librería
library(dplyr)

ruta = file.choose() # ¿donde está nuestro archivo?
dataset = import(ruta, sheet = "ABALON")
dataset # dificil de interpretar
names(dataset)

# a) ----
# Histograma del largo de los abalónes
hist(dataset$largo, freq = F)
abline(v = mean(dataset$largo),
       col = 'red')
abline(v = median(dataset$largo),
       col = 'blue')
abline(v = quantile(dataset$largo, prob = .25),
       col = 'green')
abline(v = quantile(dataset$largo, prob = .75),
       col = 'green')

moments::skewness(dataset$largo)
moments::kurtosis(dataset$largo)
summary(dataset$largo)
IQR(dataset$largo)
# El largo de los abalones se presume que tiene una distribución asimétrica
# negativa y además platicúrtica, con un mínimo de 4.40 cm y máximo de 18.60 cm
# pero entre el cuartil 1 y 3 están comprendidos el largo de 11.30 cm hasta 15 cm,
# teniendo un rango intercuartílico de 4.2 cm.

# b) ----
# filtramos la base de datos y creamos nuevos objetos
caldera <- dataset %>%
  filter(centro == 'Caldera')

coquimbo <- dataset %>%
  filter(centro == 'Coquimbo')

hist(caldera$largo,
     col = rgb(0,1,0, alpha = .2), freq = F)

hist(coquimbo$largo,
     col = rgb(0,0,1, alpha = .2), freq = F, add = T)
# Respecto a la asimetría se presume que ambas sean negativas y platicúrticas,
# pero en el centro Coquimbo se tiene más observaciones en abalones de largo menor
# con respecto al centro Caldera.

# c) ----
chiloe <- dataset %>%
  filter(centro == 'Chiloé')

boxplot(list('Caldera' = caldera$largo,
             'Coquimbo' = coquimbo$largo,
             'Chiloe' = chiloe$largo),
        col = c("lightgreen","skyblue",'lightblue'))

summary(caldera$largo);summary(coquimbo$largo);summary(chiloe$largo)
sd(caldera$largo);sd(coquimbo$largo);sd(chiloe$largo)
# Los tres centros tienen medianas muy cercanas respecto al largo de los abalones
# pero si existe una notoria diferencia en sus cuartiles 1 y 3, donde el centro
# Coquimbo tiene más observaciones en un rango más amplio, a diferencia
# de los centros Caldera y Chiloé, lo cual está reflejado en su desviación estándar
# donde claramente Coquimbo tiene mayor variabilidad.

# d) ----
cor(dataset$largo, dataset$pesot)
# El coefiente de correlación es de 0.92, así se tiene un grado de asociación positivo
# y alto, lo cual indica que cuando una variable crece la otra también.
plot(dataset$largo, dataset$pesot)
# Gráficamente se puede comprobar el coeficiente de correlación positivo, ya que
# cuando una variable crece como es el largo también crece el peso.


# e) ----
# Se diferencia por color los largos de los centros
plot(dataset$largo, dataset$pesot, pch = 20)
points(caldera$largo, caldera$pesot,
       col = 'red', pch = 20)
points(coquimbo$largo, coquimbo$pesot,
       col = 'green', pch = 20)
points(chiloe$largo, chiloe$pesot,
       col = 'blue', pch = 20)

### FIN.
