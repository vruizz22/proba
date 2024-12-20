###########################
####      EYP1113L     ####
#### Lab2 - Aplicación ####
###########################

#### Lectura de datos
#install.packages("rio") # si es que no está instalado
library(rio) # Cargamos la librería
ruta = file.choose() # ¿donde está nuestro archivo?
dataset = import(ruta, sheet = "ABALON")
dataset # dificil de interpretar

### Análisis exploratorio
#General
nrow(dataset)
colnames(dataset)
head(dataset) #primeras 6 filas
tail(dataset) #ultimas 6 filas
summary(dataset)
dataset$centro = as.factor(dataset$centro)
levels(dataset$centro)

### Estudiemos las diferentes relaciones que puedan existir
cor(dataset[,-8])
#Era de esperar que las dimensiones de los abalones estén correlacionadas
#Lo mismo ocurre con el peso.
#Lo interesante es que los anillos que son un indicador de la edad
#del abalon, no esté tan correlacionado con las dimensiones y el peso de éste.
#Si bien hay una fuerte correlacion, no es extremadamente fuerte como las anteriores.

### Será que hay centros en que se tienen abalones más grandes que otros?
library(dplyr)
#General
dataset %>%
  summarise(avglargo = mean(largo),
            avgdiametro = mean(diametro),
            avgalto = mean(alto))
#Segun centro
dataset %>%
  group_by(centro) %>%
  summarise(avglargo = mean(largo),
            avgdiametro = mean(diametro),
            avgalto = mean(alto))
#Pareciera ser que en PuertoMontt se encuentran los abalones más pequeños.
#Los más grandes están en Caldera.

### Qué hay del peso?
#General
dataset %>%
  summarise(avgpesot = mean(pesot))
#Segun centro
dataset %>%
  group_by(centro) %>%
  summarise(avgpesot = mean(pesot))
# Los abalones más pesados, un poco de sorpresa, están en Chiloé.
# Los más livianos, esperable, en Puerto Montt.

### Por último, análicemos la edad de los abalones
dataset %>%
  summarise(avganillos = mean(anillos))
#Segun centro
dataset %>%
  group_by(centro) %>%
  summarise(avganillos = mean(anillos))
# Igual que antes, los más jóvenes están en PuertoMontt,
# mientras que los más viejos en Caldera.

### El análisis anterior se basa solo en el promedio.
### La dispersión es importante también para tratar estos casos.
#General
dataset %>%
  summarise(sdlargo = sd(largo),
            sddiametro = sd(diametro),
            sdalto = sd(alto))
#Segun centro
dataset %>%
  group_by(centro) %>%
  summarise(sdlargo = sd(largo),
            sddiametro = sd(diametro),
            sdalto = sd(alto))
# Aquí existen diferencias según la variable.
# Pero a muy grandes rasgos, existe mayor dispersión
# en el tamaño de los abalones de Coquimbo, mientras
# que en Caldera la dispersión es menor.

### Qué hay del peso?
#General
dataset %>%
  summarise(sdpesot = sd(pesot))
#Segun centro
dataset %>%
  group_by(centro) %>%
  summarise(sdpesot = sd(pesot))
# Mayor dispersión en los de Coquimbo, menor en Caldera.

# Este último análisis nos dice que en Coquimbo podríamos encontrar
# abalones bien grandes como bien pequeños, así como algunos muy livianos
# y otros pesados.

### Por último, análicemos la edad de los abalones
dataset %>%
  summarise(sdanillos = sd(anillos))
#Segun centro
dataset %>%
  group_by(centro) %>%
  summarise(sdanillos = sd(anillos))
# Mayor dispersión en Chiloé, menor dispersión en PuertoMontt

# Comparadas las medias, y entendiendo que la dispersión
# si podría jugar un rol funtamental, puede ser de interés
# estudiar los extremos de las variables.
# Será que si solo observamos los abalones grandes (q80 por ejemplo)
# llegamos a conclusiones diferentes?
### Comparación de cuantiles
#General
dataset %>%
  summarise(q80largo = quantile(largo, 0.8),
            q80diametro = quantile(diametro, 0.8),
            q80alto = quantile(diametro, 0.8))
# Según centro
dataset %>%
  group_by(centro) %>%
  summarise(q80largo = quantile(largo, 0.8),
            q80diametro = quantile(diametro, 0.8),
            q80alto = quantile(diametro, 0.8))
# Vamos por variable.
# Según largo, las interpretaciones no cambian a cuando veíamos el promedio.
# Para el diametro y alto, aquellos con mayor diámetro están en Chiloé y Coquimbo.
# Es decir, la interpretación es muy distinta en este caso,
# si comparamos las que se hicieron utiizando la media.

### Veamos el peso
#General
dataset %>%
  summarise(q80pesot = quantile(pesot, 0.8))
# Según centro
dataset %>%
  group_by(centro) %>%
  summarise(q80pesot = quantile(pesot, 0.8))
# Entre los más pesados (cuantil 80), los más pesados se encuentran en Coquimbo,
# y los más livianos en Puerto Montt.
# Nuevamente, notamos algunas diferencias en cuanto a las
# interpretaciones hechas utilizando el promedio.

### Por último, observemos la edad a través de los anillos
#General
dataset %>%
  summarise(q80anillos = quantile(anillos, 0.8))
# Según centro
dataset %>%
  group_by(centro) %>%
  summarise(q80anillos = quantile(anillos, 0.8))
# Si observamos aquellos más viejos (cuantil 80), los más viejos están
# en Chiloé y los más jóvenes en Coquimbo.
# Otra vez, las interpretaciones han cambiado.

### Conclusión

# Si bien observando promedios, notamos que los abalones
# más grandes, pesados y viejos están en Caldera. No obstante,
# hay que tener cuidado en algunos casos. Debido a la dispersión
# presente en Coquimbo y Chiloé, podríamos encontrar abalones
# muy diferentes entre si en estos centros. 

# En este sentido, los centros más estables son Caldera y Puerto Montt.
# Claro que depende de muchos factores la decisión de donde invertir,
# pero a priori, pareciera ser buena opción la inversión en Caldera,
# mientras que en Coquimbo o Chiloé la inversión podría ser riesgosa,
# incluso más que en Puerto Montt, pues la dispersión de estos centros
# podría jugarnos una mala pasada.

### FIN.