#%%%%%%%%%%%%%%%%%
#%%%% LAB 02 %%%%%
#%%%%%%%%%%%%%%%%%

#install.packages('rio')
library(rio)
library(dplyr)
library(modeest)

#importa data en distintos formatos
ruta = file.choose()
bd <- import(ruta)

head(bd)
tail(bd)

str(bd)
names(bd)

# 1 Manipulación de Data en R ####

# Filtra bases de datos
bd[1 ,]
bd[c(1,3,5) ,]
bd[1:10 ,]
bd[-1,]
bd[-1:-10,]
bd[bd$Artist == 'Maroon 5',]
dplyr::filter(bd, Artist == 'Maroon 5')

#Selecciona variables en una bases de datos
bd[,1]
bd[,c(1,2)]
bd[,'Artist']
bd[,c('Title','Artist')]
dplyr::select(bd, Title)
dplyr::select(bd, Title, Artist)
dplyr::select(bd, Title:Year)

#data frame vertical -> horizontal
bd1 <- select(bd, Artist, Year, topGenre, Popularity) %>%
  filter(Artist == 'Maroon 5')
bd1
tidyr::spread(bd1, key = 'Year', value = 'Popularity')

bd2 <- select(bd, Artist, Year, topGenre, Popularity) %>%
  filter(Artist == 'Michael Jackson')
bd2
tidyr::spread(bd2, key = 'Year', value = 'Popularity')

# aggregate() Resume bases de datos
aggregate(Popularity ~ Year, data = bd2, FUN = sum)

bd2 %>%
  group_by(Artist, Year, topGenre) %>%
  summarise(Popu = sum(Popularity))




#as.factor() (Convierte una variable cuantitativa en cualitativa)
bd$Year
class(bd$Year)
as.factor(bd$Year)
bd$Year <- as.factor(bd$Year)
levels(as.factor(bd$Year))

bd$topGenre
class(bd$topGenre)
as.factor(bd$topGenre)
bd$topGenre <- as.factor(bd$topGenre)
levels(as.factor(bd$topGenre))

#as.numeric() (Convierte una variable cualitativa en cuantitativa)
bd$Year
class(bd$Year)
as.numeric(bd$Year)

# data.frame() (Construye una base de datos)
data.frame(bd2$topGenre, bd2$Artist, bd2$Year)

# as.Date() (Asigna atributo de fecha a un vector de fechas)
x <- '2024-08-18'
class(x)
y <- as.Date(x)
y
class(y)

x <- '2024/08/18'
class(x)
as.Date(x)

x <- '18/08/2024'
class(x)
as.Date(x, format = '%d/%m/%Y')


# merge() (Empalma bases de datos)
compras <- data.frame(
  id_cliente = c(20, 35, 35, 100, 50),
  producto = c('Producto A', 'Producto B', 'Producto C', 'Producto D', 'Producto E')
)
compras

clientes <- data.frame(
  id_cliente = c(20, 35, 100, 50),
  nombre = c('Juan', 'Ana', 'Pedro', 'Marta')
)
clientes

merge(clientes, compras, by = "id_cliente")



# 2 Medidas Descriptivas ####
## 2.1 Medidas Centrales ####

# Media: mean()
bd$Popularity
mean(bd$Popularity)

# Mediana: median()
median(bd$Popularity)

# Moda: mlv() librería  modeest
modeest::mlv(bd$Popularity)
modeest::mlv(bd$Artist)

## 2.2 Medidas Posición ####
#Cuantiles: quantile()
quantile(bd$Popularity)
quantile(bd$Popularity, probs = .25)

# Mínimo: min()
min(bd$Popularity)

# Máximo: max()
max(bd$Popularity)

# 3 Medidas Descriptivas ####
## 3.1 Medidas Dispersión ####
# Varianza: var()
var(bd$Popularity)

# Desviación estándar: sd()
sd(bd$Popularity)

# Coeficiente de variación: sd()/mean()
sd(bd$Popularity)/mean(bd$Popularity)

# Rango: max()-min()
max(bd$Popularity) - min(bd$Popularity)

# Rango intercuartil: IQR()
IQR(bd$Popularity)

## 3.2 Medidas Asociación ####
# Covarinza: cov()
cov(bd$Popularity, bd$Length_duration)

# Correlación: cor()
cor(bd$Popularity, bd$Length_duration)

## 3.3 Asimetría ####
# skewness: skewness()
moments::skewness(bd$Popularity)

## 3.4 Kurtosis ####
# skewness: kurtosis()
moments::kurtosis(bd$Popularity)

## 3.5 Resumen Estadístico ####
# summary: summary()
summary(bd$Popularity)

# describe: describe()
psych::describe(bd)
?describe

