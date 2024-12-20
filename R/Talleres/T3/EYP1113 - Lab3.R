####################
###   EYP1113   ###
###    Lab 3   ###
#################

#### Librerías
#install.packages('rio')
library(rio)
library(dplyr)

### Lectura de datos
ruta = file.choose()
bd = import(ruta)
names(bd)

### Manipulación previa
bd1 = select(bd, -Title,-topGenre)
names(bd1)
bd1$Artist = as.factor(bd1$Artist)

### Medidas descriptivas para la muestra
summary(bd1)
mean(bd1$Popularity);mean(bd1$Length_duration)
quantile(bd1$Popularity);quantile(bd1$Length_duration)

sd(bd1$Popularity);sd(bd1$Length_duration)

cor(bd1$Popularity, bd1$Length_duration)

### Gráficos
#par(mfrow = c(1,2))

#Distribución
hist(bd1$Popularity, breaks = 30)
abline(v = mean(bd1$Popularity), col = "darkgreen", lwd = 2)
hist(bd1$Length_duration, breaks = 30)
abline(v = mean(bd1$Length_duration), col = "darkgreen", lwd = 2)

#Asociación
plot(bd1$Popularity, bd1$Length_duration)
plot(bd1$Popularity, bd1$Length_duration, pch = 20, col = "darkblue")

### Comportamiento de Michael Jackson
bd2 = filter(bd1, Artist == "Michael Jackson")
# Medidas descriptivas para la muestra
summary(bd2)
sd(bd2$Popularity);sd(bd2$Length_duration)
cor(bd2$Popularity, bd2$Length_duration)


### Gráficos
#Distribución
hist(bd1$Popularity, col = rgb(0, 1, 0, alpha = 0.4), freq = F, ylim = c(0,0.05))
hist(bd2$Popularity, col = rgb(0, 0, 1, alpha = 0.4), freq = F, add = T)

hist(bd1$Length_duration, col = rgb(0, 1, 0, alpha = 0.4), freq = F, ylim = c(0,0.01))
hist(bd2$Length_duration, col = rgb(0, 0, 1, alpha = 0.4), freq = F, add = T)

boxplot(list("Total" = bd1$Popularity, "MJ" = bd2$Popularity),
        col = c("lightgreen","skyblue"))

library(vioplot)
vioplot(bd1$Popularity, bd2$Popularity, col = c("lightgreen","skyblue"))

vioplot(bd1$Popularity, 
        at = 1,                # Posición en el eje x
        col = "lightgreen",
        side = "left")         # Violín en el lado izquierdo
vioplot(bd2$Popularity, at = 1, col = "skyblue", side = "right", add = T)


#Asociación
plot(bd1$Popularity, bd1$Length_duration, pch = 19, col = "darkgreen")
lines(bd2$Popularity, bd2$Length_duration, type = "p", pch = 19, col = "darkblue")

#Extra
barplot(table(bd1$Artist))

library(ggplot2)
ggplot(bd1, aes(x=Popularity)) +
  geom_histogram(aes(y=..density..), alpha=1, bins=30,
                 position="identity",fill = "lightgreen",color="black") +
  labs(x='Popularidad', title="Histograma para popularidad")



### FIN.