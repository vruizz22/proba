######################
#### Evaluación 2 ###
#### EYP1113L ######
###################

##################
##### LUNES #####
################
rm(list = ls())
#### Pregunta 1
data(mtcars)
n = nrow(mtcars)
tabla_cyl = table(mtcars$cyl)/n
tabla_cyl
#0.34375
tabla_am = table(mtcars$am)/n
tabla_conjunta_cyl.am = table(mtcars$cyl, mtcars$am)/n
tabla_conjunta_cyl.am[,"1"]/tabla_am["1"]
#0.61538
tabla_vs = table(mtcars$vs)/n
tabla_conjunta_cyl.vs = table(mtcars$cyl, mtcars$vs)/n
tabla_conjunta_cyl.vs[,"1"]/tabla_vs["1"]
#0.71429

#### Pregunta 2
library(MASS)
data(Boston)
head(Boston)
m1 = lm(log(medv) ~ crim + nox + lstat, data = Boston)
summary(m1)
#Verdadero
#Falso
#Verdadero
exp(m1$coef)
#0.99044
#0.97723
#0.96022

#### Pregunta 3
data(InsectSprays)
head(InsectSprays)
count.c = InsectSprays$count[InsectSprays$spray == "C"]
n.c = length(count.c) #pocos datos
lambda.c = mean(count.c) #EMV poisson es el promedio muestral. optim() también es opción
#2.083.... (decimales aproximados)
alpha = 0.05
liminf.c = lambda.c + qt(alpha/2, n.c - 1)*sd(count.c)/sqrt(n.c)
#0.828...
limsup.c = lambda.c + qt(1-alpha/2, n.c - 1)*sd(count.c)/sqrt(n.c)
#3.338...
ppois(2, lambda = lambda.c)
#0.654...
count.d = InsectSprays$count[InsectSprays$spray == "D"]
n.d = length(count.d)
lambda.d = mean(count.d)
liminf.d = lambda.d + qt(0.025, n.c - 1)*sd(count.d)/sqrt(n.d)
limsup.d = lambda.d + qt(0.975, n.c - 1)*sd(count.d)/sqrt(n.d)
#Verdadero

#### Pregunta 4
data(USArrests)
head(USArrests)
hist(USArrests$Murder)
mean(USArrests$Murder)
#7.788
n.M = length(USArrests$Murder) #Suficientes datos. Normal o t-student es prudente
alpha = 0.1
liminf.n = mean(USArrests$Murder) + qnorm(alpha/2)*sd(USArrests$Murder)/sqrt(n.M)
limsup.n = mean(USArrests$Murder) + qnorm(1-alpha/2)*sd(USArrests$Murder)/sqrt(n.M)
liminf.t = mean(USArrests$Murder) + qt(alpha/2,nrow(USArrests)-1)*sd(USArrests$Murder)/sqrt(n.M)
limsup.t = mean(USArrests$Murder) + qt(1-alpha/2,nrow(USArrests)-1)*sd(USArrests$Murder)/sqrt(n.M)
limsup.n - liminf.n
limsup.t - liminf.t
#2.02634 o 2.06538

###################
##### MARTES #####
#################
rm(list = ls())
#### Pregunta 1
data(iris)
cov(iris$Sepal.Length,iris$Sepal.Width)
#-0.04243
mean(iris$Sepal.Length)
#5.84333
mean(iris$Sepal.Width)
#3.057333
var(iris$Sepal.Length)
#0.68569
var(iris$Sepal.Width)
#0.18998
library(mvtnorm)
cov.matrix = matrix(c(var(iris$Sepal.Length),cov(iris$Sepal.Length,iris$Sepal.Width),
                      cov(iris$Sepal.Length,iris$Sepal.Width),var(iris$Sepal.Width)),
                    ncol = 2, nrow = 2)
pmvnorm(lower = c(5,-Inf), upper = c(Inf, 3),
        mean = c(mean(iris$Sepal.Length),mean(iris$Sepal.Width)),
        sigma = cov.matrix)
#0.38958

#### Pregunta 2
library(MASS)
data(Boston)
head(Boston)
m1 = lm(log(medv) ~ crim + lstat + chas, data = Boston)
summary(m1)
#Verdadero
#Verdadero
#Verdadero
m1$coef[4]
#0.17554

#### Pregunta 3
data(InsectSprays)
head(InsectSprays)
count.a = InsectSprays$count[InsectSprays$spray == "A"]
n.a = length(count.a) #Pocos datos. Necesario t-student
lambda.a = mean(count.a) #EMV poisson es el promedio muestral. optim() también es opción
#14.500.... (decimales aproximados)
alpha = 0.01
liminf.a = lambda.a + qt(alpha/2, n.a - 1)*sd(count.a)/sqrt(n.a)
#10.2...
limsup.a = lambda.a + qt(1-alpha/2, n.a - 1)*sd(count.a)/sqrt(n.a)
#18.7... (decimales aproximados)
ppois(10, lambda = lambda.a)
#0.14...

#### Pregunta 4
data(USArrests)
head(USArrests)
Urban = USArrests$UrbanPop[USArrests$Murder>7]
hist(Urban)
mean(Urban)
#67.7692
alpha = 0.1
liminf = mean(Urban) + qt(alpha/2, length(Urban)-1)*sd(Urban)/sqrt(length(Urban))
limsup = mean(Urban) + qt(1-alpha/2, length(Urban)-1)*sd(Urban)/sqrt(length(Urban))
limsup - liminf
#9.55114


######################
##### MIÉRCOLES #####
####################
rm(list = ls())
#### Pregunta 1
data(mtcars)
n = nrow(mtcars)
tabla_vs = table(mtcars$vs)/n
tabla_am = table(mtcars$am)/n
tabla_conjunta_am.vs = table(mtcars$am, mtcars$vs)/n
tabla_conjunta_am.vs["1",]/tabla_am["1"] #dado transmisión manual
#Verdadero
tabla_conjunta_cyl.am.vs = table(mtcars$cyl, mtcars$am, mtcars$vs)/n
tabla_conjunta_cyl.am.vs["4","1","1"]
#Falso
tabla_gear = table(mtcars$gear)/n
table(mtcars$gear[mtcars$carb <= 3])/length(mtcars$gear[mtcars$carb <= 3])
#Verdadero
tabla_gear
#Falso
tabla_conjunta_am.vs[,"1"]/tabla_vs["1"]
#Verdadero

#### Pregunta 2
data(iris)
m1 = lm(Petal.Length ~ Species, data = iris)
summary(m1)
#1.462
#1.46200 + 2.79800 = 4.26
#1.46200 + 4.09000 = 5.552
#Verdadero

#### Pregunta 3
data(USArrests)
n = nrow(USArrests)
rape = USArrests$Rape # rape ~ Gamma(6,beta)
l.gamma = function(beta){
  -(n*6*log(beta) - n*log(gamma(6)) + 5*sum(log(rape)) - beta*sum(rape))
}
EMV.gamma = optim(par = 0.3, fn = l.gamma)$par
EMV.gamma
#0.282... (decimales aproximados)
hist(USArrests$Rape, freq = F)
curve(dgamma(x,6,EMV.gamma),add = T)

# rape ~ Gamma(alpha, EMV.gamma)
l.gamma2 = function(alpha){
  -(n*alpha*log(EMV.gamma) - n*log(gamma(alpha)) + (alpha-1)*sum(log(rape)) - EMV.gamma*sum(rape))
}
EMV.gamma2 = optim(par = 6, fn = l.gamma2)$par
EMV.gamma2
#5.94... (decimales aproximados)
hist(USArrests$Rape, freq = F)
curve(dgamma(x,EMV.gamma2,EMV.gamma),add = T)

1 - pgamma(20, EMV.gamma2, rate = EMV.gamma)
#0.49...(decimales aproximados)



###################
##### JUEVES #####
#################
rm(list = ls())
#### Pregunta 1
data(iris)
bd = iris[iris$Species == "versicolor",]
cor_lw = cor(bd$Petal.Length,bd$Petal.Width)
#0.78667
mul = mean(bd$Petal.Length)
#4.26
muw = mean(bd$Petal.Width)
#1.326
sdl = sd(bd$Petal.Length);sdl^2
#0.22082
sdw = sd(bd$Petal.Width);sdw^2
#0.03911
mu_l.w = mul + cor_lw*(sdl/sdw)*(2 - muw)
sigma_l.w = sqrt(sdl^2*(1 - cor_lw^2))
1 - pnorm(5, mean = mu_l.w, sd = sigma_l.w)
#0.963446
mu_w.l = muw + cor_lw*(sdw/sdl)*(3 - mul)
sigma_w.l = sqrt(sdw^2*(1 - cor_lw^2))
1 - pnorm(1, mean = mu_w.l, sd = sigma_w.l)
#0.22771

#### Pregunta 2
data(USArrests)
n = nrow(USArrests)
rape = USArrests$Rape # rape ~ Gamma(6,beta)
# rape ~ Gamma(alpha, EMV.gamma)
l.gamma = function(alpha){
  -(n*alpha*log(0.3) - n*log(gamma(alpha)) + (alpha-1)*sum(log(rape)) - 0.3*sum(rape))
}
EMV.gamma = optim(par = 6, fn = l.gamma)$par
EMV.gamma
#6.27... (decimales aproximados)
hist(USArrests$Rape, freq = F)
curve(dgamma(x,EMV.gamma,0.3),add = T)

l.gamma2 = function(beta){
  -(n*EMV.gamma*log(beta) - n*log(gamma(EMV.gamma)) + 5*sum(log(rape)) - beta*sum(rape))
}
EMV.gamma2 = optim(par = 0.3, fn = l.gamma2)$par
EMV.gamma2
#0.295... (decimales aproximados)
hist(USArrests$Rape, freq = F)
curve(dgamma(x,EMV.gamma,EMV.gamma2),add = T)

1 - pgamma(30, EMV.gamma, rate = EMV.gamma2)
#0.147...(decimales aproximados)

#### Pregunta 3
library(MASS)
data(Boston)
head(Boston)
summary(Boston$crim)
Boston$crim.cat <- cut(Boston$crim, breaks = c(-Inf, 0.2, 3.5, Inf),
                                    labels = c("Baja", "Media", "Alta"))
m1 = lm(log(medv) ~ crim.cat, data = Boston)
summary(m1)
#Verdadero
#Verdadero
#3.20042 - 0.11504 = 3.08538
#3.20042 - 0.51743 = 2.68299



### FIN.