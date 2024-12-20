#####################
###   EYP1113L  ###
###    Lab9    ###
#################
rm(list = ls())

##### Función de log-verosimilitud
### Distribución Binomial
n = 50
bin = rbinom(1, size = n, prob = 0.3)
l.bin = function(p){
  -dbinom(bin, size = n, prob = p, log = T)
}
grid = seq(0.00001, 1, length = 1000)
plot(grid, -l.bin(grid), type = "l", lwd = 2)
optim(par = 0.5, fn = l.bin)$par
EMV.bin = optim(par = 0.5, fn = l.bin, control = list(reltol = 1e-16))$par
EMV.bin - 0.3

### Distribución Normal(mu,1)
n = 50
norm = rnorm(n, mean = 5, sd = 1)
l.norm = function(mu){
  -(-n/2*log(2*pi) - n/2*(mean(norm^2) - 2*mu*mean(norm) + mu^2))
}
grid = seq(0, 10, length = 1000)
plot(grid, -l.norm(grid), type = "l", lwd = 2)
optim(par = 3, fn = l.norm)$par
EMV.norm = optim(par = 3, fn = l.norm, control = list(reltol = 1e-16))$par
EMV.norm - 5

### Distribucion Gamma(1,beta)
n = 50
gam = rgamma(n, shape = 1, rate = 1/3)
l.gamma = function(beta){
  -(n*log(beta) -beta*sum(gam))
}
grid = seq(0.00001, 3, length = 1000)
plot(grid, -l.gamma(grid), type = "l", lwd = 2)
optim(par = 3, fn = l.gamma)$par
EMV.gamma = optim(par = 3, fn = l.gamma, control = list(reltol = 1e-16))$par
EMV.gamma - 1/3


##### Intervalos de confianza
diam = c(3.236, 3.223, 3.242, 3.244, 3.228, 3.253, 3.253, 3.230)
boxplot(diam)
n = length(diam)
mean(diam)
liminf1 = mean(diam) - qt(0.975, df = n-1)*sqrt(var(diam)/n)
limsup1 = mean(diam) + qt(0.975, df = n-1)*sqrt(var(diam)/n)
liminf1;limsup1

liminf2 = mean(diam) - qnorm(0.975)*sqrt(var(diam)/n)
limsup2 = mean(diam) + qnorm(0.975)*sqrt(var(diam)/n)
liminf2;limsup2

#### Test de hipótesis
#a
n1 = 100
n2 = 80
x1 = rgamma(n1,2,2)
x2 = rgamma(n2,4,3)

#b
hist(x1,freq = F,col = rgb(0,1,0,0.8))
hist(x2,freq = F,col = rgb(0,0,1,0.5), add = T)

#c
t.test(x1, x2, alternative = "two.sided", mu = 0, var.equal = F,
       paired = F, conf.level = 0.95)
t.test(x1, x2, alternative = "two.sided", mu = 0, var.equal = T,
       paired = F, conf.level = 0.95)
curve(dt(x,178), lwd = 2, col = "darkgreen", xlim = c(-3,3))
abline(v = -4.19, col = "blue", lwd = 2)

?var.test
var.test(x1, x2, ratio = 1, alternative = "two.sided", conf.level = 0.95)
curve(df(x,99,79), lwd = 2, col = "darkgreen", xlim = c(0,2))
abline(v = 0.686, col = "blue", lwd = 2)


### FIN.
