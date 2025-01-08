
1+1
choose(10,2)


factorial()
choose(n,k) 

# Distribucion Exponencial
# Funcion de densidad: dexp(x, rate = lambda)
dexp(0, rate = 1) 
dexp(0.5, rate = 1) # f(0.5|lamba = 1)
# Confirmar:
lambda = 1
lambda*exp(-lambda*0.5)

dexp(10, rate = 1) # 0.000045

## Funcion de Distribucion Acumulada: pexp()
pexp(0, rate = 1) # = 0 porque no hay masa acumulada!
pexp(1, rate = 1) # Pr(X < 1) = 0.63
pexp(2, rate = 1) # Pr(X < 2) = 0.86
pexp(5, rate = 1) # Pr(X < 1) = 0.99
pexp(100, rate = 1) # Pr(X < 100) = 0.999999
pexp(100000, rate = 1) # Pr(X < 100000) = 0.99999999999


pexp(5, rate = 0.01) # Pr(X < 5|lambda = 0.01) = 0.048

### Distribucion Poisson
# Funcion de probabilidad puntual pX(x)
dpois(0, lambda = 1) # Pr(Y = 0) = exp(-lambda)
exp(-1)

dpois(1, lambda = 1) # Pr(Y = 1) = 0.367
dpois(2, lambda = 1) # Pr(Y = 2) = 0.183
dpois(3, lambda = 1) # Pr(Y = 3) = 0.061

# Distribucion acumulada
ppois(3, lambda = 1) # Pr(Y <= 3) = 0.981
dpois(0, lambda = 1) + dpois(1, lambda = 1) + 
  dpois(2, lambda = 1) + dpois(3, lambda = 1) # 0.981


## Simulacion de valores entre 2 y 5.
set.seed(1113) ## Semilla de simulacion (permite que todos obtenamos los mismos numeros aleatorios)
U = runif(20000, 2, 5)
hist(U, freq = FALSE, col = "gray", border = "white",
     las = 1, nclass = 5, main = "", xlab = expression(Theta[U]))



