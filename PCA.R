#Limpiamos ?rea de trabajo
rm(list=ls())

#Definimos una normal multivariada de dimensi?n p
library(MASS)
#set.seed(1)
p <- 5
(mu <- rnorm(p)) #Vector de medias
(Sigma <- rWishart(1,p,diag(p))[,,1]) #Cov(X)
(corMat <- cov2cor(Sigma)) #Cor(X)

#Simulamos n observaciones
n <- 100
A <- mvrnorm(n = n, mu, Sigma)
Ac <- scale(A, center = TRUE, scale = FALSE) #Matriz con datos centrados
#Ac <- A

#Estimaciones de mean(X'), cov(X') y cor(X')
colMeans(A)
cov(A)
cor(A)

#Estimaciones de mean(X), cov(X) y cor(X)
colMeans(Ac)
(Sigma_X_est <- cov(Ac))
t(Ac)%*%Ac/(n-1)
cor(Ac)

#PCA sobre Ac (probabilista)
L <- eigen(Sigma)$vectors
L

#PCA sobre Ac (estad?stico)
(L_est <- prcomp(Ac)$rotation) #Usa SVD

princomp(Ac)$loadings #Usa eigen (no funciona si p>n)
round(eigen(cov(Ac))$vectors,3) #Equivalentemente

#PROPIEDADES
#1. Var[Y_i] = lambda_i
#2. Cov(Y_i,Y_j)=0

#Probabilista
(Sigma_Y <- round(t(L) %*% Sigma %*% L,4))
round(eigen(Sigma)$values,4)

#Estimaci?n
Y_est <- Ac %*% L_est
#princomp(Ac)$scores #Equivalentemente
(Sigma_Y_est <- round(cov(Y_est),4))

#3. sum(Var[X])=sum(Var[Y])=sum(lambda)

#Probabilista
sum(diag(Sigma))
sum(diag(Sigma_Y))

#Estad?stico
sum(diag(Sigma_X_est))
sum(diag(Sigma_Y_est))

#4. \E[\norm{X-L(L^TX)}^2]=\sum_{i=p+1}^d\lambda_i

#Estad?stico
d <- 2
error_fun <- function(X,L) sum((X - L %*% t(L) %*% X)^2)
mean(sapply(1:nrow(Ac), function(i) error_fun(Ac[i,],L[,1:d])))
sum(diag(Sigma_Y_est)[(d+1):p])


#


