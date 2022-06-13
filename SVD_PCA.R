#Limpiamos área de trabajo
rm(list=ls())

#Para graficar
dev.new()
par(mfrow = c(5,2), mar=c(1,1,3,1))

#Matriz de datos
A <- matrix(c( 1,1,1,0,0, 3,3,3,0,0, 4,4,4,0,0, 5,5,5,0,0,
               0,2,0,4,4, 0,0,0,5,5, 0,1,0,2,2), 7, 5, byrow=TRUE)
#A <- matrix(c( 1,1,1,0,0, 3,3,3,0,0, 4,4,4,2,0, 5,5,5,1,3,
#               0,2,0,4,4, 0,0,6,5,5, 0,1,0,4,2), 7, 5, byrow=TRUE)
colnames(A) <- c("Serenity","Dune","Alien","Casablanca","Amelie")
A
Ac <- scale(A, center = TRUE, scale = FALSE)
round(colMeans(Ac),4)

#SVD A
SVD_A <- svd(A)
round(SVD_A$d,2)
round(SVD_A$u,2)
round(SVD_A$v,2)

#SVD Ac
SVD_Ac <- svd(Ac)
round(SVD_A$d,2)
round(SVD_Ac$u,2)
round(SVD_Ac$v,2)

#PCA de A
princomp(A)$loadings
eigen(cov(A))$vectors

PC <- princomp(A)$loadings
plot(PC[,1],PC[,2], main = "PC de A")
text(PC[,1],PC[,2], colnames(A))

y <- A %*% PC #Proyección
plot(y[,1],y[,2], main = "Proyección de A")
text(y[,1],y[,2])

#PCA de t(A)
eigen(cov(t(A)))$vectors
PC <- prcomp(t(A))$rotation #No se por que varían
plot(PC[,1],PC[,2], main = "PC de t(A)")
text(PC[,1],PC[,2])

y <- t(A) %*% PC
plot(y[,1],y[,2], main = "Proyección de t(A)")
text(y[,1],y[,2], rownames(y))

#Gráfica de SVD de A (V)
plot(SVD_A$v[,1],SVD_A$v[,2], main = "V de SVD de A")
text(SVD_A$v[,1],SVD_A$v[,2], colnames(A))

#Gráfica de SVD de A (U)
plot(SVD_A$u[,1],SVD_A$u[,2], main = "U de SVD de A")
text(SVD_A$u[,1],SVD_A$u[,2])

#Gráfica de SVD de Ac (V)
plot(SVD_Ac$v[,1],SVD_Ac$v[,2], main = "V de SVD de Ac")
text(SVD_Ac$v[,1],SVD_Ac$v[,2], colnames(A))

#Gráfica de SVD de Ac (U)
plot(SVD_Ac$u[,1],SVD_Ac$u[,2], main = "U de SVD de Ac")
text(SVD_Ac$u[,1],SVD_Ac$u[,2])

#SVD t(A)c
Atc <- scale(t(A), center = TRUE, scale = FALSE)
SVD_Atc <- svd(Atc)
round(SVD_Atc$d,2)
round(SVD_Atc$u,2)
round(SVD_Atc$v,2)

#Gráfica de SVD de t(A)c (V)
plot(SVD_Atc$v[,1],SVD_Atc$v[,2], main = "V de SVD de t(A)c")
text(SVD_Atc$v[,1],SVD_Atc$v[,2])

#Gráfica de SVD de t(A)c (U)
plot(SVD_Atc$u[,1],SVD_Atc$u[,2], main = "U de SVD de t(A)c")
text(SVD_Atc$u[,1],SVD_Atc$u[,2], colnames(A))

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#Descubrimentos sobre U
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#1
PC <- princomp(A)$loadings

Ac %*% PC
Ac %*% SVD_Ac$v
SVD_Ac$u %*% diag(SVD_Ac$d)

#2
(Ac %*% PC)/SVD_Ac$u

#3
(A %*% PC) - (Ac %*% PC)
colMeans(A) %*% PC

#4
scale(SVD_Ac$u)
scale(A %*% PC)
scale(Ac %*% PC)

#5
scale(SVD_Ac$u, center = FALSE, scale = apply(SVD_Ac$u, 2, sd, na.rm = TRUE))
scale(A %*% PC, center = FALSE, scale = apply(A %*% PC, 2, sd, na.rm = TRUE))
scale(Ac %*% PC, center = FALSE, scale = apply(Ac %*% PC, 2, sd, na.rm = TRUE))

#6
scale(SVD_Ac$u, center = TRUE, scale = FALSE)
scale(A %*% PC, center = TRUE, scale = FALSE)
scale(Ac %*% PC, center = TRUE, scale = FALSE)
Ac %*% PC


