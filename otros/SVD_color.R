#Limpiamos área de trabajo
rm(list=ls())

#Para guardar ejemplo (inicio)
#png("Ejemplo_F1.png", units="px", width=1125, height=633, res=300)
#dev.new()
#par(mfrow=c(1,3), mar = c(0, .2, 2, 0))

#Leemos imagen y graficamos
library(imager)
image <- load.image("F1.jpg")
str(image)
(dim <- dim(image))
plot(image, axes = FALSE)
title("Original", line = -4, cex.main = .8)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#SVD con matriz de 3 columnas (canales)
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#Convertimos a matriz
image_mat <- apply(image,4,c)
str(image_mat)

#Verificamos que es posible graficar de la matriz
image2 <- array(image_mat,dim)
#plot(as.cimg(image2), axes = FALSE, main = "Reconstruída")

#SVD
library(Matrix)
thinSVD <- svd(image_mat)

#Sumas parciales hasta k
rankMatrix(image_mat)
k <- 1
MatRan1 <- function(i) (thinSVD$d[i] * thinSVD$u[,i] %*% t(thinSVD$v[,i]))
listMatRan1 <- lapply(1:k, MatRan1)
imageRank <- Reduce("+", listMatRan1)
main <- bquote(k==.(k))
plot(as.cimg(array(imageRank,dim)), axes = FALSE)
title(main, line = -4, cex.main = .8)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#SVD 3 veces (por cada canal)
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#Convertimos a matriz
image_mat1 <- image[,,,1]
image_mat2 <- image[,,,2]
image_mat3 <- image[,,,3]
str(image_mat1)

#Verificamos que es posible graficar de las matrices
image2 <- array(c(image_mat1,image_mat2,image_mat3),dim)
#plot(as.cimg(image2), axes = FALSE, main = "Reconstruída")

#SVD
library(Matrix)
thinSVD1 <- svd(image_mat1)
thinSVD2 <- svd(image_mat2)
thinSVD3 <- svd(image_mat3)

#Sumas parciales hasta k1 (image_mat1)
rankMatrix(image_mat1)
k1 <- 10
MatRan1_1 <- function(i) (thinSVD1$d[i] * thinSVD1$u[,i] %*% t(thinSVD1$v[,i]))
listMatRan1_1 <- lapply(1:k1, MatRan1_1)
imageRank_1 <- Reduce("+", listMatRan1_1)

#Sumas parciales hasta k2 (image_mat2)
rankMatrix(image_mat2)
k2 <- 1
MatRan1_2 <- function(i) (thinSVD2$d[i] * thinSVD2$u[,i] %*% t(thinSVD2$v[,i]))
listMatRan1_2 <- lapply(1:k2, MatRan1_2)
imageRank_2 <- Reduce("+", listMatRan1_2)

#Sumas parciales hasta k3 (image_mat3)
rankMatrix(image_mat3)
k3 <- 1
MatRan1_3 <- function(i) (thinSVD3$d[i] * thinSVD3$u[,i] %*% t(thinSVD3$v[,i]))
listMatRan1_3 <- lapply(1:k3, MatRan1_3)
imageRank_3 <- Reduce("+", listMatRan1_3)

#Graficamos
image2 <- array(c(imageRank_1,imageRank_2,imageRank_3),dim)
main <- bquote(paste(k[1]==.(k1),", ",k[2]==.(k2),", ",k[3]==.(k3)))
plot(as.cimg(image2), axes = FALSE)
title(main, line = -4, cex.main = .7)

#Para guardar ejemplo (fin)
#dev.off()

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#SVD una sola matriz cbind
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#Convertimos a matriz
image_mat <- cbind(image[,,,1],image[,,,2],image[,,,3])
str(image_mat)

#Verificamos que es posible graficar de la matriz
image2 <- array(image_mat,dim)
plot(as.cimg(image2), axes = FALSE, main = "Reconstruída")

#SVD
library(Matrix)
thinSVD <- svd(image_mat)

#Sumas parciales hasta k
rankMatrix(image_mat)
k <- 10
MatRan1 <- function(i) (thinSVD$d[i] * thinSVD$u[,i] %*% t(thinSVD$v[,i]))
listMatRan1 <- lapply(1:k, MatRan1)
imageRank <- Reduce("+", listMatRan1)
plot(as.cimg(array(imageRank,dim)), axes = FALSE,
     main = paste0("Rango ",k," (columnas)"))

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#SVD una sola matriz rbind
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#Convertimos a matriz
image_mat <- rbind(image[,,,1],image[,,,2],image[,,,3])
str(image_mat)

#Verificamos que es posible graficar de la matriz
image2 <- aperm(array(t(image_mat),c(dim[2],dim[1],1,3)),c(2,1,3,4))
plot(as.cimg(image2), axes = FALSE, main = "Reconstruída")

#SVD
library(Matrix)
thinSVD <- svd(image_mat)

#Sumas parciales hasta k
rankMatrix(image_mat)
k <- 10
MatRan1 <- function(i) (thinSVD$d[i] * thinSVD$u[,i] %*% t(thinSVD$v[,i]))
listMatRan1 <- lapply(1:k, MatRan1)
imageRank <- Reduce("+", listMatRan1)

#Graficamos
image2 <- aperm(array(t(imageRank),c(dim[2],dim[1],1,3)),c(2,1,3,4))
plot(as.cimg(image2), axes = FALSE,
     main = paste0("Rango ",k," (filas)"))
