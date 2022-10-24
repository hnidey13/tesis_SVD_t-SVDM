#Limpiamos área de trabajo
rm(list=ls())

#Para guardar ejemplo (inicio)
#png("Ejemplo_escala_grises.png", units="px", width=1125, height=633, res=300)
#dev.new()
#par(mfrow=c(1,2), mar = c(.2, .2, 0, .2))

#Leemos imagen y graficamos
library(imager)
image <- grayscale(load.image("imagen2.png"))
(dim <- dim(image))
(cutdim <- c(256,256,1,1))
cutimage <- as.cimg(array(image[1:cutdim[1],1:cutdim[2],,],cutdim))
#plot(cutimage, main = "Original")

#Convertimos a matriz
image_mat <- scale(cutimage[,,1,1], center = TRUE, scale = FALSE)
str(image_mat)
#Verificamos que es posible graficar de la matriz
image2 <- array(image_mat,cutdim)
plot(as.cimg(image2), axes = FALSE)

#SVD
library(Matrix)
thinSVD <- svd(image_mat)

#Sumas parciales hasta k
rankMatrix(image_mat)
k <- 10
MatRan1 <- function(i) (thinSVD$d[i] * thinSVD$u[,i] %*% t(thinSVD$v[,i]))
listMatRan1 <- lapply(1:k, MatRan1)
imageRank <- Reduce("+", listMatRan1)
plot(as.cimg(array(imageRank,cutdim)), axes = FALSE)

#Para guardar ejemplo (fin)
#dev.off()

#PCA y SVD (es necesario centrar A)
PCA <- princomp(image_mat, cor = FALSE)

#Vectores propios
loadings(PCA)[,1]
thinSVD$v[,1]

#Valores propios
eigen(t(image_mat)%*%image_mat)$values
thinSVD$d^2







