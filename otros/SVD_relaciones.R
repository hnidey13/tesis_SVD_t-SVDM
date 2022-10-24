#Limpiamos ?rea de trabajo
rm(list=ls())

#-.-.-.
#SVD
#-.-.-.

#Matriz A
A <- matrix(c(3,1,1,
              -1,3,1),2,3,byrow=TRUE)
A <- matrix(c(4,7,-1,8,-5,-2,4,2,-1,3,-3,6),4,3)
n <- nrow(A)
p <- ncol(A)
A

#Rango de a
library(Matrix)
(ranA <- rankMatrix(A))

#Matriz AA^T
A%*%t(A)

#Matriz A^TA
t(A)%*%A

#Eigenvalores de AA^T y A^TA
(lambda2 <- eigen(A%*%t(A))$values)
(lambda2 <- eigen(t(A)%*%A)$values)
lambda <- sqrt(lambda2)

#Eigenvectores de AA^T y A^TA
#(es necesario ajustar signos manualmente)
U <- eigen(A%*%t(A))$vectors
V <- eigen(t(A)%*%A)$vectors
V[,1] <- -V[,1]
V[,2] <- -V[,2]

#Sigue siendo descomposición espectral
t(A)%*%A
V %*% diag(eigen(t(A)%*%A)$values) %*% t(V)

#Formamos matriz Lambda
Lambda <- matrix(0, nrow = n, ncol = p)
diag(Lambda) <- lambda[1:ranA]

#Comprobaci?n
U%*%Lambda%*%t(V)
A

#SVD (reducida)
thinSVD_A <- svd(A)
thinSVD_A$u %*% diag(thinSVD_A$d) %*% t(thinSVD_A$v)

#Comprobaci?n SVD (suma de matrices de rango 1)
MatRan1 <- function(i) (lambda[i] * U[,i] %*% t(V[,i]))
(listMatRan1 <- lapply(1:ranA, MatRan1))
Reduce("+", listMatRan1)

#SVD (completa)
svd_A = svd(A, nu = nrow(A), nv = ncol(A))
svd_A$u
svd_A$d
svd_A$v

#Relaci?n entre u_i y v_i
thinSVD_A$u
ui_fun <- function(i) A %*% svd_A$v[,i] / svd_A$d[i]
sapply(1:ranA, ui_fun)

thinSVD_A$v
vi_fun <- function(i) t(A) %*% svd_A$u[,i] / svd_A$d[i]
sapply(1:ranA, vi_fun)


