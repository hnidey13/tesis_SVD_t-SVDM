rm(list=ls())

#Tensor A en 2x2x2
A <- array(c(1,0,2,-1,
             -1,1,1,1),
           c(2,2,2))
A

#Matriz M 2x2
M <- matrix(c(3,1,2,1),2,2)
M

#Desdoble de forma-3 de A
A3 <- cbind(A[1,1,],A[1,2,],A[2,1,],A[2,2,])

#Producto de forma-3
M_x_A3 <- M %*% A3

#Doblez de forma-3
A_hat <- array(dim = c(2,2,2))
A_hat[1,1,] <- M_x_A3[,1]
A_hat[1,2,] <- M_x_A3[,2]
A_hat[2,1,] <- M_x_A3[,3]
A_hat[2,2,] <- M_x_A3[,4]
A_hat

#Reshape (no es necesario porque A ya es de dim 3)
A_hat

#SVD
svd1 <- svd(A_hat[,,1])
svd2 <- svd(A_hat[,,2])

#Reshape a tensores de orden p=3
U_hat <- array(cbind(svd1$u,svd2$u),c(2,2,2))
S_hat <- array(cbind(diag(svd1$d),diag(svd2$d)),c(2,2,2))
V_hat <- array(cbind(svd1$v,svd2$v),c(2,2,2))

#Regresamos el tensor U_hat al dominio original
M_x_U3 <- cbind(U_hat[1,1,],U_hat[1,2,],U_hat[2,1,],U_hat[2,2,])
U3 <- solve(M) %*% M_x_U3
U <- array(dim = c(2,2,2))
U[1,1,] <- U3[,1]
U[1,2,] <- U3[,2]
U[2,1,] <- U3[,3]
U[2,2,] <- U3[,4]
U

#Regresamos el tensor S_hat al dominio original
M_x_S3 <- cbind(S_hat[1,1,],S_hat[1,2,],S_hat[2,1,],S_hat[2,2,])
S3 <- solve(M) %*% M_x_S3
S <- array(dim = c(2,2,2))
S[1,1,] <- S3[,1]
S[1,2,] <- S3[,2]
S[2,1,] <- S3[,3]
S[2,2,] <- S3[,4]
S

#Regresamos el tensor V_hat al dominio original
M_x_V3 <- cbind(V_hat[1,1,],V_hat[1,2,],V_hat[2,1,],V_hat[2,2,])
V3 <- solve(M) %*% M_x_V3
V <- array(dim = c(2,2,2))
V[1,1,] <- V3[,1]
V[1,2,] <- V3[,2]
V[2,1,] <- V3[,3]
V[2,2,] <- V3[,4]
V

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#Calculamos el producto U(*M)S(*M)V^T
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#Primero calculamos V^T
V_hat
V_hat_T <- array(cbind(t(V_hat[,,1]),t(V_hat[,,2])), c(2,2,2)) #En este caso coinciden
V_T <- V

#Ahora calculemos U(*M)S
face_prod <- array(
    c(U_hat[,,1] %*% S_hat[,,1],
      U_hat[,,2] %*% S_hat[,,2]), c(2,2,2)
)
face_prod
temp3 <- solve(M) %*% cbind(face_prod[1,1,],face_prod[1,2,],face_prod[2,1,],face_prod[2,2,]) #desdoblando
U_starM_S <- array(dim = c(2,2,2))
U_starM_S[1,1,] <- temp3[,1]
U_starM_S[1,2,] <- temp3[,2]
U_starM_S[2,1,] <- temp3[,3]
U_starM_S[2,2,] <- temp3[,4]
U_starM_S

#Ahora calculamos A = U(*M)S(*M)V^T
#C = U(*M)S(*M)
C <- U_starM_S

#Primero calculemos C_hat

#Desdoble de forma-3 de C
C3 <- cbind(C[1,1,],C[1,2,],C[2,1,],C[2,2,])

#Multiplicamos
M_x_C3 <- M %*% C3

#Doblez
C_hat <- array(dim = c(2,2,2))
C_hat[1,1,] <- M_x_C3[,1]
C_hat[1,2,] <- M_x_C3[,2]
C_hat[2,1,] <- M_x_C3[,3]
C_hat[2,2,] <- M_x_C3[,4]
C_hat


face_prod <- array(
    c(C_hat[,,1] %*% V_hat_T[,,1],
      C_hat[,,2] %*% V_hat_T[,,2]), c(2,2,2)
)

face_prod
temp3 <- solve(M) %*% cbind(face_prod[1,1,],face_prod[1,2,],face_prod[2,1,],face_prod[2,2,]) #desdoblando
A_SVD <- array(dim = c(2,2,2))
A_SVD[1,1,] <- temp3[,1]
A_SVD[1,2,] <- temp3[,2]
A_SVD[2,1,] <- temp3[,3]
A_SVD[2,2,] <- temp3[,4]
A_SVD
A
