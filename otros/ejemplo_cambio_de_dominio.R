rm(list=ls())

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#Obtener A_hat a partir de A
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.

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

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#Proceso inverso (obtener A a partir de A_hat)
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

A_hat

#Desdoble de forma 3
M_x_A3 <- cbind(A_hat[1,1,],A_hat[1,2,],A_hat[2,1,],A_hat[2,2,])

#Multiplicación matricial inversa
A3 <- solve(M) %*% M_x_A3

#Doblez de forma 3 de A3
A <- array(dim = c(2,2,2))
A[1,1,] <- A3[,1]
A[1,2,] <- A3[,2]
A[2,1,] <- A3[,3]
A[2,2,] <- A3[,4]
A



