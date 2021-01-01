B1 <- c(1,2,3)
B2 <- c(1,2,4)
B3 <- c(1,3,4)
B4 <- c(2,3,4)

data.frame(B1,B2,B3,B4)

N <- matrix(c(1,1,1,0
              ,1,1,0,1
              ,1,0,1,1
              ,0,1,1,1),nrow=4,byrow=T)
N
rI <- diag(3,4,4);rI
C <- rI -  N %*% t(N)/3
C


