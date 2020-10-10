###Question2###
###Matrix multiplication####
##2.1
M1<-matrix(c(0:50),nrow=5,ncol=10)
M1

M2<-matrix(c(0:50),nrow=10,ncol=5)
M2


##2.2
Matrix_multip <-function(M1,M2){
  M3 <- matrix(0, nrow=nrow(M1), ncol = ncol(M2))
  for (i in 1:nrow(M1)){
    for (j in 1:ncol(M2)){
      for (k in 1:ncol(M1)){
        M3[i,j]<-M3[i,j]+M1[i,k]*M2[k,j]
        
      }
    }
  }
  return(M3)
  
}
M3<-Matrix_multip(M1,M2)