###Questions 3###
###Pascal triangle###
Pascal_triangle<-function(k){
  J<-array(0,dim=c(k,2*k-1))
  for(i in 1:k)
    J[i,c(k+1-i,i+k-1)]<-1
  if(k>2)
    for(i in 3:k)
      for(j in seq(k+3-i,k-3+i,2))
        J[i,j]<-J[i-1,j-1]+J[i-1,j+1]
      I<-ceiling(nchar(as.character(max(J)))/2)
      s<-""
      for(i in 1:I)
        s<-paste(s,"","")
      J[which(J==0)]<-s
      for(i in 1:k)
        cat(s,format(J[i,],justify="centre"),"\n")
}
Pascal_triangle(10)