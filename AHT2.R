AHT2 <- function(AT){
  
  AT[,1] <- as.character(AT[,1])
  ATS <- as.data.frame(numeric(nrow(AT)))
  
  for(i in 1:nrow(AT)){
    for(n in 1:100){
      if(substring(AT[i,],n,n)[1] == "{")
        l <- n+1
      if(substring(AT[i,],n,n)[1] == "}"){
        ATS[i,1] <- substring(AT[i,],l,n-1)[1]
        m <- n+1
        break
      }
    }
    for(n in m:100){
      if(substring(AT[i,],n,n)[1] == "{")
        r <- n+1
      if(substring(AT[i,],n,n)[1] == "}"){
        ATS[i,2] <- substring(AT[i,],r,n-1)[1]
        s <- n+1
        break
      }
    }
  }
  
  ATS[,3] <- AT[,2]
  ATS[,4] <- AT[,3]
  ATS[,5] <- AT[,4]
  
  colnames(ATS) <- c("lhs","rhs","supp","conf","lift")
  
  return(ATS)
}
