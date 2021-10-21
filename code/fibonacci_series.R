fibo1 <- function (b1){
  
  fib.a <- 1
  fib.b <- 1
  cat(fib.a,",",fib.b,",",SEP="")
  repeat{
    temp <- fib.a+fib.b
    fib.a <- fib.b
    fib.b <- temp
    cat(fib.b,",",sep="")
    if(fib.b>b1){
      cat("Break Now..")
      break
    }
  }
}

fibo2 <- function(threshold) {
  fibseq <- c(1,1)
  counter <- 2
  repeat {
    fibseq <-c(fibseq, fibseq[counter-1]+fibseq[counter]) 
    counter <- counter+1
    if(fibseq[counter]>threshold){
      break
    }
  }
  return(fibseq)
}
