#### Optimizing the t-test ####
# Did code walkthrough to understand various optimization methods. There was quite a bit learning in each steps/functions 
# Provided a summary of performance of various methods using microbenchmark() & profvis()
# Getting rid of loop processing and using apply()instead tremendously improved the mean run time
# Opening the source code and adopting the minimum required function was probably a big learning in the optimization step.
# vectorising seem to improve when the number of observations are large. 
# bite code code compilation did not add much value. 
# Optimizing the given code any further was challenging.Learning to fully understand the current level itself was tremendous. 

# Note m= 10,000 . large sample seems to help demonstrate the difference better espcially in vectorized step.

# RESULTS using microbenchmark() 

# expr                                                          min         lq        mean     median         uq       max neval cld
# for (i in 1:m) t.test(X[i, grp == 1], X[i, grp == 2])$stat 1061.2640 1172.31605 1211.418828 1183.69110 1232.45810 1867.6025   100  b
# compT(X, grp)                                                 5.8662    6.68750    6.964727    6.82170    7.01300    9.4971   100  a 
# my_t(X, grp)                                                  3.2833    3.64140    3.764239    3.70705    3.81345    4.8244   100  a 
# rowtstat(X, grp)                                              3.2826    3.49255    3.656270    3.55490    3.62380    4.8731   100  a 
# rowtstat_bc(X, grp)                                           3.0875    3.52670    3.726853    3.58705    3.86915    4.8540   100  a 


library(profvis)
library(microbenchmark)

# From Wickham's Advanced R, 17.9 (page 371)

m <- 10000
n <- 50
X <- matrix(rnorm(m*n, mean = 10, sd = 3), nrow = m)
grp <- rep(1:2, each = n/2)

# Using formula
system.time(for(i in 1:m) t.test(X[i, ] ~ grp)$stat)
# Using arguments
system.time(
  for(i in 1:m) t.test(X[i, grp ==1 ], X[i, grp == 2])$stat
)

# Use apply() to store calculations
compT <- function(x, grp){
  t.test(x[grp == 1], x[grp == 2])$stat
}
system.time(
  t1 <- apply(X, 1, compT, grp = grp)
)

# Optimize the t-test! The book walks you through, but it's good to 
# attempt each step on your own. 


############################## HW ###############################################


# See source code: 
# stats:::t.test.default

#strip out the pieces that does additional calc and keep only t calculation

my_t <- function(x, grp) {
  t_stat <- function(x) {
    m <- mean(x)
    n <- length(x)
    var <- sum((x - m) ^ 2) / (n - 1)
    list(m = m, n = n, var = var)
  }
  g1 <- t_stat(x[grp == 1])
  g2 <- t_stat(x[grp == 2])
  se_total <- sqrt(g1$var / g1$n + g2$var / g2$n)
  (g1$m - g2$m) / se_total
}
system.time(t2 <- apply(X, 1, my_t, grp = grp))

# make it faster by vectorising it.

rowtstat <- function(X, grp){
  t_stat <- function(X) {
    m <- rowMeans(X)
    n <- ncol(X)
    var <- rowSums((X - m) ^ 2) / (n - 1)
    list(m = m, n = n, var = var)
  }
  
  g1 <- t_stat(X[, grp == 1])
  g2 <- t_stat(X[, grp == 2])
  se_total <- sqrt(g1$var / g1$n + g2$var / g2$n)
  (g1$m - g2$m) / se_total
}
system.time(t3 <- rowtstat(X, grp))


# byte code compilation.

rowtstat_bc <- compiler::cmpfun(rowtstat)
microbenchmark(
  rowtstat(X, grp),
  rowtstat_bc(X, grp),
  unit = "ms"
)

# Test all changes in one place

microbenchmark(
  for(i in 1:m) t.test(X[i, grp ==1 ], X[i, grp == 2])$stat,
  compT(X, grp),
  my_t(X, grp),
  rowtstat(X, grp),
  rowtstat_bc(X, grp),
  unit = "ms"
)

profvis({
  # t <- X
  # g <- grp
  for(i in 1:m) t.test(X[i, grp ==1 ], X[i, grp == 2])$stat
  compT(X, grp)
  my_t(X, grp)
  rowtstat(X, grp)
  rowtstat_bc(X, grp)

})



