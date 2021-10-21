#### Parallel computing examples ####
# Note: things are different between Mac/Linux and Windows computers. Be sure to run the correct
# code blocks for your platform!

#### Mac/Linux example using lapply ####
# Example from Wickham's Advanced R book, Optimization chapter: http://adv-r.had.co.nz/Profiling.html#parallelise

library(parallel)  # parallel package is part of base R, no need to install!

cores <- detectCores()
cores

pause <- function(i) {
  function(x) Sys.sleep(i)
}

system.time(
  lapply(1:8, pause(0.25))
)

system.time(
  mclapply(1:8, pause(0.25))
)

#### Windows example ####
# Example also from Wickham

library(parallel)

cores <- detectCores()
cores
cluster <- makePSOCKcluster(cores)

pause <- function(i) {
  function(x) Sys.sleep(i)
}

system.time(
  lapply(1:10, pause(0.25))
)

system.time(
  parLapply(cluster, 1:10, pause(0.25))
)

#### The foreach package ####
# foreach works independently of platform
# From foreach vignette: https://cran.r-project.org/web/packages/foreach/vignettes/foreach.html
library(foreach)
library(microbenchmark)
library(doParallel)

# install.packages("doParallel")
# First, register your cores. This will depend on how many cores are available on your machine!
registerDoParallel(cores=4)

# The two operators are %do% and %dopar%. Each will return a value out of the foreach loop, 
# which can be assigned to an object. 
x <- foreach(i=1:3) %do% sqrt(i)
x

# %do& will execute the for loop sequentially and %dopar% will execute in parallel.
# This might have better or worse performance, depending on what the calculation is.

# Comparing %do% and %dopar% for 30 reps:
microbenchmark(
  foreach(i=1:30) %do% sqrt(i),
  foreach(i=1:30) %dopar% sqrt(i)
)

# Comparing %do% and %dopar% for 3,000 reps:
microbenchmark(
  foreach(i=1:3000) %do% sqrt(i),
  foreach(i=1:3000) %dopar% sqrt(i)
)

system.time(
  y1<-foreach(i=4:1, .combine='c') %do% {
    Sys.sleep(3 * i)
    i
  }
)

system.time(
  y2<-foreach(i=4:1, .combine='c') %dopar% {
    Sys.sleep(3 * i)
    i
  }
)
