source("src/advect.R")
source("src/diffuse.R")

#### Defines parameters ####
# D <- 1e-8        # Diffusion coefficient in m^2/s
D <- 0        # Diffusion coefficient in m^2/s
delta.t <- 1e-5  # Time step size in s
end.time <- 0.01    # End simulation time in s
start.x <- 5.6
start.y <- -0.97

# Creates points to follow
num.dots <- 10
dotsx <- rep(start.x, num.dots) # replicates start.x 1000 times
dotsy <- rep(start.y, num.dots) # replicates start.y 1000 times
dots.start <- matrix(c(dotsx, dotsy), num.dots, 2) # creates a matrix with num.dots rows and 2 columns filled with x & y

# Reads in position (x,y) and velocity (Ux,Uy) data
x <- as.matrix(read.table("data/x.csv", header = FALSE, sep = ","))
y <- as.matrix(read.table("data/y.csv", header = FALSE, sep = ","))
Ux <- as.matrix(read.table("data/Ux.csv", header = FALSE, sep = ","))
Uy <- as.matrix(read.table("data/Uy.csv", header = FALSE, sep = ","))
Ux[is.na(Ux)] <- 0
Uy[is.na(Uy)] <- 0

t <- 0
dots <- dots.start  # re-assigning the start matrix to dots

#### Simulation #### 
while(t <= end.time){
  dots <- advect(dots, x, y, Ux, Uy, 0.5*delta.t)
  dots <- diffuse(dots, D, delta.t)
  dots <- advect(dots, x, y, Ux, Uy, 0.5*delta.t)
  t <- t + delta.t
  print(paste("t =", t))
}
dots.end <- dots

plot(dots.end[, 1], dots.end[, 2], col = "black", pch = 19)
points(dots.start[, 1], dots.start[, 2], col = "red", pch = 19)

write.csv(dots.start, "./results/dot_start_positions.csv")
write.csv(dots.end, "./results/dot_end_positions.csv")

# test_that("integer", {expect_equal(sum(advect(dots, x, y, Ux, Uy, 0.5*delta.t)**2), 4*D*delta.t)})
# sum(advect(dots, x, y, Ux, Uy, 0.5*delta.t))**2
# 
# D
# delta.t
# 
# 
# 4*D*delta.t

test_that("integer", {expect_equal(sum(advect(dots, x, y, Ux, Uy, 0.5*delta.t)**2), 4*D*delta.t)})

install.packages("testthat")


test_that("running mean stops when it should", {
  
  expect_error( runningmean(0, c(0,0)) )
  
})

D <- 1e-8        # Diffusion coefficient in m^2/s
#D <- 0        # Diffusion coefficient in m^2/s
dots1 <- advect(dots, x, y, Ux, Uy, 0.5*delta.t)
dots2 <- diffuse(dots1, D, delta.t)
dots3 <- advect(dots2, x, y, Ux, Uy, 0.5*delta.t)


