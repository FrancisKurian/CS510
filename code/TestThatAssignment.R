source("src/advect.R")
source("src/diffuse.R")

#### Defines parameters ####
D <- 1e-8        # Diffusion coefficient in m^2/s
#D <- 0        # Diffusion coefficient in m^2/s
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

# plot(dots.end[, 1], dots.end[, 2], col = "black", pch = 19)
# points(dots.start[, 1], dots.start[, 2], col = "red", pch = 19)
# 
# write.csv(dots.start, "./results/dot_start_positions.csv")
# write.csv(dots.end, "./results/dot_end_positions.csv")
# 


test_that("integer", {expect_equal(sum(advect(dots, x, y, Ux, Uy, 0.5*delta.t)**2), 4*D*delta.t)})

dots <- dots.start  # re-assigning the start matrix to dots
D <- 1e-8         # Diffusion coefficient in m^2/s
#D <- 0        # Diffusion coefficient in m^2/s
dots1 <- advect(dots, x, y, Ux, Uy, 0.5*delta.t)
dots2 <- diffuse(dots1, D, delta.t)
dots3 <- advect(dots2, x, y, Ux, Uy, 0.5*delta.t)

dots4 <- diffuse(dots, D, delta.t)


#######################testthat#####################################################

library("testthat")
# 1. Ensures that points that have no diffusion move together
# diffuse() uses D as a coefficient. So a trivial solution would be to set D=0 and verify the output dot dataset is same as input

#Argument 1: sum(advect(dots, x, y, Ux, Uy, 0.5*delta.t)) # advect with initial dots dataset

#Argument 2: sum(diffuse((advect(dots, x, y, Ux, Uy, 0.5*delta.t)), 0, delta.t)) #advect with diffusion coeficient D=0

#pass test with D=0

test_that("No Diffusion", {expect_equal(sum(advect(dots, x, y, Ux, Uy, 0.5*delta.t)),sum(diffuse((advect(dots, x, y, Ux, Uy, 0.5*delta.t)), 0, delta.t)))})

# failed test with D=1

test_that("No Diffusion", {expect_equal(sum(advect(dots, x, y, Ux, Uy, 0.5*delta.t)),sum(diffuse((advect(dots, x, y, Ux, Uy, 0.5*delta.t)), 1, delta.t)))})

# 2.The square of the mean displacement of molecules diffusing without advection is close to 4*D*delta.t.
## Use diffuse function on initial dots dataset. Do not advect.  The square of the mean displacement of molecules diffusing without advection will be close to 4*D*delta.t

D <- 1e-8
dots <- dots.start 
dots4 <- diffuse(dots, D, delta.t)

# dots4[,2]
# dots4[1,2]
# dots4[,2]-dots4[1,2]
# mean(dots4[,2]-dots4[1,2])
# mean(dots4[,2]-dots4[1,2])**2

(mean(dots4[,2]-dots4[1,2])**2) # expected to match with 4*D*Delta.t

4*D*delta.t

test_that("Mean Square Displacement", {expect_equal(4*D*delta.t,(mean(dots4[,2]-dots4[1,2])**2))})


