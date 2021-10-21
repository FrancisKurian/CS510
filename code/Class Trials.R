# Class trials
library(lattice)
library(tidyr)
library(janitor)
library(readxl)
library(ggplot2)

#install.packages("lattice")
mtcars[5,5]

ggplot(mtcars,x=mpg, y=hp, fill=cyl)+geom_point()

ggplot(Orange, aes(x = age, y = circumference, color = Tree)) + geom_dotplot() 

ggplot2.scatterplot(data=df, xName='wt',yName='mpg',
                    groupName='cyl', size=3, backgroundColor="white",
                    groupColors=c('#999999','#E69F00', '#56B4E9'),
                    setShapeByGroupName=TRUE) 

# wide to long format 

pivot_longer()
pivot_wider()
Loblolly[]

ggplot(Loblolly.long, aes(x=Seed, y='3'))

ggplot(mtcars, aes(x=mpg,y=hp) )

ggplot(Orange, aes(x = age, y = circumference, color = Tree)) + 
  geom_line()  labs(x = "Age (days)", y = "Circumference (mm)", title = "Orange Tree Circumference Growth by Age") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Orange, aes(x = age, y = circumference, color = Tree)) + 
  geom_line()+ geom_point(pch=3)

# tydy data formats

#Lecture /29
#
#install.packages("testthat")
library(testthat)

# Sample testing script

context("testing sample")
source("../functions.R")

library(testthat)

# First test will fail! 
test_that("integer", {
  expect_equal(addtwo(-1), 
               4)
  expect_equal(addtwo(10), 
               12)
})

test_that("vector", {
  expect_equal(addtwo(c(0, 11)), 
               c(2,13))
  expect_equal(addtwo(c(-3, 2)), 
               c(-1,4))
})

test_that("li
  expect_error(addtwo(list(1)),
               "non-numeric argument to binary operator")
})

test_that("empty vector",{
  expect_equal(addtwo(c()), 
               numeric(0))
})



# 10/04

traceback()









