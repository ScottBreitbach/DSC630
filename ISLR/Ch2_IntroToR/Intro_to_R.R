# An Introduction to Statistical Learning
# 2.3 Lab: INtroduction to R

# 2.3.1 Basic Commands
# Create a vector of numbers using c() [concatenate]
x <- c(1,3,2,5)
x
# Can also use '=' to save things
x = c(1,6,2)
x
y = c(1,4,3)
# ?funcname to learn more about a function
# Add two sets of numbers together (must be same lenght)
length(x)
length(y)
x+y
# Look at a list of all objects
ls()
# Remove objects
rm(x,y)
ls()
# Remove ALL objects
rm(list=ls())
ls()
