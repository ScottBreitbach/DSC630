# An Introduction to Statistical Learning
# 2.3 Lab: INtroduction to R

## 2.3.1 BASIC COMMANDS
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
# Matrix function
?matrix
# Create a matrix
x = matrix(data=c(1,2,3,4),2,2)
x
matrix(c(1,2,3,4),2,2,byrow=TRUE)
# Apply maths
sqrt(x)
x^2
# Generate a vector of random normal variables
x = rnorm(50)
x
y = x+rnorm(50, mean=50, sd=0.1)
y
cor(x,y)
# Set a random number seed
set.seed(1303)
rnorm(50)
# Compute mean, variance, and std dev
set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)
sqrt(var(y)) == sd(y)

## 2.3.2 GRAPHICS
# Plot data using plot() function
?plot
x = rnorm(100)
y = rnorm(100)
plot(x,y)
plot(x,y, xlab="this is the x-axis", ylab="this is the y-axis", 
     main="Plot of X vs Y")
# Save the output of a plot using 'pdf()' or 'jpeg()' functions
pdf("Figure.pdf")
plot(x,y, col="green")
dev.off() # This tells R we're done creating the plot
# Create a sequence of numbers
x = seq(1,10)
x
x = 1:10 # shorthand for integer sequence
x
seq(0,1, length=10) # Ten equally space numbers from 0 to 1
x = seq(-pi,pi, length=50)
x
# 3-D contour plot
y = x
f = outer(x,y, function(x,y)cos(y)/(1+x^2))
f
contour(x,y,f)
contour(x,y,f, nlevels=45, add=T)
fa = (f-t(f))/2
contour(x,y,fa, nlevels=15)
# Create a heatmap using the 'image()' function
image(x,y,fa)
#  Make a 3-dimensional plot using 'persp()'
persp(x,y,fa)
# Control the angles with 'theta' and 'phi' arguments
persp(x,y,fa, theta=30)
persp(x,y,fa, theta=30, phi=20)
persp(x,y,fa, theta=30, phi=70)
persp(x,y,fa, theta=30, phi=40)

## 2.3.3 INDEXING DATA
# Create a matrix of data
A = matrix(1:16,4,4)
A
# Select an element of the matrix by row,column
A[2,3]
# Select elements of multiple rows/columns
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
# A single row or column is treated as a vector
A[1,]
# Use a negative (-) to omit rows/columns
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]
# Get the dimensions of a matrix: row col
dim(A)

## 2.3.4 LOADING DATA
Auto = read.table("ISLR/Ch2_IntroToR/Auto.data")
fix(Auto) # open spreadsheet in a window (must close to continue)
Auto = read.table("ISLR/Ch2_IntroToR/Auto.data", header=TRUE, na.strings="?")
fix(Auto)
# Read CSV
Auto = read.csv("ISLR/Ch2_IntroToR/Auto.csv", header=TRUE, na.strings="?")
fix(Auto)
dim(Auto)
# Look at first four rows
Auto[1:4,]
# Remove rows containing NA
Auto = na.omit(Auto)
dim(Auto)
# Check the variable names
names(Auto)

## 2.3.5 ADDITIONAL GRAPHICAL AND NUMERICAL SUMMARIES
