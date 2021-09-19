# Graphics can be saved in .pdf, .png, .jpg, .wmf, and .ps

# Standard types of graphics:
# * Bar chart
# * Pie chart
# * Histogram
# * Kernel density plot
# * Line chart
# * Box plot
# * Heat map
# * Word cloud
  
# Bar charts: CATEGORICAL
# barplot(x)

## BAR CHARTS
counts <- table(mtcars$gear)
barplot(counts)

# horizontal bar chart
barplot(counts, horiz=TRUE)

# add labels, colors, titles, etc
counts <- table(mtcars$gear)
barplot(counts, 
        main="Simple Bar Plot", 
        xlab="Improvement", 
        ylab="Frequency", 
        legend=rownames(counts), 
        col=c("red", "yellow", "green"))

# Create a stacked bar plot if the input is a matrix
counts <- table(mtcars$vs, mtcars$gear)
# beside argument FALSE by default
barplot(counts,
        main="Car Distribution by Gears and VS",
        xlab="Number of Gears",
        col=c("grey", "cornflowerblue"),
        legend=rownames(counts))

barplot(counts,
        main="Car Distribution by Gears and VS",
        xlab="Number of Gears",
        col=c("grey", "cornflowerblue"),
        legend=rownames(counts),
        beside=TRUE)

## PIE CHARTS
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, 
    labels=lbls, 
    main="Simple Pie Chart")

# Can add percentages
slices <- c(10, 12, 4, 16, 8)
pct <- round(slices/sum(slices)*100)
lbls2 <- paste(c("US", "UK", "Australia", "Germany", "France"), 
              " ", pct, "%", sep="")
pie(slices, 
    labels=lbls2,
    col=rainbow(5),
    main="Pie Chart with Percentages")

# 3-Dimensional pie chart
# install.packages("plotrix")
library(plotrix)
slices <- c(10, 12, 4, 16, 8)
pct <- round(slices/sum(slices)*100)
lbls3 <- paste(
  c("US", "UK", "Australia", "Germany", "France"), 
  " ", pct, "%", sep="")
pie3D(slices, labels=lbls3, explode=0.05, main="3D Pie Chart")

## HISTOGRAMS
# hist(x)
mtcars$mpg #miles per gallon data
hist(mtcars$mpg)

# Colored Histogram w/Different Number of Bins
hist(mtcars$mpg, breaks=8, col="darkgreen")

## KERNEL DENSITY PLOTS
# Displays distribution of continuous var more efficiently than histograms
# plot(density(x))
density_data <- density(mtcars$mpg)
plot(density_data)

# Fill in density plot w/color
density_data <- density(mtcars$mpg)
plot(density_data, main="Kernel Density of Miles Per Gallon")
polygon(density_data, col="skyblue", border="black")

## LINE CHARTS
# Represent a series of data points connected by a straight line
# Typically data that changes over time
# lines(x, y, type=)
weight <- c(2.5, 2.8, 3.2, 4.8, 5.1, 5.9, 6.8, 7.1, 7.8, 8.1)
months <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
plot(months, weight, type="b", main="Baby Weight Chart")

## BOX PLOTS
# Display the distribution of data based on 5-number summary
# Minimum; 1st Quartile; Median; 3rd Quartile; Maximum
# boxplot(data)
vec <- c(3, 2, 5, 6, 4, 8, 1, 2, 3, 2, 4)
summary(vec)
boxplot(vec)
boxplot(vec, varwidth=TRUE)
# can also use horizontal=TRUE
boxplot(vec, horizontal=TRUE)

## HEATMAPS
# 2-D representations of data w/values represented by colors. 
# Simple vs Elaborate Heat Maps
# heatmap(data, Rowv=NA, Colv=NA)
data <- read.csv("HEATMAP.csv", header=TRUE) #Note: NO CSV PROVIDED
# convert Data Frame into matrix
data <- data.matrix(data[,-1])
heatmap(data, Rowv=NA, Colv=NA, col=heat.colors(256), scale="column")

## WORD CLOUDS
# aka Tag Clouds
# install.packages("wordcloud")
library("wordcloud")
data <- read.csv("TEXT.csv", header=TRUE) #Note: AGAIN NO CSV
head(data)
wordcloud(words=data$word,
          freq=data$freq, 
          min.freq=2,
          max.words=100,
          random.order=FALSE)

## FILE FORMATS for SAVING
# Many formats to choose from
# jpeg("filename.jpg")
# pdf("filname.pdf")
# png("filename.png")
# win.metafile("filename.wmf") #Windows metafile
# bmp("filename.bmp")
# postscript("filename.ps") #PostScript file

jpeg("Week3/myplot.jpg")
counts <- table(mtcars$gear)
barplot(counts)
dev.off() #use when done with plotting commands

getwd()
# Save the same graphic output as a file
dev.copy(jpeg, filename="Week3/myplot2.jpg");
counts <- table(mtcars$gear)
barplot(counts)
dev.off()

# Can also click 'Plots' at top, then 'Explore' and 'Save as Image'
# Export as PDF: 'Graphics' >> 'Export' >> 'Save as PDF'



