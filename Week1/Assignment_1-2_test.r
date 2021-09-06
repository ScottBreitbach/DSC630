install.packages('knitr')
library('knitr')

## LOAD DATA
# install.packages('readxl')
library('readxl')
gasPrices <- read_excel('Week1/SeriesReport_20210829184422_d810b7.xlsx', 
                        range='A10:M56')
head(gasPrices)

# gasPrices$Year <- as.character(gasPrices$Year)

## CREATE DF
gasPricesDF <- as.data.frame(gasPrices)
head(gasPricesDF)
rownames(gasPricesDF) <- gasPricesDF$Year
head(gasPricesDF)
gasPricesDF$Year <- NULL

## SUMMARY STATS
summary(gasPrices)
summary(gasPricesDF)

## PLOTTING
# install.packages('ggplot2')
library('ggplot2')

hist(gasPrices$Jan)

x <- gasPrices$Jan
h <- hist(x, xlab="$USD per Gallon", 
          main="January Prices Histogram w/Normal Curve",
          xlim=c(0,5), ylim=c(0,20))
xfit <- seq(min(x), max(x), length=40)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit)

plot(Jun ~ Jul, data=gasPrices)

# Boxplots
boxplot(gasPrices$Jan)
boxplot(gasPrices)
boxplot(gasPricesDF)
boxplot(gasPricesDF, ylab="$USD per Gallon", xlab="Month", 
        main="Gas Prices by Month (1976-2021)")

# Plot kernel density plots
d <- density(gasPrices$Jan)
plot(d, xlim=c(-1, 6), ylim=c(0, .7), 
     main="Kernel Density of January Gas Prices")

d <- density(gasPrices$Jul)
plot(d, xlim=c(-1, 6), ylim=c(0, .7),
     main="Kernel Density of July Gas Prices")

names(gasPricesDF)

mean(gasPricesDF$Jan)

ggplot() + geom_line(gasPricesDF)
ggplot(gasPricesDF, fun.y='mean')

library('dplyr')
library('tidyr')
df <- gasPricesDF
df.prices <- df %>% select(Jan:Dec) %>% gather(month, price)
# df.prices

df.avg <- df.prices %>% group_by(month) %>% summarize(average=mean(price, na.rm=TRUE))
df.avg
# df.avg %>% arrange(factor(month, levels=names(df), desc(average)))
# df.avg <- df.avg %>% arrange(factor(month, levels=names(df), desc(average)))
df.avg$month <- factor(df.avg$month, levels=names(df))

names(df)
ggplot() + geom_line(data=df.avg, aes(x=month, y=average, group=NA))
# ggplot(df.avg, aes(month, average)) + geom_line(group=NA, stat="identity")


## SAVE FILE
write.csv(gasPricesDF, "gasPrices.csv", row.names=TRUE)


## EXPLORE BIVARIATE RELATIONSHIPS
# NOTE: quantile plots are not bivariate
qqnorm(gasPricesDF$Jan)
qqline(gasPricesDF$Jan)

# Correlation
cor(gasPricesDF$Jan, gasPricesDF$Jul)
cor(gasPricesDF$Jun, gasPricesDF$Jul)

lm(gasPricesDF$Jan ~ gasPricesDF$Jul)

abline(lm(gasPricesDF$Jan ~ gasPricesDF$Jul))

# Plots correlation
plot(gasPricesDF$Jan, gasPricesDF$Jul)
abline(.Last.value)

# Line isn't quite right:
# plot(gasPricesDF$Jan, gasPricesDF$Jul) + abline(lm(gasPricesDF$Jan ~ gasPricesDF$Jul))

# boxplot(gasPricesDF$Jan ~ gasPricesDF$Jul)
# table(gasPricesDF$Jan, gasPricesDF$Jul)

ggplot(gasPricesDF, aes(x=Jul, y=Jan)) + geom_point()
ggplot(gasPricesDF, aes(x=Jul, y=Jun)) + geom_point()
# ggplot(gasPricesDF, aes(x=cut(Jun, breaks=5), y=Jan)) + geom_point()
# ggplot(gasPricesDF, aes(x=cut(Jun, breaks=5), y=Jan)) + geom_boxplot() 
ggplot(gasPricesDF, aes(x=Jul, y=Jan)) + geom_point() + geom_smooth()
ggplot(gasPricesDF, aes(x=Jul, y=Jun)) + geom_point() + geom_smooth()

## This one seems good 
ggplot(gasPricesDF, aes(x=Jul, y=Jan)) + geom_point() + 
  geom_smooth(method="lm", formula='y ~ x') + 
  labs(title="Scatter plot")
ggplot(gasPricesDF, aes(x=Jul, y=Jun)) + geom_point() + geom_smooth(method="lm")
# p1 + geom_point() + geom_smooth(method="lm", formula=y ~ poly(x,2))
# p2 + geom_point() + geom_smooth(method="lm", formula=y ~ poly(x,2))
?geom_smooth

p1 <- ggplot(gasPricesDF, aes(x=Jul, y=Jan))
p2 <- ggplot(gasPricesDF, aes(x=Jul, y=Jun))

p1 + geom_density2d()
p2 + geom_density2d()

p1 + geom_density2d() + geom_point()
p2 + geom_density2d() + geom_point()


## Trying to get average by year:
ggplot(gasPrices, aes(x=Year, y=Jan)) + geom_line()
sum(gasPrices$Jan)
d <- rowMeans(gasPricesDF, na.rm=TRUE)
d <- as.data.frame(d, col.names=c("Year", "Price"))
typeof(d)
ggplot(d, aes(x=Year))
length(d)

d <- as.data.frame(rowMeans(gasPricesDF, na.rm=TRUE))
typeof(d)
d
gasPrices
gasPrices$Year
as.character(gasPrices$Year)
typeof(gasPrices)
gasPrices
gP <- gasPrices
gP$Year <- as.character(gP$Year)
gP
rowMeans(gP, na.rm=TRUE)
# Could also try average by month using colMeans()

## Back to BIVARIATE RELATIONS
"test"
df <- gasPricesDF
cor(df, method=c("pearson", "kendall", "spearman"), na.rm=TRUE)
cor(df)
cor(df, use="complete.obs")
# install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
## This is neat
chart.Correlation(df, histogram=TRUE, pch=19)
chart.Correlation(df, histogram=TRUE, pch=19) + mtext("Hi there", side=3, line=3)

?chart.Correlation
# Note: value is R^2; red stars are p-vals (3,2,1 are 0.001, 0.01, 0.1)

# install.packages("gmodels")
library("gmodels")

CrossTable(df$Jan, df$Jul, fisher=TRUE, chisq=TRUE, expected=TRUE, prop.c=FALSE, 
           prop.t=FALSE, prop.chisq=FALSE, sresid=TRUE, format="SPSS")
