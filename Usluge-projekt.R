rm(list=ls())

require(DAAG)

data = cars
speed = data$speed
dist = data$dist

#plot(cars, col='blue', pch=20, cex=2, main="Relationship between Speed and Stopping Distance for 50 Cars",
 #    xlab="Speed in mph", ylab="Stopping Distance in feet")

# Analysis
  scatter.smooth(x=speed, y=dist, main="Relationship")
  
  # divide graph area in 2 columns
  par(mfrow=c(1, 2))  
  # box plot for 'speed'
  boxplot(speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(speed)$out)) 
  # box plot for 'distance'
  boxplot(dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(dist)$out))  
  
  library(e1071)
  par(mfrow=c(1, 2))  # divide graph area in 2 columns
  plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
  polygon(density(cars$speed), col="red")
  plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
  polygon(density(cars$dist), col="red")
  
  cor(speed, dist)
  y = lm(formula = dist ~ speed, data=cars)
  
  anova(y)
  summary(y)
  plot(y)
  
#train
  set.seed(100)
  trainingRowIndex = sample(1:nrow(cars), 0.8*nrow(cars))
  trainingdata = cars[trainingRowIndex, ]
  testData = cars[-trainingRowIndex, ]
  
  y = lm(formula = dist~speed, data=trainingdata)
  Predictions = predict(y, testData)
  summary(y)
  anova(y)
  plot(y)
  plot(speed, dist)
  abline(y)
  
#cross-validation
  y = cv.lm(data, form.lm = dist~speed,m = 10, seed=110, dots=F)

