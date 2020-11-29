library(readxl)
#import dataset and have training and testing 
all_data <- read_excel("junior/15.780/final/all data.xlsx")
testing <- read_excel("junior/15.780/final/testing.xlsx")
training <- read_excel("junior/15.780/final/training.xlsx")
#search for colinearity
datacor <- all_data[, c(6, 9, 10)]
cor(datacor)

#construct linear regression
linmod <- lm(training$`Units Sales (millions)` ~ training$`Phones Available`)
linmodpredict <- predict(linmod, testing)
summary(linmod)
crazyreg <- lm(all_data$`Units Sales (millions)`~ all_data$`Phones Available` + all_data$`total colorways` + all_data$`camera options`)
summary(crazyreg)

#plot residuals vs fitted
plot(linmod, 1)

#accuracy measures
library(DMwR)
actual <- testing$`Units Sales (millions)`
regr.eval(actual, linmodpredict)

#construct time series
library("TTR")
library("grDevices")
library("forecast")

train_ts <- ts(training$`Units Sales (millions)`, frequency=4)
ts_components = decompose(train_ts)
plot(ts_components)

#plot residuals
aapllinmod <- lm(`Units Sales (millions)` ~ `Quarter (#)`, data=training)
plot(training$`Quarter (#)`, aapllinmod$residuals, main="residuals", xlab="date", ylab="residuals")
plot(aapllinmod, 1)

#acf
acf(training$`Units Sales (millions)`)

#compare models
plot(training$`Quarter (#)`, training$`Units Sales (millions)`, col="black", main="fit of linear regression model to training data", "xlab"="date", ylab="training data in black, model prediction in red" )
lines(training$`Quarter (#)`, training$`Units Sales (millions)`, col="black")
lines(training$`Quarter (#)`, predict(linmod, training), col="red")

plot(training$`Quarter (#)`, training$`Units Sales (millions)`, col="black", main="fit of linear regression model to training data", "xlab"="date", ylab="training data in black, time series in green" )
lines(training$`Quarter (#)`, training$`Units Sales (millions)`, col="black")
lines(training$`Quarter (#)`, ts_components$seasonal + ts_components$trend, col="green")