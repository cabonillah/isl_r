library("tidyverse")
library("ggplot2")
library("GGally")
library("MASS")
library("gridExtra")
library("ggfortify")


wd <- box::file()
setwd(wd)
setwd("../")

auto <- read.csv(unzip("Data/ISLR/ALL+CSV+FILES+-+2nd+Edition+-+corrected.zip", 
                          "ALL CSV FILES - 2nd Edition/Auto.csv"), na.strings = '?')

#Had to omit na's
auto <- na.omit(auto)

fit <- lm(mpg~horsepower, data = auto)
summary(fit)

#Predict mpg for 98 HP
predict(fit, data.frame(horsepower=c(98)))
#Confidence interval
predict(fit, data.frame(horsepower=c(98)), interval="confidence")
#Prediction interval
predict(fit, data.frame(horsepower=c(98)), interval="prediction")

#Dataframe of predicted response and predictor
predicted_mpg <- data.frame(pred_mpg = predict(fit, auto),
                            horsepower = auto$horsepower)

#Plot response vs. predictor and then add the regression line
auto %>% ggplot(aes(x=horsepower, y=mpg)) + 
    geom_point(color="blue") + 
    geom_line(data=predicted_mpg, aes(horsepower, pred_mpg), color="red")


#Diagnostic plots for linear model (requires ggfortify)
ggplot2::autoplot(fit, which = 1:6)

#Scatterplot matrix for the dataset (excluding "name" column)
auto[-c(9)] %>% GGally::ggpairs()

#Correlation matrix (excluding "name" column)
cor(auto[-c(9)])

#Regression on all variables except for name
fit2 <- lm(mpg ~ . - name, data = auto)
summary(fit2)

#Diagnostic plots for linear model (requires ggfortify)
ggplot2::autoplot(fit2, which = 1:6)
