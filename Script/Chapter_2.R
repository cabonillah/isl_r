library("tidyverse")
library("ggplot2")
library("GGally")
library("MASS")
library("gridExtra")


wd <- box::file()
setwd(wd)
setwd("../")
    
college <- read.csv(unzip("Data/ISLR/ALL+CSV+FILES+-+2nd+Edition+-+corrected.zip", 
                          "ALL CSV FILES - 2nd Edition/College.csv"))
rownames(college) <- college[, 1]
college <- college[, -1]
college$Private <- ifelse(college$Private == "Yes", 1, 0)
college$Private <- as.factor(college$Private)
summary(college)
pairs(college[, 1:10])
plot(college$Private, college$Outstate)
college$Elite <- ifelse(college$Top10perc > 50, 1, 0)
college$Elite <- as.factor(college$Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate)
#par(mfrow=c(2,2))
hist(college$Apps, breaks = 20)
hist(college$Expend, breaks = 30)
hist(college$Books, breaks = 20)

auto <- read.csv(unzip("Data/ISLR/ALL+CSV+FILES+-+2nd+Edition+-+corrected.zip", 
                          "ALL CSV FILES - 2nd Edition/Auto.csv"), na.strings = "?")
auto <- na.omit(auto)
str(auto)

sapply(auto[-c(8, 9)], max)-sapply(auto[-c(8, 9)], min)
sapply(auto[-c(8, 9)], mean)
sapply(auto[-c(8, 9)], sd)

sapply(auto[-c(10:86), -c(8, 9)], range)
sapply(auto[-c(10:86), -c(8, 9)], mean)
sapply(auto[-c(10:86), -c(8, 9)], sd)

auto[-c(8, 9)] %>% ggpairs()

dim(Boston)

g1 <- Boston %>% ggplot(aes(x = crim, y = ptratio)) + geom_point()
g2 <- Boston %>% ggplot(aes(x = crim, y = rm)) + geom_point()
g3 <- Boston %>% ggplot(aes(x = crim, y = age)) + geom_point()
g4 <- Boston %>% ggplot(aes(x = crim, y = rad)) + geom_point()
grid.arrange(g1, g2, g3, g4, nrow = 2)

variables <- c("crim", "tax", "ptratio")
for(i in variables) {
    g <- Boston %>% ggplot(aes(x = !!rlang::sym(i))) + geom_histogram()
    print(g)
}

summary_stats <- Boston %>% 
    dplyr::select(crim, tax, ptratio) %>% 
    summarise(across(.fns = list(Mean = mean, SD = sd)))

high_stats <- data.frame()
j <- 1
for(i in seq(1, 5, 2)){
    high_stat <- summary_stats[, i] + 1.5 * summary_stats[, i + 1]
    high_stats[1, j] <-  high_stat
    j <- j + 1
}
colnames(high_stats) <- c("crim_high", "tax_high", "ptratio_high")

Boston[which(Boston$crim > high_stats$crim_high), ]
Boston[which(Boston$tax > high_stats$tax_high), ]
Boston[which(Boston$ptratio > high_stats$ptratio_high), ]

sum(Boston$chas)

Boston[Boston$medv == min(Boston$medv), ]
summary_stats
high_stats

Boston[Boston$rm > 7, ] %>% 
    summarise(across(.cols =  c(crim, tax, ptratio), .fns = list(mean = mean)))

Boston[Boston$rm > 8, ] %>% 
    summarise(across(.cols =  c(crim, tax, ptratio), .fns = list(mean = mean)))
summary_stats
high_stats
