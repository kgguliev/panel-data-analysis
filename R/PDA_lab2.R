### Panel data: Analysis and Applications for the Social Sciences, 2022 ###
### Lab 2 ###

library(haven)
library(psych)
library(dplyr)
library(ggplot2)
library(GGally)
library(car)
library(memisc)
library(lmtest)
library(sandwich)
library(olsrr)
library(broom)

# open PDA_lab2.dta
lab2 <- read_dta("./data/PDA_lab2.dta")
head(lab2)

# Description 
# country
# gove - government effectiveness, 2007 (the Worldwide Governance Indicators)
# va - voice and accountability, 2007 (the Worldwide Governance Indicators)
# ps - stability and absence of violence, 2007 (the Worldwide Governance Indicators)
# rq - regulatory quality, 2007 (the Worldwide Governance Indicators)
# rl - rule of law, 2007 (the Worldwide Governance Indicators)
# cc - control of corruption, 2007 (the Worldwide Governance Indicators)
# gdp2 - GDP per capita, 2007
# lngdp2 - natural logarithm of GDP per capita, 2007

hist(lab2$gdp2) # skewed distribution
hist(lab2$lngdp2) 

## prepare your data
lab2 <- dplyr::select(lab2, -gdp2)
lab2 <- na.omit(lab2)

describe(lab2)

## Run a pairwise linear regression
m1 <- lm(lngdp2 ~ cc, data = lab2)
summary(m1) 

## Let us illustrate perfect multicollinearity
lab2$cc100 = 100*lab2$cc
m1_1 <- lm(lngdp2 ~ cc + cc100, data = lab2)
summary(m1_1) 

# Identify the values of Pearson's correlation coefficients
# Draw a more detailed graph
lab2
ggpairs(lab2[,c(1:7)])

# Run a multiple regression model
m2 <- lm(lngdp2 ~ cc + gove + va + rl + rq + ps, data = lab2)
summary(m2)
# Interpret the coefficient estimates and their significance. 
# If there are changes as compared to Model 1, please comment on them. What has happened to the effect of control of corruption?

## Calculate VIF 
vif <- vif(m2)
vif
## VIF values higher than 10 indicate severe multicollinearity problems

## Run a more parsimonious model. For example, on the basis of your theoretical framework,
## you can hypothesize that control of corruption and voice and accountability contribute to economic indicators
m3 <- lm(lngdp2 ~ cc + va, data = lab2)
summary(m3)

# Plot the relationship between predictors and Y 
ggplot(lab2, aes(va, lngdp2)) + geom_point()
ggplot(lab2, aes(cc, lngdp2)) + geom_point() 
ggplot(lab2, aes(m3$fitted.values, lngdp2)) + geom_point()

# Plot the relationship between squared residuals and predictor variables
ggplot(lab2, aes(va, m3$residuals^2)) + geom_point()
ggplot(lab2, aes(cc, m3$residuals^2)) + geom_point()
ggplot(lab2, aes(m3$fitted.values, m3$residuals^2)) + geom_point()

# Adjusted standard errors (heteroskedasticity-consistent)
coeftest(m3, vcov=vcovHC(m3)) 

# Cook's distance measure
# cut-off value: > 4/(N-k)
ols_plot_cooksd_bar(m3)
influence_data <- augment(m3) %>% mutate(index = 1:n())
influential <- influence_data %>% filter(.cooksd > 4/(dim(lab2)[1] - m3$rank))
influential$index
m3_upd <- update(m3, subset = c(-influential$index))
mtable(m3, m3_upd)