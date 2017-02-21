################################################################################
### R code from Applied Predictive Modeling (2013) by Kuhn and Johnson.
### Copyright 2013 Kuhn and Johnson
### Web Page: http://www.appliedpredictivemodeling.com
### Contact: Max Kuhn (mxkuhn@gmail.com)
###
### Chapter 2: A Short Tour of the Predictive Modeling Process
###
### Required packages: AppliedPredictiveModeling, earth, caret, lattice
###
### Data used: The FuelEconomy data in the AppliedPredictiveModeling package
###
### Notes: 
### 1) This code is provided without warranty.
###
### 2) This code should help the user reproduce the results in the
### text. There will be differences between this code and what is is
### the computing section. For example, the computing sections show
### how the source functions work (e.g. randomForest() or plsr()),
### which were not directly used when creating the book. Also, there may be 
### syntax differences that occur over time as packages evolve. These files 
### will reflect those changes.
###
### 3) In some cases, the calculations in the book were run in 
### parallel. The sub-processes may reset the random number seed.
### Your results may slightly vary.
###
################################################################################

################################################################################
### Section 2.1 Case Study: Predicting Fuel Economy

library(AppliedPredictiveModeling)
data(FuelEconomy)
dim(cars2010)
dim(cars2011)
dim(cars2012)

head(cars2010)
summary(cars2010)
## Format data for plotting against engine displacement

## Sort by engine displacement
cars2010 <- cars2010[order(cars2010$EngDispl),]
cars2011 <- cars2011[order(cars2011$EngDispl),]

## Combine data into one data frame
cars2010a <- cars2010
cars2010a$Year <- "2010 Model Year"
cars2011a <- cars2011
cars2011a$Year <- "2011 Model Year"

plotData <- rbind(cars2010a, cars2011a)

library(lattice)
xyplot(FE ~ EngDispl|Year, plotData,
       xlab = "Engine Displacement",
       ylab = "Fuel Efficiency (MPG)",
       between = list(x = 1.2),
       type=c("p","r"))


## Fit a single linear model and conduct 10-fold CV to estimate the error
#install.packages("caret")
#install.packages("earth")
library(caret)
set.seed(1)
lm1Fit <- train(FE ~ EngDispl, 
                data = cars2010,
                method = "lm", 
                trControl = trainControl(method= "cv"))
lm1Fit
str(lm1Fit)
plot(lm1Fit$residuals)
summary(lm1Fit)

#plot regression line
xyplot(FE + fitted(lm1Fit) ~ EngDispl, plotData,
       type = c("p", "l"), distribute.type = TRUE)

fit1 = summary(aov(FE ~ EngDispl, data = cars2010))
plot(aov(FE ~ EngDispl, data = cars2010))


## Fit a quadratic model too

## Create squared terms
cars2010$ED2 <- cars2010$EngDispl^2
cars2011$ED2 <- cars2011$EngDispl^2

set.seed(1)
lm2Fit <- train(FE ~ EngDispl + ED2, 
                data = cars2010,
                method = "lm", 
                trControl = trainControl(method= "cv"))
lm2Fit
summary(lm2Fit)

#plot regression line
xyplot(FE + fitted(lm2Fit) ~ EngDispl, plotData,
       type = c("p", "l"), distribute.type = TRUE)

## Finally a MARS model (via the earth package)

library(earth)
set.seed(1)
marsFit <- train(FE ~ EngDispl, 
                 data = cars2010,
                 method = "earth",
                 tuneLength = 15,
                 trControl = trainControl(method= "cv"))
marsFit

plot(marsFit)

#plot regression line
xyplot(FE + fitted(marsFit) ~ EngDispl, plotData,
       type = c("p", "l"), distribute.type = TRUE)

## Predict the test set data
cars2011$lm1  <- predict(lm1Fit,  cars2011)

cars2011$lm2  <- predict(lm2Fit,  cars2011)
cars2011$mars <- predict(marsFit, cars2011)
colnames(cars2011)
head(cars2011)
plot(cars2011$lm1,cars2011$FE,pch=16)
plot(cars2011$lm2,cars2011$FE,pch=16)
plot(cars2011$mars,cars2011$FE,pch=16)

cor.test(cars2011$lm1,cars2011$FE)
cor.test(cars2011$lm2,cars2011$FE)
cor.test(cars2011$mars,cars2011$FE)

## Get test set performance values via caret's postResample function

postResample(pred = cars2011$lm1,  obs = cars2011$FE)
postResample(pred = cars2011$lm2,  obs = cars2011$FE)
postResample(pred = cars2011$mars, obs = cars2011$FE)

################################################################################
### Session Information

sessionInfo()

q("no")


