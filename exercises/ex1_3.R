#3.1
library(caret)

#install.packages("mlbench")
library(mlbench)
data(Glass)
str(Glass)
summary(Glass)
dim(Glass)
#a) with vizualizations explore pred variables - distributions and relationship between predictors

#graph exploring relations between all variables
transparentTheme(pchSize = .7, trans = .3) 
splom(Glass,
       between = list(x = 1.2),
       type=c("p"))

xyplot(RI ~Na| Type,
       Glass,
       between = list(x = 1.2),
       type=c("p"))

#b) outliers? skewness?
library(e1071)
#compute skeweness for each continous variable 
apply(Glass[1:9],2,function(x) {skewness(x)}) #skewnees in the range 0.4 to 6 in abs units

#split into tain and test
inTrain <- createDataPartition(Glass$Type, p = .8)[[1]]
glassTrain <- Glass[ inTrain, ]
glassTest  <- Glass[-inTrain, ]

dim(glassTrain)
summary(glassTest)

PP <- preProcess(glassTrain, method = "BoxCox")
glassTrainPP <- predict(PP, glassTrain)


PPt <- preProcess(glassTest, method = "BoxCox")
glassTestPP <- predict(PPt, glassTest)
#compare skeweness
apply(glassTrainPP[1:9],2,function(x) {skewness(x)}) #skewnees in the range 0.4 to 6 in abs units

#c) transformations that can improve classification?
set.seed(1)
rdaFit <- train(Type ~ ., 
                data = glassTrainPP,
                method = "rda", 
                trControl = trainControl(method= "cv"))
rdaFit

glasspred <- predict(rdaFit,  glassTestPP)
table(glasspred,glassTestPP$Type)

#3.2
data(Soybean)

#a) ferquency distributions for cathegorical pred; are they degenearated?
#b) are there prediictors that are more likely to be missing, is the pattern related to classes?
#c) handle missing data, estimate prediction or imputation

#3.3
library(caret)
data(BloodBrain)
#b) distributions?
#c) strong relationship between predictors? how to reduce correlation? effect on the number of predictors for the modeling?
