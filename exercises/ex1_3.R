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

Glass[1:2,-10]

#### try to transform into log
Glass2 <-data.frame(log1p(Glass[,-10]), Type= Glass[,10])
head(data.frame(Glass2))

###plot
splom(Glass2,
      between = list(x = 1.2),
      type=c("p"))
###looks better
apply(Glass2[1:9],2,function(x) {skewness(x)}, rowname=FALSE) #skewnees is lower

####split into test and train

inTrain <- createDataPartition(Glass2$Type, p = .8)[[1]]
glass2Train <- Glass2[ inTrain, ]
glass2Test  <- Glass2[-inTrain, ]


#c) transformations that can improve classification?
set.seed(1)
svnFit <- train(Type ~ . - Ba, 
                data = glassTrainPP,
                method = "svmRadial", 
                metric <- "Accuracy",
                trControl = trainControl(method = "repeatedcv", 
                                         repeats = 5,
                                         classProbs = TRUE))
svnFit

glasspred <- predict(svnFit,  glassTest)
table(glasspred,glassTest$Type)


###with transformed dataset
glass2Train$Type=as.factor(glass2Train$Type)
summary(glass2Train)
colnames(glass2Train)=make.names(colnames(glass2Train))

set.seed(1)
svnFit2 <- train(Type ~ ., 
                data = glass2Train,
                method = "svmRadial",
                preProc = c("BoxCox","center", "scale"),
                metric <- "Accuracy",
                trControl = trainControl(method = "repeatedcv", 
                                         repeats = 5,
                                         classProbs = TRUE,
                                         savePredictions = TRUE))
svnFit2
glasspred2 <- predict(svnFit2,  glass2Test)
table(glasspred2,glass2Test$Type)
plot(svnFit2, scales = list(x = list(log = 2)))


#3.2
data(Soybean)
summary(Soybean)
levels(Soybean$Class)
#a) ferquency distributions for cathegorical pred; are they degenearated?
library(lattice)
histogram( 
  
  as.formula(paste("~",paste(colnames(Soybean),collapse="+"))), 
  
  data= Soybean, 
  scales=list(x=list(relation="free")), 
  breaks=NULL 
) 

install.packages("tabplot")
library(tabplot)
require(ggplot2)
tableplot(Soybean)
levels(Soybean$Class)
summary(Soybean)
Phyto=subset(Soybean, Class=="downy-mildew")
length(which(is.na(Phyto$germ)))/length(Phyto$germ)

#compute proportion of missiing values in column Germ
pr = data.frame(class=levels(Soybean$Class), pr =rep(0, length(levels(Soybean$Class))))
for (col in 1:length(levels(Soybean$Class))){
  Phyto=subset(Soybean, Class==levels(Soybean$Class)[col])
  pr[col,2]=length(which(is.na(Phyto$germ)))/length(Phyto$germ)
  
}

#classes: 2-4-d-injury , cyst-nematode , herbicide-injury,  phytophthora-rot should be removed

#b) are there prediictors that are more likely to be missing, is the pattern related to classes?

#there are classes that have only missing values in most of variables 

#c) handle missing data, estimate prediction or imputation
# if we remove the classes with missing values 
library(dplyr)
noMisisngSoybean <- Soybean[Soybean$Class !=  "2-4-d-injury" & Soybean$Class != "cyst-nematode" & Soybean$Class != "herbicide-injury" & Soybean$Class != "phytophthora-rot",]
summary(noMisisngSoybean )
tableplot(noMisisngSoybean )
pr = data.frame(class=levels(noMisisngSoybean$Class), pr =rep(0, length(levels(noMisisngSoybean$Class))))
for (col in 1:length(levels(noMisisngSoybean$Class))){
  Phyto=subset(noMisisngSoybean, Class==levels(noMisisngSoybean$Class)[col])
  pr[col,2]=length(which(is.na(Phyto$germ)))/length(Phyto$germ)
  
}
pr
library(caret)
preProcValues <- preProcess(noMisisngSoybean[,2:ncol(noMisisngSoybean)] , method = c("bagImpute"))#not possible on factors
#install.packages("mice")
library(mice)


tempData <- mice(noMisisngSoybean,m=5,maxit=50,meth='polyreg',seed=500)
summary(tempData)
completed <-complete(tempData,1)
summary(completed)
tableplot(completed)
#diaporthe-pod-&-stem-blight gave 40% missing values, the can me imputed 
#3.3
library(caret)
data(BloodBrain)
#b) distributions?
#c) strong relationship between predictors? how to reduce correlation? effect on the number of predictors for the modeling?
