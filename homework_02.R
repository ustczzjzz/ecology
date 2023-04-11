# Ecology Homework 2, Author: 周桢婕, Student ID: SA22008309
# Section I: Read data and data pre-Process – preliminary. 
rm(list = ls())
library(ade4)
library(tidyverse)
library(rpart)
library(caret)


# reading env and fish data, and summarizing fish abundance data by sites, and 
# combining env and total fish to a new data frame named as "env_fish". 
data(doubs,package="ade4")
env <- doubs$env
total_fish <- rowSums(doubs$fish)                     
env_fish <- cbind(env,total_fish)                    
env_fish


# visualizing the features of the new env_fish set using scatterplot(). 
featurePlot(x=env_fish[, -12],
            y=env_fish[, 12],
            plot = "scatter",
            type=c("p","smooth"),
            layout=c(3,4))


# Delet the sites with no fishes. 
# Removing all rows where any column contains an outlier. 
env_fish <-subset(env_fish,total_fish!=0)
library(outliers) 
env_fish$sel = TRUE 
for (i in 1:(ncol(env_fish)-2)) {
  tmp_col = colnames(env_fish)[i]
  env_fish[scores(env_fish[, tmp_col], type="z", prob=0.95),'sel'] = FALSE 
}
env_fish <- env_fish %>% subset(sel == TRUE)
env_fish


# identifying near zero-variance, outlies of the env variables. If yes, 
# excluding them for analysis.
nzv <- nearZeroVar(env_fish) 
if(length(nzv)!=0){
  env_fish <- env_fish[, -nzv]
}


# detecting the collinearity among env variables or removing highly correlated 
# features (with an absolute correlation of 0.75 or higher) 
env_fish_Cor <- cor(env_fish[,1:11])
highlyCor <- findCorrelation(env_fish_Cor, cutoff = .75)
env_fish <- env_fish[,-highlyCor]
comboInfo <- findLinearCombos(env_fish)
if(length(comboInfo$remove) != 0 ){
  env_fish <- env_fish[,-comboInfo$remove]
}


# Section II: Building a regression model. This section covers:
# splitting data into training and test sets, and visualizing the features and 
# targets of the training set
set.seed(1)
train_index<-createDataPartition(env_fish$total_fish,p=0.8,list = F)
training<-env_fish[train_index,]
test<-env_fish[-train_index,]
x<-as.matrix(env_fish[ ,1:6])
y<-as.factor(env_fish$total_fish)
featurePlot(x,y,plot="density")


# Creating and evaluating a baseline model between the environmental variables 
# and the total fish abundance with the tree-based algorithm
set.seed(2)
names(getModelInfo())


# Creating and evaluating a baseline model between the environmental variables
# and the total fish abundance with the tree-based algorithm
model1<-rpart(formula = total_fish ~.,
              data = training,
              control=rpart.control(minsplit=2),
              method = "anova")
library(rpart.plot)
rpart.plot(model1)


# Feature selection: depending on what algorithm learn between X and Y, i.e., 
# the variable prove useful in a tree-based algorithm, but may be less helpful 
# in a regression-based model. 
model1.pred <- predict(model1, test)
library(Metrics)
rmse(actual=test$total_fish,
     predicted=model1.pred)
fitControl<-trainControl(
  method = "repeatedcv",
  number = 30,
  repeats = 30)
set.seed(3)
model2<-train(total_fish~.,data=training,
              method="rf",
              trControl=fitControl,
              metric="RMSE",
              verbose=T)


