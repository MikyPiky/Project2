######################
## Regression Trees ##

###############
## Libraries ##
library(plyr)
library(caTools)
library(rpart)
library(rpart.plot)

##############################
## Read in large Dataframe  ##
Yield_Covariates <- read.csv("./data/data_processed/yieldData_meteo")
Yield_Covariates$X <- NULL
str(Yield_Covariates)


###########################################################
## Define data.frame only considering relevant variables ##
names(Yield_Covariates)
names <- c( 
  "siloMaize", "comId", "year",
  "PET_Apr", "PET_May", "PET_Jun", "PET_Jul", "PET_Aug", "PET_Sep","PET_Oct",
  "Prec_Apr", "Prec_May", "Prec_Jun", "Prec_Jul", "Prec_Aug", "Prec_Sep","Prec_Oct",
  "Tavg_Apr", "Tavg_May", "Tavg_Jun", "Tavg_Jul", "Tavg_Aug", "Tavg_Sep", "Tavg_Oct",
  "SMI_Apr", "SMI_May", "SMI_Jun", "SMI_Jul", "SMI_Aug", "SMI_Sep", "SMI_Oct")

Yield_Covariates_tree_SM <- Yield_Covariates[,names(Yield_Covariates)%in%names]
str(Yield_Covariates_tree_SM)
summary(Yield_Covariates_tree_SM$siloMaize)


# ################
# ## Delete NAs ##
# ################
sum(is.na(Yield_Covariates_tree_SM) )
dim(Yield_Covariates_tree_SM)
Yield_Covariates_tree_SM_nna <- na.omit(Yield_Covariates_tree_SM)
dim(Yield_Covariates_tree_SM_nna)
#
# ## Check for NAs
any(is.na(Yield_Covariates_tree_SM_nna))
# ## Reset Rownames
# rownames(Yield_Covariates_nna) <- NULL
#
# ## Further work with DataFrame without Yield_Covariates index ##
Yield_Covariates_tree_SM <- Yield_Covariates_tree_SM_nna


#####################################################
## Delete all comIds with less than 9 observations ##
table(Yield_Covariates_tree_SM$comId)
sum(table(Yield_Covariates_tree_SM$comId) < 9)
table(Yield_Covariates_tree_SM$comId) < 9 

## comIds mit weniger als 9 Beoachtungen: ##
list <- c(3101, 3102, 3402, 5111 , 5112 , 5113 , 5114, 5117, 5124, 5314,
          5315, 5334,5378,  5512, 5911,   5916,  7131,  7133, 7135, 7233, 
          7331, 7332,7334, 7335, 7337,    7338, 7339,   8111,12052, 14612, 16052 )
length(list)
list[[1]]

## Look at comIds with missing depdent data #
i <- NULL
for (i in 1:length(list)){
  print(Yield_Covariates[Yield_Covariates$comId == list[[i]],1:6]) # 2008 hier scheint nur ein Jahr ein Messfehler zu sein: diesen Lösche ich 
}
' Das Fehlen der Dateneinträge schein meistens systematisch zu sein, da es meisten Blöcke sind, welche fehlen'

## Delete comIds with less than nine values
temp <- Yield_Covariates
for (i in 1:length(list))
{
  print(Yield_Covariates[Yield_Covariates$comId==list[i],])
  temp <- (temp[!temp$comId==list[i],])
}


## Number of deleted rows ##
dim(temp) - dim(Yield_Covariates)
head(temp)

## Further use old name for data.frame
Yield_Covariates <- temp


##########################
## (Time) - Demean Data ##
Yield_Covariates_demean  <- ddply(Yield_Covariates_tree_SM, .(comId),  numcolwise(scale))  
str(Yield_Covariates_demean)

Yield_Covariates_tree_SM <- Yield_Covariates_demean 

#############################################
## Delete Spotial and Temporal Information ##
Yield_Covariates_tree_SM$comId <- NULL
Yield_Covariates_tree_SM$year <- NULL

#############################################
## Generate Inidicator for Corn Yield Loss ##
SM_logtend_quant <- ecdf(Yield_Covariates_tree_SM$siloMaize)
plot(SM_logtend_quant)
summary(SM_logtend_quant)

hist(Yield_Covariates_tree_SM$siloMaize)

SM_logtend_quant <- quantile(Yield_Covariates_tree_SM$siloMaize, probs = seq(0, 1, 0.1), na.rm=T)
SM_logtend_quant

loss  <- ifelse(Yield_Covariates_tree_SM$siloMaize < 0 , "Yes", "No")
loss
table(loss)

loss0.2  <- ifelse(Yield_Covariates_tree_SM$siloMaize < -0.2 , "Yes", "No")
loss0.2
table(loss0.2)

Yield_Covariates_tree_SM$loss <- NULL
Yield_Covariates_tree_SM$loss.1 <- NULL
Yield_Covariates_tree_SM <- data.frame(Yield_Covariates_tree_SM, loss)
Yield_Covariates_tree_SM <- data.frame(Yield_Covariates_tree_SM, loss0.2)
attach(Yield_Covariates_tree_SM)
names(Yield_Covariates_tree_SM)

# ################################################
# #### Fitting Classification Trees nach ITSL ####
# library(tree)
# tree.Yield <- tree(loss~.-siloMaize  , Yield_Covariates_tree_SM)
# summary(tree.Yield)
# par(mfrow=c(1,1))
# plot(tree.Yield)
# text(tree.Yield ,pretty =0)
# 
# tree.Yield
# 
# set.seed(2)
# train=sample (1: nrow(Yield_Covariates_tree_SM), nrow(Yield_Covariates_tree_SM)/2)
# train
# Yield_Covariates_tree_SM.test <- Yield_Covariates_tree_SM[-train ,]
# loss.test=loss[-train]
# 
# tree.Yield =tree(loss~.-siloMaize,Yield_Covariates_tree_SM,subset=train)
# tree.pred=predict(tree.Yield ,Yield_Covariates_tree_SM.test ,type="class")
# table(loss.test)
# table(tree.pred)
# table(tree.pred, loss.test)
# (table(tree.pred, loss.test)[1,1] +table(tree.pred, loss.test)[2,2]) / sum(table(tree.pred, loss.test))
# 
# set.seed(3)
# 
# cv.Yield <- cv.tree(tree.Yield, FUN = prune.misclass)
# names(cv.Yield)
# cv.Yield
# 
# par(mfrow=c(1,2))
# plot(cv.Yield$size ,cv.Yield$dev ,type="b")
# plot(cv.Yield$k ,cv.Yield$dev ,type="b")
# 
# prune.Yield <- prune.misclass(tree.Yield, best = 3)
# par(mfrow=c(1,1))
# plot(prune.Yield)
# text(prune.Yield, pretty=0)
# 
# tree.pred = predict(prune.Yield, Yield_Covariates_tree_SM.test, type="class")
# table(tree.pred, loss.test)
# (table(tree.pred, loss.test)[1,1] + table(tree.pred, loss.test)[2,2]) / sum(table(tree.pred, loss.test))
# ' Achtung, hier Treffe ich vor allem die NO und nicht die YES, aber ich interessiere mich eigentlich für die YES. 
# Die Frage ist nun, wie ich die YES Treffsicherheit erhöhe.'
# 
# summary(Yield_Covariates$Prec_Jul)
# quantile(Yield_Covariates_tree_SM$Prec_Jul, probs = seq(0, 1, 0.1), na.rm=T)
# Prec_Jul_quant <- ecdf(Yield_Covariates_tree_SM$Prec_Jul)
# Prec_Jul_quant(61.3082)
# 
# ' Das heißt, wenn weniger als 26 Prozent des Regens i'

##############################
## Fitting Regression Trees ##
library(MASS)
set.seed(1)
str(Yield_Covariates)

train = sample(1:nrow(Boston), nrow(Boston)/2)

tree.boston = tree(medv~., Boston, subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty=0)

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev ,type="b")

prune.boston <- prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

yhat <- predict(tree.boston , newdata = Boston[-train, ])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat - boston.test)^2)



#########################
## Classification Tree ##
#########################
##################
## Silage Maize ##
##################

# Split the data
set.seed(3000)
spl = sample.split(Yield_Covariates_tree_SM$loss, SplitRatio = 0.7)
Train = subset(Yield_Covariates_tree_SM, spl==TRUE)
Test = subset(Yield_Covariates_tree_SM, spl==FALSE)



names(Yield_Covariates_tree_SM)
# # CART model
# LossTree = rpart(loss ~ . - siloMaize, data = Train, method="class", minbucket=100)
# 
# table(Train$loss)
# 
# prp(LossTree)
# 
# # Make predictions
# PredictCART = predict(LossTree, newdata = Test, type = "class")
# table(Test$loss, PredictCART)
# (41+71)/(41+36+22+71)
# (table(Test$loss, PredictCART)[1,1] + table(Test$loss, PredictCART)[2,2]) / sum(table(Test$loss, PredictCART))
# # ROC curve
# library(ROCR)
# 
# PredictROC = predict(LossTree, newdata = Test)
# PredictROC
# 
# pred = prediction(PredictROC[,2], Test$loss)
# perf = performance(pred, "tpr", "fpr")
# plot(perf)
# 
# as.numeric(performance(pred, "auc")@y.values)
# 
# 
# 
# LossTree2 = rpart(loss ~. - siloMaize, data = Train, method="class", minbucket=5)
# prp(LossTree2)
# 
# LossTree3 = rpart(loss ~. - siloMaize, data = Train, method="class", minbucket=100)
# prp(LossTree3)

# VIDEO 5 - Random Forests
# Install randomForest package
# install.packages("randomForest")
library(randomForest)

# Build random forest model
lossForest = randomForest(loss ~. - siloMaize, data = Train, ntree=200, nodesize=25 )

# Convert outcome to factor
Train$loss = as.factor(Train$loss)
Test$loss = as.factor(Test$loss)

# Try again
lossForest = randomForest(loss ~. - siloMaize, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(lossForest, newdata = Test)
table(Test$loss, PredictForest)
(table(Test$loss, PredictForest)[1,1] + table(Test$loss, PredictForest)[2,2]) / sum(table(Test$loss,PredictForest))
519/(226+519)

set.seed(100)
lossForest = randomForest(loss ~. - siloMaize, data = Train, ntree=200, nodesize=25 )
PredictForest = predict(lossForest, newdata = Test)
table(Test$loss, PredictForest)
(table(Test$loss, PredictForest) [1,1] + table(Test$loss, PredictForest)[2,2])/sum(table(Test$loss, PredictForest))

set.seed(200)
lossForest = randomForest(loss ~. - siloMaize, data = Train, ntree=200, nodesize=25 )
PredictForest = predict(lossForest, newdata = Test)
table(Test$loss, PredictForest)
(table(Test$loss, PredictForest) [1,1] + table(Test$loss, PredictForest)[2,2])/sum(table(Test$loss, PredictForest))

# VIDEO 6

# Install cross-validation packages
# install.packages("caret")
library(caret)
# install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 100 )
cpGrid = expand.grid( .cp = seq(0.001,0.5,0.001)) 

# Perform the cross validation
CV <- train(loss ~. - siloMaize, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
CV
best.tree =  CV$final

prp(best.tree)

# Create a new CART model
LossTreeCV = rpart(loss ~. - siloMaize, data = Train, method="class", cp = 0.007)
prp(LossTreeCV)
# Make predictions
PredictCV = predict(LossTreeCV, newdata = Test, type = "class")
table(Test$loss, PredictCV)
(table(Test$loss, PredictCV) [1,1] + table(Test$loss, PredictCV)[2,2])/sum(table(Test$loss, PredictCV))




##################
## Winter Wheat ##
##################

## Read in large Dataframe  ##
Yield_Covariates <- read.csv("./data/data_processed/yieldData_meteo")
Yield_Covariates$X <- NULL
str(Yield_Covariates)


##################################################
#### Try to fit a CART on the Data ####
##################################################

####################################################
#### Methods form Intro to Statistical Learning ####
####################################################



# Define data.frame only considering relevant variables
names(Yield_Covariates)
names <- c( 
  "winterWheat",
  "PET_Jan", "PET_Feb", "PET_Mar", "PET_Apr", "PET_May", "PET_Jun", "PET_Jul" ,  "PET_Aug", "PET_Aug_lag", "PET_Sep_lag", "PET_Oct_lag", "PET_Nov_lag", "PET_Dec_lag",
  "Prec_Jan",  "Prec_Feb", "Prec_Mar", "Prec_Apr", "Prec_May", "Prec_Jun", "Prec_Jul", "Prec_Aug", "Prec_Aug_lag", "Prec_Sep_lag", "Prec_Oct_lag", "Prec_Nov_lag", "Prec_Dec_lag", 
   "Tavg_Jan", "Tavg_Feb", "Tavg_Mar", "Tavg_Apr", "Tavg_May", "Tavg_Jun", "Tavg_Jul", "Tavg_Aug", "Tavg_Aug_lag", "Tavg_Sep_lag", "Tavg_Oct_lag", "Tavg_Nov_lag" ,"Tavg_Dec_lag",
    "SMI_Jan", "SMI_Feb", "SMI_Mar", "SMI_Apr", "SMI_May", "SMI_Jun", "SMI_Jul", "SMI_Aug", "SMI_Aug_lag", "SMI_Sep_lag", "SMI_Oct_lag", "SMI_Nov_lag", "SMI_Dec_lag")

Yield_Covariates_tree_WW <- Yield_Covariates[,names(Yield_Covariates)%in%names]
str(Yield_Covariates_tree_WW)
summary(Yield_Covariates_tree_WW$winterWheat)


# ################
# ## Delete NAs ##
# ################
sum(is.na(Yield_Covariates_tree_WW) )
dim(Yield_Covariates_tree_WW)
Yield_Covariates_tree_WW_nna <- na.omit(Yield_Covariates_tree_WW)
dim(Yield_Covariates_tree_WW_nna)
#
# ## Check for NAs
any(is.na(Yield_Covariates_tree_WW_nna))
# ## Reset Rownames
# rownames(Yield_Covariates_nna) <- NULL
#
# ## Further work with DataFrame without Yield_Covariates index ##
Yield_Covariates_tree_WW <- Yield_Covariates_tree_WW_nna

## Generate Inidicator for Corn Yield Loss

SM_logtend_quant <- ecdf(Yield_Covariates_tree_WW$winterWheat)
plot(SM_logtend_quant)
summary(SM_logtend_quant)

SM_logtend_quant <- quantile(Yield_Covariates_tree_WW$winterWheat, probs = seq(0, 1, 0.1), na.rm=T)
SM_logtend_quant

loss  <- ifelse(Yield_Covariates_tree_WW$winterWheat < 71.17 , "Yes", "No")
loss
table(loss)

Yield_Covariates_tree_WW$loss <- NULL
Yield_Covariates_tree_WW$loss.1 <- NULL
Yield_Covariates_tree_WW <- data.frame(Yield_Covariates_tree_WW, loss)
attach(Yield_Covariates_tree_WW)
names(Yield_Covariates_tree_WW)


# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(Yield_Covariates_tree_WW$loss, SplitRatio = 0.7)
Train = subset(Yield_Covariates_tree_WW, spl==TRUE)
Test = subset(Yield_Covariates_tree_WW, spl==FALSE)

# Install rpart library
# install.packages("rpart")
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)

names(Yield_Covariates_tree_WW)
# CART model
LossTree = rpart(loss ~ . - winterWheat, data = Train, method="class", minbucket=100)

table(Train$loss)

prp(LossTree)

# Make predictions
PredictCART = predict(LossTree, newdata = Test, type = "class")
table(Test$loss, PredictCART)

(table(Test$loss, PredictCART)[1,1] + table(Test$loss, PredictCART)[2,2]) / sum(table(Test$loss, PredictCART))
# ROC curve
library(ROCR)

PredictROC = predict(LossTree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$loss)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

# VIDEO 5 - Random Forests

LossTree2 = rpart(loss ~. - winterWheat, data = Train, method="class", minbucket=5)
prp(LossTree2)

LossTree3 = rpart(loss ~. - winterWheat, data = Train, method="class", minbucket=100)
prp(LossTree3)

# Install randomForest package
# install.packages("randomForest")
library(randomForest)

# Build random forest model
lossForest = randomForest(loss ~. - winterWheat, data = Train, ntree=200, nodesize=25 )

# Convert outcome to factor
Train$loss = as.factor(Train$loss)
Test$loss = as.factor(Test$loss)

# Try again
lossForest = randomForest(loss ~. - winterWheat, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(lossForest, newdata = Test)
table(Test$loss, PredictForest)
(table(Test$loss, PredictForest)[1,1] + table(Test$loss, PredictForest)[2,2]) / sum(table(Test$loss, PredictForest))


set.seed(100)
lossForest = randomForest(loss ~. - winterWheat, data = Train, ntree=200, nodesize=25 )
PredictForest = predict(lossForest, newdata = Test)
table(Test$loss, PredictForest)
(table(Test$loss, PredictForest)[1,1] + table(Test$loss, PredictForest)[2,2]) / sum(table(Test$loss, PredictForest))

set.seed(200)
lossForest = randomForest(loss ~. - winterWheat, data = Train, ntree=200, nodesize=25 )
PredictForest = predict(lossForest, newdata = Test)
table(Test$loss, PredictForest)
(table(Test$loss, PredictForest) [1,1] + table(Test$loss, PredictForest)[2,2])/sum(table(Test$loss, PredictForest))

## Determine Classification via Cross -Validation ##
# Install cross-validation packages
# install.packages("caret")
library(caret)
# install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.001,0.5,0.001)) 

# Perform the cross validation
CV <- train(loss ~. - winterWheat, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
best.tree =  CV$final

prp(best.tree)

# Create a new CART model
LossTreeCV = rpart(loss ~. - siloMaize, data = Train, method="class", cp = 0.1)

# Make predictions
PredictCV = predict(LossTreeCV, newdata = Test, type = "class")
table(Test$loss, PredictCV)
(table(Test$loss, PredictCV) [1,1] + table(Test$loss, PredictCV)[2,2])/sum(table(Test$loss, PredictCV))

prp(LossTreeCV)


