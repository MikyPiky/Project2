######################
## Regression Trees ##

###############
## Libraries ##
library(plyr)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(randomForest)

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

## Data.frame with only SMI Data ##
names(Yield_Covariates_tree_SM)[23:length(names(Yield_Covariates_tree_SM))]
Yield_Covariates_tree_SM_SMI <- Yield_Covariates_tree_SM[23:length(names(Yield_Covariates_tree_SM))]

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

# CART model mit loss
LossTree = rpart(loss ~ . - siloMaize - loss0.2, data = Train, method="class", minbucket=100)

table(Train$loss)

prp(LossTree)

# Make predictions
PredictCART = predict(LossTree, newdata = Test, type = "class")
table(Test$loss, PredictCART)
(41+71)/(41+36+22+71)
(table(Test$loss, PredictCART)[1,1] + table(Test$loss, PredictCART)[2,2]) / sum(table(Test$loss, PredictCART))
# ROC curve
library(ROCR)

PredictROC = predict(LossTree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$loss)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

# CART model mit loss0.2
LossTree = rpart(loss0.2 ~ . - siloMaize - loss, data = Train, method="class", minbucket=100)

table(Train$loss)

prp(LossTree)


########################
## Cross - Validation ##

# Define cross-validation experiment # 
numFolds = trainControl( method = "cv", number = 10)
cpGrid = expand.grid( .cp = seq(0.1,0.5,0.01)) 

# Perform the cross validation with loss #
CV <- train(loss ~. - siloMaize - loss0.2, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
CV
LossTreeCV  =  CV$final
prp(LossTreeCV)

# Make predictions #
LossTreeCV = rpart(loss ~ .  - loss0.2, data = Train, method="class", cp=0.17)
PredictCV = predict(LossTreeCV, newdata = Test, type = "class")
table(Test$loss, PredictCV)
(table(Test$loss, PredictCV) [1,1] + table(Test$loss, PredictCV)[2,2])/sum(table(Test$loss, PredictCV))

# Perform the cross validation with loss0.2 #
CV <- train(loss0.2 ~ . - siloMaize - loss, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
CV
Loss0.2TreeCV  =  CV$final
prp(Loss0.2TreeCV)

# Make predictions #
Loss0.2TreeCV = rpart(loss0.2 ~ .  - loss, data = Train, method="class", cp=0.17)
PredictCV = predict(Loss0.2TreeCV, newdata = Test, type = "class")
table(Test$loss, PredictCV)
(table(Test$loss, PredictCV) [1,1] + table(Test$loss, PredictCV)[2,2])/sum(table(Test$loss, PredictCV))

#######################
## Fit Random Forest ##
# # Convert outcome to factor
# Train$loss = as.factor(Train$loss)
# Test$loss = as.factor(Test$loss)

# Try again
lossForest = randomForest(loss ~. - siloMaize, data = Train, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(lossForest, newdata = Test)
table(Test$loss, PredictForest)
(table(Test$loss, PredictForest)[1,1] + table(Test$loss, PredictForest)[2,2]) / sum(table(Test$loss,PredictForest))

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

####################################################################################
############################
## Only Consider SMI Data ##
############################

# Split the data
set.seed(3000)
spl = sample.split(Yield_Covariates_tree_SM_SMI$loss, SplitRatio = 0.7)
Train_SMI = subset(Yield_Covariates_tree_SM_SMI, spl==TRUE)
Test_SMI = subset(Yield_Covariates_tree_SM_SMI, spl==FALSE)



# Perform the cross validation with loss and only SMI Data.frame #
CV <- train(loss ~ . - loss0.2, data = Train_SMI, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
CV
LossTreeCV_SMI  =  CV$final
prp(LossTreeCV_SMI)

# Make predictions #
LossTreeCV_SMI = rpart(loss ~ .  - loss0.2, data = Train_SMI, method="class", cp=0.17)
prp(LossTreeCV_SMI)

PredictCV = predict(LossTreeCV_SMI, newdata = Test_SMI, type = "class")
table(Test$loss, PredictCV)
(table(Test$loss, PredictCV) [1,1] + table(Test$loss, PredictCV)[2,2])/sum(table(Test$loss, PredictCV))

# Perform the cross validation with loss0.2 and only SMI Data.frame #
CV <- train(loss0.2 ~ . - loss, data = Train_SMI, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
CV
Loss0.2TreeCV_SMI  =  CV$final
prp(Loss0.2TreeCV_SMI)

# Make predictions #
Loss0.2TreeCV_SMI <- rpart(loss0.2 ~ .  - loss, data = Train_SMI, method="class", cp=0.17)

PredictCV = predict(Loss0.2TreeCV_SMI, newdata = Test_SMI, type = "class")
table(Test$loss, PredictCV)
(table(Test$loss, PredictCV) [1,1] + table(Test$loss, PredictCV)[2,2])/sum(table(Test$loss, PredictCV))


#######################
## Fit Random Forest ##
# # Convert outcome to factor
# Train$loss = as.factor(Train$loss)
# Test$loss = as.factor(Test$loss)

# Try again
loss0.2_Forest_SMI = randomForest(loss0.2 ~. - loss, data = Train_SMI, ntree=200, nodesize=25 )

# Make predictions
PredictForest = predict(loss0.2_Forest_SMI, newdata = Test_SMI)
table(Test$loss, PredictForest)
(table(Test$loss, PredictForest)[1,1] + table(Test$loss, PredictForest)[2,2]) / sum(table(Test$loss,PredictForest))


