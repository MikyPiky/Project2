require(caret)

#load some data
data(USArrests)

### Prepare Data (postive observations)
# add a column to be the strata.  In this case it is states, it can be sites, or other locations
# the original data has 50 rows, so this adds a state label to 10 consecutive observations
USArrests$state <- c(rep(c("PA","MD","DE","NY","NJ"), each = 5))
# this replaces the existing rownames (states) with a simple numerical index
rownames(USArrests) <- seq(1:nrow(USArrests))

str(USArrests)

### Prepare data (negative observations)
# my particular problem requires positive observations from the known site locations, but
# a comparison to background locations that are not observed at any pecific site
# I need to simulate data data b/c the USArrests data only had 50 rows
# These data are simulated as random samples from a normal distribution defined by the parameters of existing data
# note: this makes a poor model, but the point here is the CV, not the model
Murder <- rnorm(100, mean(USArrests$Murder), sd(USArrests$Murder))
Assault <- rnorm(100, mean(USArrests$Assault), sd(USArrests$Assault))
UrbanPop <- rnorm(100, mean(USArrests$UrbanPop), sd(USArrests$UrbanPop))
Rape <- rnorm(100, mean(USArrests$Rape), sd(USArrests$Rape))
# the strata label for these is "none", could be "background" or "control" etc..
state <- rep("none", 100)

# Create the modeling data as a combination of positive and negative observations
dat <- rbind(USArrests, data.frame(Murder, Assault, UrbanPop, Rape, state))
str(dat)
## setting up CV folds
# get a list of unique state names to partition the positive observations in a way that
# the model is fit on data observations wholly within some states, and then tests the model on 
# data from different states. Surely, there are sample/study design issues that can be brought up here
# but my particular modeling problem required testing predictions on observations 
# in different groups than those trained on.
folds <- 5
stateSamp <- unique(USArrests$state)
# use caret::createFolds() to split the unique states into folds, returnTrain gives the index of states to train on.
stateCvFoldsIN <- createFolds(stateSamp, k = folds, returnTrain=TRUE)

# this loop can probably be an *apply function, but I am in a hurry and not an apply ninja
# the loop grabs the index numbers of the positive observations that correspond to the states/sites 
# selected for each fold.  the list obsIndexIn contains the index number of the observations
# that are in the selected states for each fold
obsIndexIn <- vector("list", folds) 
for(i in 1:length(stateCvFoldsIN)){
  x <- which(dat$state %in%  stateSamp[stateCvFoldsIN[[i]]])
  obsIndexIn[[i]] <- x
}

# the same as the above is done for the none/background/control samples
# the background observations can be drawn randomly from all background observations whos index is assigned to "noneRows" 
noneRows <- which(dat$state == "none")
noneCvFoldsIN <- createFolds(noneRows, k = folds, returnTrain=TRUE)
noneIndexIn <- vector("list", folds) 
for(i in 1:length(noneCvFoldsIN)){
  y <- noneRows[noneCvFoldsIN[[i]]]
  noneIndexIn[[i]] <- y
}

# Finally, the CV folds index for positive observations is joined with the CV index of negative/background observation
dataIndex <- mapply(c, obsIndexIn, noneIndexIn, SIMPLIFY=FALSE)
# IMPORTANT: the list components need names (e.g. "fold1" ...) b/c Caret expects them to.  Unamed list components will fail.
names(dataIndex) <- sapply(1:5, function(x) paste(c("fold", x), collapse=''))

# Set up caret trainControl to use the CV index specified in dataIndex, method is "CV" for cross-validation, folds is folds...
tr <- trainControl(index = dataIndex, method = "cv", number = folds)
# Fit your model using the train() function and pass the above object "tr" as the trControl parameter
fit <- train(Murder ~ Assault + UrbanPop + Rape, data = dat, trControl = tr, tuneLength = 2, method = "rf")

# Hopefully you have a model by now.
 fit

# Did the aobve method work?  
identical(fit$control$index, dataIndex)

# YES!

# In my example, I wrap this whole thing in another CV routine that simulates repeated stratified k-folds CV
# this is to make sure that the original random split of states/sites did not lead to some quirky segmentation of states/sites
