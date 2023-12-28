library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(ggrepel)
library(plotROC)
library(caret)
library(rpart)
library(randomForest)

#Section 1
#No coding needed

#Section 2

diabetes_dt <- fread("extdata/pima-indians-diabetes.csv") 
diabetes_dt[, Outcome := as.factor(Outcome)]
# Store feature variables that we will need for later
feature_vars <- colnames(diabetes_dt[,-c("Outcome")])
diabetes_dt

full_formula <- as.formula(paste(c("Outcome ~ ",
                                   paste(feature_vars, collapse = " + ")),
                                 collapse = ""))
# We do not use this command full_formula <- Outcome~.
# Since we are adding some columns later and we do not want the formula to be altered
dt_classifier <- rpart(full_formula,
                       data =diabetes_dt,
                       control = rpart.control(minsplit = 3, cp = 0.001))

library(rpart.plot)
rpart.plot(dt_classifier)

# Save predictions
diabetes_dt[, preds_dt := predict(dt_classifier, type="prob")[,2]]

# Plot roc curve for decision tree
ggroc <- ggplot(diabetes_dt, aes(d=as.numeric(Outcome), m=preds_dt)) +
  geom_roc() +
  geom_abline() + theme_bw()
ggroc

## 70% of the data for training
smp_size <- floor(0.70 * nrow(diabetes_dt))
## set the seed to make your partition reproducible
set.seed(13)
train_ind <- sample(seq_len(nrow(diabetes_dt)), size = smp_size)
## train on training dataset
dt_classifier <- rpart(full_formula,
                       data =diabetes_dt[train_ind],
                       control = rpart.control(minsplit = 3, cp = 0))
# get predictions for both train and test set
diabetes_dt[, preds_dt := predict(dt_classifier, type="prob",
                                  # predict on all data, not only on trainset
                                  newdata=diabetes_dt)[,2]]
# For plotting, label train and test datasets
diabetes_dt[train_ind, dataset:="train"] 
diabetes_dt[-train_ind, dataset:="test"]
ggroc <- ggplot(diabetes_dt, aes(d=as.numeric(Outcome), m=preds_dt, color=dataset)) +
  geom_roc() +
  geom_abline() + theme_bw()
ggroc

rf_classifier <- randomForest(## Define formula and data 
  full_formula,
  ## Train only on trainset
  data=diabetes_dt[train_ind],
  ## Hyper parameters
  ntree = 200, # Define number of trees
  nodesize = 20, # Minimum size of leaf nodes
  maxnodes = 7, # Maximum number of leaf nodes
  mtry = 5, # Number of feature variables as candidates for each split
  sampsize=length(train_ind),
)
rf_classifier

diabetes_dt[, preds_rf := predict(rf_classifier, type="prob", ## Predict on all data
                                  newdata=diabetes_dt)[,2]]
# Plot roc curves for each model
ggroc <- ggplot(diabetes_dt, aes(d=as.numeric(Outcome), m=preds_rf, color=dataset)) +
  geom_roc() +
  geom_abline() + theme_bw()
ggroc

calc_auc(ggroc)

logreg <-  glm(full_formula,
               data = diabetes_dt[train_ind], family = "binomial")
diabetes_dt[, preds_logReg := predict(logreg, newdata = diabetes_dt)]
# Melt to produce a preds variable that contains predictions by both the # Random forest and the log reg
melted_dt <- melt(diabetes_dt,
                  measure.vars = c("preds_logReg", "preds_rf"),
                  variable.name = "model_type", value.name = "preds")
# Create a new column to plot one ROC curve for each model type and train/test set melted_dt <- unite(melted_dt, 'model_type', model_type:dataset, sep=", ")
ggroc <- ggplot(melted_dt, aes(d=as.numeric(Outcome), m=preds, color=model_type)) +
  geom_roc() +
  geom_abline() + theme_bw()
ggroc

calc_auc(ggroc)

#Section 3

diabetes_dt[, Outcome_bool:= ifelse(Outcome==1, "yes", "no")] # somehow trainControl does not like factors...
# so we convert it to string
# generate control structure
fitControl <- trainControl(method = "cv",
                           number = 5, # number of folds
                           classProbs=TRUE, # display class probabilities 
                           summaryFunction = twoClassSummary)
                           # run CV
bool_formula <- as.formula(paste(c("Outcome_bool ~ ",
                                   paste(feature_vars, 
                                         collapse = " + ")),
                                 collapse = ""))
logreg_cv <- train(bool_formula,
                   data = diabetes_dt,
                   method = "glm",
                   trControl = fitControl,
                   ## Specify which metric to optimize
                   metric = "ROC")
logreg_cv

metrics_dt <- as.data.table(logreg_cv$resample)
metrics_dt[order(-ROC)]

metrics_dt_melt <- melt(metrics_dt, id.vars = "Resample",  variable.name = "metric")
metrics_dt_melt <- metrics_dt_melt[metric=="Sens" | metric == "Spec"]
ggplot(metrics_dt_melt,aes(x=metric, y = value)) + geom_boxplot() + geom_jitter()

for (k in c(2,20,100)){
  fitControl <- trainControl(method = "cv",
                             number = k, # number of folds
                             classProbs=TRUE, # display class probabilities 
                             summaryFunction = twoClassSummary)
                             logreg_cv <- train(bool_formula,
                                                data = diabetes_dt,
                                                ## model specification
                                                method = "glm", # we want a logistic regression family = "binomial",
                                                ## validation specification
                                                trControl = fitControl,
                                                ## Specify which metric to optimize 
                                                metric = "ROC")
                             print(logreg_cv)
}

# Grid search:
options(warn=-1)
best_auc = 0
for (mtry in c(4, 5, 10)) {
  for (nodesize in c(10, 20, 30)) { 
    for (ntree in c(100, 150, 200)) { 
      rf_classifier <- randomForest(## Define formula and data 
        full_formula,
    ## Train only on trainset
    data=diabetes_dt[train_ind],
    ## Hyper parameters
    ntree = ntree, # Define number of trees
    nodesize = nodesize, # Minimum size of leaf nodes
    maxnodes = 7, # Maximum number of leaf nodes
    mtry = mtry, # Number of feature variables as candidates for each split 
    sampsize=length(train_ind),
    )
    rf_classifier
    diabetes_dt[, preds_rf := predict(rf_classifier, type="prob", newdata=diabetes_dt)[,2]]
    # Plot roc curves for each model
                                                                                       
    ggroc <- ggplot(diabetes_dt[-train_ind], aes(d=as.numeric(Outcome), m=preds_rf)) +
      geom_roc() +
      geom_abline() + theme_bw() 
    current_auc = calc_auc(ggroc)
    if ((current_auc> best_auc)[3]) {
      best_auc <- current_auc
      best_params <- c(ntree,nodesize,mtry)
      best_params <- setNames(best_params, c("ntree", "nodesize", "mtry"))
    } 
    }
  } 
  }
print (best_auc)

best_params <- setNames(best_params, c("ntree", "nodesize", "mtry"))
print (best_params)
                         
# Random search
options(warn=-1)
best_auc <- 0
num_tries <- 10 
best_params <- c(-1,-1,-1) 
for (try in 1:num_tries){
  
  mtry <- sample(3:10,1)
  ntree <- sample(100:200,1)
  nodesize <- sample(10:30,1)
  
  rf_classifier <- randomForest(## Define formula and data 
    full_formula,
    ## Train only on trainset
    data=diabetes_dt[train_ind],
    ## Hyper parameters
    ntree = ntree, # Define number of trees
    nodesize = nodesize, # Minimum size of leaf nodes
    maxnodes = 7, # Maximum number of leaf nodes
    mtry = mtry, # Number of feature variables as candidates for each split sampsize=length(train_ind),
  )
  rf_classifier
  diabetes_dt[, preds_rf := predict(rf_classifier, type="prob", newdata=diabetes_dt)[,2]] 
  # Plot roc curves for each model
  ggroc <- ggplot(diabetes_dt[-train_ind], aes(d=as.numeric(Outcome), m=preds_rf)) +
    geom_roc() +
    geom_abline() + theme_bw() 
  current_auc = calc_auc(ggroc)
  if ((current_auc> best_auc)[3]) {
    best_auc <- current_auc
    best_params <- c(ntree,nodesize,mtry)
    best_params <- setNames(best_params, c("ntree", "nodesize", "mtry"))
  } 
  }
print (best_auc)

