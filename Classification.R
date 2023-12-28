library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(ggrepel)
library(plotROC)

#Section 1

diabetes_dt <- fread("extdata/pima-indians-diabetes.csv") 
diabetes_dt[, Outcome := as.factor(Outcome)]
# Store feature variables that we will need for later
feature_vars <- colnames(diabetes_dt[,-c("Outcome")])
diabetes_dt

diabetes_dt[, .N, by=Outcome] # absolute numbers for each class

diabetes_dt[, .N/nrow(diabetes_dt), by=Outcome] # class proportions

## Boxplot/violin since we have a binary outcome variable and numeric feature variables
melted_diabetes_dt <- melt(diabetes_dt[, .(Glucose,
                                           Insulin, Outcome, BloodPressure)],
                           id.vars="Outcome")
ggplot(melted_diabetes_dt, aes(value, Outcome)) + geom_boxplot() +
  facet_wrap(~variable, scales="free", nrow = 3)

# Fit logistic regression models for Glucose
logreg_1 <-  glm(Outcome~Glucose,
                 data = diabetes_dt, family = "binomial")
# Have a first look at the output of the model

logreg_1

summary(logreg_1)

# Inspect coefficients
coeffs <- logreg_1$coefficients
coeffs

diabetes_dt[, preds_model1 := predict(logreg_1)] 
diabetes_dt

# visualize predictions from the model.
ggplot(diabetes_dt, aes(preds_model1)) +
  geom_histogram()

ggplot(diabetes_dt, aes(preds_model1, fill = Outcome)) +
  geom_histogram(position="dodge")

confusion_matrix <- function(dt, score_column, labels_column, threshold){ 
  # The table() function is very useful for computing the confusion matrix 
  # We have to use get() to get the column from a string
  return(dt[, table(get(labels_column), get(score_column)>threshold) ])
  
}

thresholds <- c(-1,0,1)
lapply(thresholds, function(t){confusion_matrix(diabetes_dt, "preds_model1", 
                                                "Outcome", t)})

## For the last cutoff we obtain the following number of FP
confusion_matrix(diabetes_dt, "preds_model1", "Outcome", 1)["0", "TRUE"]

tpr_fpr <- function(dt, score_column, labels_column, threshold){ tpr <- NULL # TODO
fpr <- NULL # TODO
return(data.table(tpr=tpr, fpr=fpr, t=threshold))
}

tpr_fpr <- function(dt, score_column, labels_column, threshold){
  # Use confusion matrix
  cm <- confusion_matrix(diabetes_dt, score_column, labels_column, threshold)
  # determine FP, TP, FN and TN from confusion matrix
  TP <- cm["1", "TRUE"]
  FP <- cm["0", "TRUE"]
  TN <- cm["0", "FALSE"]
  FN <- cm["1", "FALSE"]
  # compute FPR and TPR
  tpr <-  TP/(TP+FN)
  fpr <-  FP/(FP+TN)
  return(data.table(tpr=tpr, fpr=fpr, t=threshold))
}

thresholds <- c(-1,0,1)
dt <- rbindlist(lapply(thresholds, function(t){ tpr_fpr(diabetes_dt, 
                                                        "preds_model1", 
                                                        "Outcome", t ) }))

ggplot(dt, aes(fpr, tpr, label=t)) +
  geom_point() +
  geom_text_repel() +
  xlim(0, 1) + ylim(0, 1) + coord_equal() +
  geom_abline(lty = 'dashed', color='gray')

#Section 2

# Fit two further models with different features
logreg_2 <-  glm(Outcome~BloodPressure,
                 data = diabetes_dt, family = "binomial")
logreg_3 <-  glm(Outcome~Insulin,
                 data = diabetes_dt, family = "binomial")
logreg_2
summary(logreg_2)

logreg_3
summary(logreg_3)

diabetes_dt[, preds_model1 := predict(logreg_1)] 
diabetes_dt[, preds_model2 := predict(logreg_2)] 
diabetes_dt[, preds_model3 := predict(logreg_3)] 
diabetes_dt

# visualize predictions from different models.
ggplot(melt(diabetes_dt[, .(preds_model1, preds_model2, preds_model3)]), aes(value)) +
  geom_histogram() + facet_wrap(~variable, scales="free")

ggplot(melt(diabetes_dt[, .(Outcome, preds_model1, preds_model2, preds_model3)]), 
       aes( fill = Outcome)] +
  geom_histogram(position="dodge") + facet_wrap(~variable, scales="free")

# Melt subset of diabetes_dt for plotting roc curves
plot_dt <- diabetes_dt[,.(Outcome, preds_model1, preds_model2, preds_model3 )] %>%
  melt(id.vars="Outcome", variable.name="logistic_fit", value.name="response")
# Plot roc curves for each model
ggroc <- ggplot(plot_dt, aes(d=as.numeric(Outcome), 
                             m=response, color=logistic_fit)) +
  geom_roc() +
  geom_abline() + theme_bw()

# geom_roc does not work with factors so we have to convert Outcome to numeric
# Add AUCs computed from function cal_auc
aucs <- as.data.table(calc_auc(ggroc))
# Add nice labels including AUC
labels <- sapply(1:3, function(i) {paste("Model", i, ", AUC:", 
                                         round(aucs[group==i, AUC], 4 )) } )

ggroc + scale_color_discrete(name = "Logistic fit", labels = labels)

full_formula <- as.formula(paste(c("Outcome ~ ",
                                   paste(feature_vars, collapse = " + ")),
                                 collapse = ""))
logreg_full <- glm(full_formula,
                   data = diabetes_dt, family = "binomial")

summary(logreg_full)

diabetes_dt[, preds_logreg_full := predict(logreg_full)]
ggplot(diabetes_dt, aes(x=preds_logreg_full, fill=Outcome)) +
  geom_histogram(position="dodge", bins=50) + theme_bw()

plot_dt <- diabetes_dt[,.(Outcome, preds_model1, preds_model2, 
                          preds_model3, preds_logreg_full )] %>%
  melt(id.vars="Outcome", variable.name="logistic_fit", value.name="response")

# Plot roc curves for each model
ggroc <- ggplot(plot_dt, aes(d=as.numeric(Outcome), m=response, color=logistic_fit)) +
  geom_roc() +
  geom_abline() + theme_bw()

ggroc

