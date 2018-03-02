library(ranger)
library(e1071)
library(caret)
library(ggplot2)  
library(lattice)
#library(RBackend)
library(MASS)
library(mlbench)
library(caTools)
library(C50)
library(caretEnsemble)


data("Boston")
#data("Sonar")
Sonar <- readRDS("datacamp modules/Sonar")
overfit <- readRDS("datacamp modules/overfit")

# Fit lm modell: model
model <- lm(price ~ ., diamonds)

# Predict on full data: p
p <- predict(model, diamonds)

# Compute errors: error
error <- p - diamonds$price

# Calculate RMSE
sqrt(mean((p - diamonds$price)^2))

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(diamonds))

# Randomly order data
diamonds <- diamonds[rows, ]

# Determine row to split on: split
split <- round(nrow(diamonds) * .80)

# Create train
train <- diamonds[1:split, ]

# Create test
test <- diamonds[(split + 1):nrow(diamonds), ]

# Fit lm model2 on train: modell
model2 <- lm(price ~ ., train)

# Predict on test: p
p <- predict(model2, test)

# Compute errors: error
error <- p - test$price

# Calculate RMSE
sqrt(mean(error^2))

# Fit lm model3 using 10-fold CV: modell
model3 <- train(
  price ~ ., diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

# Print model3 to console
print(model3)

# Fit lm model4 using 5-fold CV: modell
model4 <- train(
  medv ~ ., Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    verboseIter = TRUE
  )
)

# Print model4 to console
model4

# Fit lm modell using 5 x 5-fold CV: model5
model5 <- train(
  medv ~ ., Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    repeats = 5, verboseIter = TRUE
  )
)

# Print model5 to console
model5

# Predict on full Boston dataset
predict(model5, Boston)

# Shuffle row indices: rows
rows <- sample(nrow(Sonar))

# Randomly order data: Sonar
Sonar <- Sonar[rows, ]

# Identify row to split on: split
split <- round(nrow(Sonar) * .60)

# Create train
train <- Sonar[1:split, ]

# Create test
test <- Sonar[(split + 1):nrow(Sonar), ]

# Fit glm modell: model6
model6 <- glm(Class ~ ., family = "binomial", train)

# Predict on test: p
p <- predict(model6, test, type = "response")

# Calculate class probabilities: p_class
p_class <- ifelse(p > 0.50, "M", "R")

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])

# Apply threshold of 0.9: p_class
p_class <- ifelse(p > .9, "M", "R")

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])

# Apply threshold of 0.10: p_class
p_class <- ifelse(p > .1, "M", "R")

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])

# Predict on test: p
p <-  predict(model6, test, type = "response")

# Make ROC curve
colAUC(p, test[["Class"]], plotROC = TRUE)

# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train glm with custom trainControl: model7
model7 <- train(Class ~ ., Sonar, method = "glm", trControl = myControl)

# Print model7 to console
model7

#loading red wine dataset
#wine <- read.csv2("datacamp modules/winequality-white.csv")
wine <- readRDS("datacamp modules/wine")

# Fit random forest: model8
model8 <- train(
  quality ~ .,
  tuneLength = 1,
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model8 to console
model8

wine <- readRDS("datacamp modules/wine")

# Fit random forest: model9
model9 <- train(
  quality ~ .,
  tuneLength = 3,
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print modell to console
model9

# Plot modell
plot(model9)

wine <- readRDS("datacamp modules/wine")

# Fit random forest: model10
#model10 <- train(
#  quality ~ .,
#  tuneGrid = data.frame(mtry = c(2, 3, 7)),
#  data = wine, method = "ranger",
#  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
#)

# Print model10 to console
#model10

# Plot model10
#plot(model10)

# Create custom trainControl: myControl
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Fit glmnet model: model
model11 <- train(
  y ~ ., overfit,
  method = "glmnet",
  trControl = myControl
)

# Print model to console
model11

# Print maximum ROC statistic
max(model11[["results"]])

# Train glmnet with custom trainControl and tuning: model12
model12 <- train(
  y ~ ., overfit,
  tuneGrid = expand.grid(alpha = 0:1,
  lambda = seq(0.0001, 1, length = 20)),
  method = "glmnet",
  trControl = myControl
)

# Print model12 to console
model12

# Print maximum ROC statistic
max(model12[["results"]][["ROC"]])

breast_cancer_x <- readRDS("datacamp modules/breast_cancer_x")
breast_cancer_y <- readRDS("datacamp modules/breast_cancer_y")

# Apply median imputation: model13
model13 <- train(
  x = breast_cancer_x, y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)

# Print model13 to console
model13

# Apply KNN imputation: model14
model14 <- train(
  x = breast_cancer_x, y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "knnImpute"
)

# Print model14 to console
model14

# Fit glm with median imputation: model15
model15 <- train(
  x = breast_cancer_x, y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)

# Print model15
model15

# Fit glm with median imputation and standardization: model16
model16 <- train(
  x = breast_cancer_x, y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = c("medianImpute", "center", "scale")
)

# Print model16
model16

bloodbrain_x <- readRDS("datacamp modules/bloodbrain_x")
bloodbrain_y <- readRDS("datacamp modules/bloodbrain_y")

# Identify near zero variance predictors: remove_cols
remove_cols <- nearZeroVar(bloodbrain_x, names = TRUE, 
                           freqCut = 2, uniqueCut = 20)

# Get all column names from bloodbrain_x: all_cols
all_cols <- names(bloodbrain_x)

# Remove from data: bloodbrain_x_small
bloodbrain_x_small <- bloodbrain_x[ , setdiff(all_cols, remove_cols)]

# Fit model on reduced data: model17
model17 <- train(x = bloodbrain_x_small, y = bloodbrain_y, method = "glm")

# Print modell to console
model17

# Fit modell on reduced data: model18
model18 <- train(x = bloodbrain_x_small, y = bloodbrain_y, method = "glm")

# Print modell to console
model18

churn_x <- readRDS("datacamp modules/churn_x")
churn_y <- readRDS("datacamp modules/churn_y")

# Create custom indices: myFolds
myFolds <- createFolds(churn_y, k = 5)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

# Fit glmnet modell: model_glmnet
model_glmnet <- train(
  x = churn_x, y = churn_y,
  metric = "ROC",
  method = "glmnet",
  trControl = myControl
)

# Fit random forest: model_rf
model_rf <- train(
  x = churn_x, y = churn_y,
  metric = "ROC",
  method = "ranger",
  trControl = myControl
)
# Create model_list
model_list <- list(item1 = model_glmnet, item2 = model_rf)

# Pass modell_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)

# Create bwplot
bwplot(resamples, metric = "ROC")

# Create xyplot
xyplot(resamples, metric = "ROC")

# Create ensemble model: stack
stack <- caretStack(model_list, method = "glm")

# Look at summary
summary(stack)
