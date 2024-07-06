###### Loading Required Libraries ######
install.packages("lattice")
install.packages("ggplot2")
install.packages("caTools")
install.packages("ISLR2")
install.packages("glmnet")
install.packages("randomForest")
install.packages("themis")
library(ggplot2)
library(lattice)
library(readr)
library(caret)
library(caTools)
library(class)
library(ISLR2) # load the dataset from the library
library(leaps) # load the library to perform the subset selection method
library(glmnet) # load the package to perform regularisation and cross-validation
library(greybox)
library(corrplot)
library(randomForest)
library(readr)
library(greybox)
library(plotrix)
library(cluster)
library(corrplot)
library(caret)
library(themis)
library(randomForest)
library(pROC)
library(dplyr)


###### Loading Original Data ######
airlinesData90 <- read_csv("airlinesData90.csv")
data <- airlinesData90
View(data)
#Convert non numeric and categorical to numeric

# Names of variables in data.frame
names(data)

# find the class of all variables
sapply(data, class)

# convert all the data itno numeric
summary(data)



# Check the class of all variables again
sapply(data, class)


# Gender
data$Gender <- as.numeric(factor(data$Gender, levels = c("Male", "Female")))
# Male = 1
# Female = 2

# Customer Type
data$`Customer Type` <- as.numeric(factor(data$`Customer Type`, levels = c("Loyal Customer", "disloyal Customer")))
# Loyal Customer = 1
# disloyal Customer = 2

# data$`Type of Travel
data$`Type of Travel` <- as.numeric(factor(data$`Type of Travel`, levels = c("Personal Travel", "Business travel")))
# Personal Travel = 1
# Business travel = 2

# data$Class
data$`Class` <- as.numeric(factor(data$`Class`, levels = c("Eco", "Eco Plus", "Business")))
# Eco = 1
# Eco Plus = 2
# Business = 3

# data$satisfaction
data$`satisfaction` <- as.numeric(factor(data$`satisfaction`, levels = c("neutral or dissatisfied", "satisfied" )))
# neutral or dissatisfied = 1
# satisfied = 2


#### missing values

data <- data[, sapply(data, is.numeric)]

# Replace missing values with column mean
means <- colMeans(data, na.rm = TRUE)
for (i in seq_along(data)) {
  data[, i][is.na(data[, i])] <- means[i]
}

###### Model Construction ######
###### KNN ######


# Scale all the data into one using min-max function

data[,3] <- (data[,3]-min(data[,3])) /
  (max(data[,3])-min(data[,3]))

data[,5] <- (data[,5]-min(data[,5])) /
  (max(data[,5])-min(data[,5]))

data[,6] <- (data[,6]-min(data[,6])) /
  (max(data[,6])-min(data[,6]))

data[,7] <- (data[,7]-min(data[,7])) /
  (max(data[,7])-min(data[,7]))

data[,8] <- (data[,8]-min(data[,8])) /
  (max(data[,8])-min(data[,8]))

data[,9] <- (data[,9]-min(data[,9])) /
  (max(data[,9])-min(data[,9]))

data[,10] <- (data[,10]-min(data[,10])) /
  (max(data[,10])-min(data[,10]))

data[,11] <- (data[,11]-min(data[,11])) /
  (max(data[,11])-min(data[,11]))

data[,12] <- (data[,12]-min(data[,12])) /
  (max(data[,12])-min(data[,12]))

data[,13] <- (data[,13]-min(data[,13])) /
  (max(data[,13])-min(data[,13]))

data[,14] <- (data[,14]-min(data[,14])) /
  (max(data[,14])-min(data[,14]))

data[,15] <- (data[,15]-min(data[,15])) /
  (max(data[,15])-min(data[,15]))

data[,16] <- (data[,16]-min(data[,16])) /
  (max(data[,17])-min(data[,17]))

data[,18] <- (data[,18]-min(data[,18])) /
  (max(data[,18])-min(data[,18]))

data[,19] <- (data[,19]-min(data[,19])) /
  (max(data[,19])-min(data[,19]))

data[,20] <- (data[,20]-min(data[,20])) /
  (max(data[,20])-min(data[,20]))




data[, 21] <- ifelse(min(data[, 21]) == 0, data[, 21] / max(data[, 21]), (data[, 21] - min(data[, 21])) / (max(data[, 21]) - min(data[, 21])))



if (min(data[, 22], na.rm = TRUE) == 0 && max(data[, 22], na.rm = TRUE) == 0) {
  data[, 22] <- 0
} else {
  data[, 22] <- (data[, 22] - min(data[, 22], na.rm = TRUE)) / (max(data[, 22], na.rm = TRUE) - min(data[, 22], na.rm = TRUE))
}

# test and train set


# Split data into training and testing sets

set.seed(10)
split = sample.split(data$satisfaction, SplitRatio = 0.7)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

# Scale the predictor variables in the training and testing sets

predictors = colnames(train)[-23]  # Exclude the last column (satisfaction)
train[,predictors] = scale(train[,predictors])
test[,predictors] = scale(test[,predictors])

# Fit the KNN model using the training set

k = 22  # number of neighbors to consider
model = knn(train[,predictors], test[,predictors], train$satisfaction, k)

# Assess model performance using the testing set
confusion_matrix = table(test$satisfaction, model)
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste0("Accuracy: ", accuracy))

# Compute confusion matrix
confusion_matrix <- table(test$satisfaction, model)
print(confusion_matrix)

# Specify the training control for cross-validation
ctrl <- trainControl(method = "cv", 
                     number = 5, 
                     summaryFunction = twoClassSummary, 
                     classProbs = TRUE)

# Create a grid of values to search over
k_grid <- expand.grid(k = 1:25)

# the satisfaction variable is being treated as a continuous variable rather than a categorical variable. 
#To fix this, you can convert the satisfaction variable into a factor using the factor() function:
train$satisfaction <- ifelse(train$satisfaction == 1, "Yes", "No")

# Fit the KNN model using cross-validation to find the optimal k
set.seed(41)
knn_fit <- train(satisfaction ~ ., 
                 data = train, 
                 method = "knn", 
                 trControl = ctrl, 
                 tuneGrid = k_grid, 
                 metric = "ROC")

# View the results
knn_fit$results
plot(knn_fit)

# CORRELATION MATRIX FOR FEATURE SELECTION

# Create a correlation matrix
corr_matrix <- cor(data)

#corr_matrix

# Convert correlation matrix to a data frame for plotting
corr_data <- reshape2::melt(corr_matrix)

# Plot the correlation matrix
ggplot(corr_data, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.justification = c(1,0),
        legend.position = c(0.85, 0.15),
        legend.direction = "horizontal")

# Find pairs of features that have a correlation greater than 0.8
high_corr_pairs <- which(abs(corr_matrix) > 0.65 & corr_matrix != 1, arr.ind=TRUE)


# Drop the "Departure Delay in Minutes" column
data_no_corr <- data[, -21]
data_no_corr <- data_no_corr[, -14]
data_no_corr <- data_no_corr[, -19]
data_no_corr <- data_no_corr[, -11]
data_no_corr <- data_no_corr[, -9]

# find the class of all variables
sapply(data_no_corr, class)

# Print the updated dataset
print(data_no_corr)
data
# KNN AFTER FEATURE SELECTION

# Split data_no_corr into training and testing sets
set.seed(10)
split = sample.split(data_no_corr$satisfaction, SplitRatio = 0.7)
train_cor = subset(data_no_corr, split == TRUE)
test_cor = subset(data_no_corr, split == FALSE)

# Scale the predictor variables in the training and testing sets
predictors = colnames(train_cor)[-18]  # Exclude the last column (satisfaction)
train_cor[,predictors] = scale(train_cor[,predictors])
test_cor[,predictors] = scale(test_cor[,predictors])

# Fit the KNN model using the training set

k = 20  # number of neighbors to consider
model = knn(train_cor[,predictors], test_cor[,predictors], train_cor$satisfaction, k)

# Assess model performance using the testing set
confusion_matrix = table(test_cor$satisfaction, model)
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste0("Accuracy: ", accuracy))

# Compute confusion matrix
confusion_matrix <- table(test_cor$satisfaction, model)
print(confusion_matrix)

# Cross-validation
# Specify the training control for cross-validation
ctrl <- trainControl(method = "cv", 
                     number = 5, 
                     summaryFunction = twoClassSummary, 
                     classProbs = TRUE)

# Create a grid of values to search over
k_grid <- expand.grid(k = 1:25)

# the satisfaction variable is being treated as a continuous variable rather than a categorical variable. 
#To fix this, you can convert the satisfaction variable into a factor using the factor() function:
train_cor$satisfaction <- ifelse(train_cor$satisfaction == 1, "Yes", "No")

# Fit the KNN model using cross-validation to find the optimal k
set.seed(41)
knn_fit_F <- train(satisfaction ~ ., 
                 data = train_cor, 
                 method = "knn", 
                 trControl = ctrl, 
                 tuneGrid = k_grid, 
                 metric = "ROC")

# View the results
knn_fit_F$results
plot(knn_fit_F,main="Knn plot with optimal K value")



###### Random Forest ######

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(nrow(data), round(0.7*nrow(data)), replace = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Convert the target variable to a factor
train_data$satisfaction <- as.factor(train_data$satisfaction)
test_data$satisfaction <- as.factor(test_data$satisfaction)

# Train a random forest model using 5-fold cross-validation
model_rf <- train(satisfaction ~ ., data = train_data, method = "rf", 
                  trControl = trainControl(method = "cv", number = 12))

# Make predictions on the test set
pred_rf <- predict(model_rf, newdata = test_data)

# Calculate accuracy on the test set
conf_mat <- confusionMatrix(pred_rf, test_data$satisfaction)
conf_mat
accuracy <- conf_mat$overall['Accuracy']
accuracy*100
###### Random Forest with Feature Selection ######


data_new <- data[,-22]
data_new <- data_new[,-11]
data_new <- data_new[,-10]
data_new <- data_new[,-9]

set.seed(10)
train_indices_F <- sample(nrow(data_new), round(0.7*nrow(data_new)), replace = FALSE)
train_data_F <- data_new[train_indices_F, ]
test_data_F <- data_new[-train_indices_F, ]

# Convert the target variable to a factor
train_data_F$satisfaction <- as.factor(train_data_F$satisfaction)
test_data_F$satisfaction <- as.factor(test_data_F$satisfaction)

# Train a random forest model using 12-fold cross-validation
model_rf_F <- train(satisfaction ~ ., data = train_data_F, method = "rf", 
                    trControl = trainControl(method = "cv", number = 12))
# Make predictions on the test set
pred_rf_F <- predict(model_rf_F, newdata = test_data_F)

# Calculate accuracy on the test set
conf_mat_F <- confusionMatrix(pred_rf_F, test_data_F$satisfaction)
conf_mat_F
accuracy <- conf_mat_F$overall['Accuracy']
accuracy

###### Performance Evaluation ######
# ROC
# Train a KNN model 

knn_ROC <- train(satisfaction ~ ., data = train_cor, method = "knn",
                   trControl = trainControl(method = "cv", number = 8))
pred_knn <- predict(knn_ROC, newdata = test_cor, type="prob")

# Train a Random Forest Model
RF_ROC <- train(satisfaction ~ ., data = train_data, method = "rf",
                trControl = trainControl(method = "cv", number = 5))
pred_RF <- predict(RF_ROC, newdata = test_data, type="prob")

# Plot the ROC curve

#plot(pred_knn, main = "ROC Curve for KNN Model", print.thres = c(0.1, 0.5, 0.9))
rocCurves <- vector("list", 5)
rocCurves[[1]] <- roc(test$satisfaction ~ pred_knn[,2])
rocCurves[[2]] <- roc(test_data$satisfaction ~ pred_RF[,2])

for(i in 1:5){
  # Plot each of the ROC curves
  plot(rocCurves[[i]], print.auc=TRUE, auc.polygon=TRUE,
       mar=c(4,4,0,0), grid=TRUE)
  # Add titles to plots
  text(1.1, 0.9, names(rocCurves)[i])
}

##### LASSO Regression ######

library(ggplot2)
library(glmnet)
library(greybox)
library(lattice)
library(Information)
library(plotrix)
library(cluster)
library(plotly)
library(reshape2)
library(corrplot)
library(readxl)
excel_file <- read_excel("airlinesData90.xlsx")
warnings()
airline_data<-excel_file
excel_file<-airline_data
# create a character vector of variable names to convert
vars_to_convert <- c("Gender", "Type_of_Travel", "Class", "Customer_Type")
# loop through each variable and convert to numeric
for (var in vars_to_convert) {
  airline_data[var] <- as.numeric(factor(airline_data[[var]]))
}



View(airline_data)

# LASSO Regression
train_index <- sample(1:nrow(airline_data), size = 0.8*nrow(airline_data), replace = FALSE)
train_data <- airline_data[train_index,-22]
test_data <- airline_data[-train_index,-22]



# Fit a lasso regression model to the training data
x_train <- model.matrix(satisfaction ~ ., data = train_data)[,-1]
y_train <- train_data$satisfaction
lasso_model <- glmnet(x_train, y_train, family = "binomial", alpha = 1)



# Plot the coefficient path to select the optimal lambda
plot(lasso_model, xvar = "lambda", label = TRUE,main="Plot to determine Optimal Value of Lambda")
cv_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
opt_lambda <- cv_model$lambda.min

# Select the features with non-zero coefficients at the optimal lambda
lasso_coef <- data.frame(as.vector(coef(lasso_model, s = opt_lambda)))
lasso_coef[,2] <- rownames(coef(lasso_model, s = opt_lambda))
lasso_coef <- lasso_coef[-1,]
rownames(lasso_coef) <- 1:(nrow(lasso_coef))
colnames(lasso_coef) <- c('Coefficients','Variable')
lasso_coef <- lasso_coef[c('Variable','Coefficients')]
selected_features <- lasso_coef[lasso_coef[,2]!=0,]
rownames(selected_features) <- 1:(nrow(selected_features))
ranked_features <- selected_features[order(abs(selected_features[,2]), decreasing = TRUE),]

# Evaluate the performance of the model on the test data
x_test <- model.matrix(satisfaction ~ ., data = test_data)[,-1]
y_test <- as.factor(test_data$satisfaction)
ncol(x_test)
predictions <- predict(lasso_model, newx = x_test, s = opt_lambda, type = "response")
threshold <- 0.5
predicted_classes <- as.numeric(predictions > threshold)+1
predicted_classes <-as.factor(predicted_classes)
levels(predicted_classes) = levels(y_test)
test_accuracy <- sum(as.factor(predicted_classes) == y_test)/length(y_test)
test_accuracy
test_accuracy <- sum(as.factor(predicted_classes) == y_test)/length(y_test)
test_accuracy







