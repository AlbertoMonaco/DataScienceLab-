library(caret)
library(ranger)
<<<<<<< HEAD
library(glmnet)
library(dplyr)
=======
library(MLmetrics)
>>>>>>> c664757d113f711d27ff786bc3190b88b5fdc653

data_path <- "Data/"


load(paste0(data_path, "df_model.Rdata"))
load(paste0(data_path, "df_model_test.Rdata"))


# rf

grid <- expand.grid(mtry = seq(10, floor(sqrt(ncol(df_model))), length.out = 5),
                    splitrule = "gini",
                    min.node.size = 5)

cv <- trainControl(method = "cv", 
                     number = 3,
                     verboseIter = TRUE)

ranger_fit <- train(rating ~ ., data = df_model,
                   method = "ranger",
                   trControl = cv,
                   num.threads = 3,
                   tuneGrid = grid,
                   num.trees = 100, 
                   verbose = TRUE, 
                   importance = "impurity")


save(ranger_fit, file = "rf.Rdata")


<<<<<<< HEAD
=======
load(paste0(data_path, "df_model_test.Rdata"))


for (k in colnames(df_model)[!(colnames(df_model)  %in% colnames(df_model_test))]
) {
  df_model_test[,k] <- 0
}

fit <- predict(ranger_fit$finalModel$forest, data = df_model_test, verbose = TRUE)

Accuracy(fit$predictions, df_model_test$rating)


>>>>>>> c664757d113f711d27ff786bc3190b88b5fdc653
#Lasso

x <- model.matrix(rating~., df_model)[,-1]
y <- df_model$rating

set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial", nfolds= 3)
model_lasso <- glmnet(x, y, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)
coef(model_lasso)

save(model_lasso, file = "LASSO.Rdata")

load("LASSO.Rdata")

colnames(df_model)[!(colnames(df_model)  %in% colnames(df_model_test))]

for (k in colnames(df_model)[!(colnames(df_model)  %in% colnames(df_model_test))]
) {
  df_model_test[,k] = 0
}

x.test <- model.matrix(rating ~., df_model_test)[,-1]
probabilities <- model_lasso %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "Pos", "Neg")
observed.classes <- df_model_test$rating
mean(predicted.classes == observed.classes)

results_lasso <- cbind(caret::F_meas(data = as.factor(predicted.classes), reference = df_model_test$rating), caret::precision(data = as.factor(predicted.classes), reference = df_model_test$rating),
                       caret::recall(data = as.factor(predicted.classes), reference = df_model_test$rating))

colnames(results_lasso) <- c("F1_measure", "Precision", "Recall")

<<<<<<< HEAD


=======
>>>>>>> c664757d113f711d27ff786bc3190b88b5fdc653
