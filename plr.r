library(caret)
library(doParallel)

cl <- makeCluster(3, type = "PSOCK")  
registerDoParallel(cl)

data_path <- "Data/"
load(paste0(data_path, "df_model.Rdata"))

# rf

grid <- expand.grid(cp = "bic",
                    lambda = c(0, 10^seq(-4, -1, length.out = 4))
)
                    
cv <- trainControl(method = "cv", 
                   number = 3,
                   verboseIter = TRUE,
                   allowParallel = TRUE,
                   classProbs = TRUE)

plr_fit <- train(rating ~ ., data = df_model,
                    method = "plr",
                    trControl = cv,
                    tuneGrid = grid,
                    verbose = TRUE
)

save(plr_fit, file = "plr.Rdata")
