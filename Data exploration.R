library(naniar)
library(doParallel)  
library(tidyverse)
library(syuzhet)

no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores, type = "SOCK")  
registerDoParallel(cl)

data_path <- "Data/"

load(paste0(data_path, "preprocessed_train.Rdata"))

summary(data_train_useful)

distinct_variable <- data_train_useful %>% 
  summarise(n_drugname = n_distinct(drugName), 
            n_condition = n_distinct(condition))

drug_rating <- data_train_useful %>% 
  group_by(drugName) %>% 
  summarise(mean_rating = mean(rating), count_rating = n()) %>% 
  arrange(-count_rating)

data_train_useful %>%
  ggplot(aes(x = rating)) + 
  geom_density() +
  theme_minimal() # most reviews either 1 or 10


(
  rating_count <- data_train_useful %>% 
  group_by(rating) %>% 
  summarise(n_rat = n())
)

count_drug <- data_train_useful %>% 
  group_by(drugName) %>% 
  summarise(drug_count = n()) %>% 
  arrange(-drug_count)

gg_miss_var(data_train) #no missing in the training set
gg_miss_var(data_test) #no missing in the test set

positive_rating <- data_train_useful %>% 
  group_by(drugName) %>% 
  summarise(positive_ratio = sum(rating > 5)/n())


data_train_useful %>% ggplot(aes(y = rating)) +
  geom_boxplot()


sentiment <- get_nrc_sentiment(data_train_useful$review, cl = cl)

sentiment %>%
  group_by(positive > negative) %>%
  summarise(n())


df_model %>% 
  group_by(rating) %>%
  summarise(n())
