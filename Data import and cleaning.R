library(tidyverse)
library(tm)
library(qdap) # qualitative data analysis package (it masks %>%)
library(qdapRegex)
library(SnowballC)
library(RWeka)


data_path <- "Data/"

#Preparing Training set data

data_train <- read.csv(paste0(data_path, "drugsComTrain_raw.csv"))

data_train_useful <- data_train %>%
  filter(usefulCount > 1)

data_train_useful$review <- as.character(data_train_useful$review) # to make sure it is text

data_train_useful$review <- tolower(data_train_useful$review)
data_train_useful$review <- str_replace_all(data_train_useful$review, "[^[:alnum:]]", " ")
data_train_useful$review <- tm::removeWords(x = data_train_useful$review, stopwords(kind = "SMART"))
data_train_useful$review <- tm::removeNumbers(x = data_train_useful$review)
data_train_useful$review <- rm_white(data_train_useful$review)
data_train_useful$review <- rm_white_lead_trail(data_train_useful$review)

corpus <- VCorpus(VectorSource(data_train_useful$review)) # turn into corpus

dtm_temp <- DocumentTermMatrix(corpus, control = list(wordLengths = c(4, 50),
                                                      language = "english",
                                                      stemming = TRUE,
                                                      bounds = list(global = c(100,200000)),
                                                      tokenize = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)),
                                                      weighting = weightTfIdf
)
)

dtm_final <- removeSparseTerms(dtm_temp, 0.999)
save(dtm_final, file = paste0(data_path, "dtm.Rdata"))
save(data_train_useful, file = paste0(data_path, "preprocessed_train.Rdata"))



load(paste0(data_path, "dtm.Rdata"))

m <- as.matrix(dtm_final)
colnames(m) <- colnames(dtm_final)

df_model <- data.frame(rating = as.factor(data_train_useful$rating > 5),
                       m)

levels(df_model$rating) <- c("Neg", "Pos")

save(df_model, file = paste0(data_path, "df_model.Rdata"))


#Preparing Test set data
data_test <- read.csv(paste0(data_path, "drugsComTest_raw.csv"))

data_test_useful <- data_test %>%
  filter(usefulCount > 1)

data_test_useful$review <- as.character(data_test_useful$review) # to make sure it is text

data_test_useful$review <- tolower(data_test_useful$review)
data_test_useful$review <- str_replace_all(data_test_useful$review, "[^[:alnum:]]", " ")
data_test_useful$review <- tm::removeWords(x = data_test_useful$review, stopwords(kind = "SMART"))
data_test_useful$review <- tm::removeNumbers(x = data_test_useful$review)
data_test_useful$review <- rm_white(data_test_useful$review)
data_test_useful$review <- rm_white_lead_trail(data_test_useful$review)

corpus_test <- VCorpus(VectorSource(data_test_useful$review)) # turn into corpus

dtm_test_temp <- DocumentTermMatrix(corpus_test, control = list(wordLengths = c(4, 50),
                                                      language = "english",
                                                      stemming = TRUE,
                                                      bounds = list(global = c(100,200000)),
                                                      tokenize = function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)),
                                                      weighting = weightTfIdf
)
)

dtm_test_final <- dtm_test_temp
save(dtm_test_final, file = paste0(data_path, "dtm_test.Rdata"))
save(data_test_useful, file = paste0(data_path, "preprocessed_test.Rdata"))



load(paste0(data_path, "dtm_test.Rdata"))

m_test <- as.matrix(dtm_test_final)
colnames(m_test) <- colnames(dtm_test_final)

df_model_test <- data.frame(rating = as.factor(data_test_useful$rating > 5),
                       m_test)

levels(df_model_test$rating) <- c("Neg", "Pos")

save(df_model_test, file = paste0(data_path, "df_model_test.Rdata"))
