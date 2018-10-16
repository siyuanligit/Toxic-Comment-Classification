setwd("C:/Users/insuf/Google Drive/Thesis")

# load dependencies
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(formattable)

# load dataset
train <- read_csv("train.csv")

tagged <- train[which(train$toxic==1|train$severe_toxic==1|train$obscene==1|train$threat==1|train$insult==1|train$identity_hate==1),]

# clean dataset,
# remove anything that is not in alphabet,
# clean whitespace,
# and convert to lowercase.
comment_text <- train$comment_text %>% 
  str_replace_all("[^a-zA-Z\\s]", "") %>% 
  str_replace_all("[\\s]+", " ") %>% 
  str_to_lower() %>% 
  as.data.frame()

# change data.frame column name
names(comment_text) <- c("text")

# fix data type problem
comment_text <- comment_text %>% 
  mutate_if(is.factor, as.character)

# count the strings by words
comment_text$count <- comment_text$text %>% 
  str_count("\\S+")

sum(comment_text$count)
# count unique words in strings
comment_text$unique_count <- lapply(comment_text$text, function(x){length(paste(unique(strsplit(x, " ")[[1]])))}) %>% 
  as.numeric()
comment_text$unique_count <- ifelse(comment_text$unique_count > comment_text$count, comment_text$count, comment_text$unique_count)

# calculate the unique word percentage in strings
comment_text$unique_perc <- comment_text$unique_count / comment_text$count

# unique word percentage
ggplot(data = comment_text, aes(unique_perc)) +
  geom_histogram(binwidth = 0.1, aes(fill=..count..)) +
  scale_fill_distiller(palette = "Spectral", trans = "reverse") +
  ggtitle("Percentage of Unique Words in Each Sentence") +
  xlab("Unique Words - Percentage") +
  ylab("Density") +
  guides(fill=FALSE)
  

comment_text <- cbind(train$id, comment_text, train[,3:8])

# tag those percentage less than 20% as spam
comment_text$spam <- ifelse(comment_text$unique_perc < 0.33, 1, 0)

# remove all repeated words in these spam comments
comment_text$text[which(comment_text$spam == 1)] <- paste(unique(strsplit(comment_text$text[which(comment_text$spam == 1)], " ")[[1]]), collapse = " ")

# clean up
comment_text$count <- NULL
comment_text$unique_count <- NULL
comment_text$unique_perc <- NULL
comment_text$spam <- NULL
names(comment_text) <- names(train)



# output
write.csv(comment_text, file = "train_clean.csv", row.names = FALSE)
df <- read_csv("train_clean.csv")


test <- read_csv("test.csv")
comment_text <- test$comment_text %>% 
  str_replace_all("[^a-zA-Z\\s]", "") %>% 
  str_replace_all("[\\s]+", " ") %>% 
  str_to_lower() %>% 
  as.data.frame()
names(comment_text) <- c("text")
comment_text <- comment_text %>% 
  mutate_if(is.factor, as.character)
comment_text <- cbind(test$id, comment_text)
names(comment_text) <- names(test)
write.csv(comment_text, file = "test_clean.csv", row.names = FALSE)
df <- read_csv("test_clean.csv")



formattable(train[6187:6192,], list(
  toxic = formatter("span", style = x~ifelse(x==1, style(color = "red", font.weight = "bold"), NA)),
  severe_toxic = formatter("span", style = x~ifelse(x==1, style(color = "red", font.weight = "bold"), NA)),
  obscene = formatter("span", style = x~ifelse(x==1, style(color = "red", font.weight = "bold"), NA)),
  threat = formatter("span", style = x~ifelse(x==1, style(color = "red", font.weight = "bold"), NA)),
  insult = formatter("span", style = x~ifelse(x==1, style(color = "red", font.weight = "bold"), NA)),
  identity_hate = formatter("span", style = x~ifelse(x==1, style(color = "red", font.weight = "bold"), NA))
))

