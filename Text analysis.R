library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(readr)
library(tm)

files <- list.files('C:/Users/l/Desktop/University data/3rd course/Course paper/speeches/all speeches into dataset for R/')
fileName <- glue("C:/Users/l/Desktop/University data/3rd course/Course paper/speeches/all speeches into dataset for R/", files[1], sep = "")

read_file(fileName)


fileText <- glue(read_file(fileName))
#remove any dollar signs
fileText <- gsub("\\$", "", fileText)
#remove stopwords
fileText <- gsub("thank", "", fileText)
#tokenize - split text into words/sentences
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
#get the sentiments from the first text
#pull out only sentiment words
textdata <- tokens %>% inner_join(get_sentiments("bing"))
#count by the variables
wordcount <- count(data_frame(textdata$sentiment), vars = textdata$sentiment)
sentmnt_value <- as.numeric(wordcount[2,2]) - as.numeric(wordcount[1,2])

#file to put our output in
sentiments <- data_frame()


for(i in 1: length(files)) {
  fileName <- glue("C:/Users/l/Desktop/University data/3rd course/Course paper/speeches/all speeches into dataset for R/", files[i], sep = "")
  fileName <- trimws(fileName)
  fileText <- glue(read_file(fileName))
  fileText <- gsub("//$", "", fileText)
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
  textdata <- tokens %>% inner_join(get_sentiments("bing"))
  wordcount <- count(data_frame(textdata$sentiment),
                     vars = textdata$sentiment) #count the number of positive & negative sentiments
  sentiments[i, 1] <- as.numeric(wordcount[2,2]) - as.numeric(wordcount[1,2]) #sentiment
  sentiments[i, 2] <- substr(files[i], 1, 10) #add the year
}

colnames(sentiments) <-c("value", "year")

fileName <- glue("C:/Users/l/Desktop/University data/3rd course/Course paper/speeches/all speeches into dataset for R/", files[9], sep = "")
read_file(fileName)

#Fixing NA, counting manually
fileText <- glue(read_file(fileName))
#remove any dollar signs
fileText <- gsub("\\$", "", fileText)
#remove stopwords
fileText <- gsub("thank", "", fileText)
#tokenize - split text into words/sentences
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
#get the sentiments from the first text
#pull out only sentiment words
textdata <- tokens %>% inner_join(get_sentiments("bing"))
#count by the variables
wordcount <- count(data_frame(textdata$sentiment), vars = textdata$sentiment)
sentmnt_value <- as.numeric(wordcount[2,2]) - as.numeric(wordcount[1,2])
sentiments[9,1] = 10

write.csv(sentiments, "C:\\Users\\l\\Desktop\\University data\\3rd course\\Course paper\\sentiments.csv", row.names = FALSE)
