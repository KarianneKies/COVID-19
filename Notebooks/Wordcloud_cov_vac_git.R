install.packages("rtweet")
install.packages("dplyr")
install.packages("tidytext")
install.packages("tm")
install.packages("wordcloud")
install.packages("corpus")
library(tm)
library(rtweet)
library(tidyr)
library(tidytext)
library(dplyr)
library(wordcloud) 
library(corpus)

#Get data
app_name = 'XX'
API_key = 'XX'
API_secret_key = 'XX'
access_token = 'XX'
access_token_secret = 'XX'

data <- search_tweets("#Coronavaccine",
                      n=500,
                      include_rts = FALSE, 
                      lang = "en") # downloaded non-ASCII characters
tweets_vac = data %>% select(created_at, screen_name, text)

# Cleaning
corpus <- Corpus(VectorSource(tweets_vac$text))
corpus <- corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) 

#remove stopwords
new_stopwords <-c("coronavirus", "corona", "covid", "covidvaccine", "vaccine", 
                  "coronavaccination", "vaccination") #impractical: search for a more efficient way
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, new_stopwords)

#remove all non-ASCI words
for (i in seq(corpus)) {
  corpus[[i]] <- gsub('[^a-zA-Z|[:blank:]]', "", corpus[[i]])
}

term_doc_mat <- TermDocumentMatrix(corpus) 
matrix <- as.matrix(term_doc_mat) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
data_frame <- data.frame(word = names(words),freq=words)

wordcloud2(data=data_frame, size=5)