install.packages("rtweet")
install.packages("dplyr")
install.packages("tidytext")
install.packages("tm")
install.packages("wordcloud")
install.packages("corpus")
install.packages("SciencePo")
library(tm)
library(rtweet)
library(tidyr)
library(tidytext)
library(dplyr)
library(wordcloud) 
library(corpus)
library(stringi)
library(ggplot2)

#Get data
app_name = 'xx'
API_key = 'xx'
API_secret_key = 'xx'
access_token = 'xx'
access_token_secret = 'xx'

data <- search_tweets("#Coronavaccine",
                      n=9000,
                      include_rts = FALSE, 
                      lang = "en") # downloaded non-ASCII characters
tweets_vac = data %>% select(created_at, screen_name, text)

#Explore number of tweets per day
tweets_vac$day<- format(as.POSIXct(tweets_vac$created_at,format='%m/%d/%Y %H:%M:%S'),
                        format='%m/%d/%Y')
freq<- tweets_vac %>% 
  group_by(day) %>%
  summarise(frequency = n())

freq$frequency <- as.numeric(freq$frequency)
freq$day <- as.Date(freq$day, "%m/%d/%Y")

plot<-ggplot(data = freq, aes(x = day, y = frequency))+
  geom_line(color = "#00AFBB", size = 1)
plot + ggtitle("Number of tweets per day")+
  xlab("Date") + ylab("Frequency")

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