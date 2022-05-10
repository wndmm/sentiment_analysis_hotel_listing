# TEXT ANALYSIS DAN SENTIMENT ANALYSIS REGULER
# DARI HASIL SCRAPPING WEB DI GABUNG DENGAN DATASET PROPERTY
# ------------------------------------------------------------------------ #
# LOAD PACKAGES
library(tm)
library(wordcloud)
library(syuzhet)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(reshape2)
library(tidytext)
library(tidyr)
library(textdata)

# SENTIMENT ANALYSIS ON travelokabdg # 
# IMPORT DATA

travelokabdg <- read.csv(file.choose(), header = 1)

# CHECK STRUCTURE
str(travelokabdg)
head(travelokabdg)

# RESTRUCTURE THE DATA
reviews_utoken <- unnest_tokens(tbl = travelokabdg, input = textreviewuser, output = word)

# lib: DATA FRAME WITH STOPWORDS
lib_stp_wrds_utoken <- get_stopwords(source = "smart") 

# REMOVE STOP WORDS N NUMBER
reviews_utoken <- anti_join(reviews_utoken, lib_stp_wrds_utoken, by="word")

# lib: SENTIMENT BY BING LEXICON/DICTIONATY
lib_bing <- get_sentiments(lexicon = "bing")
#afinn <- get_sentiments(lexicon = "afinn")

# JOIN DF (bing)
utoken_bing <- inner_join(reviews_utoken, lib_bing, by= "word")

# JOIN DF (afinn)
#utoken_afinn <- inner_join(reviews_utoken, afinn, by= "word")

# WRITE RESULTS 1
write.csv(x = utoken_bing, file = "sentiment_travelokabdg.csv")

# COMPUTE THE SENTIMENT COUNTS BY ALL VARIABLES ON REVIEWS
utoken_bing <- count(utoken_bing, ratingprop, ratinguser, randomrate, propertyinfo, 
propertylabel, propertytype, landmarknear, sentiment)

#utoken_bing <- count(utoken_bing, num, ratinguser)

#utoken_afinn <- count(utoken_afinn, reviewamount, landmarknear, value)

# MANIPULATING THE DATA FRAME LONG TO WIDE FORMAT (SPREAD)
utoken_bing <- spread(key = sentiment, value = n, fill = 0, data = utoken_bing)
#utoken_afinn <- spread(key = value, value = n, fill = 0, data = utoken_afinn)

# CREATE SENTIMENT VARIABLES
utoken_bing <- mutate(sentiment = positive - negative, .data = utoken_bing)
#utoken_afinn <- mutate(value = positive - negative, .data = utoken_afinn)

mean(utoken_bing$sentiment, na.rm = T)
#mean(utoken_afinn$value, na.rm = T)

# WRITE RESULTS 2
write.csv(x = utoken_bing, 
          file = 'results_sentiment_travelokabdg_compute_mutate_spread.csv')
# DATA VIZZZZ: BAR CHART
ggplot(aes(x = ratingprop, y = sentiment, fill = landmarknear), data = utoken_bing) +
       geom_col(show.legend = F) + facet_wrap(vars(landmarknear), 
                                  ncol = 5, scales = "free_x") + 
                                  labs(x = "Rating of Property", y = "Sentiment") +
                                  theme_minimal()

ggplot(aes(x = ratinguser, y = negative, fill = propertylabel), data = utoken_bing) +
      geom_col(show.legend = F) + facet_wrap(vars(propertylabel), 
                                  ncol = 5, scales = "free_x") + 
                                  labs(x = "Listing Labels", y = "Sentiment - Negative") +
                                  theme_minimal()

ggplot(aes(x = ratinguser, y = positive, fill = propertylabel), data = utoken_bing) +
                                  geom_col(show.legend = F) + facet_wrap(vars(propertylabel), 
                                  ncol = 5, scales = "free_x") + 
                                  labs(x = "Listing Labels", y = "Sentiment - Positive") +
                                  theme_minimal()
# SEGMENT JUST BY THREAD
utoken_bing <- reviews_utoken %>% inner_join(lib_bing, by="word") %>% 
  count(landmarknear, sentiment) %>%
  spread(key = sentiment, value = n, fill = 0) %>% mutate(sentiment = positive - negative)

# BAR CHART SEGMENT
ggplot(aes(x = landmarknear, y = sentiment), data = utoken_bing) +
  geom_col(show.legend = T, fill = "dodgerblue") + 
  labs(x = "Landmark Nearby", y = "Sentiment") +
  theme_minimal()
ggplot(aes(x = landmarknear, y = positive), data = utoken_bing) +
  geom_col(show.legend = T, fill = "dodgerblue") + 
  labs(x = "Landmark Nearby", y = "Sentiment - Positive") +
  theme_minimal()
ggplot(aes(x = landmarknear, y = negative), data = utoken_bing) +
  geom_col(show.legend = T, fill = "dodgerblue") + 
  labs(x = "Landmark Nearby", y = "Sentiment - Negative") +
  theme_minimal()

# ------------------------------------------------------------------------ #

#### CREATING SOME EACH SENTIMENT SCORE IN EVERY SINGLE ROW ##
  
# CREATING CORPUS
corpustvrlk <- iconv(travelokabdg$textreviewuser)
corpustvrlk <- Corpus(VectorSource(corpustvrlk))

# SEE THE CORPUS
inspect(corpustvrlk[1:5])

# CLEANING CORPUS
corpustvrlk <- tm_map(corpustvrlk, tolower)
# INSPECT CORPUS 1;5
corpustvrlk <- tm_map(corpustvrlk, removePunctuation)
corpustvrlk <- tm_map(corpustvrlk, removeNumbers)
corpustvrlk <- tm_map(corpustvrlk, removeWords, stopwords("english"))
# REMOVE SOME COMMON WORDS NOT USED
corpustvrlk <- tm_map(corpustvrlk, 
              removeWords, c( "good", "still", "hotel", "room", "owner", "okay", "though", "didnt", "apartment", 
                              "guest house", "villa", "resort", "also","can", "got", "back", "stay", "really",
                              "just", "must", "even", "thing", "next", "maybe", "must", "need", "booked", "guests",
                              "make", "get", "dont", "thank","first", "time"," want", "bandung", "nice", "one", 
                              "front", "place", "lot", "will", "around", "come", "many", "get", "wifi"))
corpustvrlk <- tm_map(corpustvrlk, stripWhitespace)

reviews_final <- corpustvrlk
str(reviews_final)

# CREATING TERM DOC
tdm <- TermDocumentMatrix(reviews_final)
tdm <- as.matrix(tdm)
tdm[1:10,1:5]

# BAR PLOT OF WORDS
w <- rowSums(tdm)
w <- subset(w,w>=50)
barplot(w, las = 2, col = "dodgerblue")

# CREATING WORDCLOUD SKIPPPPPPP
w <- sort(rowSums(tdm), decreasing = T)
set.seed(2000)
wordcloud::wordcloud(words = names(w), freq = w, max.words = 50)

# OBTAIN SENTIMENT SCORE
sentiment_data <- iconv(travelokabdg$textreviewuser)
s <- get_nrc_sentiment(sentiment_data)
s[1:10,]

# CALCULATE REVIEW WISE SCORE
s$score <- s$positive - s$negative
s[1:10,]

sentiment_final <- s
str(sentiment_final)

# WRITE CALCULATION
write.csv(x = s, file = "results.sentiment.travelokabdg.csv")

# VIZ
get_nrc_sentiment("happy")
get_nrc_sentiment("excitement")
get_nrc_sentiment("sadness")

z <- get_sentiment(sentiment_data)

barplot(colSums(s), col = 'dodgerblue', ylab = 'count',main = 'Sentiment Scores')

