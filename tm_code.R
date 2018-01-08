##################################################################
#
# TITLE: Sentiment Analysis - Oncology
#
# DESCRIPTION: An R script to perform text mining/sentiment
#              analysis on patient survey data to better understand
#              patient experience
#
# AUTHORS: Curry W. Hilton
#          Avanthi Rachamallu
#
# LAST UPDATED: 12/26/2017 
#
##################################################################

##################################################################
#         Libraries/Packages/Custom Functions necessary          # 
##################################################################

require(tm)
require(readxl)
require(wordcloud)
require(RWeka)
require(dplyr)
require(tidytext)

library(tm)
library(readxl)
library(wordcloud)
library(RWeka)
library(dplyr)
library(tidytext)

clean <- function(corpus){
  corpus <- removePunctuation(corpus)
  corpus <- removeNumbers(corpus)
  corpus <- rm_stopwords(corpus, stopwords = qdapDictionaries::Top100Words)
  return(corpus)
}

tokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

##################################################################
#                     Data import procedure                      # 
##################################################################

tm_sample <- read_excel("tm_sample.xlsx")
head(tm_sample)

##################################################################
#                        Data cleansing                          # 
##################################################################

# clean corpus with defined function 'clean'

text_clean <- clean(tm_sample$Response)

# create source

src <- VectorSource(text_clean)

# create a corpus on the vector

text_corpus <- VCorpus(src)

# create document term matrix and adjacency matrix - unigram

text_dtm <- TermDocumentMatrix(text_corpus)
text_dtm.m <- as.matrix(text_dtm)

text_dtm.m[1:2, 5:10]

freq <- rowSums(text_dtm.m)
freq.sort <- sort(freq, decreasing = TRUE)
freq.sort[1:10]


##################################################################
#                          Analysis                              # 
##################################################################

# determine top 10 most used words in text responses

freq.1 <- freq_terms(tm_sample$Response, top=10, at.least =3, stopwords = tm::stopwords("en"))
plot(freq.1)

# wordcloud

words_freq <- data.frame(term = names(freq.1), num = freq.1)
wordcloud(words = words_freq$num.WORD, freq = words_freq$num.FREQ, max.words = 100, colors = "sky blue")


# polarity

pol.ex <- polarity(text_clean)
pol.ex

plot(pol.ex)

# Sentiment Contribution

bing_word_counts <- tid %>%
  inner_join(get_sentiments("bing"), by = c("term" = "word")) %>%
  count(term, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(term, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()