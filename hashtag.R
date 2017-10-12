#Retrieving Text from Twitter

rm(list=ls())

#Twitter API requires authentication since March 2013. Please follow instructions in "Section 3 - Authentication with OAuth" in the twitteR vignettes on # CRAN or this link to complete authentication before running the code below.

library(twitteR)
library(wordcloud)
library(stringr)
library(tm)
library(longurl)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(dplyr)
library(tidytext)
library(tidyr)
library(plotly)


setup_twitter_oauth("dr83qJt5IfcuqmicXPI9yINlA", "c0bSVElsvFtuHQnlZhnvphup98486t1Qm3BJEezTqIlNfSzvM6","37933003-LSHwa6XzUtCwXnt3HN4nw0cq37Qd8ALtnyTEAYsI3", "hXrLrYqKkzmoqyaZJsfmTI5bO5zv3yPVytR9fMDWVuSpl")
setwd("d:/Data/github/R Twitter")


rdmTweets <- searchTwitter("#lthechat", n=1500, since='2017-10-09')
n <- length(rdmTweets)
tweets <- do.call("rbind", lapply(rdmTweets, as.data.frame)) # convert to a datafame

#save data incase I want to come back later
write.csv(tweets, file="lthechat October 2017.csv") 


#tweets <- read.csv("lthechat October 2017.csv")


posters <- sort(unique(tweets$screenName))
n.posters <- length(posters)

# counts per poster
poster.freq <- sort(table(tweets$screenName), decreasing = TRUE)
poster.freq <- as.data.frame(poster.freq) # coerce to dataframe


url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

for(i in 1:n.posters) {
  this.poster <- poster.freq$Var1[i]
  subbit <- subset(tweets, tweets$screenName == this.poster)
  numtweets <- length(subbit)

  poster.freq[i,3] <- sum(str_count(subbit$text, "http")) # number of urls linked by user
  poster.freq[i,4] <- sum(subbit$favoriteCount) # number of times posts were favorited
  poster.freq[i,5] <- sum(str_count(subbit$text, "@")) # number of others referred 
  poster.freq[i,6] <- sum(subbit$retweetCount) # number of times posts were retweeted
  poster.freq[i,7] <- sum(subbit$isRetweet, na.rm=TRUE) # number of posts that were retweeted
 
  names(poster.freq) <- c("AQcreenName","Tweets", "URLs Linked","Favourited", "Refs to others","Retweeted by", "Retweeted by others" )
   
  repliesto <- sum( !is.na( subbit$replyToSN)) # number in reply to
  replylthe <- length(which(subbit$replyToSN == "LTHEchat")) # don't count
  poster.freq[i,8] <- repliesto - replylthe
  
#  textURL <- na.omit(str_extract(subbit$text, url_pattern))  
#  textURL2 <- expand_urls(textURL)
#  textURL3 <- as.list(textURL2$expanded_url)

}

colnames(poster.freq) <- c("Poster","Posts made","Links embedded","Favouriting count", "Included others", "Total retweets", "Posts retweeted", "In reply to !lthe")


######################################################
# mess with content of posts
######################################################


# get rid of posts by LTHEchat - i'm not interested in counting the time the question is asked
subbit <- subset(tweets, tweets$screenName != "LTHEchat")
mytext <- subbit$text

#################
# clean
#################

#remove retweet entries
mytext = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', mytext)
#remove emoticons
mytext <- iconv(mytext, "latin1", "ASCII", "")
# remove control characters
mytext <- str_replace_all(mytext, "[[:cntrl:]]", " ")
# remove &amp
mytext <- str_replace_all(mytext, "&amp", " ")
# remove links
mytext <- str_replace_all(mytext, "(http[^ ]*)|(www\\.[^ ]*)", " ")
# convert tweets to lower case
mytext <- tolower(mytext)
# remove at people
mytext = gsub('@\\w+', '', mytext)
# remove tags
mytext <-gsub("#[[:alnum:][:punct:]]*","",mytext)
# remove punctuation
mytext = gsub('[[:punct:]]', ' ', mytext)
# remove numbers
mytext <- gsub("\\d", "", mytext) 

# build a corpus
# VectorSource specifies that the source is character vectors.
myCorpus <- Corpus(VectorSource(mytext))

copyCorpus <- myCorpus
# myCorpus <- tm_map(myCorpus, PlainTextDocument)
# myCorpus <- tm_map(myCorpus, stemDocument, language="english")
# strip whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
#remove stopwords
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english')) #remove stopwords

tdm = TermDocumentMatrix(myCorpus)

# term frequencies
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 20)
dm <- data.frame(term = names(term.freq), freq = term.freq)
# wordcloud
wordcloud(dm$term, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

##################
# sentiment
##################

ap_td <- tibble()
ap_td <- tidy(tweets$text)
colnames(ap_td) <- "word"

ap_sentiments <- ap_td %>% right_join(get_sentiments("nrc")) %>% filter(!is.na(sentiment)) %>% count(sentiment, sort=TRUE)
                            
#ap_sentiments <- as.table(ap_sentiments)

colors <- c("Red","Green", col=cm.colors(8))
barplot(ap_sentiments$n, ylab = "Frequency", col=colors, las=3, main = "#LTHEchat 4-9 April 2017",legend.text = ap_sentiments$sentiment)



#legend.text = ap_sentiments$sentiment, names.arg = ap_sentiments$sentiment
