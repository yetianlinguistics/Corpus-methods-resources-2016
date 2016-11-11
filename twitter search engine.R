##load libraries and set up ####
library(devtools)
library(twitteR)
library(stringr)
library(plyr)

consumer_key<-"your consumer key"
consumer_secret<-"your consumer secret"
access_token<-"your access token"
access_secret<-"your access secret"

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

##search data ####
# searchTwitter(searchString, n=25, lang=NULL, since=NULL, until=NULL,
#locale=NULL, geocode=NULL, sinceID=NULL, maxID=NULL,
#resultType=NULL, retryOnRateLimit=120, ...)
# some examples
search<- searchTwitter("Trump", n=100,geocode="53.5822,-11.6383, 30mi",since='2016-10-10')
Clinton<- searchTwitter("#Clinton", n=1000,lang="en") #find ISO 639-1 code for language

#using resultType
searchTwitter('election', resultType="recent", n=15)
searchTwitter('language', resultType="popular", n=15)

#twitter timelines: activities through time
ut <- userTimeline('barackobama', n=100)
ut

#favorite tweets: returns the n most recently favorited tweets from a spefiic user
fav = favorites("barackobama", n=100)

#print tweet #50, wrapping the text
writeLines(strwrap(result$text[50],60))

## convert search results as a dataframe ####
result<-twListToDF(search)

## clean up the sentences ####
## Option 1: use R base functions
#remove URL
removeURL <- function(x) gsub("(http|\\://)[[:alnum:]]*", "", x)
#findURL<-function(x) grepl("(http|\\://)[[:alnum:]]*", x)
result$text <- sapply(result$text,removeURL) 
#result$hasURL <- sapply(result$text,findURL) 

#remove @someone
removeAT <-function(x) gsub("@\\w+", "", x)
result$text<-sapply(result$text,removeAT)

#remove hashtags
removeHash<-function(x) gsub("#[a-z,A-Z]*","",x)
result$text<-sapply(result$text,removeHash)
result$text<-gsub("RT : ","",result$text)

# find those containing emojis
result$text <- sapply(result$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#remove duplicated tweets
result<-result[!duplicated(result[,1]),]

write.csv(result,"twitter_2nd_debate.csv",row.names=F)

## Option 2: use "tm" package
library(tm)
# first you need to turn the returned text into a "corpus" object
myCorpus<-Corpus(VectorSource(search$text))
# convert to lower case
myCorpus<-tm_map(myCorpus,content_transformer(tolower))
# removed URLs
removeURL<- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

# keep a copy for stem completion later
myCorpusCopy <- myCorpus
