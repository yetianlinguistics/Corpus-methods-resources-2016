##### sentiment analysis by words ####
# from https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/tree/master/data/opinion-lexicon-English

pos.words <- scan('C:/Users/Ye Tian/Dropbox/teaching/Corpus course Nov 2016/Resources/Facebook/opinion-lexicon-English/positive-words.txt',
                  what='character',comment.char=';')
neg.words <- scan('C:/Users/Ye Tian/Dropbox/teaching/Corpus course Nov 2016/Resources/Facebook/opinion-lexicon-English/negative-words.txt',
                  what='character',comment.char=';')


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  #scores.df = data.frame(score=scores, text=sentences)
  #return(scores.df)
}

media$post_senti<-score.sentiment(media$status_message,pos.words,neg.words)
write.csv(media, "something.csv",row.names=F)

### write the following out for PoS tagging
writeLines(text = media$status_message, con = "C:/Users/Ye Tian/Dropbox/teaching/Corpus course Nov 2016/Resources/Facebook/bbc.txt")


### see if average sentiment scores of texts differ in different clusters of reaction patterns
library(dplyr)
ddply(media,"cluster",function(df)colMeans(df[,c(10:15,17)]))
