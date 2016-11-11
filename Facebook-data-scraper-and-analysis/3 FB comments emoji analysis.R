
##read in emoji dictionary and comments ####
emojis <- read.csv(".../emoji sentiment.csv", header = T)
emojis<-emojis[emojis$Sentiment.score!="#N/A",]
emojis$Sentiment.score<-as.numeric(levels(emojis$Sentiment.score))[emojis$Sentiment.score]

comments<-read.csv(".../bbcnews_facebook_comments.csv",header=T,sep="|")

## match and count emojis ####
### function for counting the number of matched emojis in each comment
# in this function text argument MUST be a vector of text
# the useBytes = TRUE is crutial for mapping the emojis
bytes_count<-function(pattern,text,ignore.case = FALSE, perl = FALSE,
                      fixed = FALSE, useBytes = TRUE){
  out<-vector(mode = "integer", length = length(text))
  r<-1
  for (r in 1:length(text)){
    if (regexpr(pattern,text[r],useBytes = TRUE)==-1) {out[r]<-0
    } else {out[r]<-(length(unlist(gregexpr(pattern,text[r],useBytes = TRUE))))}
  }
  return(out)
}

## I tried vapply, sapply, and sapply with parallel processing, the last one was the fastest
## for demonstrating, I'll just do 1000 lines of comments
## if your comments file is very big, you should probably do them in chuncks, I'd say no more than 50000 lines at a time
comments<-comments[1:1000,]

## first, use vapply, use system.time() to wrap whatever you are doing tells you how long it took
## too slow, don't do the following
system.time(emoji.frequency.vapply <- vapply(emojis$Bytes,
                                             bytes_count,FUN.VALUE = double(nrow(comments)),
                                             text=comments$comment_message, useBytes = T )
)

#as you'll see below, the above solution is slow (though much faster than a for loop)
#so let's just delete the matrix created
rm(emoji.frequency.vapply)

##### but vapply is not as fast as parallel sapply!
#Parallelised sapply is best I have tried (about half the time as vapply), so let's do that
library("parallel")
library("foreach")
library("doParallel")
library("Matrix")

## makeCluster creates a set of copies of R running in parallel and communicating over sockets.
## so how fast this goes depends on your CPU (given enough RAM)
mycluster <- makeCluster(detectCores() - 1)
registerDoParallel(mycluster, cores = detectCores() - 1)

## parSapply is a matrix where each row corresponds to each row of your comments
## and each column represents an emoji, and the number (0,1,2 etc) says whether and how many times that emoji appeared in this comment
## parSapply is simply sapply with parellel processing
system.time(emoji.freq <- parSapply(cl = mycluster, emojis$Bytes,
                      bytes_count,
                      text=comments$comment_message, useBytes = T))
#give the description of emojis as column names of this matrix
colnames(emoji.freq)<-emojis$Description
#stopping the cluster
stopCluster(mycluster)

## Analyze which emojis occurred and how frequently ####

##see which comments contain emoji
comments$hasemoji<-rowSums(emoji.freq>1)
#percentage of comments containing emoji
nrow(comments[comments$hasemoji>0,])/nrow(comments)

## count emojis
emoji.counts <- colSums(emoji.freq>0)
emoji.counts<-emoji.counts[emoji.counts>0]
##emoji.counts should show all the non-zero enojis appeared in your comments
emoji.counts

## now we turn this into a dataframe
emoji.counts<-data.frame(as.list(emoji.counts))
emoji.counts<-t(emoji.counts)
emoji.counts<-data.frame(Description= row.names(emoji.counts),counts= unname(emoji.counts[,1]))

## sort by frequency of emojis, most frequent first
emoji.counts<- emoji.counts[with(emoji.counts, order(-counts)), ]
emoji.counts<-emoji.counts[-c(1:3),]
emoji.counts$Description<-gsub("\\."," ",emoji.counts$Description)

## now we extract the sentiment score for each emoji from Novack et al. (2015)
emo.sen<-emojis[,4:5]
emoji.value <- merge(emoji.counts, emo.sen, by = 'Description')

## sometimes the descriptions in "Description" in emo.sen is slightly different from "Description" in emoji.counts, 
## and with the above merge, you get less rows in emoji.value than in emoji.counts
## if this happens, use %in% function to find out which ones didn't match
## the ones that says "FALSE" are the ones that are in emoji.count, but not in emoji.value
emoji.counts$Description %in% emoji.value$Description

## in my example, "SMILING FACE WITH HEART SHAPED EYES" in emoji.counts didn't match,
## because in emo.sen, the description was "SMILING FACE WITH HEART-SHAPED EYES"
## so let's fix that
emoji.counts$Description[emoji.counts$Description=="SMILING FACE WITH HEART SHAPED EYES"]<-"SMILING FACE WITH HEART-SHAPED EYES"
## merge again, and emoji.value and emoji.conts should have the same number of rows
emoji.value <- merge(emoji.counts, emo.sen, by = 'Description')

## now order emoji.value by frequency of emojis, most frequent first
emoji.value<- emoji.value[with(emoji.value, order(-counts)), ]
## viewing the above should tell you which are the most frequent emojis

###sentiment of comments to each post ####

## remember we wanted to see of posts with different reaction profiles get different sentiment scores based on emojis used in the commments
## each posts can receive hundreds of comments
## so we want to calculate the sentiment of each comment based on the emojis used (only ones that contain emojis)
## and then average the sentiment scores of all the commments for each post
comments.emoji<-comments[comments$hasemoji>0,]

## the following for loop gives you a column in comments.emoji called sentscore, which is the sentiment score of each comment based on emojis
for (i in 1:nrow(comments.emoji)) {
  #find the corresponding row name in emoji.freq
  whichline<-as.numeric(row.names(comments.emoji[i,]) )
  #map the emoji numbers with their corresponding sentiment score, stored as a two colomn matrix
  petit.emo<-matrix(c(emoji.freq[whichline,],emojis$Sentiment.score),nrow=nrow(emojis),ncol=2)
  ### throw away the rows with 0 number of emojis
  petit.emo<-petit.emo[petit.emo[,1]>0, , drop=FALSE] #used drop=FALSE so that it remains a matrix, rather than being converted into a vector
  ##calculate sentiment score, i use this transformation for repeated emojis: log(number of occurence)+1, so repetitions make a difference but not linear
  comments.emoji$sentscore[i]<-sum((log(petit.emo[,1])+1)*petit.emo[,2]/nrow(petit.emo)) #average of sentiment of all comments
}


## calculate an averaged sentiment score for each post ####
## each post has a unique status_id
library(dplyr)
comments.sent<-ddply(comments.emoji,"status_id",function(df)mean(df$sentscore))
colnames(comments.sent) <- c("status_id", "comment.sentiment")

## now we link this with the posts, by "status_id"
media<-read.csv("C:/Users/Ye Tian/Dropbox/teaching/Corpus course Nov 2016/Resources/Facebook/bbcnews_facebook_statuses.csv",sep=",")
media.sen<-merge(media,comments.sent,by="status_id")

##Now you can see if the average sentiment score is different for each cluster!
ddply(media.sen,"cluster",function(df)mean(df$comment.sentiment))

## and then you can play with it, plot it, do statistical tests on it, whatever you want :P 
## enjoy!!!! 


