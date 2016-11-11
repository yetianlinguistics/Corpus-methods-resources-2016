
##read in emoji dictionary and comments ####
emojis <- read.csv("C:/Users/Ye Tian/Dropbox/teaching/Corpus course Nov 2016/Resources/Facebook/emoji sentiment.csv", header = T)
emojis<-emojis[emojis$Sentiment.score!="#N/A",]
emojis$Sentiment.score<-as.numeric(levels(emojis$Sentiment.score))[emojis$Sentiment.score]

comments<-read.csv("C:/Users/Ye Tian/Dropbox/teaching/Corpus course Nov 2016/Resources/Facebook/bbcnews_facebook_comments.csv",header=T,sep="|")

#strongemojis <- emojis
#strongemojis <- subset(strongemojis, abs(Sentiment.score)>0.3)



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
#####
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

## now we analyze which emojis occurred and how frequently ####

##see which comments contain emoji
comments$hasemoji<-rowSums(emoji.freq>1)
#percentage of comments containing emoji
nrow(comments[comments$hasemoji>0,])/nrow(comments)

## count emojis
emoji.counts <- colSums(emoji.freq>0)
emoji.counts<-emoji.counts[emoji.counts>0]
##emoji.counts should show all the non-zero enojis appeared in your comments
emoji.counts

#now we turn this into a dataframe
emoji.counts<-data.frame(as.list(emoji.counts))
emoji.counts<-t(emoji.counts)
emoji.counts<-data.frame(Description= row.names(emoji.counts),counts= unname(emoji.counts[,1]))

#sort by frequency of emojis, most frequent first
emoji.counts<- emoji.counts[with(emoji.counts, order(-counts)), ]
emoji.counts<-emoji.counts[-c(1:3),]
emoji.counts$Description<-gsub("\\."," ",emoji.counts$Description)

#now we extract the sentiment score for each emoji from Novack et al. (2015)
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



####################3

  
colSums(emoji.freq)
plot(log(colSums(emoji.freq.matall)));abline(0,0,col="red3")
library(ggplot2)
all_comment_likes <-(all_comments$comment_likes)
summary(as.factor(all_comment_likes))
  
###TRYING TO COUNT :)
smile_count<-function(pattern,text,ignore.case = FALSE, perl = FALSE,
                                fixed = FALSE, useBytes = TRUE){
  out<-vector(mode = "integer", length = length(text))
  r<-1
  for (r in 1:length(text)){
    if (regexpr(pattern,text[r],useBytes = TRUE)==-1) {out[r]<-0
    } else {out[r]<-(length(unlist(gregexpr(pattern,text[r],useBytes = TRUE))))}
  }
  return(out)
}


#The above took CPU up to 94% and 0.5GB RAM per core - only 56% total though ( for FOXNews_comments)
# Error: cannot allocate vector of size 10.9 Gb
# In addition: Warning messages:
#   1: In unlist(x, recursive = FALSE) :
#   Reached total allocation of 16267Mb: see help(memory.size)
# 2: In unlist(x, recursive = FALSE) :
#   Reached total allocation of 16267Mb: see help(memory.size)
# 3: In unlist(x, recursive = FALSE) :
#   Reached total allocation of 16267Mb: see help(memory.size)
# 4: In unlist(x, recursive = FALSE) :
#   Reached total allocation of 16267Mb: see help(memory.size)
# Timing stopped at: 91.58 96.08 10728.21 


######
#emoji.frequency <- matrix(NA, nrow = 1000, ncol = nrow(emojis))
#The below works at least slightly in parallel
#system.time(
#data = foreach(i = 1:1000, #.packages = c("ncdf","chron","dplyr"),
#.combine = rbind) %dopar% {
#try({
# your operations; line 1...
# your operations; line 2...
# your output
#})
# Ignoring the try advice...
#   for (j in 1:nrow(emojis)) {
#    emoji.frequency[i,j] <- bytes_count(emojis$Bytes[j],comments$comment_message[i])
# }
# if (i %% 200 == 0) {cat(paste0("iteration ",i,"\n"))}
#}#)
#Without data = ; and timed
#system.time(foreach(i = 1:1000, .packages = c("NLP","stringr","stats"),
#              .combine = rbind) %dopar% {
#try({
# your operations; line 1...
# your operations; line 2...
# your output
#})
# Ignoring the try advice...
#               for (j in 1:nrow(emojis)) {
#                emoji.frequency[i,j] <- bytes_count(emojis$Bytes[j],comments$comment_message[i])
}
# if (i %% 200 == 0) {cat(paste0("iteration ",i,"\n"))}
})
#gave user 2.96 system 0.48 elapsed 57.22
#user  system elapsed 
#2.70    1.41   54.78

#And without parallelism
#emoji.frequency <- matrix(NA, nrow = 100000, ncol = nrow(emojis))
#emoji.frequency2 <- matrix(NA, nrow = 1000, ncol = nrow(emojis))
#system.time(
#for (i in 1:1000){
# for (j in 1:nrow(emojis)) {
#  emoji.frequency2[i,j] <- bytes_count(emojis$Bytes[j],comments$comment_message[i])
#}
#if (i %% 500 == 0) {cat(paste0("iteration ",i,"\n"))}
#}) 
#####
######
#system.time(emoji.frequency.sapply <- sapply(emojis$Bytes,
#bytes_count,
#text=comments$comment_message, useBytes = T))


#all.equal(emoji.frequency.sapply, emoji.frequency.vapply)

#####
#the following code does the same thing using FOR loop, probably slower
#emoji.frequency <- matrix(NA, nrow = nrow(comments), ncol = nrow(emojis))
#emoji.frequency <- matrix(NA, nrow = 100000, ncol = nrow(emojis))

#system.time(for (i in 1:1000){
# for (j in 1:nrow(emojis)) {
# emoji.frequency[i,j] <- bytes_count(emojis$Bytes[j],comments$comment_message[i])
#}
#if (i %% 500 == 0) {cat(paste0("iteration ",i,"\n"))}
#} )

#####
save(emoji.frequency, file="emoji_theSun.Rdata")
save(emoji.frequency, file="emoji_taz.kommune.Rdata")



#####
#see which comments contain emoji
comments$hasemoji<-rowSums(emoji.frequency>1)
#percentage of comments containing emoji
nrow(comments[comments$hasemoji>0,])/nrow(comments) 

#### sentiment for each comment


#summarzing overall sums of emojis, not specific to each comment
emoji.counts <- colSums(emoji.frequency>0)
#emojis$Sentiment.score<-as.numeric(emojis$Sentiment.score)
all.emo <- cbind(emoji.counts, emojis)
all.emo<-all.emo[all.emo$emoji.counts>0,]
all.emo<-all.emo[is.na(all.emo$Sentiment.score)==F,]
###overall sentiment by emoji only
#all.emo$Sentiment.score<-as.numeric(all.emo$Sentiment.score)
sent.score<-sum(all.emo$Sentiment.score*all.emo$emoji.counts/sum(all.emo$emoji.counts))

###sentiment of comments to each post
##comments.post<-split(comments, comments$status_id) ##to resources intensive
comments.emoji<-comments[comments$hasemoji>0,]

for (i in 1:nrow(comments.emoji)) {
  #find the corresponding row name in emoji.frequency
  whichline<-as.numeric(row.names(comments.emoji[i,]) )
  #map the emoji numbers with their corresponding sentiment score, stored as a two colomn matrix
  #petit.emo<-as.matrix(cbind(emoji.frequency[whichline,],emojis$Sentiment.score))
  
  petit.emo<-matrix(c(emoji.frequency[whichline,],emojis$Sentiment.score),nrow=842,ncol=2)
  ### throw away the rows with 0 number of emojis
  petit.emo<-petit.emo[petit.emo[,1]>0, , drop=FALSE] #used drop=FALSE so that it remains a matrix, rather than being converted into a vector
  ##calculate sentiment score, i use this transformation for repeated emojis: log(number of occurence)+1, so repetitions make a difference but not linear
  comments.emoji$sentscore[i]<-sum((log(petit.emo[,1])+1)*petit.emo[,2]/nrow(petit.emo)) #average of sentiment of all comments
}



##### the following bit was from the website, which i'm not using
#As an R user I am well aware of the cons of using a for loop and therefoe I 
#will offer up a more efficient way to identify the emoji. I use 
#vapply to accomplish the same task below. To confirm the results are identical 
#I use the all.equal function to compare the results.


emoji.frequency.vapply <- vapply(emojis$Bytes,
                                 regexpr,FUN.VALUE = integer(nrow(comments)),
                                 comments$comment_message, useBytes = T )

# get word frequencies and tokens
#tokens <- WordTokenizer(comments$comment_message)
#tokens <- Token_Tokenizer(comments$comment_message) #seems unneccesary 


#Trying to parallelise PARALLELISE
library("parallel")
library("foreach")
library("doParallel")
mycluster <- makeCluster(detectCores() - 1)
registerDoParallel(mycluster, cores = detectCores() - 1)
emoji.frequency <- matrix(NA, nrow = 1000, ncol = nrow(emojis))
#The below works at least slightly in parallel #Except now it doesn't
data = foreach(i = 1:1000, #.packages= c("NLP","stringr","stats"),
               .combine = rbind) %dopar% {
                 #try({
                 # your operations; line 1...
                 # your output
                 #})
                 # Ignoring the try advice...
                 for (j in 1:nrow(emojis)) {
                   emoji.frequency[i,j] <- bytes_count(emojis$Bytes[j],comments$comment_message[i])
                 }
                 # if (i %% 200 == 0) {cat(paste0("iteration ",i,"\n"))}
               }


#Without data = ; and timed
system.time(foreach(i = 1:1000, .packages = c("NLP","stringr","stats"),
                    .combine = rbind) %dopar% {
                      #try({
                      # your operations; line 1...
                      # your output
                      #})
                      # Ignoring the try advice...
                        for (j in 1:nrow(emojis)) {
                          emoji.frequency[i,j] <- bytes_count(emojis$Bytes[j],comments$comment_message[i])
                        }
                      # if (i %% 200 == 0) {cat(paste0("iteration ",i,"\n"))}
                      
                    })
            stopCluster(myCluster)
#gave user 2.96 system 0.48 elapsed 57.22
#user  system elapsed 
#2.70    1.41   54.78

#And without parallelism
#emoji.frequency <- matrix(NA, nrow = 100000, ncol = nrow(emojis))
emoji.frequency2 <- matrix(NA, nrow = 1000, ncol = nrow(emojis))
emoji.frequency2 <- matrix(NA, nrow = 1000, ncol = nrow(strongemojis))
system.time(
  for (i in 1:1000){
    for (j in 1:nrow(strongemojis)) {
      emoji.frequency2[i,j] <- bytes_count(strongemojis$Bytes[j],comments$comment_message[i])
    }
    if (i %% 500 == 0) {cat(paste0("iteration ",i,"\n"))}
  }) 
###Now I want to try using apply APPLY APLY

sapply(commments$comment_message ,byte_count_small())



bytes_count2<-function(pattern,text,ignore.case = FALSE, perl = FALSE,
                      fixed = FALSE, useBytes = TRUE){ifelse(regexpr(pattern,text,useBytes = TRUE)==-1 , 0 ,
                                                             length(unlist(gregexpr(pattern,text,useBytes = TRUE))))}





byte_count_small <- function(text){for (i in 1:nrow(comments)){bytes_count2(emojis$Bytes[i],comments$comment_message)}}



#################### 
##my own Ye Tian 2016/09/02
# I want to get the number of matched emojis in each comment
##function for number of matches using bytes, if no match, return 0, else return the number of matches
bytes_count<-function(pattern,text,ignore.case = FALSE, perl = FALSE,
                      fixed = FALSE, useBytes = TRUE){
  if (regexpr(pattern,text,useBytes = TRUE)==-1) {return(0)
  } else return(length(unlist(gregexpr(pattern,text,useBytes = TRUE))))
}

### the codes from the website can find only the first match of any emoji, i.e. if there are thre laughter faces, it only counts one.
### I want to count the number of matched emojis in each comment
comments<-bbcnews_comments
emoji.frequency <- matrix(NA, nrow = nrow(comments), ncol = nrow(emojis))
for (i in 1:nrow(comments)){
  for (j in 1:nrow(emojis)) {
    emoji.frequency[i,j] <- bytes_count(emojis$Bytes[j],comments$comment_message[i])
  }
  if (i %% 5000 == 0){cat(paste0("iteration: ", i, "\n"))}
} 

save(emoji.frequency, file="emoji_bbcnews.Rdata")


#####
#see which comments contain emoji
comments$hasemoji<-rowSums(emoji.frequency>1)
#percentage of comments containing emoji
nrow(comments[comments$hasemoji>0,])/nrow(comments) 

#### sentiment for each comment


#summarzing overall sums of emojis, not specific to each comment
emoji.counts <- colSums(emoji.frequency>0)
#emojis$Sentiment.score<-as.numeric(emojis$Sentiment.score)
all.emo <- cbind(emoji.counts, emojis)
all.emo<-all.emo[all.emo$emoji.counts>0,]
###overall sentiment by emoji only
#all.emo$Sentiment.score<-as.numeric(all.emo$Sentiment.score)
sent.score<-sum(all.emo$Sentiment.score*all.emo$emoji.counts/sum(all.emo$emoji.counts))

###sentiment of comments to each post
##comments.post<-split(comments, comments$status_id) ##to resources intensive
comments.emoji<-comments[comments$hasemoji>0,]

for (i in 1:nrow(comments.emoji)) {
  #find the corresponding row name in emoji.frequency
  whichline<-as.numeric(row.names(comments.emoji[i,]) )
  #map the emoji numbers with their corresponding sentiment score, stored as a two colomn matrix
  #petit.emo<-as.matrix(cbind(emoji.frequency[whichline,],emojis$Sentiment.score))
  
  petit.emo<-matrix(c(emoji.frequency[whichline,],emojis$Sentiment.score),nrow=842,ncol=2)
  ### throw away the rows with 0 number of emojis
  petit.emo<-petit.emo[petit.emo[,1]>0, , drop=FALSE] #used drop=FALSE so that it remains a matrix, rather than being converted into a vector
  ##calculate sentiment score, i use this transformation for repeated emojis: log(number of occurence)+1, so repetitions make a difference but not linear
  comments.emoji$sentscore[i]<-sum((log(petit.emo[,1])+1)*petit.emo[,2]/nrow(petit.emo)) #average of sentiment of all comments
}

## now we get an averaged sentiment score for each post
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


