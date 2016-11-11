## read the status data and process the reactions ####

media<-read.csv("C:/Users/Ye Tian/Dropbox/teaching/Corpus course Nov 2016/Resources/Facebook/bbcnews_facebook_statuses.csv",sep="|")

reactions<-media[,7:15]
summary(reactions)

## fit a model using different reactions to predict sharing
allfit<-lm(num_shares ~ num_likes + num_loves + num_wows + num_hahas + num_sads + num_angrys,data=media)
summary(allfit)

##cluster the reaction patterns ####
media$num_likes<-media$num_likes/media$num_reactions
media$num_loves<-media$num_loves/media$num_reactions
media$num_wows <-media$num_wows/media$num_reactions
media$num_hahas<-media$num_hahas/media$num_reactions
media$num_sads <-media$num_sads/media$num_reactions
media$num_angrys<-media$num_angrys/media$num_reactions

set.seed(20)
reaction.cluster<-kmeans(media[,10:15],4,nstart=20)
reaction.cluster<-kmeans(media[,10:15],3,nstart=20)

reaction.cluster
media$cluster<-reaction.cluster$cluster

## process the posts ####
removeURL <- function(x) gsub("(http|\\://)[[:alnum:]]*", "", x)
media$status_message <- sapply(media$status_message,removeURL) 

write.csv(media,"C:/Users/Ye Tian/Dropbox/teaching/Corpus course Nov 2016/Resources/Facebook/bbcnews_facebook_statuses.csv",row.names=F)


