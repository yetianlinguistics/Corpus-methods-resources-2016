## read the status data and process the reactions ####

media<-read.csv(".../your_facebook_statuses.csv",sep="|")

reactions<-media[,7:15]
summary(reactions)

## fit a model using different reactions to predict sharing ####
allfit<-lm(num_shares ~ num_likes + num_loves + num_wows + num_hahas + num_sads + num_angrys,data=media)
summary(allfit)

##cluster the reaction patterns ####
media$num_likes<-media$num_likes/media$num_reactions
media$num_loves<-media$num_loves/media$num_reactions
media$num_wows <-media$num_wows/media$num_reactions
media$num_hahas<-media$num_hahas/media$num_reactions
media$num_sads <-media$num_sads/media$num_reactions
media$num_angrys<-media$num_angrys/media$num_reactions

## cluster reation profiles with k-means####
set.seed(20)
reaction.cluster<-kmeans(media[,10:15],5,nstart=20) #reduce or increase number of clusters if this is too many or two few

reaction.cluster
media$cluster<-reaction.cluster$cluster

## write the file ####
removeURL <- function(x) gsub("(http|\\://)[[:alnum:]]*", "", x)
media$status_message <- sapply(media$status_message,removeURL) 

write.csv(media,"/your_facebook_statuses.csv",row.names=F)


