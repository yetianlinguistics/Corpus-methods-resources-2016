##simple search engine ####
#using the entire SWB corpus rather than the section tagged with dialogue act
## read in all transcriptions
all <-read.csv(".../alltrans_data.csv",header=T)

##search for utterances with a particular word or phrase
SWB_search<-function(pattern){
search<-all[grep(pattern,alltrans$Utterance,ignore.case=T),]
}

search<-SWB_search("\\belection\\b") #\\b mark word boundaries

## get preceding and following turns ####
## the following script find the turn that the target utterance is in, get the entire turn out
## then it find the previous turns and the following turns from the interlocutor, and get them out
## you can change how many previous or following turns you want

all$turnID<-paste(all$file,all$turn.num)

search<-all[grep("\\<ugly\\>",all$Utterance,ignore.case=T),] ##\\<word\\> is another way of searching for whole words
search<-all[grep("content with|pretty content|very content|re content|somewhat content|am content|is content",all$Utterance,ignore.case=T),]
search$turnID<-paste(search$file,search$turn.num)

for (i in 1:nrow(search)) {
  search$turn.0[i]<-paste(all$speaker[which(all$turnID==search$turnID[i])[1]],":",paste(all$Utterance[which(all$turnID==search$turnID[i])],collapse=";"),sep = '')
  ifelse(search$turn.num[i]>3,
         search$turn.minus3[i]<-paste(all$speaker[which(all$turnID==paste(search$file[i],(search$turn.num[i]-3)))[1]],":",paste(all$Utterance[which(all$turnID==paste(search$file[i],(search$turn.num[i]-3)))],collapse = ';'),sep=""),
         search$turn.minus3[i]<-"")
  ifelse(search$turn.num[i]>2,
         search$turn.minus2[i]<-paste(all$speaker[which(all$turnID==paste(search$file[i],(search$turn.num[i]-2)))[1]],":",paste(all$Utterance[which(all$turnID==paste(search$file[i],(search$turn.num[i]-2)))],collapse = ';'),sep=""),
         search$turn.minus2[i]<-"")
  ifelse(search$turn.num[i]>1,
         search$turn.minus1[i]<-paste(all$speaker[which(all$turnID==paste(search$file[i],(search$turn.num[i]-1)))[1]],":",paste(all$Utterance[which(all$turnID==paste(search$file[i],(search$turn.num[i]-1)))],collapse = ';'),sep=""),
         search$turn.minus1[i]<-"")
  
  ifelse(search$turn.num[i]<max(all$utt.num[all$file==search$file[i]]),
         search$turn.1[i]<-paste(all$speaker[which(all$turnID==paste(search$file[i],(search$turn.num[i]+1)))[1]],":",paste(all$Utterance[which(all$turnID==paste(search$file[i],(search$turn.num[i]+1)))],collapse = ';'),sep=""),
         search$turn.1[i]<-"")  
  ifelse(search$turn.num[i]<(max(all$utt.num[all$file==search$file[i]]-1)),
         search$turn.2[i]<-paste(all$speaker[which(all$turnID==paste(search$file[i],(search$turn.num[i]+2)))[1]],":",paste(all$Utterance[which(all$turnID==paste(search$file[i],(search$turn.num[i]+2)))],collapse = ';'),sep=""),
         search$turn.2[i]<-"")
}
search<-subset(search, select=c("ID","Start","End","speaker","Utterance","turnID","turn.minus3","turn.minus2","turn.minus1","turn.0","turn.1","turn.2"))

write.csv(search,".../yourfile.csv",row.names=F)