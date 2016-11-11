
## search in the SWB tagged corpus####
## this is a subsection of the entire SWB corpus, PoS tagged, parsed and annotated for dialogue acts
rm(list=ls(all=TRUE))
setwd("C:/Users/Ye Tian/Dropbox/DUEL my own/Switchboard_Lund_maptask_BNC/CORPUS tagged_treebank_swda/swda")
switchlist<-list.files(path = "C:/Users/Ye Tian/Dropbox/DUEL my own/Switchboard_Lund_maptask_BNC/CORPUS tagged_treebank_swda/swda",
                       pattern=".csv",recursive = TRUE, include.dirs = TRUE, no.. = TRUE) #getting only "words" files from all subfolders

swda_search<-function(pattern){
search<-list()
for (i in 1:length(switchlist)){
  words <- read.csv(switchlist[i],header=TRUE)
  result<-words[grep(pattern,words$text,ignore.case=T),] ##this example search for discourse markers
  search[[i]]<-result
}
search<-do.call(rbind,search)
}

search<-swda_search("\\{D .+\\}")
search<-swda_search("\\bchocolate\\b")

write.csv(search,file="your path/yourfile.csv")


## search in act_tag (dialogue act) ####
# to see the list of dialogue act tags
list<-read.csv("C:/Users/Ye Tian/Dropbox/DUEL my own/Switchboard_Lund_maptask_BNC/CORPUS tagged_treebank_swda/act tag list.csv",header=T)

## dialogue search function
swda_act_search<-function(pattern){
search<-list()
for (i in 1:length(switchlist)){
  words <- read.csv(switchlist[i],header=TRUE)
  result<-words[grep(pattern,words$act_tag,ignore.case=T),] ##counting number of whole words, nn is negative answers
  search[[i]]<-result
}
search<-do.call(rbind,search)
}

search_act<-swda_act_search("wh")

## search PoS tags ####
swda_pos_search<-function(pattern){
  search<-list()
  for (i in 1:length(switchlist)){
    words <- read.csv(switchlist[i],header=TRUE)
    result<-words[grep(pattern,words$pos,ignore.case=T),] ##counting number of whole words, nn is negative answers
    search[[i]]<-result
  }
  search<-do.call(rbind,search)
}

search_pos<-swda_pos_search("\\btalk/VB\\b")

