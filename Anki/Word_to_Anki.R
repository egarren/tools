###ANKI script
rm(list=ls())
library(readtext)
library(purrr)
library(data.table)
library(stringr)
library(dplyr)

#import file
set<-"UW"
rt<-readtext(paste0(set,"_notes.docx"))
all_cards <-unlist(strsplit(rt$text,split="\n"))

# #update if needed (previously imported older notes)
# old<-readtext(paste0("./old/",set,"_notes.docx"))
# old_cards <-unlist(strsplit(old$text,split="\n"))
# all_cards<-setdiff(all_cards,old_cards)

colon_cards <-all_cards[grepl(":",all_cards)]
colon_first <-unlist(map(strsplit(colon_cards,":"),1))
colon_cards_filtered <- colon_cards[!grepl("→",colon_first)]
  
arrow_cards <-all_cards[grepl("→",all_cards) & !grepl(":",all_cards)]
arrow_cards <-c(arrow_cards,colon_cards[grepl("→",colon_first)])

equal_cards <-all_cards[grepl("=",all_cards)  & !grepl(":",all_cards)]
other_cards <-all_cards[!grepl("→",all_cards) & !grepl(":",all_cards)& !grepl("=",all_cards)]

#colon cards
anki_colon<-gsub(": ",": {{c1::", colon_cards_filtered)
anki_colon<-paste0(anki_colon,"}}")

#equal cards
anki_equal<-gsub("= ","= {{c1::", equal_cards)
anki_equal<-paste0(anki_equal,"}}")

#arrow cards
df<-plyr::ldply(strsplit(arrow_cards,"→"),rbind)
df<-cbind(arrow_cards,df)
if(ncol(df)==5){colnames(df)<-c("text","prompt","ans1","ans2","ans3")}
if(ncol(df)==6){colnames(df)<-c("text","prompt","ans1","ans2","ans3","ans4")}
parentheses <-grepl("\\(",df$ans1) & !grepl("\\)",df$ans1) & grepl("\\)",df$ans2)
if(length(parentheses)>0){
  df$ans1[parentheses]<-paste0(df$ans1[parentheses],"→",df$ans2[parentheses])
  df$ans2[parentheses]<-NA
}
parentheses <-grepl("\\(",df$ans1) & !grepl("\\)",df$ans1) & !grepl("\\(",df$ans2) & !grepl("\\)",df$ans2) &grepl("\\)",df$ans3)
if(length(parentheses)>0){
  df$ans1[parentheses]<-paste0(df$ans1[parentheses],"→",df$ans2[parentheses],"→",df$ans3[parentheses])
  df$ans2[parentheses]<-NA
  df$ans3[parentheses]<-NA
}

if(ncol(df)==6){
  parentheses <-grepl("\\(",df$ans1) & !grepl("\\)",df$ans1) & !grepl("\\(",df$ans2) & !grepl("\\)",df$ans2) & !grepl("\\)",df$ans3) & grepl("\\)",df$ans4)
  if(length(parentheses)>0){
    df$ans1[parentheses]<-paste0(df$ans1[parentheses],"→",df$ans2[parentheses],"→",df$ans3[parentheses],"→",df$ans4[parentheses])
    df$ans2[parentheses]<-NA
    df$ans3[parentheses]<-NA
    df$ans4[parentheses]<-NA
  }
}
#make parentheses another answer
parentheses2 <-grepl("\\(",df$ans1) & grepl("\\)",df$ans1)
if(length(parentheses2)>0){
  df$ans1_parentheses[parentheses2]<-str_match(df$ans1[parentheses2], "\\(([^()]*)\\)")[,2]
  rows_to_keep<-startsWith(df$ans1_parentheses,"NBS") | startsWith(df$ans1_parentheses,"tx")
  rows_to_keep[is.na(rows_to_keep)]<-F
  df$ans2[rows_to_keep]<-df$ans1_parentheses[rows_to_keep]
  df$ans1[rows_to_keep]<-gsub("\\(([^()]*)\\)","",df$ans1[rows_to_keep])
}
#categorize clozes
for(j in 1:3){
  rows_to_change<-startsWith(df[[paste0("ans",j)]]," ")
  rows_to_change[is.na(rows_to_change)]<-F
  df[[paste0("ans",j)]][rows_to_change]<-sub(" ","",df[[paste0("ans",j)]][rows_to_change])
}
df$code1<-"dx"
df$code2<-ifelse(is.na(df$ans2),NA,"tx")
df$code3<-ifelse(is.na(df$ans3),NA,"tx")
for(i in c("tx","dx","NBS","s/e","c/f","w/u","ppx","ddx","d/t")){
  df$code1<-ifelse(grepl(i,df$prompt), i, df$code1)
  df$prompt[grepl(i,df$prompt)]<-gsub(i,"",df$prompt[grepl(i,df$prompt)])
}
for(j in 1:3){
  for(i in c("tx","dx","NBS","s/e","c/f","w/u","ppx","ddx","d/t","RF")){
    rows_to_change<-startsWith(df[[paste0("ans",j)]],i)
    rows_to_change[is.na(rows_to_change)]<-F
    df[[paste0("code",j)]]<-ifelse(rows_to_change, i, df[[paste0("code",j)]])
    df[[paste0("ans",j)]][rows_to_change]<-gsub(i,"",df[[paste0("ans",j)]][rows_to_change])
  }
}
#make cloze column
df$anki<-paste0(df$prompt,"<br>",df$code1,": {{c1::",df$ans1,"}}")
df$anki[!is.na(df$ans2)]<-paste0(df$anki[!is.na(df$ans2)],"<br>",df$code2[!is.na(df$ans2)],": {{c2::",df$ans2[!is.na(df$ans2)],"}}")
df$anki[!is.na(df$ans3)]<-paste0(df$anki[!is.na(df$ans3)],"<br>",df$code3[!is.na(df$ans3)],": {{c3::",df$ans3[!is.na(df$ans3)],"}}")

# if first four words are same, combine into one card
df$first6<-substr(df$text, start = 1, stop = 6)
# df$id<-consecutive_id(df$first6)
df2<-df %>% 
  mutate(id = consecutive_id(first6)) %>%
  summarise(anki=paste0(anki,collapse="<br><br>"), .by = c(id, first6)) %>% 
  select(-id)

anki<-c(df2$anki,anki_equal,anki_colon)
write.table(anki,paste0(set,"_anki.txt"),col.names=F,row.names=F,quote=F,sep="\t")
write.table(other_cards,paste0(set,"_other.txt"),col.names=F,row.names=F,quote=F,sep="\t")

#add following to top of each file
#separator:tab
#html:true