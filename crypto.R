library(readxl)
library(lubridate)
library(dplyr)
library(xlsx)
library(crypto2)

#download historical crypto data
coins <- crypto_list(only_active=TRUE)
coins2<-coins[coins$symbol %in% c("BTC","ETH"),]
coins2<-coins2[1:2,]
coin_hist <- crypto_history(coins2,start_date="20210101", end_date="20241018",interval="1h", finalWait=FALSE)
coin_hist2<-coin_hist[,c("symbol","timestamp","close")]

#import data
bf<-read.csv("blockfi2.csv")
bf$date<-as.Date(gsub(" .*$", "",bf$Confirmed.At),format="%m/%d/%y")
bf$time<-gsub("^\\S+ ", "",bf$Confirmed.At)
bf$date_time<-strptime(paste(bf$date,bf$time), format="%Y-%m-%d %H:%M")
bf$time_round<-round(bf$date_time,units="hours")

#merge data
coin_hist2$time_round<-round(coin_hist2$timestamp,units="hours")
bf<-left_join(bf,coin_hist2[,c("symbol","time_round","close")],by=c("Cryptocurrency"="symbol","time_round"="time_round"))
bf$USD<-bf$Amount * bf$close
bf$USD[bf$Cryptocurrency=="GUSD"]<-bf$Amount[bf$Cryptocurrency=="GUSD"]
bf2<-bf[,c("Cryptocurrency","Transaction.Type","Amount","date_time","close","USD")]

#tax stats
table(bf2$Transaction.Type)
bf2 %>%
  group_by(Transaction.Type) %>%
  summarise(sum=sum(USD))

bf2[bf2$Transaction.Type %in% c("Interest Payment","Crypto Transfer"),] %>%
  group_by(Cryptocurrency) %>%
  summarise(sum=sum(Amount))

#export
bf3<-bf2[bf2$Transaction.Type %in% c("BIA Withdraw","Crypto Transfer"),]
tt_buy<-bf3[bf3$Transaction.Type=="Crypto Transfer",]
colnames(tt_buy)<-c("Received Asset","Type","Received Amount","Date","Market Value","Sent Amount")
tt_buy$Type="Buy"
tt_buy$`Sent Asset`="USD"
tt_buy$`Market Value Currency`="USD"
tt_sell<-bf3[bf3$Transaction.Type=="BIA Withdraw",]
colnames(tt_sell)<-c("Sent Asset","Type","Sent Amount","Date","Market Value","Received Amount")
tt_sell$Type="Sell"
tt_sell$`Received Asset`="USD"
tt_sell$`Market Value Currency`="USD"
tt<-rbind(tt_sell,tt_buy)
tt$`Fee Asset`=NA
tt$`Fee Amount`=NA
tt<-tt[,c("Date","Type","Sent Asset","Sent Amount","Received Asset","Received Amount","Fee Asset","Fee Amount","Market Value Currency","Market Value")]
write.csv(tt,file="turbotax_crypto_export.csv",row.names=F,na="")
