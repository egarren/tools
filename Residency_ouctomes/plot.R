rm(list=ls())
library(readxl)
library(tidyr)
library(stringr)

setwd("~/Documents/7. M4/Residency application/Choosing program")

##HMS - IM match
df<-read_excel("choosing_program.xlsx",sheet=1)
df$tot<-rowSums(df[,c(2:5)])
df2<-df
df2[,c(2:5)]<- df2[,c(2:5)] / df$tot
df3<-gather(df2, "MGH","BWH","BIDMC","Stanford", key="Program", value = "Freq")
ggplot(df3, aes(Year, Freq, colour = Program, group = Program)) +
  geom_line()

#HMS MD-PhD alum
#specialty trends
df<-read_excel("choosing_program.xlsx",sheet=2)
df <- df %>% fill(Year, .direction = "down")
num<-as.data.frame(table(df$Year))
colnames(num)<-c("Year","Tot")
tab<-as.data.frame(table(df$Year,df$Specialty))
colnames(tab)<-c("Year","Specialty","Count")
tab<-left_join(tab,num,by="Year")
tab$Freq<-tab$Count / tab$Tot
top <- names(sort(table(df$Specialty), decreasing = TRUE)[1:5])
ggplot(tab[tab$Specialty %in% top,], aes(Year, Freq, colour = Specialty, group = Specialty)) +
  geom_line()

#program trends for IM
df$Program[df$Program=="Stnaford"]<-"Stanford"
df2<-df[df$Specialty =="IM",]
num<-as.data.frame(table(df2$Year))
colnames(num)<-c("Year","Tot")
tab<-as.data.frame(table(df2$Year,df2$Program))
colnames(tab)<-c("Year","Program","Count")
tab<-left_join(tab,num,by="Year")
tab$Freq<-tab$Count / tab$Tot
top <- names(sort(table(df2$Program), decreasing = TRUE)[1:5])
ggplot(tab[tab$Program %in% top,], aes(Year, Freq, colour = Program, group = Program)) +
  geom_line()

##MGH PSP alum
#specialty trends
df<-read_excel("choosing_program.xlsx",sheet=3)
df <- df %>% fill(Year, .direction = "down")
num<-as.data.frame(table(df$Year))
colnames(num)<-c("Year","Tot")
tab<-as.data.frame(table(df$Year,df$Fellowship))
colnames(tab)<-c("Year","Fellowship","Count")
tab<-left_join(tab,num,by="Year")
tab$Freq<-tab$Count / tab$Tot
top <- names(sort(table(df$Fellowship), decreasing = TRUE)[1:5])
ggplot(tab[tab$Fellowship %in% top,], aes(Year, Freq, colour = Fellowship, group = Fellowship)) +
  geom_line()
MGH_fellowship<-tab

#program trends for Onc
df2<-df[df$Fellowship =="Onc",]
num<-as.data.frame(table(df2$Year))
colnames(num)<-c("Year","Tot")
tab<-as.data.frame(table(df2$Year,df2$Program))
colnames(tab)<-c("Year","Program","Count")
tab<-left_join(tab,num,by="Year")
tab$Freq<-tab$Count / tab$Tot
top <- names(sort(table(df2$Program), decreasing = TRUE)[1:5])
ggplot(tab[tab$Program %in% top,], aes(Year, Freq, colour = Program, group = Program)) +
  geom_line()
MGH_onc<-tab

## BWH SIR alum
df<-read_excel("choosing_program.xlsx",sheet=4)
df <- df %>% fill(Year, .direction = "down")
df$fellowship2 <- str_extract(df$Fellowship, "^[^(]+") %>% str_trim()
df$Program <- str_extract(df$Fellowship, "(?<=\\().+?(?=\\))")
df$position2<-NA
for(i in c("Prof","Fellow","Instructor")){
  df$position2[grepl(i,df$Position)]<-i
}
unique(df$fellowship2)
df$fellowship2[df$fellowship2 %in% c("Oncology","Oncology DFCI","Heme/Onc","Hematology")] <- "Onc"
df$fellowship2[df$fellowship2 %in% c("Cardiology")] <- "Card"
df$fellowship2[df$fellowship2 %in% c("Infectious Diseases","ID","Infectious Disease")] <- "ID"
df$fellowship2[df$fellowship2 %in% c("Gastroenterology")] <- "GI"
df$fellowship2[df$fellowship2 %in% c("Rheumatology","Allergy/Rheumatology")] <- "Rheum"
df$fellowship2[df$fellowship2 %in% c("Endocrine","Endocrinology")] <- "Endo"
df$fellowship2[df$fellowship2 %in% c("Allergy","Allergy/Immunology","Allergy & Immunology")] <- "All"
df$fellowship2[df$fellowship2 %in% c("Pulmonary & Critical Care","Critical Care")] <- "Pulm"
unique(df$Program)
df$Program[df$Program %in% c("Memorial Sloan Kettering","MSKCC","MSK")]<-"MSK"
df$Program[df$Program %in% c("MD Anderson Cancer Center","MD Anderson")]<-"MD_And"
df$Program[df$Program %in% c("BWH/MGH","BWH/MGH/BIDMC" ,"MGH/BWH","Partners","MGH/BCH" )]<-"Harvard"
df$Program[df$Program %in% c("Penn","Penn/CHOP","University of Pennsylvania")]<-"Penn"
df$Program[df$Program %in% c("DCFI")]<-"DFCI"
df$Program[df$Program %in% c("U of Washington")]<-"UW"
df$Program[df$Program %in% c("U of Michigan","U Michigan")]<-"Michigan"

#specialty trends
num<-as.data.frame(table(df$Year))
colnames(num)<-c("Year","Tot")
tab<-as.data.frame(table(df$Year,df$fellowship2))
colnames(tab)<-c("Year","Fellowship","Count")
tab<-left_join(tab,num,by="Year")
tab$Freq<-tab$Count / tab$Tot
top <- names(sort(table(df$fellowship2), decreasing = TRUE)[1:5])
ggplot(tab[tab$Fellowship %in% top,], aes(Year, Freq, colour = Fellowship, group = Fellowship)) +
  geom_line()
BWH_fellowship<-tab

#program trends for Onc
df2<-df[df$fellowship2 =="Onc",]
num<-as.data.frame(table(df2$Year))
colnames(num)<-c("Year","Tot")
tab<-as.data.frame(table(df2$Year,df2$Program))
colnames(tab)<-c("Year","Program","Count")
tab<-left_join(tab,num,by="Year")
tab$Freq<-tab$Count / tab$Tot
top <- names(sort(table(df2$Program), decreasing = TRUE)[1:5])
ggplot(tab[tab$Program %in% top,], aes(Year, Freq, colour = Program, group = Program)) +
  geom_line()
BWH_onc<-tab

##MGH vs BWH
#onc fellowship choice
MGH_fellowship$IM_loc<-"MGH"
BWH_fellowship$IM_loc<-"BWH"
df<-rbind(MGH_fellowship,BWH_fellowship)
df$Year<-as.numeric(as.character(df$Year))
ggplot(df[df$Fellowship=="Onc",], aes(Year, Freq, colour = IM_loc, group = IM_loc)) +
  geom_line()

#onc placement in DFCI
MGH_onc$IM_loc<-"MGH"
BWH_onc$IM_loc<-"BWH"
df<-rbind(MGH_onc,BWH_onc)
df$Year<-as.numeric(as.character(df$Year))
ggplot(df[df$Program=="MSK",], aes(Year, Freq, colour = IM_loc, group = IM_loc)) +
  geom_line()

##Heme/Onc incoming classes
df<-read_excel("choosing_program.xlsx",sheet=5)
df <- df %>% fill(Fellowship, .direction = "down")
df2<-df[df$Fellowship=="MSK",]
df2$tot<-rowSums(df2[,c(3:9)])
df2[,c(3:9)]<- df2[,c(3:9)] / df2$tot
df3<-df2[,c(2:9)]
df3<-gather(df3, colnames(df3)[2:8], key="Program", value = "Freq")
ggplot(df3, aes(Year, Freq, colour = Program, group = Program)) +
  geom_line()
