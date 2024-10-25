rm(list=ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(xlsx)

#import data and metadata
metadata<-read_excel("Mouse Record.xlsx",guess_max=10000) 
metadata$mouse<-as.character(metadata$mouse)
plate = "plate1"
raw<-read_excel(paste0(plate,"_data.xlsx"))
layout<-read_excel(paste0(plate,"_layout.xlsx"),sheet="Sheet2")
raw<-left_join(raw,layout,by="ID")
raw<-left_join(raw,metadata[,c("mouse","tx2")],by=c("sample"="mouse"),keep=F)

#analysis
ag="ssDNA"
df<-raw[raw$ag==ag,]

#STD curve
std1_conc = 8000 #STD1 concentration in ng/mL
dil = 5 #dilution fold
std_df<-data.frame(sample=c("STD1","STD2","STD3","STD4","STD5","STD6","STD7","BLANK"),
                   dilution=seq(from=0,to=7))
std_df$conc = std1_conc / (dil ^ std_df$dilution)
std_df$conc[std_df$sample=="BLANK"] = 0
df2<-df[df$sample %in% std_df$sample,]
df2<-left_join(df2,std_df,by="sample")
y=log(df2$conc,base=10)
x=df2$Data
plot(y ~ x)
fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), data = data.frame(x, y))
summary(fit)
lines(seq(0, 4, length.out = 100), 
      predict(fit, newdata = data.frame(x = seq(0, 4, length.out = 100))))

#calculate sample concentrations
sample_dil=20
df$conc_ng<- sample_dil * 10^(coef(fit)[["Asym"]]/(1+exp((coef(fit)[["xmid"]]-df$Data)/coef(fit)[["scal"]])))
df$conc_ug<-df$conc_ng / 1000
ggbarplot(df,x="tx2",y="conc_ug",add=c("mean_se","jitter"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle(ag)
ggbarplot(df,x="tx2",y="Data",add=c("mean_se","jitter"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle(ag)

#export
write.xlsx(as.data.frame(df[order(df$tx2),]),paste0(plate,"_",ag,"_analyzed.xlsx"),row.names=F,showNA=F)

