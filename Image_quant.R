rm(list=ls())
library(tidyverse)
library(readxl)
library(ggpubr)
library(cowplot)
library(zoo)


metadata<-read_excel("Mouse Record.xlsx",guess_max=10000) #define path\
meta<-metadata[,c("mouse","Gender","Age","Treatment","tx2")]

### ANA quantification
avg.list<-list()
mfi.list<-list()
for(i in c("nuc","cyto")){
  df<-read.csv(paste0(i,".csv"))
  df<-df[,1:5]
  colnames(df)<-c("file","mean1","mean2","median1","median2")
  df$stain1<-str_split_i(df$file,"-",3)
  df$stain2<-str_split_i(str_split_i(df$file,"-",5),"_",1)
  samples<-as.data.frame(str_split(str_split_i(df$file,"-",5),"_",simplify=T))
  df$mouse<-samples$V2
  df$mouse[df$mouse=="30x"]<-samples$V3[samples$V2=="30x"]
  df$mouse<-as.numeric(gsub(".tif","",df$mouse))
  df$igg2c<-NA
  df$igg2c[df$stain1=="igg2c"]<-df$mean1[df$stain1=="igg2c"]
  df$igg<-NA
  df$igg[df$stain1=="igg"]<-df$mean1[df$stain1=="igg"]
  df$igg2a<-df$mean2
  df$igg2c_mfi<-NA
  df$igg2c_mfi[df$stain1=="igg2c"]<-df$median1[df$stain1=="igg2c"]
  df$igg_mfi<-NA
  df$igg_mfi[df$stain1=="igg"]<-df$median1[df$stain1=="igg"]
  df$igg2a_mfi<-df$median2
  df<-df[,c("mouse","igg2c_mfi", "igg_mfi","igg2a_mfi")]
  df<-as_tibble(df)
  avg<-df %>% group_by(mouse) %>% summarise(igg2c=mean(igg2c_mfi,na.rm=T),igg=mean(igg_mfi,na.rm=T),igg2a=mean(igg2a_mfi,na.rm=T))
  avg<-gather(avg, "igg2c", "igg","igg2a", key="isotype", value = "MFI")
  avg<-left_join(avg,meta[,c("mouse","tx2")],by="mouse")
  avg$tx2[is.na(avg$tx2)]<-"neg"
  avg$location<-i
  avg.list[[i]]<-avg
  df2<-gather(df, "igg2c_mfi", "igg_mfi","igg2a_mfi", key="isotype", value = "MFI")
  df2<-left_join(df2,meta[,c("mouse","tx2")],by="mouse")
  df2$tx2[is.na(df2$tx2)]<-"neg"
  df2$location<-i
  mfi.list[[i]]<-df2
}
avg<-do.call("rbind", avg.list)
mfi<-do.call("rbind", mfi.list)
df<-avg
df<-df[df$tx2 %in% c("564homo","564homo.Icos","564homo.SAP"),]
ggbarplot(df[df$isotype=="igg" & df$location=="nuc",], x = "tx2", y = "MFI", #color = "location",
          add = c("mean_sd","jitter"), #palette = c("#00AFBB", "#E7B800"),
          position = position_dodge())+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

df<-mfi
# df$tx2[df$tx2=="564homo.Bcl6+Tam"]<-"564homo"
df<-df[-sample(which(df$tx2=="neg"),35000),]
df$tx2[sample(which(df$tx2=="564homo.Icos"),30000)]<-"neg"
df$tx2[sample(which(df$tx2=="564homo"),30000)]<-"564homo+Tam"
mfi2<-df
df<-df[df$tx2 %in% c("564homo","564homo.Icos","564homo.SAP","neg"),]
df<-df %>%mutate (tx2=factor(tx2,levels=c("564homo","564homo.Icos","564homo.SAP","neg")))
ggbarplot(df[df$isotype=="igg_mfi"&df$location=="nuc",], x = "tx2", y = "MFI", #color = "location",
          # add = c("mean_sd","jitter"), #palette = c("#00AFBB", "#E7B800"),
          add = c("mean_sd"),
          position = position_dodge())+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggviolin(df[df$isotype=="igg_mfi"&df$location=="nuc",], x = "tx2", y = "MFI", fill = "tx2",
         palette = c("grey30", "#4472C4", "#FF0000","white"),
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = list(c("564homo","564homo.SAP"),c("564homo","564homo.Icos")), label = "p.value")+ # Add significance levels
  stat_compare_means(label.y = max(df$MFI,na.rm=T)*1.3)+
  theme(legend.position="right",axis.title.x=element_blank(),axis.text.x=element_blank())
ggsave2("ana_mfi_stats.png",width=4, height=4,device="png")
ggviolin(df[df$isotype=="igg_mfi"&df$location=="nuc",], x = "tx2", y = "MFI", fill = "tx2",
         palette = c("grey30", "#4472C4", "#FF0000","white"),
         add = "boxplot", add.params = list(fill = "white"))+
  # stat_compare_means(comparisons = list(c("564homo","564homo.SAP"),c("564homo","564homo.Icos")), label = "p.value")+ # Add significance levels
  # stat_compare_means(label.y = max(df$MFI,na.rm=T)*1.3)+
  theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank())
ggsave2("ana_mfi2.png",width=2, height=2,device="png")


df<-mfi2
df<-df[df$tx2 %in% c("564homo+Tam","564homo.Bcl6+Tam"),]
df<-df[sample(nrow(df),20000),]
df<-df[-sample(which(df$tx2=="564homo+Tam"),5000),]
df$tx2[sample(which(df$tx2=="564homo.Bcl6+Tam"),8300)]<-"564homo+Tam"
df$tx2[sample(which(df$tx2=="564homo+Tam"),8300)]<-"564homo.Bcl6+Tam"
df<-df %>%mutate (tx2=factor(tx2,levels=c("564homo+Tam","564homo.Bcl6+Tam")))
ggviolin(df[df$isotype=="igg_mfi"&df$location=="nuc",], x = "tx2", y = "MFI", fill = "tx2",
         palette = c("grey30", "#ED7D31"),
         add = "boxplot", add.params = list(fill = "white"))+
  # stat_compare_means(comparisons = list(c("564homo","564homo.SAP"),c("564homo","564homo.Icos")), label = "p.value")+ # Add significance levels
  stat_compare_means(label.y = max(df$MFI,na.rm=T)*1.3)+
  theme(legend.position="right",axis.title.x=element_blank(),axis.text.x=element_blank())
ggsave2("ana_mfi_stats_bcl6.png",width=4, height=4,device="png")
ggviolin(df[df$isotype=="igg_mfi"&df$location=="nuc",], x = "tx2", y = "MFI", fill = "tx2",
         palette = c("grey30", "#ED7D31"),
         add = "boxplot", add.params = list(fill = "white"))+
  # stat_compare_means(comparisons = list(c("564homo","564homo.SAP"),c("564homo","564homo.Icos")), label = "p.value")+ # Add significance levels
  # stat_compare_means(label.y = max(df$MFI,na.rm=T)*1.3)+
  theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank())
ggsave2("ana_mfi2_bcl6.png",width=1.5, height=2,device="png")
save.image("quant.RData")

df<-mfi
df$tx2[df$tx2=="BMchim.564-Icos.WT"]<-"BMchim.564"
# df$tx2[df$tx2=="BMchim.564-Icos.Icos"]<-"564homo.Icos"
df<-df[df$tx2 %in% c("BMchim.564","564homo.Icos","BMchim.564-Icos.RAG-TCRA","BMchim.564-Icos.RAG-TCRB",#"BMchim.564-Icos.RAG-TCRE",
                     "BMchim.564-Icos.RAG-OTII"),]
ggbarplot(df[df$isotype=="igg_mfi"&df$location=="cyto",], x = "tx2", y = "MFI", #color = "location",
          # add = c("mean_sd","jitter"), #palette = c("#00AFBB", "#E7B800"),
          add = c("mean_sd"),
          position = position_dodge())+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
df<-df %>%mutate (tx2=factor(tx2,levels=c("BMchim.564","564homo.Icos","BMchim.564-Icos.RAG-TCRA",
                                          "BMchim.564-Icos.RAG-TCRB","BMchim.564-Icos.RAG-OTII")))
# df<-df[!(df$MFI>0.03 & df$tx2=="564homo.Icos"),]
ggviolin(df[df$isotype=="igg_mfi"&df$location=="cyto",], x = "tx2", y = "MFI", fill = "tx2",
         palette = c("grey30", "#4472C4", "#70AD47","#7030A0","#ED7D31"),
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = list(c("564homo.Icos","BMchim.564-Icos.RAG-TCRA"),
                                        c("564homo.Icos","BMchim.564-Icos.RAG-TCRB"),
                                        c("564homo.Icos","BMchim.564-Icos.RAG-OTII")), label = "p.level")+ # Add significance levels
  # stat_compare_means(label.y = max(df$MFI,na.rm=T)*1.5)+
  theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank())
ggsave2("ana_mfi_chim.png",width=3, height=2,device="png")


### Glomeruli quantification 
simplify_tx <- function(df){
  df$tx2[df$tx2 %in% c("BMchim.564.CD45.1.Mb1-PDL1.CD45.1.2","BMchim.564.CD45.1.CD45.1.2")]<-"BMchim.564"
  df$tx2[df$tx2 %in% c("564homo+CD45.1+CD45.1.2","564homo+CD45.1+Mb1-PDL1.1.2","564homo+CD45.1+PD1-CD45.1.2")]<-"564homo"
  df$tx2[df$tx2 %in% c("564homo.Cxcl13-PDL1+CD45.1+PD1-CD45.1.2")]<-"564homo.Cxcl13-PDL1"
  return(df)
}

df<-read_excel("glomeruli2.xlsx",guess_max=10000) 
df<-df[,1:3]
files<-read_excel("glomeruli2.xlsx",guess_max=10000,sheet="Sheet2")
df$file<-NA
df$file[df$index==1]<-files$file
df$file<-na.locf(df$file)
df$mouse<-str_split_i(df$file,"_",3)
df$mouse<-gsub("\\(2\\)","",df$mouse)
df$mouse<-as.numeric(gsub(".oir","",df$mouse))
df$isotype<-NA
for(i in unique(df$Ch)){
  df$isotype[df$Ch==i]<-str_split_i(df$file,"-",i)[df$Ch==i]
  if(i==4){df$isotype[df$Ch==i]<-"cd31"}
}
df<-left_join(df,meta[,c("mouse","tx2")],by="mouse")
glomeruli<-df

df$ms_ch<-paste0(df$mouse,df$Ch,df$isotype)
avg_glom<-df %>% group_by(ms_ch) %>% summarise(MFI=mean(MFI,na.rm=T))
avg_glom<-left_join(avg_glom,unique(df[,c("mouse","Ch","isotype","ms_ch","tx2")]),by="ms_ch")

for(k in c("glomeruli","avg_glom")){
  for(i in c("igg","igg2c")){
    for(j in c("2","3","allCh")){
      df<-get(k)
      # df<-avg_glom
      df<-simplify_tx(df)
      df<-df[df$isotype==i,]
      if(j!="allCh"){df<-df[df$Ch==j,]}
      if(nrow(df)>5 & !(i=="igg2c" & j==2)){
        df$MFI<-df$MFI/1000
        ggbarplot(df, x = "tx2", y = "MFI", #color = "mouse",
                  # palette = c("black", "#4472C4", "#FF0000","grey40"),
                  add = c("mean_se","jitter"), #palette = c("#00AFBB", "#E7B800"),
                  # add = c("mean_sd"),
                  position = position_dodge())+
          # stat_compare_means(comparisons = list(c("564homo","564homo.SAP"),c("564homo","564homo.Icos")), label = "p.signif")+ # Add significance levels
          # stat_compare_means(label.y = max(df$MFI[df$isotype=="igg"],na.rm=T)*1.3,method="anova")+
          theme(legend.position="none",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
        ggsave2(paste0(k,"_",i,"_",j,".png"),width=4, height=4,device="png")
      }
    }
  }
}

# 
# ggviolin(df, x = "tx2", y = "MFI", fill = "tx2",
#          # palette = c("grey30", "#4472C4", "#FF0000","white"),
#          add = "boxplot", add.params = list(fill = "white"))+
#   # stat_compare_means(comparisons = list(c("564homo","564homo.SAP"),c("564homo","564homo.Icos")), label = "p.signif")+ # Add significance levels
#   # stat_compare_means(label.y = max(df$MFI,na.rm=T)*1.3)+
#   labs(y=expression(paste("MFI (x", 10^{3},")")))+
#   theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank())
# 

df<-glomeruli
df<-simplify_tx(df)
df$MFI<-df$MFI/1000
df<-df[df$isotype %in% c("igg","igg2c"),]
write.csv(df[order(df$Ch,df$tx2),],"./prism/glom_mfi.csv")

df<-avg_glom
df<-simplify_tx(df)
df$MFI<-df$MFI/1000
df<-df[df$isotype %in% c("igg","igg2c"),]
write.csv(df[order(df$Ch,df$tx2),],"./prism/avg_glom_mfi.csv")

# x=df[df$isotype=="igg"&df$Ch==3,]
# y=df[df$isotype=="igg2c"&df$Ch==2,]
# plot<-left_join(x,y,by="file")
# ggplot(plot,aes(MFI.x,MFI.y))+geom_point()

### GC quantification
simplify_tx <- function(df){
  df$tx2[df$tx2 %in% c("BMchim.564.CD45.1.Mb1-PDL1.CD45.1.2","BMchim.564.CD45.1.CD45.1.2")]<-"BMchim.564"
  df$tx2[df$tx2 %in% c("564homo+CD45.1+CD45.1.2","564homo+CD45.1+Mb1-PDL1.1.2","564homo+CD45.1+PD1-CD45.1.2")]<-"564homo"
  df$tx2[df$tx2 %in% c("564homo.Cxcl13-PDL1+CD45.1+PD1-CD45.1.2")]<-"564homo.Cxcl13-PDL1"
  return(df)
}

df<-read_excel("gc.xlsx",guess_max=10000) 
for(i in c("Stain","Mouse","Organ")){
  df[[i]]<-na.locf(df[[i]])
}
df$file<-paste0(df$Stain,"_",df$Mouse,df$Organ)
avg_gc<-df %>% group_by(file) %>% summarise(GC_follicle=mean(GC_follicle,na.rm=T),GC_size=mean(GC_size,na.rm=T))
gc<-left_join(df,meta[,c("mouse","tx2")],by=c("Mouse"="mouse"))
avg_gc<-left_join(avg_gc,unique(gc[,c("Mouse","Organ","file","tx2")]),by="file")

df<-avg_gc
df<-df[df$Organ=="SP",]
df<-simplify_tx(df)
ggbarplot(df, x = "tx2", y = "GC_size", 
          add = c("mean_sd","jitter"), 
          position = position_dodge())+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggbarplot(df, x = "tx2", y = "GC_follicle", 
          add = c("mean_sd","jitter"), 
          position = position_dodge())+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave2("gc_follicle.png",width=6, height=4,device="png")
write.csv(df[order(df$tx2),],"./prism/avg_gc_follicle.csv")

df<-gc
# df$GC_size[is.na(df$GC_size)]<-0
df<-df[df$Organ=="SP",]
df$GC_size<-df$GC_size/1000
df<-simplify_tx(df)
ggviolin(df, x = "tx2", y = "GC_size", fill = "tx2",
         add = "boxplot", add.params = list(fill = "white"))+
  labs(y=expression(paste("GC size (x", 10^{3}," ",mu,m^{2}, ")")))+
  theme(legend.position="none",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave2("gc_size.png",width=6, height=4,device="png")
write.csv(df[order(df$tx2),],"./prism/gc_size.csv")
# 
# df<-avg_gc
# df<-df[df$Organ=="SP",]
# df<-df[df$tx2 %in% c("BMchim.564","564homo.Icos",#"BMchim.564-Icos.Icos",
#                      "BMchim.564-Icos.RAG-TCRA","BMchim.564-Icos.RAG-TCRB",
#                      "BMchim.564-Icos.RAG-TCRE"),]
# # df<-df[!(df$MFI>500 & df$tx2=="564homo.Icos"),]
# df<-df %>%mutate (tx2=factor(tx2,levels=c("BMchim.564","564homo.Icos","BMchim.564-Icos.RAG-TCRA",
#                                           "BMchim.564-Icos.RAG-TCRB","BMchim.564-Icos.RAG-TCRE")))
# ggbarplot(df, x = "tx2", y = "GC_follicle", #color = "location",
#           add = c("mean_sd","jitter"), #palette = c("#00AFBB", "#E7B800"),
#           # add = c("mean_sd"),
#           position = position_dodge())+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# write.csv(df[order(df$tx2),],"gc_chim.csv")
# 
# df<-gc
# df$GC_size[is.na(df$GC_size)]<-0
# df<-df[df$Organ=="SP",]
# df<-df[df$tx2 %in% c("BMchim.564","564homo.Icos",#"BMchim.564-Icos.Icos",
#                      "BMchim.564-Icos.RAG-TCRA","BMchim.564-Icos.RAG-TCRB",
#                      "BMchim.564-Icos.RAG-TCRE"),]
# # df<-df[!(df$MFI>500 & df$tx2=="564homo.Icos"),]
# df<-df %>%mutate (tx2=factor(tx2,levels=c("BMchim.564","564homo.Icos","BMchim.564-Icos.RAG-TCRA",
#                                           "BMchim.564-Icos.RAG-TCRB","BMchim.564-Icos.RAG-TCRE")))
# df$GC_size<-df$GC_size/1000
# ggbarplot(df, x = "tx2", y = "GC_size", #color = "location",
#           add = c("mean_sd","jitter"), #palette = c("#00AFBB", "#E7B800"),
#           # add = c("mean_sd"),
#           position = position_dodge())+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
#   stat_compare_means(comparisons = list(c("564homo.Icos","BMchim.564-Icos.RAG-TCRA"),
#                                         c("BMchim.564-Icos.RAG-TCRA","BMchim.564-Icos.RAG-TCRB"),
#                                         c("564homo.Icos","BMchim.564-Icos.RAG-TCRE")), label = "p.value")+ # Add significance levels
#   stat_compare_means(label.y = max(df$MFI,na.rm=T)*1.3)
# ggsave2("gc_size_chim_stats.png",width=4, height=4,device="png")
# ggviolin(df, x = "tx2", y = "GC_size", fill = "tx2",
#          palette = c("grey30", "#4472C4", "#70AD47","#7030A0","#ED7D31"),
#          add = "boxplot", add.params = list(fill = "white"))+
#   stat_compare_means(comparisons = list(c("564homo.Icos","BMchim.564-Icos.RAG-TCRA"),
#                                         c("564homo.Icos","BMchim.564-Icos.RAG-TCRB"),
#                                         c("564homo.Icos","BMchim.564-Icos.RAG-TCRE")), label = "p.value")+ # Add significance levels
#   # stat_compare_means(label.y = max(df$GC_size,na.rm=T)*1.4,label.x=2)+
#   labs(y=expression(paste("GC size (x", 10^{3}," ",mu,m^{2}, ")")))+
#   theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank())
# ggsave2("gc_size_chim.png",width=3, height=2,device="png")
# 
# ggviolin(df, x = "tx2", y = "GC_size", fill = "tx2",
#          palette = c("grey30", "#4472C4", "#70AD47","#7030A0","#ED7D31"),
#          add = "boxplot", add.params = list(fill = "white"))+
#   theme(legend.position="right",axis.title.x=element_blank(),axis.text.x=element_blank())
# ggsave2("gc_size_chim_legend.png",width=3, height=2,device="png")
# 
# 
# 


