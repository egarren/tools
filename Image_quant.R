rm(list=ls())
library(tidyverse)
library(readxl)
library(ggpubr)
library(cowplot)
library(zoo)


metadata<-read_excel("Mouse Record.xlsx",guess_max=10000) #define path\
meta<-metadata[,c("mouse","Gender","Age","Treatment","tx2")]

### ANA quantification
simplify_tx <- function(df){
  df$tx2[df$tx2 %in% c("BMchim.564.CD45.1.Mb1-PDL1.CD45.1.2","BMchim.564.CD45.1.CD45.1.2")]<-"BMchim.564"
  df$tx2[df$tx2 %in% c("564homo+CD45.1+CD45.1.2","564homo+CD45.1+Mb1-PDL1.1.2","564homo+CD45.1+PD1-CD45.1.2")]<-"564homo"
  df$tx2[df$tx2 %in% c("564homo.Cxcl13-PDL1+CD45.1+PD1-CD45.1.2")]<-"564homo.Cxcl13-PDL1"
  return(df)
}
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
df<-simplify_tx(df)
df$mouse<-as.factor(df$mouse)
for(i in unique(df$isotype)){
  for(j in unique(df$location)){
    ggbarplot(df[df$isotype==i & df$location==j,], x = "tx2", y = "MFI", #color = "mouse",
              add = c("mean_sd"), #palette = c("#00AFBB", "#E7B800"),
              position = position_dodge())+
      geom_jitter(aes(color=mouse))+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    ggsave2(paste0("./output/avg_ANA",i,"_",j,".png"),width=6, height=6,device="png")
  }
}
df<-df[df$isotype %in% c("igg","igg2c","igg2a"),]
write.csv(df[order(df$tx2,df$isotype),],"./prism/avg_ANA_mfi.csv")

df<-mfi
df<-simplify_tx(df)
df$mouse<-as.factor(df$mouse)
for(i in unique(df$isotype)){
  for(j in unique(df$location)){
    ggviolin(df[df$isotype==i&df$location==j,], x = "tx2", y = "MFI", #fill = "tx2",
             add = "boxplot", add.params = list(fill = "white"))+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    ggsave2(paste0("./output/ANA_",i,"_",j,".png"),width=6, height=6,device="png")
  }
}
write.csv(df[order(df$tx2,df$isotype),],"./prism/ANA_mfi.csv")

comparisons<-list(c("564homo","564homo.Cxcl13-PDL1"),c("564homo","564homo.Mb1-PDL1"),
                  c("BMchim.564","BMchim.Cxcl13-PDL1.564"),c("BMchim.564","BMchim.564-Mb1-PDL1"))
for(k in c(1:length(comparisons))){
  comp=comparisons[[k]]
  df2<-df[df$tx2 %in%comp,]
  for(i in unique(df2$isotype)){
    for(j in unique(df2$location)){
      df3<-df2[df2$isotype==i&df2$location==j,]
      if(i=="igg2c_mfi"){iso="IgG2c"}
      if(i=="igg2a_mfi"){iso="IgG2a"}
      if(i=="igg_mfi"){iso="IgG"}
      if(j=="nuc"){loc="Nuclear"}
      if(j=="cyto"){loc="Cytoplasmic"}
      ggviolin(df3, x = "tx2", y = "MFI", fill = "tx2",
               palette = c("grey30","white"),
               add = "boxplot", add.params = list(fill = "white"))+
        labs(y=paste0(loc," ",iso," (MFI)"))+
        stat_compare_means(label.y = max(df3$MFI,na.rm=T)*1.1,label="p.signif",comparisons=list(comp))+
        theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank())
      ggsave2(paste0("./output/ANA_stat_comp",k,"_",i,"_",j,".png"),width=2.5, height=3,device="png")
    }
  }
}



unique(df[order(df$tx2),c("mouse","tx2")])

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
  if(i==1){df$isotype[df$Ch==1]<-"dapi"}
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
     df<-simplify_tx(df)
      df<-df[df$isotype==i,]
      if(j!="allCh"){df<-df[df$Ch==j,]}
      if(nrow(df)>5 & !(i=="igg2c" & j==2)){
        df$MFI<-df$MFI/1000
        ggbarplot(df, x = "tx2", y = "MFI", #color = "mouse",
                 add = c("mean_se","jitter"), #palette = c("#00AFBB", "#E7B800"),
                 position = position_dodge())+
         theme(legend.position="none",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
        ggsave2(paste0("./output/",k,"_",i,"_",j,".png"),width=4, height=4,device="png")
      }
    }
  }
}

df<-glomeruli
df<-simplify_tx(df)
df$MFI<-df$MFI/1000
df<-df[df$isotype %in% c("igg","igg2c"),]
write.csv(df[order(df$Ch,df$tx2,df$isotype),],"./prism/glom_mfi.csv")

df<-avg_glom
df<-simplify_tx(df)
df$MFI<-df$MFI/1000
df<-df[df$isotype %in% c("igg","igg2c"),]
write.csv(df[order(df$Ch,df$tx2,df$isotype),],"./prism/avg_glom_mfi.csv")


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
ggsave2("./output/gc_follicle.png",width=6, height=4,device="png")
write.csv(df[order(df$tx2),],"./prism/avg_gc_follicle.csv")

df<-gc
df<-df[df$Organ=="SP",]
df$GC_size<-df$GC_size/1000
df<-simplify_tx(df)
ggviolin(df, x = "tx2", y = "GC_size", fill = "tx2",
         add = "boxplot", add.params = list(fill = "white"))+
  labs(y=expression(paste("GC size (x", 10^{3}," ",mu,m^{2}, ")")))+
  theme(legend.position="none",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave2("./output/gc_size.png",width=6, height=4,device="png")
write.csv(df[order(df$tx2),],"./prism/gc_size.csv")



