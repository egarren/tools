rm(list=ls())
library(ggplot2)
library(cowplot)
library(ggpubr)
library(Seurat)
library(readxl)
library(dplyr)
library(tidyr)
library(pheatmap)
library(xlsx)

#load metadata
metadata<-read_excel("Mouse Record.xlsx",guess_max=10000) #define path\

#load qPCR data
data<-read_excel("raw_cq.xlsx")
data<-data[,c("Well","Cq","Gene")]

#load qpcr meta
qpcr_meta<-read_excel("qpcr_meta.xlsx")

#add metadata
data<-left_join(data,qpcr_meta[,c("Well","Sample")],by="Well")
data<-left_join(data,qpcr_meta2[,c("Sample","Mouse","Cell")],by="Sample")
data$Mouse<-as.numeric(data$Mouse)
data<-left_join(data,metadata[,c("mouse","tx2")],by=c("Mouse"="mouse"),keep=F)
data$tx2[is.na(data$tx2)]<-"Neg"

#calculate ddCt
data$dCt<-0
for(i in unique(data$Sample)){
  for(j in unique(data$Gene)){
    data$dCt[data$Sample ==i & data$Gene==j]<- data$Cq[data$Sample ==i & data$Gene==j] -  data$Cq[data$Sample ==i & data$Gene=="Ywhaz"]
  }
}
data$ddCt<-0
for(i in unique(data$Sample)){
  for(j in unique(data$Gene)){
    avgCt = mean(data$dCt[data$Cell=="Tcon" & data$Gene==j],na.rm=T)
    data$ddCt[data$Sample ==i & data$Gene==j]<- data$dCt[data$Sample ==i & data$Gene==j] -  avgCt
  }
}
data$FC = 2^(- data$ddCt)

#normalize to control
data$FC2<-0 
for(i in unique(data$Sample)){
  for(j in unique(data$Gene)){
    avgFC = mean(data$FC[data$Cell=="Tcon" & data$Gene==j],na.rm=T)
    data$FC2[data$Sample ==i & data$Gene==j]<- data$FC[data$Sample ==i & data$Gene==j] /  avgFC
  }
}

#compare expression
df<-data
df<-df[df$Gene=="Pdcd1",]
df2<-data[data$Gene=="Sostdc1",]
df3<-left_join(df[,c("Sample","FC")],df2[,c("Sample","FC","Cell","tx2")],by="Sample")
df3<-df3[df3$Sample != "Neg",]
df3<-df3[!is.na(df3$Sample),]
rownames(df3)<-df3$Sample
df4<-df3[!is.na(df3$FC.x) & !is.na(df3$FC.y),]
df4$FC.x <- df4$FC.x / mean(df4$FC.x[df4$Cell=="Tcon"])
df4$FC.y <- df4$FC.y / mean(df4$FC.y[df4$Cell=="Tcon"])
cols <- c("Tcon" = "forestgreen", "PD1_hi" = "blue", 
          "PD1_med"=adjustcolor( "blue", alpha.f = 0.6), "PD1_lo"=adjustcolor( "blue", alpha.f = 0.25))
ggplot(df4, aes(x = FC.x, y = FC.y, color = Cell))+geom_point()+ 
  scale_x_continuous(trans='log10',labels = ~ format(.x, scientific = FALSE))+
  scale_y_continuous(trans='log10',labels = ~ format(.x, scientific = FALSE))+
  geom_smooth(method = "lm",aes(group = 1),show.legend=F,colour="black",size=0.5,fill="grey80")+
  scale_colour_manual(values = cols, breaks=c("Tcon","PD1_lo","PD1_med","PD1_hi"))+
  labs(x="*Pdcd1*",y="*Sostdc1*",color="")+theme_bw()+
  theme(axis.title.x = ggtext::element_markdown(),axis.title.y = ggtext::element_markdown())
ggsave2("scatter.png",width=4, height=2.5,device="png")
fit1 <- lm(FC.y ~ FC.x, data = df4) 
summary(fit1) 

write.xlsx(df4[order(df4$Cell),],file="qpcr_compare2.xlsx")

#get rid of data
data<-data[data$Gene != "Ywhaz",]
data<-data[data$tx2 != "Neg",]
data2<-data
write.xlsx(data2,file="qpcr_analyzed2.xlsx")
write.xlsx(data2[order(data2$Gene,data2$Cell,data2$tx2),],file="qpcr_analyzed3.xlsx")

#individual graph
df<-data2
df<-df[df$Gene=="Sostdc1",]
df<-as.data.frame(df)
rownames(df)<-df$Sample
ggbarplot(df, x = "Cell", y = "FC", 
          add = c("mean_se", "jitter"),
          color = "tx2",
          position = position_dodge(0.8), legend="right")


#heatmap
df<-data2
df2<-df[df$tx2 != "x",]
df<-df2 %>% dplyr::select(Sample,Gene,FC)
df<-spread(df, key=Gene, value=FC)
df<-as.data.frame(df)
rownames(df)<-df$Sample
df<-subset(df,select=-Sample)
df<-t(df) 
pheatmap(log(df))
annot.meta<-unique(df2[,c("Sample","Cell","tx2")])
annot.col<-as.data.frame(annot.meta)
row.names(annot.col) <- annot.col$Sample
annot.col<-subset(annot.col,select=-Sample)
pheatmap(log(df),annotation_col=annot.col,show_colnames=F)
p1<-pheatmap(log(df),annotation_col=annot.col,show_colnames=F)
save_pheatmap_png <- function(x, filename, width=2000, height=700, res = 150) {
  png(filename, width = width, height = height, res = res)
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
  dev.off()
}
save_pheatmap_png(p1, "qpcr.png")

save.image("qPCR.graphs.RData")