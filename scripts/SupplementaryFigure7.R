library(ggplot2)
library(ggpubr)

##############
# FUNCTIONS

make_ggplot<-function(df, id){
  df<-df[df$ID==id,]
  graph<-ggplot(data=df, aes(x=estimates)) + geom_histogram(binwidth = 10)  +coord_cartesian(ylim=c(0, 100000), xlim=c(0,1500)) + xlab("100 x log2(Ground Truth Expression in TPM + 1)") + ylab("Frequency of Isoforms") 
}

#########################

ggplot_results<-read.table(gzfile("../data/SupplementaryFigure7.gz"))

p1<-make_ggplot(ggplot_results, "RSEM")
p2<-make_ggplot(ggplot_results, "Polyester_bias")
p3<-make_ggplot(ggplot_results, "Polyester_unbias")

ggarrange(p1,p2,p3, ncol=2, nrow=2, labels=c("A", "B", "C"))
ggsave("../pdfs/SupplementaryFigure7.pdf", plot=last_plot())
ggsave("../pngs/SupplementaryFigure7.png", plot=last_plot())
