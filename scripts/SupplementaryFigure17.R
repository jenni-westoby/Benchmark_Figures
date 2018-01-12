library(tidyverse)

###########################################################
# FUNCTIONS

make_df<-function(df, name_df, origin_df){
  percentage_df<-sum(df$tpm==0)/nrow(df) * 100
  bulk_df<-data.frame(cell=name_df, percentage=percentage_df, origin=origin_df)
  return(bulk_df)
}

make_zeros_plot<-function(df){
  graph<-ggplot(data=df, aes(y=percentage, x=origin)) + geom_jitter(alpha=0.5, position=position_jitter(width = .2)) #+ scale_x_discrete(labels=c("Polyester (3' bias)", "Polyester (no bias)", "RSEM"))
  graph<-graph + ylab("% of Isoforms Which Are Unexpressed") + xlab("Simulation") 
}

########################

#read in BLUEPRINT sc #zeros data
sc_blue<-read.table(gzfile("../data/SupplementaryFigure7.gz"))

sc_blue<-sc_blue[sc_blue$ID=="RSEM",]
sc_blue<-sc_blue %>% group_by(cell, ID) %>% summarise((sum(estimates==0)/128809)*100)
names(sc_blue)[3]<-c("percentage")
sc_blue<-data.frame(sc_blue[,1], sc_blue[,3], origin="BLUEPRINT single cell")


#read in Kolod et al sc #zeros data
sc_kolod<-read.table(gzfile("../data/SupplementaryFigure12.gz"))


sc_kolod<-sc_kolod %>% group_by(cell) %>% summarise((sum(estimates==0)/128809)*100)
names(sc_kolod)[2]<-c("percentage")
sc_kolod<-data.frame(sc_kolod, origin="ES single cell")


#bulk BLUEPRINT expression matrix
bulk_B_1<-read.table("~/data/quant_results/B_B_M_1_B1-Aligned.sortedByCoord.out.ex.bam/abundance.tsv",header=T)
bulk_B_1<-make_df(bulk_B_1, "bulk_B_1", "BLUEPRINT bulk")

bulk_B_2<-read.table("~/data/quant_results/B_B_M_1_B2-Aligned.sortedByCoord.out.ex.bam/abundance.tsv", header=T)
bulk_B_2<-make_df(bulk_B_2, "bulk_B_2", "BLUEPRINT bulk")

bulk_B_3<-read.table("~/data/quant_results/B_B_M_1_B3-Aligned.sortedByCoord.out.ex.bam/abundance.tsv", header=T)
bulk_B_3<-make_df(bulk_B_3, "bulk_B_3", "BLUEPRINT bulk")


#bulk Kolod et al expression matrix
bulk_ES<-read.table("~/data/quant_results/ERR522956/abundance.tsv", header=T)
bulk_ES<-make_df(bulk_ES, "bulk_ES", "ES bulk")

master_df<-rbind(sc_blue, sc_kolod, bulk_B_1,bulk_B_2,bulk_B_3, bulk_ES)
make_zeros_plot(master_df)

ggsave("../pdfs/SupplementaryFigure17.pdf", plot=last_plot())
ggsave("../pngs/SupplementaryFigure17.png", plot=last_plot())

