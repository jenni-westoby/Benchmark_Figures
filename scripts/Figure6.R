library(ggplot2)
library(ggpubr)
library(reshape2)

##############################
# FUNCTIONS

get_percentage_hist<-function(df){
  no_zeros<-df#[df$value!=0,]
  no_zeros$value<-no_zeros$value==2
  two_iso_percentage<-no_zeros %>% group_by(genes) %>% summarise(percentage(value))
  ggplot(data = two_iso_percentage, aes(x=`percentage(value)`)) + geom_histogram() + xlab("% Cells Which Express Both Isoforms") + ggtitle(" ")
} 

#####################################
# MAKE FIGURE 6A

barplot_data<-read.table("../data/Figure6_number_of_isoforms.txt")
B_barplot_data<-barplot_data[barplot_data$ID=="B_1",]
ES_barplot_data<-barplot_data[barplot_data$ID=="ES",]
  
  
B_0_1_2_barplot<-ggplot(data=B_barplot_data, aes(x=value)) + geom_bar() + xlab("Number of Isoforms Expressed") + ggtitle("B lymphocytes")
ES_0_1_2_barplot<-ggplot(data=ES_barplot_data, aes(x=value)) + geom_bar() + xlab("Number of Isoforms Expressed") + ggtitle("ES cells")

##############################################################
#MAKE FIGURE 6B

exprs_percent_data<-read.table("../data/Figure_6_percent_exprs.txt")
colnames(exprs_percent_data)[2]<-"expression"
colnames(exprs_percent_data)[3]<-"percentage"
B_exprs_percent_data<-exprs_percent_data[exprs_percent_data$ID=="B_1",]
ES_exprs_percent_data<-exprs_percent_data[exprs_percent_data$ID=="ES",]

B_percent_hist<-ggplot(data=B_exprs_percent_data, aes(x=percentage)) + geom_histogram() + xlab("% Cells Which Express Both Isoforms") + ggtitle(" ")
ES_percent_hist<-ggplot(data=ES_exprs_percent_data, aes(x=percentage)) + geom_histogram() + xlab("% Cells Which Express Both Isoforms") + ggtitle(" ")

#################################################################
#MAKE FIGURE 6C

B_exprs_percent_plot<-ggplot(data =B_exprs_percent_data, aes(y=expression,x=percentage)) + geom_point() + ylab("log2(Counts + 1)") + xlab("% Cells Which Express Both Isoforms") + ggtitle(" ")
ES_exprs_percent_plot<-ggplot(data = ES_exprs_percent_data, aes(y=expression,x=percentage)) + geom_point() + ylab("log2(Counts + 1)") + xlab("% Cells Which Express Both Isoforms") + ggtitle(" ")

ggarrange(B_0_1_2_barplot, ES_0_1_2_barplot, B_percent_hist, ES_percent_hist, B_exprs_percent_plot, ES_exprs_percent_plot, ncol =2, nrow=3, labels=c("A", "", "B", "", "C"))
ggsave("../pdfs/Figure6.pdf", plot=last_plot(), height= 225, width=170, units=c("mm"))
ggsave("../pngs/Figure6.png", plot=last_plot(), height= 225, width=170, units=c("mm"))