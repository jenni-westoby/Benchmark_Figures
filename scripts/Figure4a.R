library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(gtable)
library(ggpubr)
library(reshape2)
library(tidyverse)

####################################
# FUNCTIONS

#Function to process data
process_data<-function(df,cell_type){
  df<-df[df$cell_type==as.name(cell_type),]
  df<-cbind(df, max_spearmans_error = df$spearmans_results + df$spearmans_error, min_spearmans_error = df$spearmans_results - df$spearmans_error)
  df<-cbind(df, max_nrmse_error = df$nrmse_results + df$nrmse_error, min_nrmse_error = df$nrmse_results - df$nrmse_error)
  return(df)
}

#Function to make figure legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

#Function to plot results
plot_results<-function(title, yaxis_text, xaxis_text, legend_true, df, results,max_error, min_error){
  
  #Plot results
  p1<-ggplot(data=df, aes_(x= ~percentage_zeros, y=as.name(results), group=~tools, colour=~factor(tools)))+ geom_line() + geom_errorbar(aes_(ymin=as.name(min_error), ymax=as.name(max_error)), width=1) 
  p1<-p1 + labs(x=xaxis_text, y= yaxis_text, colour="Tools") + scale_x_reverse() +scale_colour_manual(values=cbbPalette) + ggtitle(title)
  if (legend_true == FALSE){
    p1<-p1 + theme(legend.position = 'none', text = element_text(size=14), plot.title = element_text(size=14) )
  }
  
  return(p1)
  
  
}
###########################################

#Read in data
figure_4a_data<-read.table("../data/Figure4a.txt")

#Process data for ggplot
Bs_figure_4a_data<-process_data(figure_4a_data, "B")
ES_figure_4a_data<-process_data(figure_4a_data, "ES")

#Set up colour blind friendly palette
tools<-c("RSEM","Salmon Alignment", "Salmon Quasi", "Salmon SMEM", "Sailfish", "eXpress", "Kallisto")
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(cbbPalette)<-tools

#Create B lymphocyte graphs
B_drop_spear<-plot_results("\nB lymphocytes", "Spearman's Rho", " ", FALSE, Bs_figure_4a_data, "spearmans_results", "max_spearmans_error", "min_spearmans_error")
B_drop_NRMSE<-plot_results("", "NRMSE","Threshold % Dropouts", FALSE,Bs_figure_4a_data, "nrmse_results", "max_nrmse_error", "min_nrmse_error")

#Create ES cell graphs
ES_drop_spear<-plot_results("\nES cells", "","", FALSE, ES_figure_4a_data, "spearmans_results", "max_spearmans_error", "min_spearmans_error")
ES_drop_NRMSE<-plot_results("", "","Threshold % Dropouts", FALSE, ES_figure_4a_data, "nrmse_results", "max_nrmse_error", "min_nrmse_error")

#Create figure legends
leg<-g_legend(plot_results("Spearman's Rho", "Spearman's Rho","Threshold Percentage of Dropouts", TRUE, ES_figure_4a_data, "spearmans_results", "max_spearmans_error", "min_spearmans_error"))

#Arrange graphs
Figure4a<-ggarrange(ggarrange(B_drop_spear + ylim(0.6,1), B_drop_NRMSE + ylim(0,50), nrow=2, ncol=1),  ggarrange(ES_drop_spear + ylim(0.6,1), ES_drop_NRMSE + ylim(0,50),nrow=2, ncol=1), leg,                                        
                    nrow = 1,
                    ncol = 3, widths=c(2,2,1)
) 

ggsave("../pdfs/Figure4a.pdf", last_plot())
ggsave("../pngs/Figure4a.png", last_plot())
