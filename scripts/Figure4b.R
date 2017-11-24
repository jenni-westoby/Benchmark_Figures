library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(gtable)
library(ggpubr)
library(reshape2)
library(tidyverse)
library(MASS)
library(viridis)

#######################################################
#Functions

#Function to find point density
get_density <- function(x, y, n = 100) {
  dens <- MASS::kde2d(x = x, y = y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

#Function to plot graphs
plot_cor_expr_zeros<-function(df, ID, x_lab_text, y_lab_text, title, legend){
  
  #filter by ID
  df<-df[df$ID==as.name(ID),]
  
  #find density
  df$Density <- get_density(df$expression, df$percent_zeros)
  
  #create graph
  p1<-ggplot(data=df, aes(x=expression, y=percent_zeros, colour=Density)) + geom_point() + scale_color_viridis()
  p1<-p1 + xlab(x_lab_text) + ylab(y_lab_text) + ggtitle(title) + theme(text = element_text(size=14), plot.title = element_text(size=14))
  if (legend==FALSE){
    p1<- p1 + theme(legend.position = 0)
  }
  
  return(p1)
}

# Function to create legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

#########################################################################

Figure_4b_data<-read.table("../data/Figure4b.txt")

leg_cor<-g_legend(plot_cor_expr_zeros(Figure_4b_data, "simulated_B", " ", "% zeros", "Simulated\nB lymphocytes", TRUE))
B_sim_cor_expr_zeros<-plot_cor_expr_zeros(Figure_4b_data, "simulated_B", " ", "% Zeros", "Simulated\nB lymphocytes", FALSE)
B_real_cor_expr<-plot_cor_expr_zeros(Figure_4b_data, "real_B", "log2(counts + 1)", "% Zeros", "Real\nB lymphocytes", FALSE)

ES_sim_cor_expr_zeros<-plot_cor_expr_zeros(Figure_4b_data, "simulated_ES"," ", " ", "\nSimulated ES cells", FALSE)
ES_real_cor_expr_zeros<-plot_cor_expr_zeros(Figure_4b_data, "real_ES", "log2(counts + 1)", " ", "\nReal ES cells", FALSE)

Figure4b<-ggarrange(ggarrange(B_sim_cor_expr_zeros, ES_sim_cor_expr_zeros, B_real_cor_expr, ES_real_cor_expr_zeros, ncol=2, nrow=2), leg_cor, ncol = 2, widths=c(4,1))

ggsave("../pdfs/Figure4b.pdf", last_plot())
ggsave("../pngs/Figure4b.png", last_plot())

