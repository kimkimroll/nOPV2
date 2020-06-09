setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/nopv2/06052020")

install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggrepel")
library(ggrepel)
library(ggplot2)
library(dplyr)
library(tidyr)

#upload csv file with data
n1<-read.csv("nopv2_itd.csv", header = TRUE, na.strings=TRUE)

#backup data
all_backup<-n1

#change CPE1+/2+ to 2/3
n1$cpe[n1$cpe == "1+"] <- '2'
n1$cpe[n1$cpe == "2+"] <- '3'
n1$cpe<-as.numeric(n1$cpe)
n1$panPV<-as.character(n1$panPV)
n1$panPV<-replace(n1$panPV, n1$panPV == "NA", "negative")

#filter first passage detections
n1$any_polio<-rep("negative", nrow(n1))

n1$any_polio[n1$panPV == "positive" | n1$sabin2 == "positive" | n1$nopv2_1b == "positive"]<-"positive"
n1$any_polio[n1$passage_cells != "R" & n1$passage_cells !="L"] <-"not-run"
n1$any_polio<-as.character(n1$any_polio)


#filter for L-arm passage
nn<-n1[n1$virus == "1b" & n1$passage_cells == "R" |  n1$passage_cells == "RL" |  n1$passage_cells == "RLR",]

#filter for R-arm passage
nn<-n1[n1$virus == "1b" & n1$passage_cells == "L" |  n1$passage_cells == "LR",]


#set shape, if needed
nshape<-(as.numeric(c("negative" = "1", "not-run" = "1", "positive" = "10")))

rr<-ggplot(nn, aes(titer, cpe))+
  geom_jitter(height = .5, size = 5, stroke = 1.5, alpha = 0.8)+
  facet_grid(virus~passage_cells)+
  theme_bw() +
  theme( axis.text = element_text( size = 16 ),
         axis.text.x = element_text( size = 12, angle = -45, vjust = -.05),
         axis.title = element_text( size = 16, face = "bold" ),
         strip.text.x = element_text( size = 20),
         strip.text.y = element_text( size = 16, angle =0),
         legend.text = element_text( size = 16, angle =0),
         legend.title = element_text( size = 16, angle =0),
         legend.position="right") +
  scale_x_log10(breaks=c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)) +
  geom_hline(yintercept=2.5, linetype= "dotted", size = 1)+
  ylab("CPE")+
  xlab("Titer")+
  labs(col="NPEV Background")+
 scale_y_discrete(limits=c(1, 2, 3, 4))


#save graph as PNG image
ggsave("n1-LR.png", dpi = 500, height = 9, width = 15, units = "in")

