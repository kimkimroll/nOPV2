setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/nopv2/final/serial_dilutions")


library(readr)
library(dplyr)
library(ggplot2)
library(scales)

#upload your data csv
kim<-read.csv("kim-alldata.csv", header = TRUE, nrows = FALSE)

#filter by assay
k_panev<-kim[kim$assay == "panEV",]

k_panev$cpe<-as.factor(k_panev$cpe)

#legend titles
k_panev$virus <- factor(k_panev$virus, levels = c("2a", "1b", "sl3"),
                  labels = c("2a", "1b", "SL3"))

k <- ggplot(k_panev, aes(titer, ct, colour = cpe, shape = rnax)) +
  
  geom_point(size = 5, stroke = 2, alpha = 0.8) + 
  geom_line(color = "darkgray", linetype = "dotted", size=1.5, width=4) +
  facet_grid(cell~virus) + 
  theme_bw() + 
  theme( axis.text = element_text( size = 16 ),
         axis.text.x = element_text( size = 18 ),
         axis.ticks.length = unit(6, "pt"),
         axis.title = element_text( size = 20),
         axis.text.y = element_text( size = 16 ),
         strip.text.x = element_text( size = 20),
         strip.text.y = element_text( size = 20, angle =0),
         panel.spacing = unit(2, "lines"),
         legend.text = element_text( size = 16),
         legend.title = element_text( size = 20),
         legend.position="right" ) +
   scale_x_log10(limits = c(1, NA), 
                labels = trans_format("log10", math_format(10^.x)),
                breaks=trans_breaks("log10", function(x) 10^x, n=8))+
  scale_y_reverse()+
  ylab('Ct')+
  xlab('Titer')


kcolor<-c("0"="#37527d", 
  "1"="#665397", 
  "2"="#a54798", 
  "3"="#dd2f7d", 
  "4"="#ff304a")

k2<- k + scale_colour_manual(name = 'CPE', values = kcolor) 


#squares for Ever
#k3 <- k2 + scale_shape_manual(values = c(15,19)) + 
#k3
#ggsave("ever1.png", dpi = 72, height = 9, width = 15, units = "in")


k3 <- k2 + scale_shape_manual(name = 'RNA Extracted', values = c(21,19), labels = c('No','Yes')) + 

guides(fill = guide_legend(override.aes = list(color = kcolor)),
       color = guide_legend(override.aes = list(shape = 19)))

#save as PNG image
ggsave("kim1panev.png", dpi = 72, height = 9, width = 24, units = "in")


