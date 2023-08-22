## 3. Analisi della correlazione tra le variabili
### 3-1) Correlazioni tra le singole variabili {.tabset}


#### Mean

library(PerformanceAnalytics)
chart.Correlation(wbcd[,c(2:11)],histogram=TRUE, col="grey10", pch=1, main="Cancer Mean")


#### SE

library(psych)
pairs.panels(wbcd[,c(12:21)], method="pearson",
             hist.col = "#1fbbfa", density=TRUE, ellipses=TRUE, show.points = TRUE,
             pch=1, lm=TRUE, cex.cor=1, smoother=F, stars = T, main="Cancer SE")


#### Worst

library(ggplot2)
library(GGally)
ggpairs(wbcd[,c(22:31)],)+ theme_bw()+
  labs(title="Cancer Worst")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=13))




### 3-2) Visualizza la relazione tra tutte le variabili (inclusa la diagnosi) {.tabset}



library(ggplot2)
library(GGally)


#### Mean

ggpairs(wbcd[,c(2:11,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


#### SE

ggpairs(wbcd[,c(12:21,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer SE")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


#### Worst

ggpairs(wbcd[,c(22:31,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer Worst")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))




### 3-3) Visualizza il grafico di ggcorr {.tabset}


#### Mean

ggcorr(wbcd[,c(2:11)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))



#### SE

ggcorr(wbcd[,c(12:21)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer SE")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))



#### Worst

ggcorr(wbcd[,c(22:31)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer Worst")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
