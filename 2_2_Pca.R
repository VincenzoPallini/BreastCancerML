## 4. Principal Component Analysis (PCA)


library(factoextra)
wbcd_pca <- transform(wbcd)	


### 4-1) Summary {.tabset}

all_pca <- prcomp(wbcd_pca[,-1], cor=TRUE, scale = TRUE)
summary(all_pca)


#### Mean
##The cumulative proportion from PC1 to PC3 is about 88.7%. (above 85%)

mean_pca <- prcomp(wbcd_pca[,c(2:11)], scale = TRUE)
summary(mean_pca)


#### SE
##The cumulative proportion from PC1 to PC4 is about 86.7%. (above 85%)

se_pca <- prcomp(wbcd_pca[,c(12:21)], scale = TRUE)
summary(se_pca)


#### Worst
##The cumulative proportion from PC1 to PC3 is about 85.8%. (above 85%)

worst_pca <- prcomp(wbcd_pca[,c(22:31)], scale = TRUE)
summary(worst_pca)



### 4-2) Screeplot {.tabset}


#### All
##Line lies at point PC6

fviz_eig(all_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer All Variances - PCA",
       x = "Principal Components", y = "% of variances")


#### Mean
##Line lies at point PC4

fviz_eig(mean_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer Mean Variances - PCA",
       x = "Principal Components", y = "% of variances")


#### SE
##Line lies at point PC4

fviz_eig(se_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer SE Variances - PCA",
       x = "Principal Components", y = "% of variances")


#### Worst
##Line lies at point PC4

fviz_eig(worst_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer Worst Variances - PCA",
       x = "Principal Components", y = "% of variances")


### 4-3) Get PCA Variables {.tabset}
#### All
##### Get PCA Variables

all_var <- get_pca_var(all_pca)
all_var


##### Quality of representation of PCA
##Correlation between variables and PCA

library("corrplot")
corrplot(all_var$cos2, is.corr=FALSE)


##### Contributions of variables to PCA
##To highlight the most contributing variables for each components

corrplot(all_var$contrib, is.corr=FALSE)	


##### Contributions of variables to PC1 & PC2

library(gridExtra)
p1 <- fviz_contrib(all_pca, choice="var", axes=1, fill="pink", color="grey", top=10)
p2 <- fviz_contrib(all_pca, choice="var", axes=2, fill="skyblue", color="grey", top=10)
grid.arrange(p1,p2,ncol=2)



#### Mean
##### Get PCA Variables

mean_var <- get_pca_var(mean_pca)
mean_var


##### Quality of representation of PCA
##Correlation between variables and PCA

library("corrplot")
corrplot(mean_var$cos2, is.corr=FALSE)


##### Contributions of variables to PCA
##To highlight the most contributing variables for each components

corrplot(mean_var$contrib, is.corr=FALSE)	


##### Contributions of variables to PC1 & PC2

library(gridExtra)
p1 <- fviz_contrib(mean_pca, choice="var", axes=1, fill="pink", color="grey", top=10)
p2 <- fviz_contrib(mean_pca, choice="var", axes=2, fill="skyblue", color="grey", top=10)
grid.arrange(p1,p2,ncol=2)


#### SE
##### Get PCA Variables

se_var <- get_pca_var(se_pca)
se_var


##### Quality of representation of PCA
##Correlation between variables and PCA

library("corrplot")
corrplot(se_var$cos2, is.corr=FALSE)


##### Contributions of variables to PCA
##To highlight the most contributing variables for each components

corrplot(se_var$contrib, is.corr=FALSE)	


##### Contributions of variables to PC1 & PC2

##library(gridExtra)
p1 <- fviz_contrib(se_pca, choice="var", axes=1, fill="pink", color="grey", top=10)
p2 <- fviz_contrib(se_pca, choice="var", axes=2, fill="skyblue", color="grey", top=10)
grid.arrange(p1,p2,ncol=2)


#### Worst
##### Get PCA Variables

worst_var <- get_pca_var(worst_pca)
worst_var


##### Quality of representation of PCA
##Correlation between variables and PCA

library("corrplot")
corrplot(worst_var$cos2, is.corr=FALSE)


##### Contributions of variables to PCA
##To highlight the most contributing variables for each components

corrplot(worst_var$contrib, is.corr=FALSE)	


##### Contributions of variables to PC1 & PC2

library(gridExtra)
p1 <- fviz_contrib(worst_pca, choice="var", axes=1, fill="pink", color="grey", top=10)
p2 <- fviz_contrib(worst_pca, choice="var", axes=2, fill="skyblue", color="grey", top=10)
grid.arrange(p1,p2,ncol=2)



### 4-4) See the plot - color variables by groups {.tabset}
#value centers : put the optimal principal component value that we chosen above.

#### All
##optimal PC value : PC1~PC6

set.seed(218)
res.all <- kmeans(all_var$coord, centers = 6, nstart = 25)
grp <- as.factor(res.all$cluster)

fviz_pca_var(all_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")


#### Mean
##optimal PC value : PC1~PC3

set.seed(218)
res.mean <- kmeans(mean_var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.mean$cluster)

fviz_pca_var(mean_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")


#### SE
##optimal PC value : PC1~PC4

set.seed(218)
res.se <- kmeans(se_var$coord, centers = 4, nstart = 25)
grp <- as.factor(res.se$cluster)

fviz_pca_var(se_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")


#### Worst
##optimal PC value : PC1~PC3

set.seed(218)
res.worst <- kmeans(worst_var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.worst$cluster)

fviz_pca_var(worst_pca, col.var = grp, 
             palette = "jco",
             legend.title = "Cluster")





### 4-5) See the Biplot {.tabset}

library("factoextra")

#### All

fviz_pca_biplot(all_pca, col.ind = wbcd$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)


#### Mean

fviz_pca_biplot(mean_pca, col.ind = wbcd$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)


#### SE

fviz_pca_biplot(se_pca, col.ind = wbcd$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)


#### Worst

fviz_pca_biplot(worst_pca, col.ind = wbcd$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)
