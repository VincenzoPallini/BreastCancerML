library(needs)
needs(readr,
      dplyr,
      ggplot2,
      corrplot,
      gridExtra,
      pROC,
      MASS,
      caTools,
      caret,
      caretEnsemble,
      doMC,
      reshape2,
      factoextra)
#registerDoMC(cores = 3)



## 1) CARICAMENTO DATASET
wbcd <- read.csv("data.csv", header=T, stringsAsFactors=F)


#### structure

str(wbcd)

#### summary

summary(wbcd)

#### head

knitr::kable(head(wbcd))

# Factoring dell'attributo target "diagnosis"
data$diagnosis = factor(data$diagnosis)
sapply(data, class)

# Rimozione NULL Data
wbcd$X <- NULL










## 2) ANALISI ESPLORATIVA
# Analisi in dettaglio per ogni attributo
summary(data)

# Controllo di eventuali valori nulli 
apply(data, 2, function (data) sum(is.na(data)))

# Esaminiamo il bilanciamento dei dati
table(data$diagnosis)
prop.table(table(data$diagnosis))
pie(table(data$diagnosis))


# Boxplot di ogni attributo rispetto al target
# si evidenzia che in generale, le diagnosi maligne hanno punteggi più alti in tutti gli attributi
# Mean
df.m = melt(data[,-c(1,13:32)], id.var = "diagnosis")
p = ggplot(data = df.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=diagnosis)) + facet_wrap( ~ variable, scales="free")+ xlab("Variables") + ylab("")+ guides(fill=guide_legend(title="Group"))
p


#Se
df.m = melt(data[,-c(1,3:12,23:32)], id.var = "diagnosis")
p = ggplot(data = df.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=diagnosis)) + facet_wrap( ~ variable, scales="free")+ xlab("Variables") + ylab("")+ guides(fill=guide_legend(title="Group"))
p


#Worst
df.m = melt(data[,c(2,23:32)], id.var = "diagnosis")
p = ggplot(data = df.m, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=diagnosis)) + facet_wrap( ~ variable, scales="free")+ xlab("Variables") + ylab("")+ guides(fill=guide_legend(title="Group"))
p


# Osserviamo la correlazione tra gli attributi
matrice_correlazione = cor(data[,3:ncol(data)])
corrplot(matrice_correlazione, order="hclust", tl.cex=1, addrect = 8) #C'è una grande correlazione tra alcuni attributi





## 3) PCA - Principal Component Analysis 
pca_res <- prcomp(data[,3:ncol(data)], center = TRUE, scale = TRUE)
summary(pca_res)
fviz_eig(pca_res, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer All Variances - PCA",
       x = "Principal Components", y = "% of variances")






## KNN
# divido dataset
set.seed(123)
smp_size <- floor(0.70 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

# controllo bilanciamento di train e test set
prop.table(table(train$diagnosis))*100
prop.table(table(test$diagnosis))*100


# train
control <- trainControl(method='repeatedcv', number=10, repeats=3)
knnFit <- train(diagnosis ~ ., data = train[,-1], method = "knn", trControl = control,tuneLength = 20)
plot(knnFit)


# testing
knnPredict <- predict(knnFit,newdata = test )
cm_knn<-confusionMatrix(knnPredict, test$diagnosis )
cm_knn




## K-MEANS
# train
predict.kmeans <- function(newdata, object){
  centers <- object$centers
  n_centers <- nrow(centers)
  dist_mat <- as.matrix(dist(rbind(centers, newdata)))
  dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
  max.col(-dist_mat)
}

learn_kmeans <- kmeans(train[,-c(1,2)], centers=2)

# testing
pre_kmeans <- predict.kmeans(test[,-c(1,2)],learn_kmeans)
pre_kmeans <- factor(ifelse(pre_kmeans == 1,"B","M"))
cm_kmeans <- confusionMatrix(pre_kmeans, test$diagnosis)
cm_kmeans




