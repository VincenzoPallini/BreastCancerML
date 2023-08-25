nrows <- NROW(wbcd)
set.seed(218)				            ## fissare il valore casuale
index <- sample(1:nrows, 0.7 * nrows)	## mischiare e dividere

#train <- wbcd		        	        ## 569 test data (100%)
train <- wbcd[index,]			        ## 398 test data (70%)
test <- wbcd[-index,]  		            ## 171 test data (30%)

### 5-2) Controllare la proporzione di diagnosi (Benigna/Maligna) {.tabset}
#### train

prop.table(table(train$diagnosis))


#### test

prop.table(table(test$diagnosis))


fitControl <- trainControl(method="cv",
                           number = 5,
                           preProcOptions = list(thresh = 0.99), # soglia per il preprocesso pca
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)


### 5-3) Applicare tutti i metodi ML ai dati {.tabset}

library(caret)

#### K-Means
##### Fare in modo che KMEANS preveda la funzione
##dobbiamo creare una funzione che preveda l'utilizzo dei metodi kmeans, poiché la funzione orgin predict non supporta kmeans.

predict.kmeans <- function(newdata, object){
    centers <- object$centers
    n_centers <- nrow(centers)
    dist_mat <- as.matrix(dist(rbind(centers, newdata)))
    dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
    max.col(-dist_mat)
}


##### Applico kmeans
##bisogna applicare i centri a 2, dato che ci sono solo due fattori (benigno, maligno)

library(caret)
learn_kmeans <- kmeans(train[,-1], centers=2)

pre_kmeans <- predict.kmeans(test[,-1],learn_kmeans)
pre_kmeans <- ifelse(pre_kmeans == 1,"Benign","Malignant")
cm_kmeans <- confusionMatrix(pre_kmeans, test$diagnosis)
cm_kmeans


##### plot

library(factoextra)
learn_kmeans$cluster <- ifelse(learn_kmeans$cluster == 1,"Benign","Malignant")
fviz_cluster(learn_kmeans, data = train[,-1])

## Applico Rete Neurale

model_pca_nnet <- train(diagnosis~.,
                        train,
                        method="nnet",
                        metric="ROC",
                        preProcess=c('center', 'scale', 'pca'),
                        tuneLength=10,
                        trace=FALSE,
                        trControl=fitControl)

pred_pca_nnet <- predict(model_pca_nnet, test)
cm_pca_nnet <- confusionMatrix(pred_pca_nnet, test$diagnosis, positive = "M") ## Errore: valore di "positive" non è presente nel dataset test$diagnosis
cm_pca_nnet
