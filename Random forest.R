install.packages('rgl')
library('rgl')
library('dplyr')
library('ggplot2')
raw.Training <- read.csv('training_dataset.csv',stringsAsFactors = FALSE)
raw.Testing <- read.csv('evaluation_dataset.csv', stringsAsFactors = FALSE)
str(raw.Training)
View(raw.Training)
View(raw.Testing)

summary(raw.Training)
is.na(raw.Training)
summary(raw.Testing)
is.na(raw.Training)
sum(is.na(raw.Training))
########################
raw.Training[is.na(raw.Training)] <- 'None'
raw.Testing[is.na(raw.Testing)] <- 'None'

Train <- data.frame(lapply(raw.Training, function(x) as.numeric(as.factor(as.character(x)))))
View(Train)


Test <- data.frame(lapply(raw.Testing, function(x) as.numeric(as.factor(as.character(x)))))
View(Test)
##########################

pca <- prcomp(Train,scale=TRUE)
plot(pca$x[,1],pca$x[,2])
pca.var <- pca$sdev^2
pca.var.pecnt <- round(pca.var/sum(pca.var)*100,1)
barplot(pca.var.pecnt,main="Variation Plot",xlab="Principal Component",ylab="Percent Variation")

loading_scores <- pca$rotation[ ,1]
var_scores <- abs(loading_scores)
var_scores_ranked <- sort(var_scores, decreasing = TRUE)
top_10_var <- names(var_scores_ranked[1:10])
#pca$rotation[top_10_var,1]

install.packages("party")
library(party)
cf1 <- cforest(SalePrice ~ . , data= Train, control=cforest_unbiased(mtry=2,ntree=50)) 
varimp(cf1)


##########################
install.packages("randomForest")
library(randomForest)
#ModelRF <-randomForest(SalePrice~YearBuilt+GarageYrBlt+GarageCars+BsmtQual+YearRemodAdd+ExterQual+BsmtFinSF1+ExterQual+OverallQual+GarageArea + Foundation +FullBath,data = Train)

#ModelRF1 <-randomForest(SalePrice~BsmtQual+OverallQual+FullBath+KitchenQual+ GrLivArea+GarageArea+GarageCars+LotArea+YearBuilt+Fireplaces,data = Train)

ModelLM <-lm(SalePrice~YearBuilt+GarageYrBlt+GarageCars+BsmtQual+YearRemodAdd+ 
               ExterQual+BsmtFinSF1+ExterQual+OverallQual+GarageArea + Foundation +FullBath,
             data = raw.Training)

ModelLM1 <-lm(SalePrice~BsmtQual+OverallQual+FullBath+KitchenQual+
                GrLivArea+GarageArea+GarageCars+LotArea+YearBuilt+Fireplaces,data = raw.Training)

validation<-predict(ModelLM,raw.Training)
validation1<-predict(ModelLM1,raw.Training)


SSE=sum((validation-raw.Training$SalePrice)^2)
SSE
RMSE=(SSE/nrow(raw.Training))^(0.5)
RMSE
SSE=sum((validation1-raw.Training$SalePrice)^2)
SSE
RMSE=(SSE/nrow(raw.Training))^(0.5)
RMSE

Test<-predict(ModelLM,raw.Training)
Test1<-predict(ModelLM1,raw.Training)


write.csv(Test,file = "House_price_prediction.csv", col.names = c(Id,SalePrice))
write.csv(Test1,file = "House_price_prediction1.csv", col.names = c(Id,SalePrice))


