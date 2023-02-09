View(dataset)
str(dataset)
#need to do preprocessing before checking for outliers
#plotting before or after


getwd()
setwd("/Users/ja/Desktop/college/Level 8/Mining")
dataset = read.csv('diabetes_csv.csv')


#-----------------------------------data summary


#number of objects
nrow(dataset)

#number of attributes
ncol(dataset)

#dataset data types
str(dataset)

#data summary
summary(dataset)

#number of null cells
sum(is.na(dataset))



#-----------------------------------plotting

boxplot(dataset$preg)
boxplot(dataset$plas)
boxplot(dataset$pres)
boxplot(dataset$skin)
boxplot(dataset$insu)
boxplot(dataset$mass)
boxplot(dataset$pedi)
boxplot(dataset$age)


#histogram plotting
hist(dataset$preg)
hist(dataset$plas)
hist(dataset$pres)
hist(dataset$skin)
hist(dataset$insu)
hist(dataset$mass)
hist(dataset$pedi)
hist(dataset$age)

#scatter plotting
#install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(dataset$preg, dataset$age, dataset$mass)
scatterplot3d(dataset$plas, dataset$insu, dataset$pedi)
scatterplot3d(dataset$pres, dataset$skin, dataset$mass)

#bar char
#install.packages("dplyr")
library(dplyr)
dataset$class %>% table() %>% barplot() 
tab <- dataset$class %>% table()
precentages <- tab %>% prop.table() %>% round(3) * 100 
txt <- paste0(names(tab), '\n', precentages, '%')
bb <- dataset$class %>% table() %>% barplot(axisnames=F, main='class', ylab='Frequency',col=c('lightblue', 'purple'))
text(bb, tab/2, labels=txt, cex=1.5)


#-----------------------------------preprocessing


#replacing null with avg
dataset$pres = ifelse(is.na(dataset$pres), ave(dataset$pres , FUN = function(x) mean(x,na.rm=TRUE)), dataset$pres)
dataset$skin = ifelse(is.na(dataset$skin), ave(dataset$skin , FUN = function(x) mean(x,na.rm=TRUE)), dataset$skin)
dataset$mass = ifelse(is.na(dataset$mass), ave(dataset$mass , FUN = function(x) mean(x,na.rm=TRUE)), dataset$mass)
dataset$plas = ifelse(is.na(dataset$plas), ave(dataset$plas , FUN = function(x) mean(x,na.rm=TRUE)), dataset$plas)
sum(is.na(dataset))

#removing null insulin cells
dataset = na.omit(dataset)

#removing outliers
#preg
Q1 <- quantile(dataset$preg, .25)
Q3 <- quantile(dataset$preg, .75)
IQR <- IQR(dataset$preg)
outliers =(subset(dataset, dataset$preg > (Q1 - 1.5*IQR) & dataset$preg < (Q3 + 1.5*IQR)))
nrow(dataset)-nrow(outliers)
dataset <- subset(dataset, dataset$preg > (Q1 - 1.5*IQR) & dataset$preg < (Q3 + 1.5*IQR))

#plasma
Q1 <- quantile(dataset$plas, .25)
Q3 <- quantile(dataset$plas, .75)
IQR <- IQR(dataset$plas)
outliers =(subset(dataset, dataset$plas > (Q1 - 1.5*IQR) & dataset$plas < (Q3 + 1.5*IQR)))
nrow(dataset)-nrow(outliers)
dataset <- subset(dataset, dataset$plas > (Q1 - 1.5*IQR) & dataset$plas < (Q3 + 1.5*IQR))

#pressure
Q1 <- quantile(dataset$pres, .25)
Q3 <- quantile(dataset$pres, .75)
IQR <- IQR(dataset$pres)
outliers =(subset(dataset, dataset$pres > (Q1 - 1.5*IQR) & dataset$pres < (Q3 + 1.5*IQR)))
nrow(dataset)-nrow(outliers)
dataset <- subset(dataset, dataset$pres > (Q1 - 1.5*IQR) & dataset$pres < (Q3 + 1.5*IQR))

#skin Thickness
Q1 <- quantile(dataset$skin, .25)
Q3 <- quantile(dataset$skin, .75)
IQR <- IQR(dataset$skin)
outliers =(subset(dataset, dataset$skin > (Q1 - 1.5*IQR) & dataset$skin < (Q3 + 1.5*IQR)))
nrow(dataset)-nrow(outliers)
dataset <- subset(dataset, dataset$skin > (Q1 - 1.5*IQR) & dataset$skin < (Q3 + 1.5*IQR))

#insulin
Q1 <- quantile(dataset$insu, .25)
Q3 <- quantile(dataset$insu, .75)
IQR <- IQR(dataset$insu)
outliers =(subset(dataset, dataset$insu > (Q1 - 1.5*IQR) & dataset$insu < (Q3 + 1.5*IQR)))
nrow(dataset)-nrow(outliers)
dataset <- subset(dataset, dataset$insu > (Q1 - 1.5*IQR) & dataset$insu < (Q3 + 1.5*IQR))

#mass
Q1 <- quantile(dataset$mass, .25)
Q3 <- quantile(dataset$mass, .75)
IQR <- IQR(dataset$mass)
outliers =(subset(dataset, dataset$mass > (Q1 - 1.5*IQR) & dataset$mass < (Q3 + 1.5*IQR)))
nrow(dataset)-nrow(outliers)
dataset <- subset(dataset, dataset$mass > (Q1 - 1.5*IQR) & dataset$mass < (Q3 + 1.5*IQR))

#pedi
Q1 <- quantile(dataset$pedi, .25)
Q3 <- quantile(dataset$pedi, .75)
IQR <- IQR(dataset$pedi)
outliers =(subset(dataset, dataset$pedi > (Q1 - 1.5*IQR) & dataset$pedi < (Q3 + 1.5*IQR)))
nrow(dataset)-nrow(outliers)
dataset <- subset(dataset, dataset$pedi > (Q1 - 1.5*IQR) & dataset$pedi < (Q3 + 1.5*IQR))

#age
Q1 <- quantile(dataset$age, .25)
Q3 <- quantile(dataset$age, .75)
IQR <- IQR(dataset$age)
outliers =(subset(dataset, dataset$age > (Q1 - 1.5*IQR) & dataset$age < (Q3 + 1.5*IQR)))
nrow(dataset)-nrow(outliers)
dataset <- subset(dataset, dataset$age > (Q1 - 1.5*IQR) & dataset$age < (Q3 + 1.5*IQR))

#Encoding
dataset$class = factor(dataset$class, levels = c("tested_negative", "tested_positive"), labels = c(0,1))



#--------------------------------------------------------------------------------------classification1

set.seed(1234)
ind <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.7, 0.3))
trainData <- dataset[ind==1,]
testData <- dataset[ind==2,]

#install.packages('party')
library(party)
myFormula <- class ~ preg + mass + plas + pres + skin + insu + pedi + age
dataset_ctree <- ctree(myFormula, data=trainData)

# check the prediction
table(predict(dataset_ctree), trainData$class)
print(dataset_ctree)
plot(dataset_ctree,type="simple")
plot(dataset_ctree)


# predict on test data
testPred <- predict(dataset_ctree, newdata = testData)
table(testPred, testData$class)




#install.packages('caret')
library(caret)

results <- confusionMatrix(testPred, testData$class)
acc <- results$overall["Accuracy"]*100
acc
results


#--------------------------------------------------------------------------------------classification2
set.seed(1234)
ind <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.75, 0.25))
trainData <- dataset[ind==1,]
testData <- dataset[ind==2,]

install.packages('party')
library(party)
myFormula <- class ~ preg + mass + plas + pres + skin + insu + pedi + age
dataset_ctree <- ctree(myFormula, data=trainData)

# check the prediction
table(predict(dataset_ctree), trainData$class)
print(dataset_ctree)
plot(dataset_ctree,type="simple")
plot(dataset_ctree)


# predict on test data
testPred <- predict(dataset_ctree, newdata = testData)
table(testPred, testData$class)



install.packages('caret')
library(caret)
results <- confusionMatrix(testPred, testData$class)
acc <- results$overall["Accuracy"]*100
acc
results
as.table(results)
as.matrix(results)
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")
print(results)

#--------------------------------------------------------------------------------------classification3
set.seed(1234)
ind <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.8, 0.2))
trainData <- dataset[ind==1,]
testData <- dataset[ind==2,]

install.packages('party')
library(party)
myFormula <- class ~ preg + mass + plas + pres + skin + insu + pedi + age
dataset_ctree <- ctree(myFormula, data=trainData)

# check the prediction
table(predict(dataset_ctree), trainData$class)
print(dataset_ctree)
plot(dataset_ctree,type="simple")
plot(dataset_ctree)


# predict on test data
testPred <- predict(dataset_ctree, newdata = testData)
table(testPred, testData$class)

# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
datas<- train(class ~., data = dataset, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)


install.packages('caret')
library(caret)
results <- confusionMatrix(testPred, testData$class)
acc <- results$overall["Accuracy"]*100
acc
results
as.table(results)
as.matrix(results)
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")
print(results)

#---------------------------------------------------------------------------------------------------Clustring1
# k-means clustering set a seed for random number generation  to make the results reproducible
set.seed(8953)
#Remove class
dataset3 <- dataset[, unlist(lapply(dataset, is.numeric))]
dataset3 <- scale(dataset3)
# run kmeans clustering to find 4 clusters
kmeans.result <- kmeans(dataset3, 4)
# print the clusterng result
kmeans.result
## visualize clustering
#install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans.result, data = dataset3)

#silhouette for each cluster
sil <- silhouette(kmeans.result$cluster,dist(dataset3))
fviz_silhouette(sil)



#---------------------------------------------------------------------------------------------------Clustring2
# k-means clustering set a seed for random number generation  to make the results reproducible
set.seed(8953)
#Remove class
dataset3 <- dataset[, unlist(lapply(dataset, is.numeric))]
dataset3 <- scale(dataset3)
# run kmeans clustering to find 4 clusters
kmeans.result <- kmeans(dataset3, 3)
# print the clusterng result
kmeans.result
# visualize clustering
#install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans.result, data = dataset3)
#install.packages("fpc")
library(fpc)
#kmeansruns() :
#It calls  kmeans() to perform  k-means clustering
#It initializes the k-means algorithm several times with random points from the data set as means.
#It estimates the number of clusters by Calinski Harabasz index or average silhouette width
kmeansruns.result <- kmeansruns(dataset3)  
kmeansruns.result
fviz_cluster(kmeans.result, data = dataset3)
#silhouette for each cluster
library (cluster)
sil <- silhouette(kmeans.result$cluster,dist(dataset3))
fviz_silhouette(sil)

#---------------------------------------------------------------------------------------------------Clustring3
# k-means clustering set a seed for random number generation  to make the results reproducible
set.seed(8953)
#Remove class
dataset3 <- dataset[, unlist(lapply(dataset, is.numeric))]
dataset3 <- scale(dataset3)
# run kmeans clustering to find 4 clusters
kmeans.result <- kmeans(dataset3, 2)
# print the clusterng result
kmeans.result
#visualize clustering
install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans.result, data = dataset3)

#silhouette for each cluster
sil <- silhouette(kmeans.result$cluster,dist(dataset3))
fviz_silhouette(sil)
#--------------------------------------------------------------------------------------------
#for all clusters
fviz_nbclust(dataset3, kmeans, method = "silhouette")+ labs(subtitle = "Silhouette method")


