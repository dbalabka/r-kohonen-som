#source('./my_kohonen_som.R');

set.seed(1);
data(iris)
monitor <- function(obj) {
  points(rep(obj@iteration, length(obj@output)), obj@output, pch = 20, col = 1:length(obj@output))
  lines(obj@iteration, obj@learnRateCurrent * 30)
}
data <- iris[,1:4]#iris[sample.int(nrow(iris)),1:4]
plot(c(1,nrow(data)), c(-20,20), type = "n")
network <- my_kohonen_som(data[sample(nrow(data)),], monitor=monitor, learnRate=0.6);
predictionResult <- my_kohonen_som.predict(network, scale(data))
print(paste('Count of clusters: ', length(unique(predictionResult))));
predictTable <- cbind(predictionResult, as.matrix(iris[,5]))
print(predictTable)
stop("uoiuhiu");


data(iris)
library(kohonen) 
set.seed(101)
train.obs <- sample(nrow(iris), 50) # get the training set observations
train.set <- scale(iris[train.obs,][,-5]) # check info about scaling data below
test.set  <- scale(iris[-train.obs, ][-5],
                   center = attr(train.set, "scaled:center"),
                   scale  = attr(train.set, "scaled:scale"))
som.iris <- som(train.set, grid = somgrid(5, 5, "hexagonal"))
plot(som.iris)

som.prediction <- 
  predict(som.iris, newdata = test.set,
          trainX = train.set,
          trainY = classvec2classmat(iris[,5][train.obs]))

table(iris[,5][-train.obs], som.prediction$prediction)

