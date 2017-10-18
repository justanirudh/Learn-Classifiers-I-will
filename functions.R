library(readxl)
library("RWeka")
library(caret) 
library(partykit)
library(kernlab)

#GENERIC FUNCTIONS

#prepares dataset eligible for modelling
prepareDataset <- function(path){
  dataset <- read_excel(path)
  dataset <- dataset[-2]
  dataset$Continent <- as.factor(dataset$Continent) 
  return(dataset)
}

#divides dataset into training and test
divideDataset <- function(dataset){
  ind <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.8, 0.2))
  training <- dataset[ind==1,]
  test <- dataset[ind==2,]
  return(list(training, test))
}

#predicting for all models
genPredict <- function(model, test){
  pred <- predict(model, test)
  return(pred) 
}

#knn and svm preprocessing
genPreProcess <- function(training){
  trainX <- training[,names(training) != "Continent"]
  preProcValues <- preProcess(x = trainX, method = c("center", "scale"))
  return(preProcValues)
}

#MODEL-SPECIFIC FUNCTIONS

#C4.5 model
c45Model <- function(training){
  model <- J48(Continent~., training)
  return(model)
}

#RIPPER model
RIPPERModel <- function(training){
  model <- JRip(Continent~., training)
  return(model)
}

#knn model
knnModel <- function(training){
  ctrl <- trainControl(method="cv")
  model <- train(Continent~., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
  return(model)
}

#svm model
svmModel <- function(training){
  ctrl <- trainControl(method="cv")
  grid <- expand.grid(C = c(seq(0.25,10, by=0.25)))
  model <- train(Continent~., data = training, method = "svmLinear", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20, tuneGrid = grid)
  return(model)
}

#MODEL-RUN FUNCTIONS

#Run C4.5 classification
c45Run <- function(path){
  dataset <- prepareDataset(path)
  
  accuracies <- array(5)
  best_conf <- NULL
  best_model <- NULL
  max_acc <- NULL
  
  #Classify 5 times and find avg, std. dev of accuracies, max_accuracy, precision, f-measure, etc.
  for(count in 1:5){
    seed <- count + 8
    set.seed(seed)
    
    #divide datasets
    list_datasets <- divideDataset(dataset)
    training <- list_datasets[[1]]
    test <- list_datasets[[2]]
    
    #creating model: Continent as a function of Overall Life and Rank
    model <- c45Model(training)
    
    #prediction
    pred <-  genPredict(model, test)
    
    #confusion matrix
    actual <- test$Continent
    conf <- table(pred, actual)
    conf <- confusionMatrix(conf)
    
    #accuracy
    accuracy <- conf$overall[1]
    if( is.null(max_acc) || accuracy > max_acc){
      max_acc = accuracy
      best_conf = conf
      best_model = model
    }
    accuracies[count] = accuracy
  }
  
  plot(best_model)
  
 return(list(accuracies, best_conf, max_acc, best_model))
}

#Run RIPPER classification
RIPPERRun <- function(path){
  dataset <- prepareDataset(path)
  
  accuracies <- array(5)
  best_conf <- NULL
  best_model <- NULL
  max_acc <- NULL
  
  #Classify 5 times and find avg and std. dev of accuracies
  for(count in 1:5){
    seed <- count + 8
    set.seed(seed)
    
    #divide datasets
    list_datasets <- divideDataset(dataset)
    training <- list_datasets[[1]]
    test <- list_datasets[[2]]
    
    #creating model: Continent as a function of Overall Life and Rank
    model <- RIPPERModel(training)
    
    #prediction
    pred <-  genPredict(model, test)
    
    #confusion matrix
    actual <- test$Continent
    conf <- table(pred, actual)
    conf <- confusionMatrix(conf)
    
    #accuracy
    accuracy <- conf$overall[1]
    if( is.null(max_acc) || accuracy > max_acc){
      max_acc = accuracy
      best_conf = conf
      best_model = model
    }
    accuracies[count] = accuracy
  }
  return(list(accuracies, best_conf, max_acc, best_model))
}

knnRun <- function(path){
  dataset <- prepareDataset(path)
  
  accuracies <- array(5)
  best_conf <- NULL
  best_model <- NULL
  max_acc <- NULL
  
  #Classify 5 times and find avg and std. dev of accuracies
  for(count in 1:5){
    seed <- count + 8
    set.seed(seed)
    
    #divide datasets
    list_datasets <- divideDataset(dataset)
    training <- list_datasets[[1]]
    test <- list_datasets[[2]]
    
    ##pre-processing; centering and scaling
    preProcValues <- genPreProcess(training)
    
    #creating model: Continent as a function of Overall Life and Rank
    set.seed(seed)
    model <- knnModel(training)
    
    #prediction
    pred <- genPredict(model, test)
    
    ##get confusion matrix
    conf <- confusionMatrix(pred, test$Continent )
    
    #get accuracy
    accuracy <- conf$overall[1]
    if( is.null(max_acc) || accuracy > max_acc){
      max_acc = accuracy
      best_conf = conf
      best_model = model
    }
    
    accuracies[count] = accuracy
  }
  
  plot(best_model)
  
  return(list(accuracies, best_conf, max_acc, best_model))
}

svmRun <- function(path){
  dataset <- prepareDataset(path)
  
  accuracies <- array(5)
  best_conf <- NULL
  best_model <- NULL
  max_acc <- NULL
  
  #Classify 5 times and find avg and std. dev of accuracies
  for(count in 1:5){
    seed <- count + 8
    set.seed(seed)
    
    #divide datasets
    list_datasets <- divideDataset(dataset)
    training <- list_datasets[[1]]
    test <- list_datasets[[2]]
    
    ##pre-processing; centering and scaling
    preProcValues <- genPreProcess(training)
    
    #creating model: Continent as a function of Overall Life and Rank
    set.seed(seed)
    model <- svmModel(training)
    
    #prediction
    pred <- genPredict(model, test)
    
    ##get confusion matrix
    conf <- confusionMatrix(pred, test$Continent )
    
    #get accuracy
    accuracy <- conf$overall[1]
    if( is.null(max_acc) || accuracy > max_acc){
      max_acc = accuracy
      best_conf = conf
      best_model = model
    }
    
    accuracies[count] = accuracy
  }
  
  plot(best_model)
  
  return(list(accuracies, best_conf, max_acc, best_model))
}

#RUN CLASSIFICATIONS

#Give correct path of xlsx file here
path <- "~/R/project1/dataset.xlsx"

#C4.5 run
results <- c45Run(path)

#RIPPER run
#results <- RIPPERRun(path)

#knn run
#results <- knnRun(path)

#svm run
#results <- svmRun(path)

accuracies <- results[[1]]
best_conf <- results[[2]]
max_acc <- results[[3]]
best_model <- results[[4]]
print(best_conf) #printing thest best confusion matrix and related information 
precision <- best_conf$byClass[,5]
recall <- best_conf$byClass[,6]
f1 <- best_conf$byClass[,7]
avg_accuracy <- mean(accuracies)
std_accuracies <- sd(accuracies)
print(best_model) #printing the best model
