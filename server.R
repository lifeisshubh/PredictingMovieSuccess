#Server.R

library(ROCR)
library(caTools)
library(rpart)
library(rpart.plot)
library(fastAdaboost)
library(caret)
library(e1071)
library(randomForest)
library(PerformanceAnalytics)
train <- read.csv("data/train.csv")
train$Success <- as.factor(train$Success)
train$Genre <- as.factor(train$Genre)
train$Sequel <- as.factor(train$Sequel)
train$MPAA_Rating <- as.factor(train$MPAA_Rating)
av <- ""



logisticRegModel <- glm(Success ~ youtubeView + FacebookMovieLike + Budget + runtime + Screen,family = binomial,data=train)
CARTModel <- rpart(Success ~ youtubeView + FacebookMovieLike + Budget + runtime + Screen, data=train, method = "class",minbucket=30)
rfModel <- randomForest(Success ~ youtubeView + FacebookMovieLike + Budget + runtime + Screen,data=train,nodesize=25,ntree=2000)
adaBoModel <- adaboost(Success ~ youtubeView + FacebookMovieLike + Budget + runtime + Screen,data=train,nIter=100)
svmModel <- svm(Success ~ youtubeView + FacebookMovieLike + Budget + runtime + Screen,data=train,cost=100,gamma=0.01)





shinyServer(function(input,output){
  
  
  
  
  
  output$contents <- renderTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    test <- read.csv(inFile$datapath)
    test$Success <- as.factor(test$Success)
    test$Genre <- as.factor(test$Genre)
    test$Sequel <- as.factor(test$Sequel)
    test$MPAA_Rating <- as.factor(test$MPAA_Rating)
    
    
    
    prLogistic <- predict(logisticRegModel,type="response",newdata = test)
    prCART <- predict(CARTModel,type="class",newdata = test)
    prRF <- predict(rfModel, newdata=test)
    prAB <- predict(adaBoModel, newdata = test)
    prSVM <- predict(svmModel, newdata = test)
    tlr <- table(test$Success,prLogistic>0.5)
    tca <- table(test$Success,prCART)
    tada <- table(test$Success,prAB$class)
    tsvm <- table(test$Success,prSVM)
    trf <- table(test$Success,prRF)
    av <- NULL
    nm <- NULL
    alr <- (tlr[1]+tlr[4])/(tlr[1]+tlr[2]+tlr[3]+tlr[4])
    aca <- (tca[1]+tca[4])/(tca[1]+tca[2]+tca[3]+tca[4])
    aada <- (tada[1]+tada[4])/(tada[1]+tada[2]+tada[3]+tada[4])
    asvm <- (tsvm[1]+tsvm[4])/(tsvm[1]+tsvm[2]+tsvm[3]+tsvm[4])
    arf <- (trf[1]+trf[4])/(trf[1]+trf[2]+trf[3]+trf[4])
    if(input$lr==TRUE){
      av <- c(alr)
      nm <- c("LR")
      output$cfMatlr<-renderPlot({
        
        textplot(      #wrap textplot around capture.output
          capture.output(     #capture output of confusionMatrix in text format
            confusionMatrix(prLogistic>0.5, test$Success)  #your original code here
          ))
        
        
      })
    }
    if(input$ca==TRUE){
      av <- c(av,aca)
      nm <- c(nm,"CART")
      
      output$cfMatcart<-renderPlot({
        
        textplot(      #wrap textplot around capture.output
          capture.output(     #capture output of confusionMatrix in text format
            confusionMatrix(prCART, test$Success)  #your original code here
          ))
        
      })
      
      
    }
    if(input$rf==TRUE){
      av <- c(av,arf)
      nm <- c(nm,"RF")
      
      output$cfMatrf<-renderPlot({
        
        textplot(      #wrap textplot around capture.output
          capture.output(     #capture output of confusionMatrix in text format
            confusionMatrix(prRF, test$Success)  #your original code here
          ))
        
      })
      
      
    }
    if(input$sv==TRUE){
      av <- c(av,asvm)
      nm <- c(nm,"SVM")
      
      output$cfMatSVM<-renderPlot({
        
        textplot(      #wrap textplot around capture.output
          capture.output(     #capture output of confusionMatrix in text format
            confusionMatrix(prSVM, test$Success)  #your original code here
          ))
        
      })
      
      
    }
    if(input$ab==TRUE){
      av <- c(av,aada)
      nm <- c(nm,"AdaBoost")
      
      output$cfMatab<-renderPlot({
        
        textplot(      #wrap textplot around capture.output
          capture.output(     #capture output of confusionMatrix in text format
            confusionMatrix(prAB$class, test$Success)  #your original code here
          ))
        
      })
      
      
    }
    
    
    if(is.null(av)){
      av <- c(0)
      nm <- c("")
    }
    names(av) <- nm
    
    output$phonePlot <- renderPlot({
      
      
      barplot(av, ylim=c(0,1),ylab = "Accuracy",xlab = "Model", main = "Model vs Accuracy")  
      
      
    })
    
    
    
    
    
    
    
    output$rowcal <- renderText({
      paste("Entries in Dataset : ",nrow(test))
    })
    
    
    
    
    test
  })
  
  # Fill in the spot we created for a plot
    
  
  
    
})
  
