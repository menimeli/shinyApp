library(shiny)
library(bomrang)
library(tidyverse)
library(forecast)
library(dplyr)
library(randomForestSRC)
library(caret)

shinyServer(function(input, output) {
    
    l <- reactive(input$loops)
    f <- reactive(input$folds)
    class <- reactive(input$classifier)
    observe({
        if (( "rf" %in% class() && l() > 20) || ("svm" %in% class() && l() > 20 )){
            showNotification(ui = "Run time will be long!", duration = 10, type = "message")
        }
    })
   
    output$accuracy = renderPlot({
        cvK = l()  # number of CV folds
        cl = class()
        cv_50acc5_knn = cv_50acc5_svm = cv_50acc5_rf = c()
        cv_acc_knn = cv_acc_svm = cv_acc_rf = c()
        X<- readRDS("X.rds")
        y<- readRDS("y.rds")
        n_sim = l() ## number of repeats
        if ("knn" %in% cl){
            x <- reactive(input$k) 
            val<-x()
        }
        
        for (i in 1:n_sim) {
            cvSets = cvTools::cvFolds(nrow(X), cvK)  # permute all the data, into 5 folds
            cv_acc_knn = cv_acc_svm = cv_acc_rf = c()
            
            for (j in 1:cvK) {
                test_id = cvSets$subsets[cvSets$which == j]
                X_test = X[test_id, ]
                X_train = X[-test_id, ]
                y_test = y[test_id]
                y_train = y[-test_id]
                
                if ("knn" %in% cl){
    
                    ## KNN
                    fit5 = class::knn(train = X_train, test = X_test, cl = y_train, k = val)
                    cv_acc_knn[j] = table(fit5, y_test) %>% diag %>% sum %>% `/`(length(y_test))
                    
               }
                if ("svm"  %in% cl){
                    ## SVM
                    svm_res <- e1071::svm(x = X_train, y = as.factor(y_train))
                    fit <- predict(svm_res, X_test)
                    cv_acc_svm[j] = table(fit, y_test) %>% diag %>% sum %>% `/`(length(y_test))
                    
                }
                if ( "rf"  %in% cl){
                    ## RandomForest
                    rf_res <- randomForest::randomForest(x = X_train, y = as.factor(y_train))
                    fit <- predict(rf_res, X_test)
                    cv_acc_rf[j] = table(fit, y_test) %>% diag %>% sum %>% `/`(length(y_test))
                    
                }
               
                
            }
            
            if ("knn"  %in% cl){
               cv_50acc5_knn <- append(cv_50acc5_knn, mean(cv_acc_knn))
            }
            if ("svm" %in% cl){
                cv_50acc5_svm <- append(cv_50acc5_svm, mean(cv_acc_svm))
            }
            if ("rf" %in% cl){
                cv_50acc5_rf <- append(cv_50acc5_rf, mean(cv_acc_rf))
            }
            
           
        } ## end for
        if ("knn" %in% cl && "svm" %in% cl && "rf" %in% cl){
            boxplot(cv_50acc5_knn, cv_50acc5_svm, cv_50acc5_rf, names = c("knn", "svm", "rf"), ylab = "Accuracy")
            title("Cross Validation Accuracy for All Classifiers")
            
        }
        else if ("knn" %in% cl && "rf" %in% cl){
            boxplot(cv_50acc5_knn, cv_50acc5_rf, names = c("knn", "rf"), ylab = "Accuracy")
            title("Cross Validation Accuracy for Random Forests and KNN")
            
        }
        else if ("svm" %in% cl && "rf" %in% cl){
            boxplot(cv_50acc5_svm, cv_50acc5_rf, names = c("svm", "rf"), ylab = "Accuracy")
            title("Cross Validation Accuracy for Random Forests and SVM")
            
            
        }
        else if ("knn" %in% cl && "svm" %in% cl){
            boxplot(cv_50acc5_knn, cv_50acc5_svm, names = c("knn", "svm"), ylab = "Accuracy")
            title("Cross Validation Accuracy for KNN and SVM")
            
        }
       
        else if ("knn" %in% cl){
            boxplot(cv_50acc5_knn, xlab = "knn", ylab = "Accuracy")
            title("Cross Validation Accuracy for K Nearest Neighbours")
            
        }
        else if ("svm" %in% cl){
            boxplot(cv_50acc5_svm, xlab = "svm", ylab = "Accuracy")
            title("Cross Validation Accuracy for Support Vector Machines")
            
        }
        else if ("rf" %in% cl){
            boxplot(cv_50acc5_rf, xlab = "rf", ylab = "Accuracy")
            title("Cross Validation Accuracy for Random Forests")
        }

    })
    
})
