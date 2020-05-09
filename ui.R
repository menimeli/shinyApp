library(shiny)


shinyUI(
  fluidPage(

    titlePanel(title = "Cross Validation Accuracy for Different Classifiers and Hyper-Parameters"),
   
    selectInput("classifier", label = "Classifier(s)", multiple = TRUE, choices = c("K nearest neighbors" = "knn", "Support vector machines" = "svm", "Random forests" = "rf"), selected = "knn"),
    numericInput("loops", "Number of CV repeats", 25, min = 1, max = 50),
    numericInput("folds", "Number of CV folds", 5, min = 2, max = 10),
      sidebarLayout(
          conditionalPanel( condition = "input.classifier.includes('knn')",
                sidebarPanel(
                 sliderInput(inputId = "k",
                            label = "Value of k (for k nearest neighbours):",
                            min = 1,
                            max = 20,
                            value = 5)
                )
            ),
      mainPanel(
        shiny::plotOutput(outputId = "accuracy")
        )
      
    )
    
  )
)
