library(shiny)
library(shinyWidgets)
library(shinythemes)

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ggplot2) 

library(dplyr)
library(caret)
library(tree)
library(randomForest)

############## Additional code for our Report  #################################

## proportion of each class
#table(factor(stars.full$class))/ length(stars.full$class)

## histogram for all features
#hist.data.frame(stars) # please enlarge to margin to make this command works

# train the tree model
#stars.tree = tree(class~., train)
#summary(stars.tree)
#stars.tree
# plot the tree
#plot(stars.tree)
#text(stars.tree, pretty = 1)

#pred = predict(stars.tree, test[,-which(colnames(stars)=='class')], type = 'class')
#paste("The decision tree classify the stars with accuracy:", mean(pred==test$class))
# prune
#set.seed(321)
#stars.cv = cv.tree(stars.tree, FUN = prune.misclass)
#stars.cv
##plot CV results
#plot(stars.cv$size,stars.cv$dev,type="b",
#     xlab="number of leaves of the tree",ylab="CV error rate%",
#     cex.lab=1.5,cex.axis=1.5)
#plot(stars.cv$k,stars.cv$dev,type="b",
#     xlab=expression(alpha),ylab="CV error rate%",
#     cex.lab=1.5,cex.axis=1.5)

## prune the tree
#stars.prune=prune.misclass(stars.tree,best=3)
#plot(stars.prune)
#text(stars.prune,pretty=1)
##predict the test instances
#pred.prune=predict(stars.prune,test[,-which(colnames(stars)=='class')],type="class")
#mean(pred.prune==test$class)

## just as a comparison with rf, no further analysis
#set.seed(921)
## build bagging model
#stars.bag=randomForest(class~.,data=train,mtry=16,importance=TRUE,ntree=500)
#stars.bag
## prediction
#pred.bag=predict(stars.bag,newdata=test[,-which(colnames(stars)=='class')])
## accuracy
#mean(pred.bag==test$class)

#plot(stars.rf$err.rate[,1], type = "l")
#plot(stars.rf)
#tail(plot(stars.rf)) # check which line represents which.
#legend("topright",legend=c("Overall OOB Error","Galaxy OOB Error","QSO OOB Error", "Star OOB Error"), lty = 2, col=c("black","red","green","blue")) #pch=10,

## 3 confusion matrix: previous output
#conf = stars.rf$confusion # same as the one in the output of stars.rf

# The description of the features in our data set
ol =tags$ol(
  tags$li("obj_ID = Object Identifier, the unique value that identifies the object in the image catalog used by the CAS"),
  tags$li("alpha = Right Ascension angle (at J2000 epoch)"),
  tags$li("delta = Declination angle (at J2000 epoch)"),
  tags$li("u = Ultraviolet filter in the photometric system"),
  tags$li("g = Green filter in the photometric system"),
  tags$li("r = Red filter in the photometric system"),
  tags$li("i = Near Infrared filter in the photometric system"),
  tags$li("z = Infrared filter in the photometric system"),
  tags$li("run_ID = Run Number used to identify the specific scan"),
  tags$li("rereun_ID = Rerun Number to specify how the image was processed"),
  tags$li("cam_col = Camera column to identify the scanline within the run"),
  tags$li("field_ID = Field number to identify each field"),
  tags$li("spec_obj_ID = Unique ID used for optical spectroscopic objects (this means that 2 different observations with the same spec_obj_ID must share the output class)"),
  tags$li("redshift = redshift value based on the increase in wavelength"),
  tags$li("plate = plate ID, identifies each plate in SDSS"),
  tags$li("MJD = Modified Julian Date, used to indicate when a given piece of SDSS data was taken"),
  tags$li("fiber_ID = fiber ID that identifies the fiber that pointed the light at the focal plane in each observation")
)

# Setup the ui object
ui <- fluidPage(theme = shinytheme("slate"),  # Setting the theme of the app
                
                # setting background Image
                setBackgroundImage(src = 'pexels-rakicevic-nenad-1274260_2.jpg'), 
                
                # Creating the navbar
                navbarPage(   
                  "Stellar classifier",
                
                # creating the Home tabs     
                  tabPanel("Home",
                           column(12, align="center",
                                  wellPanel(h1('This is a Stellar Classifier'),
                                            h3('We create this classifier in order to classify stellar objects based on their spectral characteristics into three classes: Stars, Galaxies and Quasars'),
                                            uiOutput("images"),
                                            h3('This classifiacation is one of the most fundamental in astronomy.'),
                                            h3('To perform this task we used two machine learning techniques: decision tree and random forest.')
                                            )
                           ) 
                  ),
                  # Creating Training Dataset Tab
                  tabPanel("Training Dataset", h1("Our Training Dataset"), wellPanel(dataTableOutput('data')) ),
                  
                  # Creating Decision Tree Tab
                  tabPanel("Decision Tree",
                           #Creating a side bar in order to include our inputs
                           sidebarLayout(
                             sidebarPanel(
                               # Button inputs
                               h4("Input for Prediction:"),
                               numericInput(inputId = "NumOpsTree", label = "Number of observations to predict", value = 15 , min = 1, max = 100),
                               p("Please click the 'Explore the data' button in order to generate a random test set of unseen observations that will appear in Data Tab."),
                               actionButton(inputId = "dataTree", label = "Explore the data"),
                               hr(),
                               h4("Predict:"),
                               p("Please click the 'Predict' button in order to generate the prediction for the selected test set. Please click the 'Prediction' tab to see the results"),
                               actionButton(inputId = "PredictTree", label = "Predict")
                               ),
                             # Creating Main Panel for Outputs
                             mainPanel(
                               # Create 2 Tab set Panels
                               tabsetPanel(type = "tabs",
                                           # Creating the Data Tab for main panel
                                           tabPanel("Data",
                                                    h3('Status/Output'),
                                                    verbatimTextOutput('DataContentsTree'),
                                                    tableOutput('tabledataTree'),
                                                    #Description of the data
                                                    wellPanel(tags$hr(),
                                                              h3("Dataset description"),
                                                              p("This dataset contains spectral characteristics for 10000 stellar objects. More specificaly it contains the following columns:"),
                                                              h3('Stars Classes'),
                                                              p("The classes of stellar  objects can be: galaxies, quasars or stars."),
                                                              h3("Variables:"),
                                                              ol) # Description of variables
                                           ),  
                                           # Creating the Prediction Tab for main panel
                                           tabPanel("Prediction",
                                                    # Status/Output Text Box
                                                    tags$label(h3('About our Decision Tree:')),
                                                    wellPanel(p('Decision tree is a very important machine learning algorithm, mainly used for classification problems.
                                                         It segments feature spaces into numbers of smaller (simpler) regions by using tree shape. 
                                                         It is a straightforward and interpretable model with a clear visualisation.')),
                                                    # Creating a Layout with 1 row and 2 columns
                                                    fluidRow(
                                                      column(7, plotOutput(outputId = "mainPlotTree")),
                                                      column(5, wellPanel(h5("Confusion Matrix:"), tableOutput("ConfusionMatrixTree")))
                                                      ),
                                                    # Creating a Layout with 1 row and 3 columns
                                                    fluidRow(
                                                      column(5, wellPanel(h5("Input:"), tableOutput("TestTree"))),
                                                      column(4, wellPanel(h5("Because in the Decision tree:"), tableOutput('tableExplanationTree'))),
                                                      column(3, wellPanel(h5("We predict:"), tableOutput('tablePredictionTree'))),
                                                    ),
                                                    # Creating a Layout with 1 row and 1 column
                                                    fluidRow(
                                                      column(12, wellPanel(verbatimTextOutput('contentsTree')))
                                                    ),         
                                           )
                                )
                              )
                             )
                  ),
                  
                  # Creating Random Forest Tab
                  tabPanel("Random Forest",
                           sidebarLayout(
                             #Creating a side bar in order to include our inputs
                             sidebarPanel(
                               # Button inputs
                               h4("Input for Prediction:"),
                               numericInput(inputId = "NumOpsRF", label = "Number of observations to predict", value = 15 , min = 1, max = 100),
                               p("Please click the 'Explore the data' button in order to generate a random test set of unseen observations that will appear in Data Tab."),
                               actionButton(inputId = "dataRF", label = "Explore the data"),
                               hr(),
                               h4("Predict:"),
                               p("Please click the 'Predict' button in order to generate the prediction for the selected test set. Please click the 'Prediction' tab to see the results"),
                               actionButton(inputId = "PredictRF", label = "Predict"),
                               ),
                             
                             # Creating Main Panel for Outputs
                             mainPanel(
                               tabsetPanel(type = "tabs", 
                                           tabPanel("Data",
                                                    tags$label(h3('Status/Output')), # Status/Output Text Box
                                                    verbatimTextOutput('DataContentsRF'),
                                                    tableOutput('tabledataRF'),
                                                    wellPanel(tags$hr(),
                                                              h3("Dataset description"),
                                                              p("This dataset contains spectral characteristics for 10000 stellar objects. More specificaly it contains the following columns:"),
                                                              h3('Stars Classes'),
                                                              p("The classes of stellar  objects can be: galaxies, quasars or stars."),
                                                              h3("Variables:"),
                                                              ol) # Description of variables
                                           ),
                                           
                                           # Creating Tab for Random forest explanation
                                           tabPanel("About Random Forest",
                                                    wellPanel(
                                                      h3('How the Random Forrest works:'),
                                                      hr(),
                                                      p('Random forest is a very popular model in machine learning, it is mainly used for classification problems.
                                                        It is consisted of many decision trees and contains randomness while building each decision tree.
                                                        The result of the model is based on majority vote of the decisions from trees.'),
                                                      # Output image
                                                      imageOutput(outputId = "Explanation_img_RF")
                                                      
                                                    )
                                           ),
                                           # Creating a prediction results table
                                           tabPanel("Prediction",
                                                    # Status/Output Text Box
                                                    tags$label(h3('Our Random Forrest:')),
                                                    # Creating a Layout with 1 row and 2 columns
                                                    fluidRow(
                                                      column(7, plotOutput(outputId = "mainPlotRF")),
                                                      column(5, wellPanel(h5("Confusion Matrix:"), tableOutput("ConfusionMatrixRF")))
                                                    ),
                                                    # Creating a Layout with 1 row and 3 columns
                                                    fluidRow(
                                                      column(5, wellPanel(h5("Input:"), tableOutput("TestRF"))),
                                                      column(4, wellPanel(h5("Summary of the voting results:"), tableOutput('tableExplanationRF'))),
                                                      column(3, wellPanel(h5("We predict:"), tableOutput('tablePredictionRF'))),
                                                    ),
                                                    # Creating a Layout with 1 row and 1 column
                                                    fluidRow(
                                                      column(12, wellPanel(verbatimTextOutput('contentsRF')))
                                                    ),  
                                                    ),
                                           )
                               )
                             )
                  ),
                  inverse = T # to make the navbar black
                )
) 

# Setup the server object
server <- function(input, output) {

  stars.full = read.csv('star_classification.csv', header = TRUE)
  sum(is.na(stars.full))
  stars.full$class = factor(stars.full$class)
  set.seed(234)
  stars = sample_n(stars.full,500)
  remain = anti_join(stars.full, stars, "spec_obj_ID")
  stars = subset(stars, select = -c(rerun_ID))
  # split train and test datasets
  set.seed(123)
  train.index = createDataPartition(stars$class, p=0.7, list=FALSE)
  train = stars[train.index,]
  test = stars[-train.index,]
  
  # training the decision tree using caret
  fitcontrol=trainControl(method = "repeatedcv",search = "grid")
  set.seed(111) 
  stars.rpart=train(train[,-which(colnames(train)=='class')],train[,which(colnames(train)=='class')], 
                    method = "rpart", tuneLength=5,
                    trControl = fitcontrol)
  
  # train the random forest
  set.seed(921)
  # build rf model
  stars.rf=randomForest(class~.,data=train,mtry=4,importance=TRUE,ntree=330) # first tried 500, but 330 has the lowest error rate.
  stars.rf
  
  
  ######## Functions for the decision tree tab #########
  
  ## For the Data Tab
  
  # Status/output Text plot
  output$DataContentsTree = renderPrint({
    if (input$dataTree>0) { 
      isolate("Here are the features of the observations you choose.") 
    } else {
      return("Please choose a number of observations (between 1 to 100) you want to classify and click the 'Explore the data' button to preview them.")
    }
  })
  
  # To preview the data
  observeEvent(input$dataTree, {output$tabledataTree = renderTable({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsTree)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    stars.test
  } 
  )})
  
  
  ## For prediction tab
  
  # For plotting the decision tree
  observeEvent(input$PredictTree, {output$mainPlotTree = renderPlot({
    fancyRpartPlot(stars.rpart$finalModel)
  })})
  
  # confusion matrix
  observeEvent(input$PredictTree, {output$ConfusionMatrixTree = renderTable({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsTree)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    pred.rpart = predict(stars.rpart, stars.test)
    df =as.data.frame(table(pred.rpart, stars.test.actual$class))
    colnames(df)= c("Predicted","Actual","Freq")
    df
  })
  })
  
  # test set
  observeEvent(input$PredictTree, {output$TestTree = renderTable({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsTree)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    stars.test[,c('obj_ID','redshift')]
  })
  }) 
  
  # For the criterion used to classify the data
  observeEvent(input$PredictTree, {output$tableExplanationTree = renderTable({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsTree)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    stars.test
    lst = seq(0, input$NumOpsTree)
    for (i in 1:input$NumOpsTree){
      if (stars.test[i,]$redshift < 0.0074){
        lst[i] = "redshift < 0.0074"}
      else if (stars.test[i,]$redshift < 0.97){
        lst[i] = "0.0074 <= redshift < 0.97"}
      else {lst[i] = "0.97 <= redshift" }
    }
    df = as.data.frame(lst)
    colnames(df) = "Decision"
    df
  } 
  )})
  
  # For Predictions Table
  observeEvent(input$PredictTree, {output$tablePredictionTree = renderTable({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsTree)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    pred.rpart = predict(stars.rpart, stars.test)
    df = as.data.frame(pred.rpart)
    colnames(df) = "Class"
    df
  })
  })
  
  # FOr Accuracy output
  observeEvent(input$PredictTree, {output$contentsTree = renderPrint({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsTree)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    pred.rpart = predict(stars.rpart, stars.test)
    pred.rpart
    return(paste("The decision tree classify the stars with accuracy:",(mean(pred.rpart==stars.test.actual$class))))
  })
  })
  
  
  ######## Functions for the random Forest tab ########
  
  ## For the Data Tab
  
  # For Status/output Text plot
  output$DataContentsRF = renderPrint({
    if (input$dataRF>0) { 
      isolate("Here are the features of the observations you choose.") 
    } else {
      return("Please choose a number of observations (between 1 to 100) you want to classify and click the 'Explore the data' button to preview them.")
    }
  }) 
  
  # For preview of data
  observeEvent(input$dataRF, {output$tabledataRF = renderTable({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsRF)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    stars.test
  } 
  )})
  
  
  ## For Prediction Tab
  
  ## For plot of the Importance of the variables
  observeEvent(input$PredictRF, {output$mainPlotRF = renderPlot({
    set.seed(222)
    var.imp=varImpPlot(stars.rf)
    imp <- as.data.frame(var.imp)
    imp$varnames <- rownames(imp) # row names to column
    rownames(imp) <- NULL  
    imp$var_categ <- rep(1,16) 
    
    ggplot(imp, aes(x=reorder(varnames, MeanDecreaseGini), weight=MeanDecreaseGini)) +
      geom_bar() +
      labs(title = "Variable Importance in Random Forest") +
      ylab("MeanDecreaseGini") +
      xlab("Variable Name")
  })})
  
  # For confusion matrix
  observeEvent(input$PredictRF, {output$ConfusionMatrixRF = renderTable({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsRF)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    pred.rf=predict(stars.rf, newdata=stars.test)
    df =as.data.frame(table(pred.rf,stars.test.actual$class))
    colnames(df)= c("Predicted","Actual","Freq")
    df
  })
  })
  
  # For previewing test set
  observeEvent(input$PredictRF, {output$TestRF = renderTable({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsRF)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    stars.test[,c('obj_ID','redshift')]
  })
  }) 
  
  # For the criterion used to classify the data
  observeEvent(input$PredictRF, {output$tableExplanationRF = renderTable({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsRF)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    pred.rf.prob=predict(stars.rf, newdata=stars.test, type = "prob")
    df = pred.rf.prob*330
    df
  } 
  )})
  
  # For predictions table
  observeEvent(input$PredictRF, {output$tablePredictionRF = renderTable({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsRF)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    pred.rf=predict(stars.rf, newdata=stars.test)
    df = as.data.frame(pred.rf)
    colnames(df) = "Class"
    df
  })
  })
  
  # For Accuracy output
  observeEvent(input$PredictRF, {output$contentsRF = renderPrint({
    set.seed(125)
    stars.test.actual = sample_n(remain,input$NumOpsRF)
    stars.test = subset(stars.test.actual, select = -c(rerun_ID,class))
    pred.rf=predict(stars.rf, newdata=stars.test)
    return(paste("The Random Forest classify the stars with accuracy:",(mean(pred.rf==stars.test.actual$class))))
  })
  })
  
  ## For the About Random Forrest Tab
  # Plot the image
  output$Explanation_img_RF <- renderImage({ list(src = "www/tree_2.jpg",width = "100%",height = '100%')}, deleteFile = F)
  
  ######## Functions for the Data Exploration tab ########
  output$data = renderDataTable(stars)
  
  
  
  # Images in the home page
  output$images <- renderUI({
    tags$div(img(src = "Star.jpg", width = 90, height = 90), img(src = "Galaxy.jpg", width = 90, height = 90), img(src = "Quasar.jpg", width = 90, height = 90))
  })
  
} 

# Knit hte two objects together
shinyApp(ui = ui, server = server)