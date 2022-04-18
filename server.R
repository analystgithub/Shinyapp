library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(DT)
library(plotly)
library(corrplot)
library(caret)
library(stargazer)
library(MASS)
library(caTools)
library(broom)


dd <- read.csv("portland_housing1.csv")

drop_cols <- c()
for (i in colnames(dd)){
  if (sum(is.na(dd[[i]])) > 4000){
    drop_cols <- c(drop_cols, i)
  }
}
dd <- dd[ , !(names(dd) %in% drop_cols)]



shinyServer(function(input, output, session) {
  
  input_X <- reactiveValues(data=dd)
  

  input_X_model <- reactive({
    if (is.null(input$X)) {
      dt <- dd
    }
    else{
      dt <- dd[, c(input$X)]
    }
    
  })
  
  
  observe({
    lname <- names(input_X$data)
    updateSelectInput(session = session,
                      inputId = "y",
                      choices = lname)
  })
  
  splitSlider <- reactive({
    input$Slider1 / 100
  })
  
  
  
  output$selected_var <- renderText({ 
    paste("")
  })
  
  observeEvent(input$action, {
    output$selected_var <- renderText({ 
      paste("All the Na Values are replaced with the respective column means.")
    })
    
    new_output <- reactive({

      dd1 <- input_X$data
      for(i in input$var){
        dd1[is.na(dd1[,i]), i] <- mean(dd1[,i], na.rm = TRUE)
      }
      return(dd1)
    })
    input_X$data <- new_output()
  })
  
  calc_mode <- function(x){
    
    distinct_values <- unique(x)
    distinct_tabulate <- tabulate(match(x, distinct_values))
    distinct_values[which.max(distinct_tabulate)]
  }
  
  output$selected_var1 <- renderText({ 
    paste("")
  })
  
  observeEvent(input$action1, {
    output$selected_var1 <- renderText({ 
      paste("All the Na Values are replaced with the respective column modes")
    })
    
    new_output1 <- reactive({
      
      dd1 <- input_X$data
      for(i in input$var_cat){
        dd1[is.na(dd1[,i]), i] <- calc_mode(dd1[,i])
      }
      return(dd1)
    })
    input_X$data <- new_output1()
  })
  
  
  output$Summ <-
    renderPrint(
      stargazer(
        input_X$data,
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )
  output$Summ_old <- renderPrint(summary(input_X$data))
  output$structure <- renderPrint(str(input_X$data))
  
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(input_X_model()),
             splitSlider() * nrow(input_X_model()))
    })# row indices for training data
  
  train_data <- reactive({
    tmptraindt <- input_X_model()
    tmptraindt[trainingRowIndex(), ]
  })
  
  test_data <- reactive({
    tmptestdt <- input_X_model()
    tmptestdt[-trainingRowIndex(),]
  })
  

  
  output$cntTrain <-
    renderText(paste("Train Data:", NROW(train_data()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(test_data()), "records"))
  
  output$Data <- renderDT(input_X$data)
  
  
  
  
  #Code section for Linear Regression-----------------------------------------------------------------------------
  
  f <- reactive({
    as.formula(paste(input$y, "~."))
  })
  
  
  Linear_Model <- reactive({
    lm(f(), data = train_data())
  })
  
  output$Model <- renderPrint(summary(Linear_Model()))
  output$Model_new <-
    renderPrint(
      stargazer(
        Linear_Model(),
        type = "text",
        title = "Model Results",
        digits = 1
      )
    )
  
  Importance <- reactive({
    varImp(Linear_Model(), scale = FALSE)
  })
  
  tmpImp <- reactive({
    
    imp <- as.data.frame(varImp(Linear_Model()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  output$ImpVar <- renderPrint(tmpImp())
  
  price_predict <- reactive({
    predict(Linear_Model(), test_data())
  })
  
  tmp <- reactive({
    tmp1 <- test_data()
    tmp1[, c(input$y)]
  })
  
  
  actuals_preds <-
    reactive({
      data.frame(cbind(actuals = tmp(), predicted = price_predict()))
    })
  
  Fit <-
    reactive({
      (
        plot(
          actuals_preds()$actuals,
          actuals_preds()$predicted,
          pch = 16,
          cex = 1.3,
          col = "red",
          main = "Best Fit",
          xlab = "Actual",
          ylab = "Predicted"
        )
      )
    })
  
  output$Prediction <- renderPlot(Fit())
  
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2))
    plot(Linear_Model())
    par(mfrow = c(1, 1))
    
  })
  
  output$digest <- renderExplorer({
    
    explorer(data = dd$data, demo = F)
    
  })
  
  
})



