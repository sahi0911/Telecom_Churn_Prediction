

#install.packages("shinydashboard")
#install.packages("caret")
#install.packages("lattice")
#install.packages("MLmetrics")
#install.packages("h2o")

#Libraries
library(shinydashboard)
library(rsconnect)
library(shiny)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lattice)
library(purrr)
library(caret)
library(MLmetrics)
library(h2o)
#Reading Data
df<- read.csv(file = "C:/Users/sahit/Downloads/telecom_users.csv")
head(df)

df1 <- df
df1$SeniorCitizen <- as.factor(df1$SeniorCitizen)

#Check Missing Values
df1 %>% map(~ sum(is.na(.)))

#Imputing Total Charges
df <- df %>%
  mutate(TotalCharges = replace(TotalCharges,
                                is.na(TotalCharges),
                                median(TotalCharges, na.rm = T)))
df1 <- df1 %>%
  mutate(TotalCharges = replace(TotalCharges,
                                is.na(TotalCharges),
                                median(TotalCharges, na.rm = T)))

df1 <- df1 %>% select(-customerID)
set.seed(0)
#Train - Test Split
nrow(df1)

X_train<-df1[1:4800,]
X_test<-df1[4801:5986,]
drops <- c("Churn")
X_train[ , !(names(X_train) %in% drops)]
X_train<- X_train[ , !(names(X_train) %in% drops)]
X_test<- X_test[ , !(names(X_test) %in% drops)]
y_train <- df1[1:4800,"Churn"]
y_test <- df1[4801:5986,"Churn"]

nrow(train)
nrow(test)


#Random Forest
h2o.init()
data <- as.h2o(df1)
y <- "Churn"                                
x <- setdiff(names(X_train), y)
parts <- h2o.splitFrame(data, 0.8, seed=99) # randomly partition data into 80/20
train <- parts[[1]]                         # random set of training obs
valid <- parts[[2]]
valid

rf <- h2o.randomForest(x,y, train, max_runtime_secs=300)
pred_rf <- h2o.predict(rf, valid)

rf_prediction <- ifelse(pred_rf['predict']=="Yes",1,0)
y_actual <- ifelse(valid['Churn']=="Yes",1,0)

rf_f1<-F1_Score(y_pred = as.vector(rf_prediction['C1']), y_true = as.vector(y_actual['C1']),positive=1)
rf_auc<-AUC(y_pred = as.vector(rf_prediction['C1']), y_true = as.vector(y_actual['C1']))
h2o.varimp_plot(log)

#GBM
gbm <- h2o.gbm(x,y, train, max_runtime_secs=300)
pred_gbm <- h2o.predict(gbm, valid)
gbm_prediction <- ifelse(pred_gbm['predict']=="Yes",1,0)
y_actual <- ifelse(valid['Churn']=="Yes",1,0)
gbm_f1<-F1_Score(y_pred = as.vector(gbm_prediction['C1']), y_true = as.vector(y_actual['C1']),positive=1)
gbm_auc<-AUC(y_pred = as.vector(gbm_prediction['C1']), y_true = as.vector(y_actual['C1']))

#Logistic
log <- h2o.glm(x,y, train, max_runtime_secs=300)
pred_log <- h2o.predict(log, valid)
log_prediction <- ifelse(pred_log['predict']=="Yes",1,0)
y_actual <- ifelse(valid['Churn']=="Yes",1,0)
log_f1<-F1_Score(y_pred = as.vector(log_prediction['C1']), y_true = as.vector(y_actual['C1']),positive=1)
log_auc<-AUC(y_pred = as.vector(log_prediction['C1']), y_true = as.vector(y_actual['C1']))

#Naive Bayes
nb <- h2o.naiveBayes(x,y, train, max_runtime_secs=300)
pred_nb <- h2o.predict(nb, valid)
nb_prediction <- ifelse(pred_nb['predict']=="Yes",1,0)
y_actual <- ifelse(valid['Churn']=="Yes",1,0)
nb_f1<-F1_Score(y_pred = as.vector(nb_prediction['C1']), y_true = as.vector(y_actual['C1']),positive=1)
nb_auc<-AUC(y_pred = as.vector(nb_prediction['C1']), y_true = as.vector(y_actual['C1']))

models <- c("Logistic", "Naive Bayes", "Random Forest", "GBM")
f1 <- c(log_f1,nb_f1, rf_f1,gbm_f1)
auc_scores <- c(log_auc,nb_auc, rf_auc,gbm_auc)

df_result <- data.frame(models,f1,auc_scores)
names(df_result) <- c("Model", "F1", "AUC")
head(df_result)
names(df_result[2:3])

df_result1 <- subset(df_result, select=c(Model,AUC))
df_result2 <- subset(df_result, select=c(Model,F1))
names(df_result1) = c("Model","Metric")
names(df_result2) = c("Model","Metric")
df_result1
df_result2
#Rshiny
rsconnect::setAccountInfo(name='sahithanne09',
                          token='B35532F8B5B6313C49247D3FA3222A82',
                          secret='Op1/cfDO/s0xJ3Z5uaQ0epeUu7cm8C9TZx3yvFi4')


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow(
  box(
    title = "Churn vs Tenure"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("Tenure", height = "300px")
    ,sliderInput("Tenure", "Slider Tenure:", 1, 60, 36)
  )
  ,box(
    title = "Churn vs Contract"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("Contract", height = "300px")
    ,checkboxGroupInput("Contract", "Types of Contract:",
                        c("Month-to-month" = "Month-to-month",
                          "One year" = "One year",
                          "Two year" = "Two year"))
    
  ) )
frow3 <- fluidRow(
  box(
    title = "Churn vs Internet Service"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("InternetService", height = "300px")
    ,checkboxGroupInput("InternetService", "Types of Internet Service:",
                        c("DSL" = "DSL",
                          "Fiber Optic" = "Fiber optic",
                          "No" = "No"))
  )
  ,box(
    title = "Churn vs Total Charges"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("Charges", height = "300px")
    ,sliderInput("Charges", "Slider input:", 0, 7500, 2500)
  )  )
frow4 <-fluidRow(
  plotOutput("Performance", height = "300px")
  ,selectInput("Performance", "Choose a Metric:",
               c("F1" = "F1","AUC" = "AUC"),selected="F1"
  ))

frow5 <-fluidRow(
  plotOutput("Feature", height = "300px")
  ,selectInput("Feature", "Choose the model for feature importance:",
               c("Logistic" = "Logistic","GBM" = "GBM", "Random Forest" = "Random Forest"),selected="F1"
  ))
frow6 <-fluidRow(  headerPanel('K-Means clustering'),
                   sidebarPanel(
                     selectInput('xcol', 'X Variable', c('tenure','MonthlyCharges','TotalCharges')),
                     selectInput('ycol', 'Y Variable', c('tenure','MonthlyCharges','TotalCharges'),
                                 selected = names(df)[[2]]),
                     numericInput('clusters', 'Cluster count', 2,
                                  min = 1, max = 3)
                   ),
                   plotOutput('plot1'))

# combine the two fluid rows to make the body
#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Telecom Customers Data")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Results", tabName = "results", icon = icon("result")),
    menuItem("Feature Importance", tabName = "feature", icon = icon("feature")),
    menuItem("Predictive", tabName = "Predictive", icon = icon("Predictive"))
  )
)
body <- dashboardBody(tabItems(
  tabItem(tabName = "dashboard",frow1, frow2, frow3), tabItem(tabName = "results",frow4), tabItem(tabName = "feature",frow5), tabItem(tabName = "Predictive",frow6)))


ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='blue')


# create the server functions for the dashboard  
server <- function(input, output) {
  
  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(nrow(df), format="d", big.mark=',')
      ,paste('Total Number of Data Points')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "green")  
  })
  output$value2 <- renderValueBox({
    valueBox(
      formatC(NCOL(df) , format="d", big.mark=',')
      ,'Number of Features'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC('26%' , format="d", big.mark=',')
      ,'Churn Rate'
      ,icon = icon(name='thumbs-down',lib='glyphicon')
      ,color = "red")  
  })
  
  data1 <- reactive({
    subset(df, df$tenure < input$Tenure)})
  #creating the plotOutput content
  output$Tenure <- renderPlot({
    ggplot(as.data.frame(data1()), aes(x = tenure, fill = Churn)) +
      geom_density() +
      labs(x = "Tenure in Months")
    
  })
  data2 <- reactive({
    subset(df, df$Contract %in% input$Contract)})
  
  output$Contract <- renderPlot({
    ggplot(as.data.frame(data2()), aes(x = Churn, fill = Contract)) +
      geom_bar()
    
  })
  
  data3 <- reactive({
    subset(df, df$InternetService %in% input$InternetService)})
  output$InternetService <- renderPlot({ggplot(as.data.frame(data3()), aes(x = Churn, fill = InternetService )) +
      geom_bar()  
  })
  
  data4 <- reactive({
    subset(df, df$TotalCharges < input$Charges)
    
  })
  
  
  
  output$Charges <-renderPlot({ggplot(as.data.frame(data4()),aes(x =TotalCharges , fill = Churn)) +
      geom_histogram(binwidth = 300) +
      labs(x = "Total Charges in $")
  })
  
  
  
  output$Performance <-
    
    renderPlot({
      if (input$Performance == "F1")
      {
        ggplot(df_result, aes( x= Model,y = F1,fill=Model)) +
          geom_bar(stat='identity') +
          labs(x = "Models") + labs(y="F1")}
      else{
        ggplot(df_result, aes( x= Model,y = AUC,fill=Model)) +
          geom_bar(stat='identity') +
          labs(x = "Models") + labs(y="AUC")
      }
      
    })
  
  
  output$Feature <-
    
    renderPlot({
      if (input$Feature == "Logistic")
      {
        h2o.varimp_plot(log)}
      else if (input$Feature == "GBM"){
        h2o.varimp_plot(gbm)
      }
      else
      {
        h2o.varimp_plot(rf)
      }
      
    })
  selectedData <- reactive({
    df[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
}


shinyApp(ui, server)