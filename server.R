
library(shiny)
library(shinydashboard)
library(plotly)
library(datasets)
library(tidyverse)
library(haven)
library(rgl)
#library(tree)
library(caret)
library(DT)
library (cluster)
library(dplyr)
options(dplyr.print_min = 5)
options(tibble.print_min = 5)
library(knitr)
opts_chunk$set(message = T, cache = F, warning = T)



# from https://wonder.cdc.gov/aids-v2002.html

HIV<-read_csv("HIV_DD.csv")

HIV<-filter(HIV, !is.na(Cases)&!is.na(Population))

#HIV<-HIV%>%select(-'Rate per 100000')
#HIV<-HIV %>% mutate(Rate_per_100000 = 100000*Cases/Population)

HIV$Year<-factor(HIV$Year)
HIV$Indicator<-factor(HIV$Indicator)
HIV$Cases<-as.integer(HIV$Cases)
HIV$Population<-as.integer(HIV$Population)


HIV<-HIV %>% mutate(Rate_per_100000 = 100000*Cases/Population)
HIV<-filter(HIV, !is.na(Cases)&!is.na(Rate_per_100000))%>% select(-`Rate per 100000`)

HIV


HIV2<-HIV%>%group_by(Geography)%>%summarise(meanCase=mean(Cases,na.rm=T), meanPop=mean(Population,na.rm=T))


HIV$Survive <- ifelse((HIV$Indicator == "AIDS deaths" | HIV$Indicator == "HIV deaths"), 0 ,1)

set.seed(100)
index<-sample(1:nrow(HIV), 800)
index

HIV3<-HIV[index,]
HIV3<-HIV3
HIV3<-filter(HIV3, !is.na(Cases)&!is.na(Rate_per_100000)&!is.na(Survive))

write.csv(HIV3, file = "HIV3.csv",row.names=FALSE, na="")


HIV4<-read_csv("HIV3.csv")
#HIV4<-HIV4()

#HIVPC<-reactive({HIV%>%select(Cases,Population,Rate_per_100000) })




function(input, output, session) {


  #HIV<-reactive(HIV)
  #HIV2<-reactive(HIV2)
 # HIV3<-reactive(HIV3)
  #HIV4<-reactive(HIV4)

 # HIV<-HIV()

  #HIV2<-HIV2()

 # HIV3<-HIV3()
  # HIV4<-HIV4()

  output$plotCluster<-renderPlot({

    H2<-HIV2%>%select(input$var)

    hc <- hclust(dist(H2), "ave")
    plot(hc, hang = -1)
  } )



HIVPC<-HIV%>%select(Cases,Population,Rate_per_100000)

  output$PCAresult<-renderPrint({

    PCs <- prcomp(select(HIV,Cases,Population,Rate_per_100000), center = TRUE, scale = TRUE)
    PCs
  })

  output$plotPC <-renderPlot(
    {
      data<-HIVPC
      dat <- princomp(data,cor=TRUE,score=TRUE)
      biplot(dat)
    })


 # HIVPC<-reactive({HIV%>%select(Cases,Population,Rate_per_100000) })


  HIV3<-HIV[index,]
  HIV3<-HIV3
  HIV3<-filter(HIV3, !is.na(Cases)&!is.na(Rate_per_100000)&!is.na(Survive))

  write.csv(HIV3, file = "HIV3.csv",row.names=FALSE, na="")

  set.seed(100)
  #split into training and test
  train <- sample(1:nrow(HIV3), size = nrow(HIV3)*0.8)
  test <- dplyr::setdiff(1:nrow(HIV3), train)
  HIVTrain <- HIV3[train, ]
  HIVTest <- HIV3[test, ]

  output$trainresult<-renderPrint({

    trainTree <- train(Survive ~ ., data = HIVTrain, method = input$treemodelmethod,
                       trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                       preProcess = c("center", "scale"))
    trainTree
  })

 # output$misclassRate<-renderText({
#  predictTree <- table(data.frame(pred = predict(trainTree, HIVTest), true = HIVTest$Survive))
#    #misclassificatoon rate
#    misclassRate<-1- sum(diag(predictTree)/sum(predictTree))

 #   paste("the misclassificatoon rate of method of  " ,input$treemodelmethod, "is " , round(misclassRate, 2) )

#  })

  HIV4<-select(HIV4,Survive,Year,Indicator, Population)

  HIV4 <-filter(HIV4,!is.na(Year)&!is.na(Indicator)&!is.na(Population)&!is.na(Survive))
  #HIV4<-select(HIV4,Survive,Year,Indicator, Geography)


  #output$GLmtrain<-renderPrint({

   # glmFit <- glm(Survive ~ input$var, data = HIV4, family = "binomial")
    #glmFit
 # })

  output$trace_table <- renderDataTable({
    HIV5<-HIV3[1:50,]
    datatable(cbind(HIV5), options = list(paging = FALSE))

  })

  output$plot5 <- renderPlot({

    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())

    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')

    for (i in 1:15) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }


    plot(HIV3$Population, HIV3$Cases)

  })

  output$info5 <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })




#  output$plot <- renderPlot({
#   plot(cars, type=input$plotType)
#  })
#
#  output$summary <- renderPrint({
 #   summary(cars)
#  })

 # output$table <- DT::renderDataTable({
 #   DT::datatable(cars)
  }

