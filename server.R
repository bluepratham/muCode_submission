library(shiny)
library(plotly)
library(ggplot2)
library(broom)
library(stringr)
library(tidyverse)
library(readxl)
library(lubridate)
# Define server logic required to draw a histogram
function(input, output, session) {
   
   file = reactive({
     a = input$dt
       fdbk = readxl::read_excel("MSU_EmpDataset.xlsx", 1)
       empD = readxl::read_excel("MSU_EmpDataset.xlsx", 3)
       fdbk$Interview_Date = dmy(fdbk$Interview_Date)
       empD$JoinDate = ymd(empD$JoinDate)
       fData = merge(fdbk , empD, by = 'EmpID')
       year(fData$Interview_Date[!is.na(fData$Interview_Date) &
                                   fData$Interview_Date < '2016-08-09'] ) = 2017
       
       year(fData$Interview_Date[!is.na(fData$Interview_Date) &
                                   fData$Interview_Date > '2017-03-23'] ) = 2016
       for (i in 4:9){
         fData[[i]] = as.factor(fData[[i]])
       }
       
       dData = fData
       for (i in 4:9){
         dData[[i]] = as.numeric(dData[[i]])
       }
       
       return( fData)
       }
   )
   num = reactive({
     a = file()
     colnames(a)[sapply(file(),function(x)length(unique(x))>15 & !is.character(x))]
   })
   
   fac = reactive({
     a = file()
     colnames(a)[sapply(a, function(x) length(unique(x))<20 )]
   })
   
   output$str = renderPrint(str(file()))
   
   output$summary = renderPrint({
     summary(file()[sapply(file(),function(x)length(unique(x))>15 & !is.character(x)) ])
   })
   
   # output$summary = renderTable({
   #   a = tidy(summary(file()[sapply(file(),is.numeric)]))
   #   a$a = stringr::str_split(as.character(a$Freq), ":", simplify = T)[,1]
   #   a$b = stringr::str_split(as.character(a$Freq), ":", simplify = T)[,2]
   #   spread(a[,c(2,4,5)], a,b)
   #   })
   
   
   output$cat = renderUI({
     selectInput('q', "Select Variables", fac())


   })
   output$cattbl = renderTable(
     tidy(summary(file()[input$q]))[-1]
   )

   output$head = renderDataTable(file(),
                                 options = 
                                   list(lengthMenu = c(5, 30, 50), pageLength = 5))
   
########################################################
   #UNIVARIATE
   
   output$uniSlide = renderUI({
     selectInput('uniSld', 'select variables', choices = num())#choices = (colnames(file())) )
   })
   
   output$uniColor = renderUI({
     a = file()
     tagList(
       checkboxInput('uni','Slice-By Categorical Variables ?'),
       conditionalPanel( 'input.uni == true', 
                         selectizeInput('c', 'Color by',c("None", fac()) , selected = NULL))
     )
   })
   
   output$uniplt = renderPlotly({
     a = file()
     if (input$c == "None"){ ggplotly(
       ggplot() + geom_histogram(aes(a[[input$uniSld]]))
       )
     }else{
       ggplotly(ggplot()+geom_histogram(aes(a[[input$uniSld]], fill = a[[input$c]])))
     }
     
   })
   
   output$unibox = renderPlotly({
     a = file()
     if(input$c == "None"){
       plot_ly(x = a[[input$uniSld]],type = 'box')
     }else{
       plot_ly(x = a[[input$uniSld]], color = a[[input$c]] , type = 'box')
       }
   })
   
   
 ######################################################
   #BIVAR##############
   output$biSelect = renderUI({
     a = file()
     tagList(
       selectInput('x', "X Axis", num()) ,
       selectInput('y', 'Y axis', num()[sample(length(num()))])
     )
   })
   
   output$biColor = renderUI({
     a = file()
     tagList(
       checkboxInput('b','Slice-By Categorical Variables ?'),
       conditionalPanel( 'input.b == true', 
                         selectizeInput('a', 'Color by',c("None", fac()) , selected = NULL))
     )
   })
   
   output$biPlt = renderPlotly({
     a = file()
     if (input$a == "None") {ggplotly(
       ggplot() + geom_point(aes(x =a[[input$x]] , y = a[[input$y]]) ) 
       )
     }else{plot_ly(x =a[[input$x]] , y = a[[input$y]], color = a[[input$a]] )}
     
     })
   ##############################################3
   ##Statistical Tests
   output$statFac = renderUI({
     a = file()
     tagList(
       selectInput('xF', "First Variable", fac()) ,
       selectInput('yF', 'Second Variable', fac())
     )
   })
   output$statFacR = renderPrint(chisq.test(file()[[input$xF]], file()[[input$yF]]))

}










