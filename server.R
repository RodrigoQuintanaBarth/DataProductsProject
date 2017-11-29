

library(shiny)


shinyServer(function(input, output) {
   
  output$select_response_variable <- renderUI({
    
      if(input$datasets_options == "") {
          
          return()
      }
      
    options = get(input$datasets_options)
    
    column_names = names(options)
    
    selectInput("select_response",
                "Select response variable:",
                as.list(column_names))
    
  })
  
  output$button = renderUI({
      if(is.null(input$select_response)) {
          
          return()
          
      }
      
      actionButton("operate","Search best correlator:")
      
  })
  
  MoreCorrelated = function(dataset,response) {
      
      library(dplyr)
      
      dataset = dataset[complete.cases(dataset),]
      
      dataset1 = dataset[,-(grep(response,names(dataset)))]
      
      name =c()
      correlation = c()
      
      for(i in 1:length(dataset1[1,])) {
          
          correlation[i] = cor(dataset1[,i],dataset[,response])
          name[i] = names(dataset1)[i]
          
      }
     correlation_absolute = sqrt(correlation^2) 
     corr_table = cbind(name,correlation,correlation_absolute)
     corr_table = as.data.frame(corr_table)
     
     corr_table = arrange(corr_table,desc(correlation_absolute))
     corr_table$correlation = as.numeric(as.character(corr_table$correlation))
     corr_table = mutate(corr_table,sign = ifelse(correlation > 0,"positive","negative"))
     
     closer_variable = c(as.character(corr_table$name[1]),
                         as.character(corr_table$correlation_absolute[1]),
                         as.character(corr_table$sign[1]))
     
     text_analysis =paste("The variable with higher correlation with ",
                          response,
                          " is ",closer_variable[1]," with a correlation rate of ",
                          closer_variable[2]," correlating ",closer_variable[3],"ly to it",sep = "")
  
     closer_variable[4] = text_analysis
     
     return(closer_variable)   
     
  }
  
  MoreCorrelatedInstantiated = eventReactive(input$operate,{
      
      MoreCorrelated(get(input$datasets_options),input$select_response)
      
  })
  
  
  
  output$better_variable = renderText(MoreCorrelatedInstantiated()[1])
  
  output$better_variable_rate = renderText(MoreCorrelatedInstantiated()[2])
  
  output$text_summary = renderText(MoreCorrelatedInstantiated()[4])
  
  output$help = renderUI({
      
     
      
      textOutput("doc")
  })
  
  output$doc = renderText("This application finds the variable that best correlates
with a selected response variable in a chosen dataset: How to use it? 1) Select a valid dataset from the side input bar.
  2) Select the variable from that dataset for which you want to find the variable 
  with a higher correlation to it.
  3) Press the search best correlated button to run. At the main panel you'll see
a tab set with the variable with higher correlation, how strong it was, if it's
positively or negatively correlated and a plot of the response variable and the variable with highest correlation. Hope you find it useful!")

output$plot = renderPlot({
    
    if(is.null(input$datasets_options) | 
       is.null(input$select_response)) {
        
        return()
    }
    
    plot(get(input$datasets_options)[,as.character(MoreCorrelatedInstantiated()[1])],
         get(input$datasets_options)[,input$select_response],
         xlab = as.character(MoreCorrelatedInstantiated()[1]),
         ylab = input$select_response)
    
    
    
})
  
  
  output$tabs = renderUI({
      
      if(!input$operate) {
          
          return()
          
      }
      
      
          tabsetPanel(
              tabPanel("Summary",textOutput("text_summary")),
              tabPanel("More highly correlated variable",textOutput("better_variable")),
              tabPanel("Correlation rate",textOutput("better_variable_rate")),
              tabPanel("Plot variables",plotOutput("plot"))
              
          
          
      )
  })
  
  
})
