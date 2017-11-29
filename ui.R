


library(shiny)



shinyUI(fluidPage(
  
 
  titlePanel("Higher Correlation finder"),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
   
  sidebarLayout(
    sidebarPanel(
       selectInput("datasets_options",
                   "Select dataset:",list(
                       "mtcars","airquality","freeny",""),
                   selected = ""
                   ),
       uiOutput("select_response_variable"),
       uiOutput("button"),
       uiOutput("help")
       
                   
                   
                   
                  
    ),
    
    
    mainPanel(
        
        
        uiOutput("tabs")
      
        
    )
       
    )
  ))

