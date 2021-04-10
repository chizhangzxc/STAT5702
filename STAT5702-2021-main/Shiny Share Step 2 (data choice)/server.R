#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
   
    #Use those radioButtons to select the data to use
    #Note that the input was called "dataSource" in the ui.R file
    #Here it is an element from a list.
    #I extract it using input$nameOfThing
    if (input$dataSource == "DataFile2006") {
      x = read.csv("pop2006.csv")
    } else {
      if(input$dataSource == "DataFile2011"){
        x = read.csv("pop2011.csv")
      }else{
        #This means do not use any data.  This will break things.
        x=NULL
      }
    }
    #I didn't need to use if{}else{if{}else{}}
    # but I do so to show how to use multiple if else statements.
    
    
    # generate bins based on input$bins from ui.R
    x    <- x[-1, 3] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
