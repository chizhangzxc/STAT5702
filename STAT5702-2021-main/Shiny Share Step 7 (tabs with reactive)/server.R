# step 7
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
   
  
  #I'm now moving a few things around
  #Keep the common things here
  # Reactive expressions are 
  # called whenever the inputs change. The output functions that
  # depend on the tab will then deal with the changed input
  # But it might not need to change.
  # Essentially put the common pieces across both tabs in reactives.
  #
  
  x <- reactive({
    #Use those radioButtons to select the data to use
    #Note that the input was called "dataSource" in the ui.R file
    #Here it is an element from a list.
    #I extract it using input$nameOfThing
    if (input$dataSource == "DataFile2006") {
      read.csv("pop2006.csv")
    } else {
      if(input$dataSource == "DataFile2011"){
        read.csv("pop2011.csv")
      }else{
        #This means do not use any data.  This will break things.
        NULL
      }
    }
  
    #I didn't need to use if{}else{if{}else{}}
    # but I do so to show how to use multiple if else statements.
})
  
  
      
  # This is the thing that you get in the first tab.  
  # If we are in the second tab see below.
  # I will call this PopPlots
  #so that I can bring it into the appropriate tab in 
  # the ui file.
  #########START OF TAB 1
  output$PopPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- x()[-1, 3] #Note that reactive variables are more like functions...
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #Take the input limits from the side bar with numeric entry
    XLIMITS = c(input$xmin, input$xmax)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',xlim = XLIMITS,
         prob=input$DensityLogical,
         main = "Population Count")
    
    #use logical statements to see if we should include a density plot
    #This comes from the input
    #The value should be TRUE or FALSE
    #Note that I changed the histogram so that its y axis depends on this outcome
    #If we are to include a density plot
    # then we need to make sure that the histogram is scaled to be a probability
    #I did that using prob=input$DensityLogical in the histogram
    #
    if(input$DensityLogical){
      if(input$BoundaryCorrect ){  #input$BoundaryCorrect comes from the conditional panel
        xuse = c(-x,x)
        Dens = density(xuse,from = input$xmin)
        Dens$y = Dens$y*2
      }else{
        Dens = density(x)
      }
      lines(Dens)
    }
  })
  #########END OF TAB 1
  
  
  #########START OF TAB 2
  # This is the thing that you get in the first tab.  
  # If we are in the second tab see below.
  # I'll call this output DwellPlot and will bring it into the ui
  output$DwellPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- x()[-1, 4] #Note that reactive variables are more like functions
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #Take the input limits from the side bar with numeric entry
    XLIMITS = c(input$xmin, input$xmax)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',xlim = XLIMITS,
         prob=input$DensityLogical,
         main = "Private Dwellings")
    
    #use logical statements to see if we should include a density plot
    #This comes from the input
    #The value should be TRUE or FALSE
    #Note that I changed the histogram so that its y axis depends on this outcome
    #If we are to include a density plot
    # then we need to make sure that the histogram is scaled to be a probability
    #I did that using prob=input$DensityLogical in the histogram
    #
    if(input$DensityLogical){
      if(input$BoundaryCorrect ){  #input$BoundaryCorrect comes from the conditional panel
        xuse = c(-x,x)
        Dens = density(xuse,from = input$xmin)
        Dens$y = Dens$y*2
      }else{
        Dens = density(x)
      }
      lines(Dens)
    }
  })

  #########END OF TAB 2
  
})
