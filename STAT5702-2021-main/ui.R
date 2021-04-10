# 
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Population Data"),
  # Include radio buttons  (Only one of these can be selected)
  radioButtons("dataSource", "",
               c("Data Source 2006" = "DataFile2006",
                 "Data Source 2011" = "DataFile2011")),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # This is a slider for the number of bins
      #The value determined here will be used as an input in server.R
      #The minimum is 1
      #The max is 50
      #It starts at a value of 30
      #by default it will grow by 1 but here I specify that it will step by 2
      #
      # The format is 
      #sliderInput(VariableName, Stuff2WriteInTheWebPage, MinimumValue,MaxValue,Default,StepIncrement)
      sliderInput("bins",  #This is the variable name, the value of which is determined by the slider.
                  "Number of bins:",#Label on the html page
                  min = 1,#minimum for the slider
                  max = 50,#maximum for the slider
                  value = 30,#default value
                  step=2),#slider increments
      #Now take in some numeric inputs for the x limits.
      # There are 2 input values defining the variables named 'xmin' and 'xmax'
      # Note that a suggested default value is given 
      # The format is 
      #numericInput(VariableNameForServer, Stuff2WriteInTheWebPage, DefaultValue)
      numericInput("xmin", "x-axis minimum:", 1000),
      numericInput("xmax", "x-axis maximum value:", 1000000),
      #Now let's include a checkbox
      #The usual format exists:
      #checkboxInput(VariableNameForServer, Stuff2WriteInTheWebPage, DefaultValue)
      checkboxInput("DensityLogical", strong("Add density plot?"), FALSE),
      
      #Note that the basic density plot will have problems at the left boundary
      # because the data is skewed.
      #If someone wants the density plot they should have the option of 
      # using the edge correction that we did earlier in class.
      #This is a conditional panel
      # It is used when an earlier input condition is met.
      #
      # You can use all of the usual logical statements
      #EXCEPT note that logicals must be lower case
      #
      # format of conditionalPanel is:
      # conditionalPanel(condition, stuff2do1, stuff2do2,...)
      conditionalPanel(
        condition = "input.DensityLogical == 'TRUE'",   #check the logical lower/upper case
        helpText(HTML("<h3>You might want to adjust the boundary estimate</h3>")),  #This is just big text
        checkboxInput("BoundaryCorrect", strong("Correct the density plot at zero?"), FALSE)  #This is a new input
        #
        #
      )
    ),#End of sidepanel
    
    # Show a plot of the generated distribution
    mainPanel(
      #
      #
      # Now I want different Tabs
      # Each tab will give me different options#
      # In this case I will have 2 tabs
      # Those 2 tabs will use different variables
      # But they could do totally different things
      # 
        tabsetPanel(type = "tabs",
                    tabPanel("Population Data", plotOutput("PopPlot")),
                    tabPanel("Private Dwellings", plotOutput("DwellPlot"))
        )
    )
      
  )
))
