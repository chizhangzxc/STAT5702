
#There are 2 changes in this ui file.
# 1 - it shows how to install and load libraries


#From https://shiny.rstudio.com/articles/basics.html
library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

# App title ----
titlePanel("Hello Shiny!"),

# Sidebar layout with input and output definitions ----
sidebarLayout(

# Sidebar panel for inputs ----
sidebarPanel(

# Input: Slider for the number of bins ----
sliderInput(inputId = "bins",
label = "Here's a slider that doesn't do anything:",
min = 1,
max = 50,
value = 30)

),

####### NOTE THE MIXTURE OF HTML TEXT AND PLOTS ##########

# Main panel for displaying outputs ----
mainPanel(
HTML("<h1>Title goes here</h1><br><p>This is how you put words into the tabs with plots</p>
     <p> This is basic HTML code inserted as an argument to the <em>mainPanel()</em> that will 
     make it easier to some domain expertise to your shiny app.
     <p> Remember that communication and domain expertise are important aspects of data science.
     Note that I'm installing and loading libraries so the first time this page is accessed it will take a little longer"),
# Output: plot
#plotOutput(outputId = "word"),
plotOutput(outputId = "world"),
HTML("<p>Below is a table of the LOCALLY installed packages that were installed using my 'checkpackages.R' function. 
These were installed into my app directory by my server.R file.   
     See the announcement (and code) for how to install more packages"),
column(12,
       dataTableOutput('tableLoc'),
       HTML("<p>Below is a table of all of the installed packages.  
     See the announcement (and code) for how to install more packages"),
       column(12,
              dataTableOutput('table')
       )
       
)
)
)
)
)
