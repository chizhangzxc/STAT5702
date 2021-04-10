#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#From https://shiny.rstudio.com/articles/basics.html

#################LOADING AND INSTALLING LIBRARIES
source("checkpackages.R")


#loads and installs if necessary
#Some libraries are complicated and do not readily install
checkpackages("sp")
checkpackages("rworldmap")
checkpackages("rworldxtra")



shinyServer(function(input, output) {

  #Table of locally installed libraries
output$tableLoc <- renderDataTable(installed.packages(lib=getwd()))
#Table of  libraries that are installed on the server by the rcg sfu people:
output$table <- renderDataTable(installed.packages())


output$world <- renderPlot({
#Make a map just to prove that the library install worked  
  worldmap = getMap(resolution = "low")
  dim(worldmap)
  NrthAm = worldmap[which(worldmap$ADMIN =="Canada"), ]
  plot(NrthAm, col = "white", bg = "lightblue",
             xlim = c(-140, -55), ylim = c(55,60))
})


})
