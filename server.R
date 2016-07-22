shinyServer(function(input,output){
  library("arules")
  library("MASS")
  source("AP.R")
  
  output$Aplot <- reactivePlot(
    function(){
      AP(input$Q,input$Cex,input$As,input$Ms,input$Supp,input$Conf,input$Lift,2,input$Lhs)
    })    
})
