shinyServer(function(input,output){
  source("AP.R")
  
  output$Aplot <- reactivePlot(
    function(){
      AP(input$Q,input$Cex,input$As,input$Ms,input$Supp,input$Conf,input$Lift,2,input$Lhs)
    })    
})
