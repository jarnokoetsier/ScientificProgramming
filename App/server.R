#Server
server <- function(input, output, session) {
  
  # Variable mean of training data
  varMeans <- eventReactive(input$Predict, {
    return(colMeans(trainingData[, c("radius_worst","texture_worst","perimeter_worst","smoothness_worst")]))
  })
  
  # Variable SD of training data
  varSD <- eventReactive(input$Predict, {
    return(apply(trainingData[, c("radius_worst","texture_worst","perimeter_worst","smoothness_worst")],2,sd))
  })
  
  # Collect input
  inputValues <- eventReactive(input$Predict,{
    values <- log(c(input$RadiusWorst, input$TextureWorst, input$PerimeterWorst, input$SmoothnessWorst)+0.5)
    names(values) <- c("radius_worst","texture_worst","perimeter_worst","smoothness_worst")
    return(values)
  })
  
  #Predict
  prob <- eventReactive(input$Predict,{
    req(inputValues())
    prob <- predict(finalModel, inputValues(), type = "response")
    return(prob)
  })
 
}