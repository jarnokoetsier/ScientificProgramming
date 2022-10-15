#=============================================================================#
# File: server.R
# Date: October 15, 2022										                                      
# Author: Jarno Koetsier                                                      
# Data: NA
#
# R version: 4.2.1 (getRversion())
# RStudio version: 2022.7.1.544 (RStudio.Version())
#=============================================================================#

server <- function(input, output, session) {
  
  #******************************************************************************#
  # 1) Preparation
  #******************************************************************************#
  sendSweetAlert(
    session = session,
    title = paste0("Hi ", Sys.getenv("USERNAME")),
    text = tags$span("Welcome to the ", tags$b("Class Prediction App"), "! ", tags$br(),
                     "If you need information about how to use the app, please click
                     on the ", tags$em("Information"), " tab."),
    type = "info",
    html = TRUE,
    btn_labels = "Yeah let's go!",
    showCloseButton = TRUE)
  
  #Example
  observeEvent(input$example, {
    
    # Get example data from test data
    exampleData <- testData[,colnames(testData) %in% colnames(trainingData_filtered)]
    
    # Select random sample from example data
    r <- sample(1:nrow(exampleData),1)
    
    # Update numeric input
    updateNumericInput(session, "AreaWorst",
                       label = NULL,
                       value = round(exp(exampleData$area_worst[r])-1,2))
    updateNumericInput(session, "ConcavePointsWorst",
                       label = NULL,
                       value = round(exp(exampleData$concave_points_worst[r])-1,2))
    updateNumericInput(session, "TextureWorst",
                       label = NULL,
                       value = round(exp(exampleData$texture_worst[r])-1,2))

  })
  
  # Variable mean of training data
  varMeans <- eventReactive(input$Predict, {
    return(colMeans(trainingData[, colnames(trainingData_filtered)]))
  })
  
  # Variable SD of training data
  varSD <- eventReactive(input$Predict, {
    return(apply(trainingData[, colnames(trainingData_filtered)],2,sd))
  })
  
  # Collect input
  inputValues <- eventReactive(input$Predict,{
    
    # Require mean and SD of the variables
    req(varMeans())
    req(varSD())
    
    # log transform
    values_log <- log(c(input$TextureWorst, input$AreaWorst, input$ConcavePointsWorst)+1)
    
    #Scale data
    values_scaled <- (values_log - varMeans())/varSD()
    
    # Add names
    names(values_scaled) <- colnames(trainingData_filtered)
    
    #Return
    return(values_scaled)
  })
  
  #******************************************************************************#
  # 2) Probability plot
  #******************************************************************************#
  
  #Predict
  prob <- eventReactive(input$Predict,{
    
    # Require input values
    req(inputValues())
    
    # Calculate class probability
    prob <- predict(finalModel, inputValues(), type = "response")
    
    # Return class probability
    return(prob)
  })
 
  # Render probability plot
  output$ProbPlot <- renderPlotly({
    req(prob())
    prob <- prob()
    
    # Get class probability for each training sample
    pred_prob_train <- predict(finalModel, trainingData_filtered, type = "response")
    trainingProb <- data.frame(Class = trainingClass,
                               Probability = pred_prob_train[,1])
    
    textDF <- data.frame(Class = 1.5,
                         y = c(prob + 0.03),
                         label = "Prediction")
    
    # Make probability plot
    trainingProb_plot <- ggplot(trainingProb, aes(x = Class, y = Probability)) +
      geom_violin(alpha = 0.5) +
      geom_jitter(aes(color = Probability), position=position_jitter(0.2), size = 2) +
      geom_hline(yintercept = prob, linetype = "dashed", color = "red", size = 1) +
      geom_text(data = textDF, aes(x = Class, y = y, label = label), color = "red", size = 7, fontface = 2) +
      ggtitle("Class probabilities of training data") +
      xlab(NULL) +
      ylab("Class Probability") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5,
                                      face = "bold",
                                      size = 16,
                                      colour = "white"),
            plot.subtitle = element_text(hjust = 0.5,
                                         size = 10,
                                         color = "white"),
            legend.position = "none",
            legend.title = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(colour = "white", size = 10),
            axis.line=element_line(colour="white"),
            axis.title = element_text(colour = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#343434", color = "#343434"),
            plot.background = element_rect(fill = "#343434"),
            legend.background = element_rect(color = "#343434", fill = "#343434"),
            legend.text = element_text(colour = "white"),
      )+
      scale_color_viridis_c()
    
    return(ggplotly(trainingProb_plot, tooltip = c("x", "y")))
  })
  
  # Give class prediction and probability
  output$prob_text <- renderText({
    req(prob())
    if(prob() > 0.5){
      class <- "'Maligant'"
    } else {
      class <- "'Benign'"
    }
    txt <- paste0("Predicted Class: ", class, 
                  "\nClass Probability: ", round(prob(),3))
    
    return(txt)
  })

  #******************************************************************************#
  # 3) PCA
  #******************************************************************************#
  
  # Make PCA model
  pcaList <- eventReactive(input$Predict, {
    pcaList <-  prcomp(trainingData_filtered,        
                       retx = TRUE,
                       center = FALSE,
                       scale = FALSE)
  })
  
  # Render PCA plot
  output$PCAplot <- renderPlotly({
    
    # Require input values
    req(inputValues())
    inputValues <- inputValues()
    
    # Require PCA object
    req(pcaList())
    pcaList <- pcaList()
    
    # Scores training data
    scores_train <- as.data.frame(pcaList$x)
    scores_train$ID <- rownames(scores_train)
    scores_train <- inner_join(scores_train, sampleInfo_filtered, by = c("ID" = "id"))
    
    # Explained variance
    explVar <- round(((pcaList$sdev^2)/sum(pcaList$sdev^2))*100,2)
    
    # Scores test data
    scores_proj <- as.data.frame(t(as.matrix(inputValues)) %*% as.matrix(pcaList$rotation))
    scores_proj$ID <- rownames(scores_proj)
    
    # Make PCA plot
    PCAtrain <- ggplot()+
      geom_point(data = scores_train, aes(x = PC1, y = PC2, shape = diagnosis, color = diagnosis), size = 2, alpha = 0.7) +
      geom_point(data = scores_proj, aes(x = PC1, y = PC2), color = "red", size = 3, alpha = 1) +
      geom_text(data = scores_proj, aes(x = PC1, y = PC2), label = "Observation", color = "white", size = 3.5) +
      scale_shape_manual(values = c(15,17,0,2)) +
      xlab(paste0("PC1 (", explVar[1], "%)")) +
      ylab(paste0("PC2 (", explVar[2], "%)")) +
      labs(title = "PCA score plot of training data", 
           caption = "NOTE: The PCA model is constructed using the training data only. The test data is projected.",
           color = "Diagnosis",
           shape = "") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5,
                                      face = "bold",
                                      size = 16,
                                      colour = "white"),
            plot.subtitle = element_text(hjust = 0.5,
                                         size = 10,
                                         color = "white"),
            legend.position = "bottom",
            legend.title = element_blank(),
            panel.border = element_blank(),
            axis.text = element_text(colour = "white", size = 10),
            axis.line=element_line(colour="white"),
            axis.title = element_text(colour = "white"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#343434", color = "#343434"),
            plot.background = element_rect(fill = "#343434"),
            legend.background = element_rect(color = "#343434", fill = "#343434"),
            legend.text = element_text(colour = "white"),
      )+
      scale_color_manual(values = c("#7EC8E3", "#E12A36"))
    
    return(ggplotly(PCAtrain, tooltip = c("x", "y")))
  })
  
  #******************************************************************************#
  # 4) Scatter plot
  #******************************************************************************#
  
  observedValue <- eventReactive(input$Predict,{
    observedValue <- as.data.frame(t(as.data.frame(c(input$TextureWorst, 
                                    input$AreaWorst, 
                                    input$ConcavePointsWorst))))
    
    return(observedValue)
    
  })
   
   output$scatter <- renderPlotly({
     req(observedValue())
     
     # Get training data
     plotData <- trainingData[, colnames(trainingData) %in% colnames(trainingData_filtered)]
     
     # Reverse the log transformation
     plotData <- exp(plotData) - 1
     
     # Add ID and diagnosis to data
     plotData$ID <- rownames(plotData)
     plotData <- inner_join(plotData, sampleInfo_filtered, by = c("ID" = "id"))
     
     
     # get selected feature
     features <- c("Texture Worst",
                   "Area Worst",
                   "Concave Points Worst")
     
     f1 <- which(features == input$Xaxis)
     f2 <- which(features == input$Yaxis)
     
     # Observed value
     observedValue <- observedValue()
     colnames(observedValue) <- colnames(plotData)[1:3]
     
     scatter <- ggplot()+
       geom_point(aes(x = plotData[,f1], y = plotData[,f2], shape = plotData$diagnosis, 
                      color = plotData$diagnosis, text = paste0("X: ", plotData[,f1], "\nY: ", plotData[,f2])), size = 2, alpha = 0.7) +
       geom_point(aes(x = observedValue[,f1], y = observedValue[,f2]), color = "red", size = 3, alpha = 1) +
       geom_text(aes(x = observedValue[,f1], y = observedValue[,f2]), label = "Observation", color = "white", size = 3.5) +
       scale_shape_manual(values = c(15,17,0,2)) +
       xlab(features[f1]) +
       ylab(features[f2]) +
       labs(title = NULL,
            color = "Diagnosis",
            shape = "") +
       ggtitle("Scatter plot of training data") +
       theme_classic() +
       theme(plot.title = element_text(hjust = 0.5,
                                       face = "bold",
                                       size = 16,
                                       colour = "white"),
             plot.subtitle = element_text(hjust = 0.5,
                                          size = 10,
                                          color = "white"),
             legend.position = "bottom",
             legend.title = element_blank(),
             panel.border = element_blank(),
             axis.text = element_text(colour = "white", size = 10),
             axis.line=element_line(colour="white"),
             axis.title = element_text(colour = "white"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(fill = "#343434", color = "#343434"),
             plot.background = element_rect(fill = "#343434"),
             legend.background = element_rect(color = "#343434", fill = "#343434"),
             legend.text = element_text(colour = "white"),
       )+
       scale_color_manual(values = c("#7EC8E3", "#E12A36"))
     
     return(ggplotly(scatter, tooltip = c("text")))

   })
   

   #******************************************************************************#
   # 5) Histogram
   #******************************************************************************#
   
   output$histPlot <- renderPlotly({
     req(observedValue())
     
     # Get training data
     plotData <- trainingData[, colnames(trainingData) %in% colnames(trainingData_filtered)]
     
     # Reverse the log transformation
     plotData <- exp(plotData) - 1
     
     # Add ID and diagnosis to data
     plotData$ID <- rownames(plotData)
     plotData <- inner_join(plotData, sampleInfo_filtered, by = c("ID" = "id"))
     
     
     # get selected feature
     features <- c("Texture Worst",
                   "Area Worst",
                   "Concave Points Worst")
     
     f1 <- which(features == input$feature)
     
     # Observed value
     observedValue <- observedValue()
     colnames(observedValue) <-colnames(plotData)[1:3]
     
     histPlot <- ggplot()+
       geom_histogram(aes(x = plotData[,f1], fill = plotData$diagnosis), bins = 100, position = "identity", alpha = 0.5) +
       geom_vline(xintercept = observedValue[,f1], size = 1, color = "red", linetype = "dashed") +
       xlab(features[f1]) +
       ylab("Count") +
       labs(title = NULL,
            fill = "Diagnosis",
            shape = "") +
       ggtitle("Histogram of training data") +
       theme_classic() +
       theme(plot.title = element_text(hjust = 0.5,
                                       face = "bold",
                                       size = 16,
                                       colour = "white"),
             plot.subtitle = element_text(hjust = 0.5,
                                          size = 10,
                                          color = "white"),
             legend.position = "bottom",
             legend.title = element_blank(),
             panel.border = element_blank(),
             axis.text = element_text(colour = "white", size = 10),
             axis.line=element_line(colour="white"),
             axis.title = element_text(colour = "white"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_rect(fill = "#343434", color = "#343434"),
             plot.background = element_rect(fill = "#343434"),
             legend.background = element_rect(color = "#343434", fill = "#343434"),
             legend.text = element_text(colour = "white"),
       )+
       scale_fill_manual(breaks = c("Malignant","Benign"),
                          values = c("#E12A36", "#7EC8E3"))
     
     return(ggplotly(histPlot, tooltip = "y"))
     
   })
  
  
}