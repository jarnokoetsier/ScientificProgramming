ui <- fluidPage(
  theme = shinytheme("spacelab"),
  setBackgroundColor("#343434"),
  fluidPage(
    
    #**************************************************************************#
    # Side panel
    #**************************************************************************#
    sidebarPanel(
      
      #Title
      h2(strong("Class Prediction")),
      
      hr(),
      actionBttn(inputId = "example", 
                 label = "Example",
                 style = "simple",
                 color = "success"),

      br(),
      br(),
      
      # Perimeter Worst
      numericInput(inputId = "PerimeterWorst", 
                label = "Perimeter Worst", 
                value = NULL),
      
      # Radius Worst
      numericInput(inputId = "RadiusWorst", 
                   label = "Radius Worst", 
                   value = NULL),
      
      # Mean Concave Points
      numericInput(inputId = "ConcavePointsMean", 
                   label = "Mean Concave Points", 
                   value = NULL),
      
      # Texture Worst
      numericInput(inputId = "TextureWorst", 
                   label = "Texture Worst", 
                   value = NULL),
      
      # Predict
      hr(),
      actionBttn(inputId = "Predict", 
                 label = "Predict",
                 style = "simple",
                 color = "primary",
                 icon = icon("sync"))

      
      
    ), #End Sidebar panel
    
    
    
    
    #**************************************************************************#
    # Main panel
    #**************************************************************************#
    mainPanel(
      
      br(),
      
      #Tabs
      tabsetPanel(
        
        #Gene expression tab
        tabPanel("Class Probability", value = "probs",
                 plotlyOutput("ProbPlot",
                            width = "1000px", 
                            height="600px")%>% 
                   withSpinner(color="#FFFFFF"),
                 
                verbatimTextOutput("prob_text"),
                 
                 tags$head(tags$style("#prob_text{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                 )
                 )
                 ),
        
        #Peptide expression tab
        tabPanel("PCA", value = "pca",
                 plotlyOutput("PCAplot",
                              width = "1000px", 
                              height="600px")%>% 
                   withSpinner(color="#FFFFFF")
                 ),
        
        #Peptide expression tab
        tabPanel("Scatter Plot", 
                 ),
        
        #correlation tab
        tabPanel("Histogram",
                 )
        
      ), #End tabset panel
      
      
    ) #End main panel
    
  ) #FluidPage
) #tagList

