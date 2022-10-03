ui <- fluidPage(
  theme = shinytheme("spacelab"),
  setBackgroundColor("#343434"),
  fluidPage(
    
    #**************************************************************************#
    # Side panel
    #**************************************************************************#
    sidebarPanel(
      
      #Title
      h2(strong("Predict")),
      
      hr(),
      

      
      # Perimeter Worst
      numericInput(inputId = "PerimeterWorst", 
                label = "Perimeter Worst", 
                value = 1),
      
      # Radius Worst
      numericInput(inputId = "RadiusWorst", 
                   label = "Texture Worst", 
                   value = 1),
      
      # Smoothness Worst
      numericInput(inputId = "SmoothnessWorst", 
                   label = "Texture Worst", 
                   value = 1),
      
      # Texture Worst
      numericInput(inputId = "TextureWorst", 
                   label = "Texture Worst", 
                   value = 1),
      
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
        tabPanel("Class Probability"
                 ),
        
        #Peptide expression tab
        tabPanel("PCA",       
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

