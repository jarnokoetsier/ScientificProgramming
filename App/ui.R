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
                 color = "success",
                 icon("fa-solid fa-rotate", verify_fa = FALSE)),

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
                 icon("fa-solid fa-play", verify_fa = FALSE))

      
      
    ), #End Sidebar panel
    
    
    
    
    #**************************************************************************#
    # Main panel
    #**************************************************************************#
    mainPanel(
      
      br(),
      
      #Tabs
      tabsetPanel(
        
        #Class probability tab
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
        
        #PCA tab
        tabPanel("PCA", value = "pca",
                 plotlyOutput("PCAplot",
                              width = "1000px", 
                              height="600px")%>% 
                   withSpinner(color="#FFFFFF")
                 ),
        
        #Scatter plot tab
        tabPanel("Scatter Plot", 
                 br(),
                 fluidRow(
                   column(width = 3,
                          selectInput(inputId = "Xaxis", 
                                      label = tags$span(style="color: white;","x-axis"),
                                      choices = c("Perimeter Worst",
                                                  "Radius Worst",
                                                  "Texture Worst",
                                                  "Mean Concave Points"
                                      ),
                                      selected = "Perimeter Worst",
                                      multiple = FALSE)
                          ),
                   column(width = 3,
                          selectInput(inputId = "Yaxis", 
                                      label = tags$span(style="color: white;","y-axis"),
                                      choices = c("Perimeter Worst",
                                                  "Radius Worst",
                                                  "Texture Worst",
                                                  "Mean Concave Points"
                                      ),
                                      selected = "Texture Worst",
                                      multiple = FALSE),
                     )
                 ),
                 
                 hr(),

                 plotlyOutput("scatter",
                              width = "1000px", 
                              height="600px")%>% 
                   withSpinner(color="#FFFFFF")
                 
                 ),
        
        #Histogram tab
        tabPanel("Histogram",
                 br(),
                 
                 selectInput(inputId = "feature", 
                             label = NULL,
                             choices = c("Perimeter Worst",
                                         "Radius Worst",
                                         "Texture Worst",
                                         "Mean Concave Points"),
                             selected = "Perimeter Worst",
                             multiple = FALSE),
                 
                 hr(),
                 
                 plotlyOutput("histPlot",
                              width = "1000px", 
                              height="600px")%>% 
                   withSpinner(color="#FFFFFF")
                 
                 )
        
      ), #End tabset panel
      
      
    ) #End main panel
    
  ) #FluidPage
) #tagList

