#=============================================================================#
# File: ui.R
# Date: October 5, 2022										                                      
# Author: Jarno Koetsier                                                      
# Data: NA
#
# R version: 4.2.1 (getRversion())
# RStudio version: 2022.7.1.544 (RStudio.Version())
#=============================================================================#

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
      
      # Area Worst
      numericInput(inputId = "AreaWorst", 
                   label = "Area Worst", 
                   value = NULL),
      
      #Concave Points Worst
      numericInput(inputId = "ConcavePointsWorst", 
                   label = "Concave Points Worst", 
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
        tabPanel("Class Probability", value = "probs", icon = icon("fa-solid fa-house", verify_fa = FALSE),
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
        tabPanel("PCA", value = "pca", icon = icon("fa-solid fa-square-check", verify_fa = FALSE),
                 plotlyOutput("PCAplot",
                              width = "1000px", 
                              height="600px")%>% 
                   withSpinner(color="#FFFFFF")
                 ),
        
        #Scatter plot tab
        tabPanel("Scatter Plot", icon = icon("fa-solid fa-arrow-pointer", verify_fa = FALSE),
                 br(),
                 fluidRow(
                   column(width = 3,
                          selectInput(inputId = "Xaxis", 
                                      label = tags$span(style="color: white;","x-axis"),
                                      choices = c("Texture Worst",
                                                  "Area Worst",
                                                  "Concave Points Worst"
                                      ),
                                      selected = "Area Worst",
                                      multiple = FALSE)
                          ),
                   column(width = 3,
                          selectInput(inputId = "Yaxis", 
                                      label = tags$span(style="color: white;","y-axis"),
                                      choices = c("Texture Worst",
                                                  "Area Worst",
                                                  "Concave Points Worst"
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
        tabPanel("Histogram", icon = icon("fa-solid fa-arrow-pointer", verify_fa = FALSE),
                 br(),
                 
                 selectInput(inputId = "feature", 
                             label = NULL,
                             choices = c("Texture Worst",
                                         "Area Worst",
                                         "Concave Points Worst"
                             ),
                             selected = "Perimeter Worst",
                             multiple = FALSE),
                 
                 hr(),
                 
                 plotlyOutput("histPlot",
                              width = "1000px", 
                              height="600px")%>% 
                   withSpinner(color="#FFFFFF")
                 
                 ),
        
        #Information tab
        tabPanel("Information",icon = icon("fa-regular fa-circle-info", verify_fa = FALSE),
                 h1(strong("Information"), style = 'color:white'),
                 br(),
                 h4("To get started with an example, click on ", strong("Example"), 
                    " and then on ", strong("Predict"),
                    " in the left-hand panel.",
                    style = 'color:white'),
                 h4("Now, you can click on the different tabs to explore your results.",
                    style = 'color:white')
        )
        
      ), #End tabset panel
      
      
    ) #End main panel
    
  ) #FluidPage
) #tagList

