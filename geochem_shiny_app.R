
library(shiny)
library(tidyverse)



#set slider range to the range of color in dataset
min.geochem <- min(geochem)
max.geochem <- max(geochem)


# Set a vector of axis variables as characters
axis_vars <- names(geochem)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Microecology Data Viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
      selectInput(inputId = "xvar",
                  label = "X axis",
                  choices = axis_vars,
                  selected = "x"),
      
      selectInput(inputId = "yvar",
                  label = "Y axis",
                  choices = axis_vars,
                  selected = "y"),
      submitButton("Go!")
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("microecology_data_sim")
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  data_filt <- reactive({
    
    
    low.geochem <- input$geochem.adjuster[1]
    hi.geochem <- input$geochem.adjuster[2]
    
    geochem_filt <- geochem %>%
      filter(depth >= low.geochem) %>%
      filter(depth <= hi.geochem)
  })
  
  
  output$geochem_filt_plot <- renderPlot({
    ggplot(geochem_filt(), aes_string(x = input$xvar, y = input$yvar, color = "clarity")) +
      geom_point()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
