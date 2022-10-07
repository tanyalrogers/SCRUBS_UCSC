# 1. Info --------------------------------------------------------------------
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# example shiny app
# Matthew Kustra 2/4/22
# 2. Load up libraries -------------------------------------------------------
library(shiny)
library(tidyverse) 
library(plotly)


# 3. Loading in Data/processing data --------------------------------------
# read in Data frame
Data <- read.csv("Data/example_data.csv")
# Make a separate dataframe in wide format for scatter plots
Data_wide <- Data %>%
  pivot_wider(names_from = Species, values_from = Abundance)

# Define UI for application that draws a histogram
ui <- fluidPage(
  title="My First app",
  tabsetPanel( # need to make this to say we are having multiple tabs
    tabPanel(
      "Homepage", # each tab panel
      titlePanel(h1("Hello welcome to my first shiny app", align = "center")), # way to center things
      fluidRow(column(
        12, # says I want to use all 12 columns
        h3("Description:"), # use header 3 size text
        br(), # make a line break
        p("This is my first shiny app using a fake tidepool dataset created by Dr. Tanya Rogers. Each tab is a different kind of graph. Enjoy!"), # make paragraph for description
      )),
      fluidRow(column(12, imageOutput("tidepool"))) # want to use the tidepool image for entire homepage
      # Application title
    ), tabPanel(
      "Histograms", # name of the tab
      titlePanel("Histograms"), # gives us the title of the webpage

      # Sidebar with a slider input for number of bins
      sidebarLayout( # saying to use a sidebarLayout. Two parts: sidebarPanel and mainPanel
        sidebarPanel( # usually where you put controls that user interacts with
          sliderInput("bins", # name of the input that you access in server wiht input$xxx
            "Number of bins:", # what you are displaying
            min = 1, # miniumum of slider input
            max = 30, # max of slider input
            value = 5
          ), # starting value
          radioButtons(
            "sp_hist", # name of input
            "What Species to show?", # the header
            choices = list( # where you put the choices
              "Mussels" = "mussels", # name to show = variable to use in server
              "Snails" = "snails", # name to show  = variable to use in server
              "Barnacles" = "barnacles"
            ),
            selected = "mussels" # this is where you put the default
          ),
          radioButtons(
            "fill", # name of input
            "What color to fill?", # the header
            choices = list( # where you put the choices
              "Blue" = "royalblue2", # name to show = variable to use in server
              "Pink" = "hotpink1", # name to show  = variable to use in server
              "Green" = "seagreen2"
            ),
            selected = "royalblue2" # this is where you put the default
          )
        ),
        mainPanel( # usually where the plot or main thing is
          plotOutput("distPlot") # where you specify the name of the object you are plotting
        )
      )
    ),
    tabPanel(
      "Scatter plots", # name of third tab
      titlePanel("Scatter plots"), # gives us the title of the webpage
      # Sidebarlayout
      sidebarLayout( # saying to use a sidebarLayout. Two parts: sidebarPanel and mainPanel
        sidebarPanel( # usually where you put controls that user interacts with
          # condition is that input.bysite is true
          radioButtons( # multiple buttons where user can select
            "specs_x", # name of input
            "Species on X-axis:", # the header
            choices = list( # where you put the choices
              "Mussels" = "mussels", # name to show = variable to use in server
              "Snails" = "snails", # name to show  = variable to use in server
              "Barnalces" = "barnacles" # name to show  = variable to use in server
            ),
            selected = "mussels" # this is where you put the default,in this case I want to select all as the default
          ),
          radioButtons( # multiple buttons where user can select
            "specs_y", # name of input
            "Species on Y-axis:", # the header
            choices = list( # where you put the choices
              "Mussels" = "mussels", # name to show = variable to use in server
              "Snails" = "snails", # name to show  = variable to use in server
              "Barnalces" = "barnacles" # name to show  = variable to use in server
            ),
            selected = "snails" # this is where you put the default
          ),
          checkboxInput( # use checkbox; so either true or false
            "threeD", # name of input
            "Plot in 3D?", # the header
            FALSE # set default to unchecked or false
          ),
          conditionalPanel(
            condition = "input.threeD",
            radioButtons( # multiple checkboxes where user can select
              "specs_z", # name of input
              "Species on Z-axis:", # the header
              choices = list( # where you put the choices
                "Mussels" = "mussels", # name to show = variable to use in server
                "Snails" = "snails", # name to show  = variable to use in server
                "Barnalces" = "barnacles" # name to show  = variable to use in server
              ),
              selected = "barnacles" # this is where you put the default
            )
          )
        ),

        # Show a plot of the generated distribution
        mainPanel( # usually where the plot or main thing is
          plotlyOutput("ScatterPlot", height = "800px") # where you specify the name of the object you are plotting, and how large it is
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) { # server is where the actual plotting/data crunching happens
  # notice that distPlot matches what is inside plotOutput...plotOutput("distPlot")
  output$distPlot <- renderPlot({ # use renderPlot to actually make the plot
    # think of it as a function...everything that happens in this is the code to generate the plot
    ggplot(data = Data[Data$Species == input$sp_hist, ], aes(Abundance)) + # subsetting data based on input$sp_hist
      geom_histogram(binwidth = input$bins, fill = input$fill) + # set bins to input$bins from ui.R
      ggtitle(input$sp_hist)
    # access input values with dollar sign.
    # notice that bins matches first expression in sliderInput
  })
  output$tidepool <- renderImage(
    { # use renderImage to actually make the image
      file <- normalizePath(file.path("Data/tidepool.JPG")) # have to set the path with this function
      list(src = file, width = "100%", length = "100%") # return a list with source filee path,and you can set width/length
    },
    deleteFile = FALSE
  ) # want to st the app to not delete the file

  output$ScatterPlot <- renderPlotly({ # use renderPlotly to actually make scatterplot
    # think of it as a function...everything that happens in this is the code to generate the plot
    if (!input$threeD) { # if we are only plotting in 2D
      p1 <- ggplot(data = Data_wide, aes_string(x = input$specs_x, y = input$specs_y, fill = "site")) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = input$specs_x, y = input$specs_y)
      return(p1)
    } else { # if we want 3D plotting we can directly do this in Plotly
      # plotly uses ~ tildas for column names
      # get() is used to convert string of column name to column name
      p2 <- plot_ly(Data_wide, z = ~ get(input$specs_z), y = ~ get(input$specs_y), x = ~ get(input$specs_x)) %>% # like ggplot you can add layers, but use piping
        add_markers(color = ~site) %>% # this is like geom_point
        layout(scene = list(yaxis = list(title = input$specs_y), xaxis = list(title = input$specs_x), zaxis = list(title = input$specs_z))) # this lets you rename axes
      return(p2)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
