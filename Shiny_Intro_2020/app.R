#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#example shiny app 
#Matthew Kustra 1/14/21
#load up different libraries
library(shiny)
library(ggplot2)
#set plottng theme 
mytheme<-theme_classic()+theme(legend.position ="bottom",#this puts legend on the bottom
                               axis.title = (element_text(face="bold")),#this makes the axis titles in bold,
                               axis.line=element_line(color="black",size=2),#Makes the axis line black and  thicker
                               text=element_text(size=15,face="bold"),#makes all the text larger and bold
                               plot.title = element_text(hjust = 0.5))
#set the default theme to mytheme 
theme_set(mytheme)
#load datasets
Data<-read.csv("Data/example_data.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(#need to make this to say we are having multiple tabs
   tabPanel("Homepage",#each tab panel 
            titlePanel(h1("Hello welcome to my first shiny app",align="center")),#way to center things
            fluidRow(column(12,#says I want to use all 12 columns 
                            h3("Description:"), #use header 3 size text
                            br(),#make a line break
                            p("This is my first shiny app using a fake tidepool dataset created by Dr. Tanya Rogers. Each tab is a different kind of graph. Enjoy!"),#make paragraph for description
                            )),
            fluidRow(column(12,plotOutput("tidepool")))#want to use the tidepool image for entire homepage
   # Application title
   ),tabPanel("Histograms",#name of the tab
   titlePanel("Histograms"), #gives us the title of the webpage
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(#saying to use a sidebarLayout. Two parts: sidebarPanel and mainPanel
      sidebarPanel(#usually where you put controls that user interacts with
         sliderInput("bins",#name of the input that you access in server wiht input$xxx
                     "Number of bins:",#what you are displaying 
                     min = 1,#miniumum of slider input
                     max = 30,#max of slider input
                     value = 5),#starting value 
         radioButtons(
           "sp_hist",#name of input
           "What Species to show?",#the header
           choices = list(#where you put the choices
             "Mussels" = "mussels", #name to show = variable to use in server
             "Snails" = "snails", #name to show  = variable to use in server
             "Barnacles" = "barnacles"
           ),
           selected = "mussels"#this is where you put the default
         ),
         radioButtons(
           "fill",#name of input
           "What color to fill?",#the header
           choices = list(#where you put the choices
             "Blue" = "royalblue2", #name to show = variable to use in server
             "Pink" = "hotpink1", #name to show  = variable to use in server
             "Green" = "seagreen2"
           ),
           selected = "royalblue2"#this is where you put the default
         )
      ),
      
      mainPanel(#usually where the plot or main thing is
         plotOutput("distPlot")#where you specify the name of the object you are plotting
      )
   )),
   tabPanel("Boxplots",#name of third tab
            titlePanel("Boxplots"), #gives us the title of the webpage
            # Sidebarlayout
            sidebarLayout(#saying to use a sidebarLayout. Two parts: sidebarPanel and mainPanel
              sidebarPanel(#usually where you put controls that user interacts with
                checkboxInput(#use checkbox; so either true or false
                  "bysite",#name of input
                  "Facet by sites?",#the header
                 FALSE#set default to unchecked or false
                ),
                conditionalPanel(condition="input.bysite",#setting up condtional panel
                                 #condition is that input.bysite is true
                checkboxGroupInput(#multiple checkboxes where user can select
                  "sites",#name of input
                  "Sites to display:",#the header
                  choices = list(#where you put the choices
                    "A" = "A", #name to show = variable to use in server
                    "B" = "B", #name to show  = variable to use in server
                    "C" = "C" #name to show  = variable to use in server
                  ),
                  selected = c("A","B","C")#this is where you put the default,in this case I want to select all as the default
                ))
              ),
              
              # Show a plot of the generated distribution
              mainPanel(#usually where the plot or main thing is
                plotOutput("BoxPlot")#where you specify the name of the object you are plotting
              )
            ))
))

# Define server logic required to draw a histogram
server <- function(input, output) {#server is where the actual plotting/data crunching happens
   #notice that distPlot matches what is inside plotOutput...plotOutput("distPlot")
   output$distPlot <- renderPlot({#use renderPlot to actually make the plot
     #think of it as a function...everything that happens in this is the code to generate the plot
     ggplot(data=Data[Data$Species==input$sp_hist,],aes(Abundance))+#subsetting data based on input$sp_hist
       geom_histogram(binwidth=input$bins,fill=input$fill)+#set bins to input$bins from ui.R
       ggtitle(input$sp_hist)
      #access input values with dollar sign.
      #notice that bins matches first expression in sliderInput
   })
   output$tidepool<-renderImage({#use renderImage to actually make the image
     file<-normalizePath(file.path("Data/tidepool.JPG"))#have to set the path with this function
     list(src = file,width = "100%",length="100%")#return a list with source filee path,and you can set width/length
     },deleteFile = FALSE)#want to st the app to not delete the file
   
   output$BoxPlot <- renderPlot({#use renderPlot to actually make boxplot
     #think of it as a function...everything that happens in this is the code to generate the plot
     Data<-Data[Data$site%in%input$sites,]#subset data based on input
     p1<-ggplot(data=Data,aes(x=tide.height,y=Abundance,fill=Species))+geom_boxplot()
     if(input$bysite){#need to see if we want to facet, if so we fact
       return(p1+facet_grid(.~site))
     }else{#if we don't facet then we don't
       return(p1)
     }
   })
     
}

# Run the application 
shinyApp(ui = ui, server = server)

