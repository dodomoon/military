# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)

# Define UI for application
ui <- navbarPage(
  title = "World Military Data",
  theme = shinytheme("sandstone"),
  tabPanel(
    title = "Introduction",
    mainPanel(
        "This Shiny app looks at military data for each country. While all the data is downloaded from The World Bank's DataBank,
        each dataset has its own unique original source.
        Personnel data is from the International Institute for Strategic Studies.
        Arms Transfers data is from the Stockholm International Peace Research Institute."
    )
  ),
  tabPanel(
    title = "Personnel",
    sidebarLayout(
      sidebarPanel(
        textInput("title", "Title", "GDP vs life exp"),
        numericInput("size", "Point size", 1, 1),
        checkboxInput("fit", "Add line of best fit", FALSE),
        radioButtons("colour", "Point colour",
                     choices = c("blue", "red", "green", "black"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Armed Forces Personnel, Total",
            plotOutput("personnelPlot")
          ),
          tabPanel(
            title = "Armed Forces Personnel, % of Total Labor Force"
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Arms Trade",
    tabsetPanel(
      tabPanel(
        title = "Imports"
      ),
      tabPanel(
        title = "Exports"
      ),
      tabPanel(
        title = "Combined"
      )
    )
  ),
  tabPanel(
    title = "Spending",
    tabsetPanel(
      tabPanel(
        title = "Military Expenditure, current USD"
      ),
      tabPanel(
        title = "Military Expenditure, % of GDP"
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # provide URL to Github code
  url <- a("github", href="https://github.com/dodomoon/world_military")
  
  # output link to Github code
  output$git_url <- renderUI({
    tagList("See the Code:", url)
  })
  
  # first plot
  output$personnelPlot <- renderPlot({
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

