# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(tidyverse)

# Armed forces personnel, total
# International Institute for Strategic Studies, The Military Balance.
personnel <- read_csv("data/personnel/API_MS.MIL.TOTL.P1_DS2_en_csv_v2_10225185.csv", skip = 4)

# Armed forces personnel (% of total labor force)
# International Institute for Strategic Studies, The Military Balance.
personnel_per <- read_csv("data/personnel_per/API_MS.MIL.TOTL.TF.ZS_DS2_en_csv_v2_10224838.csv", skip = 4)

# Arms imports (SIPRI trend indicator values)
# Stockholm International Peace Research Institute ( SIPRI ), Arms Transfers Programme.
imports <- read_csv("data/imports/API_MS.MIL.MPRT.KD_DS2_en_csv_v2_10230080.csv", skip = 4)

# Arms exports (SIPRI trend indicator values)
# Stockholm International Peace Research Institute ( SIPRI ), Arms Transfers Programme
exports <- read_csv("data/exportsAPI_MS.MIL.XPRT.KD_DS2_en_csv_v2_10230414.csv", skip = 4)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

