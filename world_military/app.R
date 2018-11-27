# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(tidyverse)
library(data.table)

setwd("~/Desktop/Harvard/Fall 2018/GOV 1005/world_military/world_military")

# Armed forces personnel, total
# International Institute for Strategic Studies, The Military Balance.
personnel <- read_csv("data/personnel/API_MS.MIL.TOTL.P1_DS2_en_csv_v2_10225185.csv", skip = 4)

personnel <- personnel %>%
  filter(`Country Name` == "United States")
personnel <- personnel[, -c(1:33, 62, 63)]

m1 <- t(personnel)
personnel2 <- data.frame(r1= row.names(m1), m1, row.names=NULL)

# personnel2 <- data.frame(t(personnel[-1]))
# names(personnel2) <- unlist(personnel[,1], use.names=FALSE)
# 
# personnel2[is.na(personnel2)] <- 0
# 
# personnel2 <- personnel2 %>%
#   mutate_all(funs(replace(., is.na(.), 0)))
# 
# %>%
#   mutate(sum = rowSums(.[1:264]))

# Armed forces personnel (% of total labor force)
# International Institute for Strategic Studies, The Military Balance.
personnel_per <- read_csv("data/personnel_per/API_MS.MIL.TOTL.TF.ZS_DS2_en_csv_v2_10224838.csv", skip = 4)

# Arms imports (SIPRI trend indicator values)
# Stockholm International Peace Research Institute ( SIPRI ), Arms Transfers Programme.
imports <- read_csv("data/imports/API_MS.MIL.MPRT.KD_DS2_en_csv_v2_10230080.csv", skip = 4)

# Arms exports (SIPRI trend indicator values)
# Stockholm International Peace Research Institute ( SIPRI ), Arms Transfers Programme
exports <- read_csv("data/exports/API_MS.MIL.XPRT.KD_DS2_en_csv_v2_10230414.csv", skip = 4)

ui <- fluidPage(
  titlePanel("World Military Data"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("tab")
      ),
    mainPanel("This Shiny app looks at military data for each country. While all the data is downloaded from The World Bank's DataBank,
                each dataset has its own unique original source.
                Personnel data is from the International Institute for Strategic Studies.
                Arms Transfers data is from the Stockholm International Peace Research Institute.", 
              plotOutput("personnelPlot"))
  )
)

server <- function(input, output) {
  url <- a("github", href="https://github.com/dodomoon/world_military")
  output$tab <- renderUI({
    tagList("See the Code:", url)
  })
  
  output$personnelPlot <- renderPlot({
    
    personnel2 %>%
      ggplot(aes(x = r1, y = m1)) +
      # create scatterplot
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      # add title, subtitle
      labs(title = "Armed forces personnel, United States",
           x = "Year",
           y = "Total Personnel")
    
  })
}

# # Define UI for application that draws a histogram
# ui <- fluidPage(
#    
#    # Application title
#    titlePanel("Old Faithful Geyser Data"),
#    
#    # Sidebar with a slider input for number of bins 
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("bins",
#                      "Number of bins:",
#                      min = 1,
#                      max = 50,
#                      value = 30)
#       ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#          plotOutput("distPlot")
#       )
#    )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#    
#    output$distPlot <- renderPlot({
#       # generate bins based on input$bins from ui.R
#       x    <- faithful[, 2] 
#       bins <- seq(min(x), max(x), length.out = input$bins + 1)
#       
#       # draw the histogram with the specified number of bins
#       hist(x, breaks = bins, col = 'darkgray', border = 'white')
#    })
# }

# Run the application 
shinyApp(ui = ui, server = server)

