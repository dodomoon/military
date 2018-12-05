# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)

# Armed forces personnel, total
# International Institute for Strategic Studies, The Military Balance.
personnel <- read_csv("data/personnel/API_MS.MIL.TOTL.P1_DS2_en_csv_v2_10225185.csv", skip = 4)[, -c(2:4, 63)] %>%
  # bring multiple columns under single column "year"
  # convert year into integers
  gather("year", "total", 2:59, na.rm = TRUE, convert = TRUE) %>%
  # rename column variable to be more simple
  rename(country = `Country Name`) %>%
  # convert total into integers
  mutate(total = parse_number(total))

# Armed forces personnel (% of total labor force)
# International Institute for Strategic Studies, The Military Balance.
personnel_per <- read_csv("data/personnel_per/API_MS.MIL.TOTL.TF.ZS_DS2_en_csv_v2_10224838.csv", skip = 4)[, -c(2:4, 63)] %>%
  # bring multiple columns under single column "year"
  # convert year into integers
  gather("year", "per", 2:59, na.rm = TRUE, convert = TRUE) %>%
  # rename column variable to be more simple
  rename(country = `Country Name`) %>%
  # convert percentage into integers
  mutate(per = parse_number(per))

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
        selectInput(inputId = "countries",
                    label = "Select Country(s)",
                    choices = personnel$country,
                    multiple = TRUE,
                    selected = "United States")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Armed Forces Personnel, Total",
            plotOutput("personnelPlot")
          ),
          tabPanel(
            title = "Armed Forces Personnel, % of Total Labor Force",
            plotOutput("personnelperPlot")
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
  
  output$personnelPlot <- renderPlot({
    personnel %>%
      filter(country %in% input$countries) %>%
      ggplot(aes(x = year, y = total, color = country)) +
      geom_line() +
      labs(title = "Armed forces personnel, total",
           x = "Year",
           y = "Total Personnel")
  })
  
  output$personnelperPlot <- renderPlot({
    personnel_per %>%
      filter(country %in% input$countries) %>%
      ggplot(aes(x = year, y = per, color = country)) +
      geom_line() +
      labs(title = "Armed forces personnel, % of total labor force",
           x = "Year",
           y = "Percentage")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

