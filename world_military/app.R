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

# Metadata from personnel dataset
personnel_meta <- read_csv("data/personnel/Metadata_Country_API_MS.MIL.TOTL.P1_DS2_en_csv_v2_10225185.csv")[, -c(4, 6)]

# create dataframe of table names that are not countries
non_countries <- personnel_meta %>%
  filter(is.na(Region)) %>%
  select(TableName)

# create dataframe of regions, excluding insignificant ones
regions <- non_countries %>%
  filter(!(str_detect(TableName, "\\(")),
         !(str_detect(TableName, "mall states")),
         !(str_detect(TableName, "dividend")),
         !(str_detect(TableName, "IDA")),
         !(str_detect(TableName, "IBRD")),
         !(str_detect(TableName, "income")),
         TableName != "Fragile and conflict affected situations",
         TableName != "Euro area",
         TableName != "Least developed countries: UN classification")

# create dataframe of income groups
income_groups <- personnel_meta %>%
  select(IncomeGroup) %>%
  distinct() %>%
  na.omit()

# Define UI for application
ui <- navbarPage(
  title = "World Military Data",
  theme = shinytheme("sandstone"),
  tabPanel(
    title = "Personnel",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "countries",
                    label = "Select Country(s)",
                    # list only countries
                    # remove non_countries that are in the country column
                    choices = personnel$country[!personnel$country %in% non_countries$TableName],
                    multiple = TRUE,
                    selected = "United States"),
        selectInput(inputId = "regions",
                    label = "Select Region(s)",
                    choices = regions$TableName,
                    multiple = TRUE),
        selectInput(inputId = "income_groups",
                    label = "Select Income Group(s)",
                    choices = income_groups$IncomeGroup,
                    multiple = TRUE),
        h6("Armed forces personnel are active duty military personnel, including
           paramilitary forces if the training, organization, equipment, and control
           suggest they may be used to support or replace regular military forces.
           Labor force comprises all people who meet the International Labour
           Organization's definition of the economically active population."),
        h6("Income groups are based on GNI per capita calculated using the World Bank Atlas method.
           Low-income economies are those with GNI per capita of $995 or less in 2017;
           lower middle-income economies are those with GNI per capita between $996 and $3,895 in 2017;
           upper middle-income economies are those with GNI per capita between $3,896 and $12,055 in 2017;
           high-income economies are those with GNI per capita of $12,056 or more in 2017."),
        h6("Similarily, the organizational categories such as OECD and European Union are based
           on membership status in 2017."),
        h6("Source: International Institute for Strategic Studies, The Military Balance."),
        uiOutput("git_url")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Total",
            plotOutput("personnelPlot")
          ),
          tabPanel(
            title = "% of Labor Force",
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
        title = "Imports, over time",
        sidebarLayout(
          sidebarPanel(
            
          ),
          mainPanel(
            
          )
        )
      ),
      tabPanel(
        title = "Top Importers",
        sidebarLayout(
          sidebarPanel(
            
          ),
          mainPanel(
            
          )
        )
      ),
      tabPanel(
        title = "Exports, over time",
        sidebarLayout(
          sidebarPanel(
            
          ),
          mainPanel(
            
          )
        )
      ),
      tabPanel(
        title = "Top Exporters",
        sidebarLayout(
          sidebarPanel(
            
          ),
          mainPanel(
            
          )
        )
      ),
      tabPanel(
        title = "By Weapon Category",
        sidebarLayout(
          sidebarPanel(
            
          ),
          mainPanel(
            
          )
        )
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
  # output link to Github code
  output$git_url <- renderUI({
    h6(a("GitHub", href="https://github.com/dodomoon/world_military"))
  })
  
  personnel_selection <- reactive({
    c(input$countries, input$regions, input$income_groups)
  })
  
  output$personnelPlot <- renderPlot({
    personnel %>%
      filter(country %in% personnel_selection()) %>%
      ggplot(aes(x = year, y = total, color = country)) +
      geom_line() +
      labs(title = "Armed forces personnel, total",
           x = "Year",
           y = "Total Personnel") +
      scale_color_discrete(name = "Country(s)")
  })
  
  output$personnelperPlot <- renderPlot({
    personnel_per %>%
      filter(country %in% personnel_selection()) %>%
      ggplot(aes(x = year, y = per, color = country)) +
      geom_line() +
      labs(title = "Armed forces personnel, % of total labor force",
           x = "Year",
           y = "Percentage") +
      scale_color_discrete(name = "Country(s)")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

