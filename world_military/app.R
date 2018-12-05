# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(scales)

# Armed forces personnel, total
# International Institute for Strategic Studies, The Military Balance.
personnel <- read_csv("data/personnel/API_MS.MIL.TOTL.P1_DS2_en_csv_v2_10225185.csv", skip = 4)[, -c(2:4, 63)] %>%
  # bring multiple columns under single column "year"
  # convert year into integers
  gather("year", "total", 2:59, convert = TRUE) %>%
  # rename column variable to be more simple
  rename(country = `Country Name`) %>%
  # convert total into integers
  mutate(total = parse_number(total))

# Armed forces personnel (% of total labor force)
# International Institute for Strategic Studies, The Military Balance.
personnel_per <- read_csv("data/personnel_per/API_MS.MIL.TOTL.TF.ZS_DS2_en_csv_v2_10224838.csv", skip = 4)[, -c(2:4, 63)] %>%
  # bring multiple columns under single column "year"
  # convert year into integers
  gather("year", "per", 2:59, convert = TRUE) %>%
  # rename column variable to be more simple
  rename(country = `Country Name`) %>%
  # convert percentage into integers
  mutate(per = parse_number(per)) %>%
  mutate(per = per / 100)

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

imports <- read_csv("data/imports/TIV-Import-All-1950-2017-rs.csv", skip = 10)[, -70] %>%
  # bring multiple columns under single column "year"
  # convert year into integers
  gather("year", "tiv", 2:69, convert = TRUE) %>%
  # rename column variable to be more simple
  rename(country = X1) %>%
  filter(country != "Total")

exports <- read_csv("data/exports/API_MS.MIL.XPRT.KD_DS2_en_csv_v2_10230414.csv", skip = 4)[, -c(2:4, 63)] %>%
  # bring multiple columns under single column "year"
  # convert year into integers
  gather("year", "tiv", 2:59, convert = TRUE) %>%
  # rename column variable to be more simple
  rename(country = `Country Name`) %>%
  # in millions
  mutate(tiv = tiv / 1000000)

orgs <- imports %>%
  # after string splitting by * and taking the second string,
  # if the value is not NA, that means there were originally two asterisks
  filter(!is.na(sapply(strsplit(country, "\\*"), `[`, 2))) %>%
  # remove the asterisks from the organization names
  mutate(country = sapply(strsplit(country, "\\*"), `[`, 1))

# rebel groups have only one asterisk (*)
rebels <- imports %>%
  # filter out every string that has at least one asterisk
  filter(str_detect(country, "\\*")) %>%
  # remove the asterisks
  mutate(country = sapply(strsplit(country, "\\*"), `[`, 1)) %>%
  # match with the international organizations and remove those that overlap
  filter(!country %in% orgs$country)

imports <- imports %>%
  # remove the asterisks
  mutate(country = sapply(strsplit(country, "\\*"), `[`, 1))

weapon_total <- read_csv("data/exports/TIV-Export-All-1950-2017-wc.csv", skip=10)[, -70] %>%
  gather("year", "tiv", 2:69, convert = TRUE) %>%
  rename(type = X1) %>%
  filter(type == "Total") %>%
  spread(type, tiv) %>%
  rename(total = Total)

weapon <- read_csv("data/exports/TIV-Export-All-1950-2017-wc.csv", skip=10)[, -70] %>%
  gather("year", "tiv", 2:69, convert = TRUE) %>%
  rename(type = X1) %>%
  filter(type != "Total") %>%
  merge(weapon_total, by = "year") %>%
  group_by(year) %>%
  mutate(per = tiv / total)

expenditure <- read_csv("data/exports/API_MS.MIL.XPRT.KD_DS2_en_csv_v2_10230414.csv", skip = 4)[, -c(2:4, 63)] %>%
  # bring multiple columns under single column "year"
  # convert year into integers
  gather("year", "spending", 2:59, convert = TRUE) %>%
  # rename column variable to be more simple
  rename(country = `Country Name`) %>%
  # in millions
  mutate(spending = spending / 1000000)

nuke <- read_csv("data/number-of-nuclear-warheads-in-the-inventory-of-the-nuclear-powers.csv") %>%
  rename(n = X4)

# Define UI for application
ui <- navbarPage(
  title = "World Military Data",
  theme = shinytheme("sandstone"),
  tabPanel(
    title = "Personnel",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "per_years",
                    label = "Years",
                    min = 1985, max(personnel$year),
                    step = 1,
                    value = c(1985, 2017)),
        selectInput(inputId = "per_countries",
                    label = "Select Country(s)",
                    # list only countries
                    # remove non_countries that are in the country column
                    choices = personnel$country[!personnel$country %in% non_countries$TableName],
                    multiple = TRUE,
                    selected = "United States"),
        selectInput(inputId = "per_regions",
                    label = "Select Region(s)",
                    choices = regions$TableName,
                    multiple = TRUE),
        selectInput(inputId = "per_income_groups",
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
        h6("Similarily, organizational categories such as OECD and European Union are based
           on membership status in 2017."),
        h6("Source: International Institute for Strategic Studies, The Military Balance."),
        tags$h6(HTML("<a href='https://github.com/dodomoon/world_military'>GitHub</a>"))
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
        title = "Imports over time",
        sidebarLayout(
          sidebarPanel(
            sliderInput(inputId = "imp_years",
                        label = "Years",
                        min(imports$year), max(imports$year),
                        step = 1,
                        value = c(1990, 2010)),
            selectInput(inputId = "imp_countries",
                        label = "Select Country(s)",
                        # list only countries
                        # remove rebels and int orgs that are in the country column
                        choices = imports$country[!imports$country %in% orgs$country &
                                                  !imports$country %in% rebels$country],
                        multiple = TRUE,
                        selected = "United States"),
            selectInput(inputId = "imp_rebels",
                        label = "Select Rebel Force(s)",
                        choices = rebels$country,
                        multiple = TRUE),
            selectInput(inputId = "imp_orgs",
                        label = "Select International Organization(s)",
                        choices = orgs$country,
                        multiple = TRUE),
            h6("Arms transfers cover the supply of military weapons through sales,
               aid, gifts, and those made through manufacturing licenses. Data cover
               major conventional weapons such as aircraft, armored vehicles, artillery,
               radar systems, missiles, and ships designed for military use. Excluded 
               are transfers of other military equipment such as small arms and light 
               weapons, trucks, small artillery, ammunition, support equipment, technology
               transfers, and other services. Figures are SIPRI Trend Indicator Values (TIVs)
               expressed in US$ million at constant (1990) prices."),
            h5("A '0' indicates that the value of deliveries is less than 0.5 million but still more than 0."),
            h6("Source: Stockholm International Peace Research Institute (SIPRI), Arms Tranfer Database."),
            tags$h6(HTML("<a href='https://github.com/dodomoon/world_military'>GitHub</a>"))
          ),
          mainPanel(
            plotOutput("importOvertime")
          )
        )
      ),
      tabPanel(
        title = "Top Importers",
        sidebarLayout(
          sidebarPanel(
            sliderInput(inputId = "imptop_years",
                        label = "Years",
                        min(imports$year), max(imports$year),
                        step = 1,
                        value = c(1990, 2010)),
            numericInput(inputId = "imptop_num",
                         label = "How many countries?",
                         value = 5,
                         min = 2, max = 15,
                         step = 1),
            h6("Arms transfers cover the supply of military weapons through sales,
               aid, gifts, and those made through manufacturing licenses. Data cover
               major conventional weapons such as aircraft, armored vehicles, artillery,
               radar systems, missiles, and ships designed for military use. Excluded 
               are transfers of other military equipment such as small arms and light 
               weapons, trucks, small artillery, ammunition, support equipment, technology
               transfers, and other services. Figures are SIPRI Trend Indicator Values (TIVs)
               expressed in US$ million at constant (1990) prices."),
            h5("A '0' indicates that the value of deliveries is less than 0.5 million but still more than 0."),
            h6("Source: Stockholm International Peace Research Institute (SIPRI), Arms Tranfer Database."),
            tags$h6(HTML("<a href='https://github.com/dodomoon/world_military'>GitHub</a>"))
          ),
          mainPanel(
            plotOutput("importTop")
          )
        )
      ),
      tabPanel(
        title = "Exports over time",
        sidebarLayout(
          sidebarPanel(
            sliderInput(inputId = "exp_years",
                        label = "Years",
                        min(exports$year), max(exports$year),
                        step = 1,
                        value = c(1990, 2010)),
            selectInput(inputId = "exp_countries",
                        label = "Select Country(s)",
                        # list only countries
                        # remove non_countries that are in the country column
                        choices = exports$country[!exports$country %in% non_countries$TableName],
                        multiple = TRUE,
                        selected = "United States"),
            selectInput(inputId = "exp_regions",
                        label = "Select Region(s)",
                        choices = regions$TableName,
                        multiple = TRUE),
            selectInput(inputId = "exp_income_groups",
                        label = "Select Income Group(s)",
                        choices = income_groups$IncomeGroup,
                        multiple = TRUE),
            h6("Arms transfers cover the supply of military weapons through sales,
               aid, gifts, and those made through manufacturing licenses. Data cover
               major conventional weapons such as aircraft, armored vehicles, artillery,
               radar systems, missiles, and ships designed for military use. Excluded 
               are transfers of other military equipment such as small arms and light 
               weapons, trucks, small artillery, ammunition, support equipment, technology
               transfers, and other services. Figures are SIPRI Trend Indicator Values (TIVs)
               expressed in US$ million at constant (1990) prices."),
            h6("Income groups are based on GNI per capita calculated using the World Bank Atlas method.
               Low-income economies are those with GNI per capita of $995 or less in 2017;
               lower middle-income economies are those with GNI per capita between $996 and $3,895 in 2017;
               upper middle-income economies are those with GNI per capita between $3,896 and $12,055 in 2017;
               high-income economies are those with GNI per capita of $12,056 or more in 2017."),
            h6("Similarily, organizational categories such as OECD and European Union are based
               on membership status in 2017."),
            h6("Source: Stockholm International Peace Research Institute (SIPRI), Arms Tranfer Database."),
            tags$h6(HTML("<a href='https://github.com/dodomoon/world_military'>GitHub</a>"))
          ),
          mainPanel(
            plotOutput("exportOvertime")
          )
        )
      ),
      tabPanel(
        title = "Top Exporters",
        sidebarLayout(
          sidebarPanel(
            sliderInput(inputId = "exptop_years",
                        label = "Years",
                        min(exports$year), max(exports$year),
                        step = 1,
                        value = c(1990, 2010)),
            numericInput(inputId = "exptop_num",
                         label = "How many countries?",
                         value = 5,
                         min = 2, max = 15,
                         step = 1),
            h6("Arms transfers cover the supply of military weapons through sales,
               aid, gifts, and those made through manufacturing licenses. Data cover
               major conventional weapons such as aircraft, armored vehicles, artillery,
               radar systems, missiles, and ships designed for military use. Excluded 
               are transfers of other military equipment such as small arms and light 
               weapons, trucks, small artillery, ammunition, support equipment, technology
               transfers, and other services. Figures are SIPRI Trend Indicator Values (TIVs)
               expressed in US$ million at constant (1990) prices."),
            h6("Source: Stockholm International Peace Research Institute (SIPRI), Arms Tranfer Database."),
            tags$h6(HTML("<a href='https://github.com/dodomoon/world_military'>GitHub</a>"))
            ),
          mainPanel(
            plotOutput("exportTop")
          )
        )
      ),
      tabPanel(
        title = "Weapon Category",
        sidebarLayout(
          sidebarPanel(
            sliderInput(inputId = "weapon_years",
                        label = "Years",
                        min(weapon$year), max(weapon$year),
                        step = 1,
                        value = c(min(weapon$year), max(weapon$year))),
            checkboxGroupInput(inputId = "weapon_type",
                               label = "Select Weapon Types",
                               choices = unique(weapon$type)),
            h6("Arms transfers cover the supply of military weapons through sales,
               aid, gifts, and those made through manufacturing licenses. Data cover
               major conventional weapons such as aircraft, armored vehicles, artillery,
               radar systems, missiles, and ships designed for military use. Excluded 
               are transfers of other military equipment such as small arms and light 
               weapons, trucks, small artillery, ammunition, support equipment, technology
               transfers, and other services. Figures are SIPRI Trend Indicator Values (TIVs)
               expressed in US$ million at constant (1990) prices."),
            h6("Source: Stockholm International Peace Research Institute (SIPRI), Arms Tranfer Database."),
            tags$h6(HTML("<a href='https://github.com/dodomoon/world_military'>GitHub</a>"))
          ),
          mainPanel(
            plotOutput("weaponPlot"),
            h6("Aircraft: all fixed-wing aircraft and helicopters, including unmanned 
               aircraft (UAV/UCAV) with a minimum loaded weight of 20 kg. Exceptions 
               are microlight aircraft, powered and unpowered gliders and target drones."),
            h6("Air defence systems: (a) all land-based surface-to-air missile (SAM) systems, and (b) all anti-aircraft guns with a calibre of more than 40 mm or with multiple barrels with a combined caliber of at least 70 mm. This includes self-propelled systems on armoured or unarmoured chassis."),
            h6("Anti-submarine warfare weapons: rocket launchers, multiple rocket launchers and mortars for use against submarines, with a calibre equal to or above 100 mm."),
            h6("Armoured vehicles: all vehicles with integral armour protection, including all types of tank, tank destroyer, armoured car, armoured personnel carrier, armoured support vehicle and infantry fighting vehicle. Vehicles with very light armour protection (such as trucks with an integral but lightly armoured cabin) are excluded."),
            h6("Artillery: naval, fixed, self-propelled and towed guns, howitzers, multiple rocket launchers and mortars, with a calibre equal to or above 100 mm."),
            h6("Engines: (a) engines for military aircraft, for example, combat-capable aircraft, larger military transport and support aircraft, including large helicopters; (b) engines for combat ships -  fast attack craft, corvettes, frigates, destroyers, cruisers, aircraft carriers and submarines; (c) engines for most armoured vehicles - generally engines of more than 200 horsepower output."),
            h6("Missiles: (a) all powered, guided missiles and torpedoes, and (b) all unpowered but guided bombs and shells. This includes man-portable air defence systems (MANPADS) and portable guided anti-tank missiles. Unguided rockets, free-fall aerial munitions, anti-submarine rockets and target drones are excluded."),
            h6("Sensors: (a) all land-, aircraft- and ship-based active (radar) and passive (e.g. electro-optical) surveillance systems with a range of at least 25 kilometres, with the exception of navigation and weather radars, (b) all fire-control radars, with the exception of range-only radars, and (c) anti-submarine warfare and anti-ship sonar systems for ships and helicopters"),
            h6("Satellites: Reconnaissance satellites."),
            h6("Ships: (a) all ships with a standard tonnage of 100 tonnes or more, and (b) all ships armed with artillery of 100-mm calibre or more, torpedoes or guided missiles, and (c) all ships below 100 tonnes where the maximum speed (in kmh) multiplied with the full tonnage equals 3500 or more. Exceptions are most survey ships, tugs and some transport ships."),
            h6("(a) all turrets for armoured vehicles fitted with a gun of at least 12.7 mm calibre or with guided anti-tank missiles, (b) all turrets for ships fitted with a gun of at least 57-mm calibre, and (c) all turrets for ships fitted with multiple guns with a combined calibre of at least 57 mm, and (d) air refueling systems as used on tanker aircraft.")
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Spending",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "spe_years",
                    label = "Years",
                    min(expenditure$year), max(expenditure$year),
                    step = 1,
                    value = c(min(expenditure$year), max(expenditure$year))),
        selectInput(inputId = "spe_countries",
                    label = "Select Country(s)",
                    # list only countries
                    # remove non_countries that are in the country column
                    choices = expenditure$country[!expenditure$country %in% non_countries$TableName],
                    multiple = TRUE,
                    selected = "United States"),
        selectInput(inputId = "spe_regions",
                    label = "Select Region(s)",
                    choices = regions$TableName,
                    multiple = TRUE),
        selectInput(inputId = "spe_income_groups",
                    label = "Select Income Group(s)",
                    choices = income_groups$IncomeGroup,
                    multiple = TRUE),
        h6("Military expenditures data from SIPRI are derived from the NATO definition,
               which includes all current and capital expenditures on the armed forces,
               including peacekeeping forces; defense ministries and other government
               agencies engaged in defense projects; paramilitary forces, if these are
               judged to be trained and equipped for military operations; and military
               space activities. Such expenditures include military and civil personnel,
               including retirement pensions of military personnel and social services
               for personnel; operation and maintenance; procurement; military research
               and development; and military aid (in the military expenditures of the donor
               country). Excluded are civil defense and current expenditures for previous
               military activities, such as for veterans' benefits, demobilization, conversion,
               and destruction of weapons."),
        h6("Income groups are based on GNI per capita calculated using the World Bank Atlas method.
               Low-income economies are those with GNI per capita of $995 or less in 2017;
               lower middle-income economies are those with GNI per capita between $996 and $3,895 in 2017;
               upper middle-income economies are those with GNI per capita between $3,896 and $12,055 in 2017;
               high-income economies are those with GNI per capita of $12,056 or more in 2017."),
        h6("Similarily, organizational categories such as OECD and European Union are based
               on membership status in 2017."),
        h6("Source: Stockholm International Peace Research Institute (SIPRI), Yearbook: Armaments,
               Disarmament and International Security."),
        tags$h6(HTML("<a href='https://github.com/dodomoon/world_military'>GitHub</a>"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Total",
            plotOutput("spendingPlot")
          ),
          tabPanel(
            title = "% of GDP"
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Nuclear Weapons",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "nuke_years",
                    label = "Years",
                    min(nuke$Year), max(nuke$Year),
                    step = 1,
                    value = c(min(nuke$Year), max(nuke$Year))),
        selectInput(inputId = "nuke_countries",
                    label = "Select Country(s)",
                    # list only countries
                    # remove rebels and int orgs that are in the country column
                    choices = nuke$Entity,
                    multiple = TRUE,
                    selected = c("United States", "Russia")),
        h6("Having reached a peak in the late 1980s, the number of nuclear warheads has
            dropped significantly. But more countries now 
            possess them."),
        h6("Source: Bulletin of the Atomic Scientists."),
        tags$h6(HTML("<a href='https://github.com/dodomoon/world_military'>GitHub</a>"))
      ),
      mainPanel(
        plotOutput("nukePlot")
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  personnel_selection <- reactive({
    c(input$per_countries, input$per_regions, input$per_income_groups)
  })
  
  imports_selection <- reactive({
    c(input$imp_countries, input$imp_rebels, input$imp_orgs)
  })
  
  exports_selection <- reactive({
    c(input$exp_countries, input$exp_regions, input$exp_income_groups)
  })
  
  expenditure_selection <- reactive({
    c(input$spe_countries, input$spe_regions, input$spe_income_groups)
  })
  
  output$personnelPlot <- renderPlot({
    personnel %>%
      filter(country %in% personnel_selection()) %>%
      filter(year >= input$per_years[1] & year <= input$per_years[2]) %>%
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
      filter(year >= input$per_years[1] & year <= input$per_years[2]) %>%
      ggplot(aes(x = year, y = per, color = country)) +
      geom_line() +
      labs(title = "Armed forces personnel, % of total labor force",
           x = "Year",
           y = "Percentage") +
      scale_color_discrete(name = "Country(s)") +
      scale_y_continuous(labels = percent)
  })
  
  output$importOvertime <- renderPlot({
    imports %>%
      filter(country %in% imports_selection()) %>%
      filter(year >= input$imp_years[1] & year <= input$imp_years[2]) %>%
      ggplot(aes(x = year, y = tiv, color = country)) +
      geom_point() +
      geom_line() +
      labs(title = "Arms imports over time",
           x = "Year",
           y = "SIPRI TIV (in millions)") +
      scale_color_discrete(name = "Country(s)")
  })
  
  output$importTop <- renderPlot({
    imports %>%
      filter(year >= input$imptop_years[1] & year <= input$imptop_years[2]) %>%
      mutate(tiv = replace_na(tiv, 0)) %>%
      group_by(country) %>%
      summarize(total_tiv = sum(tiv)) %>%
      arrange(desc(total_tiv)) %>%
      head(input$imptop_num) %>%
      ggplot(aes(x = reorder(country, -total_tiv), y = total_tiv)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = total_tiv), vjust=-0.3, size=3.5) +
      labs(title = "Top importers",
           x = "Country",
           y = "Total TIV (in millions)")
  })
  
  output$exportOvertime <- renderPlot({
    exports %>%
      filter(country %in% exports_selection()) %>%
      filter(year >= input$exp_years[1] & year <= input$exp_years[2]) %>%
      ggplot(aes(x = year, y = tiv, color = country)) +
      geom_point() +
      geom_line() +
      labs(title = "Arms exports over time",
           x = "Year",
           y = "SIPRI TIV (in millions)") +
      scale_color_discrete(name = "Country(s)")
  })
  
  output$exportTop <- renderPlot({
    exports %>%
      filter(year >= input$exptop_years[1] & year <= input$exptop_years[2]) %>%
      mutate(tiv = replace_na(tiv, 0)) %>%
      filter(!country %in% non_countries$TableName) %>%
      filter(!str_detect(country, "\\(")) %>%
      group_by(country) %>%
      summarize(total_tiv = sum(tiv)) %>%
      arrange(desc(total_tiv)) %>%
      head(input$exptop_num) %>%
      ggplot(aes(x = reorder(country, -total_tiv), y = total_tiv)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = total_tiv), vjust=-0.3, size=3.5) +
      labs(title = "Top exporters",
           x = "Country",
           y = "Total TIV (in millions)")
  })
  
  output$spendingPlot <- renderPlot({
    expenditure %>%
      filter(country %in% expenditure_selection()) %>%
      filter(year >= input$spe_years[1] & year <= input$spe_years[2]) %>%
      ggplot(aes(x = year, y = spending, color = country)) +
      geom_line() +
      labs(title = "Military spending",
           x = "Year",
           y = "Expenditure in USD (in millions)") +
      scale_color_discrete(name = "Country(s)")
  })
  
  output$weaponPlot <- renderPlot({
    weapon %>%
      filter(year >= input$weapon_years[1] & year <= input$weapon_years[2]) %>%
      filter(type %in% input$weapon_type) %>%
      ggplot(aes(x = year, y = per, fill = type)) +
      geom_area() +
      labs(title = "Share of each weapon type in arms purchases",
           x = "Year",
           y = "% of Total TIV") +
      scale_fill_discrete(name = "Weapon Types") +
      scale_y_continuous(labels = percent)
  })
  
  output$nukePlot <- renderPlot({
    nuke %>%
      filter(Year >= input$nuke_years[1] & Year <= input$nuke_years[2]) %>%
      filter(Entity %in% input$nuke_countries) %>%
      ggplot(aes(x = Year, y = n, fill = Entity)) +
      geom_area() +
      labs(title = "Number of nuclear warheads in inventory",
           x = "Year",
           y = "Warheads") +
      scale_fill_discrete(name = "Country(s)")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

