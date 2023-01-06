# A shiny app that let user select a year range as well as a country from a list, and then plot the data from the selected country.
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(data.table)

pal <- colorNumeric("Blues", NULL)

# Read data
countries <- readRDS("data/countries.RDS")

out2in <- list()
for (year in 2011:2021) {
  filename <- paste("data/out2in/", year, ".csv", sep = "")
  temp <- read.csv(filename, na.strings = "na", row.names = 1)
  colnames(temp)[which(names(temp) == "NA.")] <- "NA"
  out2in[[year - 2010]] <- temp
}

valid_countries <- read.csv("data/valid_countries.csv", na.strings = "na")
name2code <- as.list(valid_countries)$two
names(name2code) <- as.list(valid_countries)$name
code2name <- as.list(valid_countries)$name
names(code2name) <- as.list(valid_countries)$two

# Define the app
header <- dashboardHeader(title = "Human Conversation Dynamics")
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Country Details", tabName = "countryYear"),
    menuItem("About", tabName = "about")
  )
)
body <- dashboardBody(
  tabItems(

    # Third tab content
    tabItem(
      tabName = "countryYear",
      # Select year of interest and country of interest
      fluidRow(
        box(selectizeInput("countryCountryYear", "Select a country (region):", sort(countries$`Country (region)`))),
        box(sliderInput("yearCountryYear", "Select a year:", 2011, 2021, 2011, sep = ""))
      ),
      # Two map plots, the first outgoing, the second incoming
      fluidRow(
        box(title = "Export (retweeted by)", leafletOutput("outgoingMapCountryYear", height = 400), status = "primary"),
        box(title = "Import (retweeting)", leafletOutput("incomingMapCountryYear", height = 400), status = "primary")
      ),
      # Two lists, the first outgoing, the second incoming
      fluidRow(
        box(title = "Export (retweeted by)", dataTableOutput("outgoingTableCountryYear"), status = "primary"),
        box(title = "Import (retweeting)", dataTableOutput("incomingTableCountryYear"), status = "primary")
      )
    ),
    
    # Fifth tab content
    tabItem(
      tabName = "about",
      h2("About"),
      "Visit the",
      a(href = "https://github.com/zeyu42/human_conversation_dynamics", "Github repo"),
      "for an introduction to this app."
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "purple")



server <- function(input, output, session) {
  # Define how the map should look like, given country map and data
  output$outgoingMapCountryYear <- renderLeaflet({
    leaflet(countries) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE, minZoom=1, maxZoom=5)
      ) %>%
      addTiles()
  })
  output$incomingMapCountryYear <- renderLeaflet({
    leaflet(countries) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE, minZoom=1, maxZoom=5)
      ) %>%
      addTiles()
  })
  map_out_proxy <- leafletProxy("outgoingMapCountryYear", session)
  map_in_proxy <- leafletProxy("incomingMapCountryYear", session)
  
  # Watch tab
  observe({
    if (input$tabs == "countryYear" && input$countryCountryYear != "") {
      # Prepare incoming and outgoing data frames
      data_in <- out2in[[input$yearCountryYear - 2010]][name2code[input$countryCountryYear]]
      data_out <- transpose(out2in[[input$yearCountryYear - 2010]][name2code[input$countryCountryYear],])
      data_out$Country <- rownames(data_in)
      colnames(data_out) <- c("Count", "Country (region)")
      data_in$Country <- data_out$Country
      colnames(data_in) <- colnames(data_out)
      data_in <- data_in[order(data_in$Count, decreasing = TRUE),]
      data_out <- data_out[order(data_out$Count, decreasing = TRUE),]
      data_in$`Country (region)` <- sapply(data_in$`Country (region)`, function(x) code2name[x])
      data_out$`Country (region)` <- sapply(data_out$`Country (region)`, function(x) code2name[x])
      data_in <- data_in[complete.cases(data_in),]
      data_out <- data_out[complete.cases(data_out),]
      row.names(data_in) <- NULL
      row.names(data_out) <- NULL
      
      # Add the two data frames to the `countries` object, which contains geographical info
      countries$data_in <- data_in$Count[match(countries$ISO_A2, sapply(data_in$`Country (region)`, function(x) name2code[x]))]
      countries$data_out <- data_out$Count[match(countries$ISO_A2, sapply(data_out$`Country (region)`, function(x) name2code[x]))]
      
      # Display everything, data frames first, then maps
      output$outgoingTableCountryYear <- renderDataTable({
        datatable(data_out, options = list(pageLength = 20, order = list(1, 'desc')))
      })
      output$incomingTableCountryYear <- renderDataTable({
        datatable(data_in, options = list(pageLength = 20, order = list(1, 'desc')))
      })
     
      map_out_proxy %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = countries, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
          fillColor = ~ pal(log10(data_out + 1)),
          label = ~ paste0(`Country (region)`, ": ", formatC(data_out, big.mark = ","))
        ) %>%
        addLegend(
          data = countries, title = "Count", position = "bottomleft", pal = pal, values =  ~log10(data_out + 1),
          labFormat = labelFormat(transform = function(x) round(10^x))
        )
      
      map_in_proxy %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = countries, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
          fillColor = ~ pal(log10(data_in + 1)),
          label = ~ paste0(`Country (region)`, ": ", formatC(data_in, big.mark = ","))
        ) %>%
        addLegend(
          data = countries, title = "Count", position = "bottomleft", pal = pal, values =  ~log10(data_out + 1),
          labFormat = labelFormat(transform = function(x) round(10^x))
        )
    }
  })
}

shinyApp(ui, server)
