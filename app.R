# A shiny app that let user select a year range as well as a country from a list, and then plot the data from the selected country.
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(data.table)
library(purrr)

net_log <- function(x) {
  map_vec(x, function(y) {
    if (is.na(y)) NA else if (y < 0) -log10(-y+1) else log10(y+1)
  })
}

net_exp <- function(x) {
  map_vec(x, function(y) {
    if (is.na(y)) NA else if (y < 0) -10^(-y) else 10^y
  })
}

pal <- colorNumeric("Blues", NULL)
net_pal <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(pal(x))
  }
  domain <- range(x, na.rm = TRUE)
  if (domain[1] > 0)
    pos_domain <- c(domain[1], domain[2]) else pos_domain <- c(0, domain[2])
  if (domain[2] < 0)
    neg_domain <- c(domain[1], domain[2]) else neg_domain <- c(domain[1], 0)
  if (pos_domain[1] <= pos_domain[2])
    pos_pal <- colorNumeric("Blues", pos_domain) else pos_pal <- colorNumeric("Blues", NULL)
  if (neg_domain[1] <= neg_domain[2])
    neg_pal <- colorNumeric("Reds", neg_domain, reverse=TRUE) else neg_pal <- colorNumeric("Reds", NULL, reverse=TRUE)
  map_vec(x, function(y){
    if (is.na(y)) {
      return(pos_pal(y))
    } else if (y >= 0) {
      return(pos_pal(y))
    } else {
      return(neg_pal(y))
    }
  })
}
attr(net_pal, "colorType") <- "numeric"

globality_pal <- colorNumeric("Blues", domain = c(0, 1))

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
    menuItem("Import", tabName = "importYear"),
    menuItem("Export", tabName = "exportYear"),
    menuItem("Net Export", tabName = "netExportYear"),
    menuItem("Globality", tabName = "globalityYear"),
    menuItem("Per Country", tabName = "countryYear"),
    menuItem("About", tabName = "about")
  )
)
body <- dashboardBody(
  tabItems(
    
    # Import
    tabItem(
      tabName = "importYear",
      box(sliderInput("yearImportYear", "Select a year:", 2011, 2021, 2011, sep = "")),
      box(title = "Import (retweeting)", leafletOutput("importMapYear", height = 400), dataTableOutput("importTableYear"), status = "primary")
    ),
    
    # Export
    tabItem(
      tabName = "exportYear",
      box(sliderInput("yearExportYear", "Select a year:", 2011, 2021, 2011, sep = "")),
      box(title = "Export (retweeted by)", leafletOutput("exportMapYear", height = 400), dataTableOutput("exportTableYear"), status = "primary")
    ),

    # Net export
    tabItem(
      tabName = "netExportYear",
      box(sliderInput("yearNetExportYear", "Select a year:", 2011, 2021, 2011, sep = "")),
      box(title = "Net export (retweeted by - retweeting)", leafletOutput("netExportMapYear", height = 400), dataTableOutput("netExportTableYear"), status = "primary")
    ),
    
    # Globality
    tabItem(
      tabName = "globalityYear",
      box(sliderInput("yearGlobalityYear", "Select a year:", 2011, 2021, 2011, sep = "")),
      box(title = "Globality", leafletOutput("globalityMapYear", height = 400), dataTableOutput("globalityTableYear"), status = "primary")
    ),
    
    # Per country import and export
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
    
    # About
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
  # Define how the maps should look like, given country map and data
  
  output$importMapYear <- renderLeaflet({
    leaflet(countries) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE, minZoom=1, maxZoom=5)
      ) %>%
      addTiles()
  })
  
  output$exportMapYear <- renderLeaflet({
    leaflet(countries) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE, minZoom=1, maxZoom=5)
      ) %>%
      addTiles()
  })
  
  output$netExportMapYear <- renderLeaflet({
    leaflet(countries) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE, minZoom=1, maxZoom=5)
      ) %>%
      addTiles()
  })
  
  output$globalityMapYear <- renderLeaflet({
    leaflet(countries) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE, minZoom=1, maxZoom=5)
      ) %>%
      addTiles()
  })
  
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
  import_map_proxy <- leafletProxy("importMapYear", session)
  export_map_proxy <- leafletProxy("exportMapYear", session)
  net_export_map_proxy <- leafletProxy("netExportMapYear", session)
  globality_map_proxy <- leafletProxy("globalityMapYear", session)
  map_out_proxy <- leafletProxy("outgoingMapCountryYear", session)
  map_in_proxy <- leafletProxy("incomingMapCountryYear", session)
  
  # Watch tab
  observe({
    if (input$tabs == "countryYear" && input$countryCountryYear != "") {
      # Prepare incoming and outgoing data frames
      data_in <- out2in[[input$yearCountryYear - 2010]][name2code[input$countryCountryYear]]
      data_out <- t(as.data.frame(out2in[[input$yearCountryYear - 2010]][name2code[input$countryCountryYear],]))
      colnames(data_out) <- "Count"
      data_out <- as.data.frame(data_out)
      data_out$`Country (region)` <- rownames(data_in)
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
          data = countries, title = "Count", position = "bottomleft", pal = pal, values =  ~log10(data_out + 1), opacity = 0.7,
          labFormat = labelFormat(transform = function(x) round(10^x))
        )
      
      map_in_proxy %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = countries, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
          fillColor = ~ pal(log10(data_in + 1)),
          label = ~ paste0(`Country (region)`, ": ", formatC(data_in, big.mark = ","))
        ) %>%
        addLegend(
          data = countries, title = "Count", position = "bottomleft", pal = pal, values =  ~log10(data_in + 1), opacity = 0.7,
          labFormat = labelFormat(transform = function(x) round(10^x))
        )
    }
    else if (input$tabs == "importYear") {
      import <- colSums(out2in[[input$yearImportYear - 2010]]) - diag(as.matrix(out2in[[input$yearImportYear - 2010]]))
      import <- as.data.frame(import)
      colnames(import) <- "Count"
      import$`Country (region)` <- sapply(rownames(import), function(x) code2name[x])
      import <- import[order(import$Count, decreasing = TRUE),]
      import <- import[complete.cases(import),]
      rownames(import) <- NULL
      countries$import <- import$Count[match(countries$ISO_A2, sapply(import$`Country (region)`, function(x) name2code[x]))]
      
      output$importTableYear <- renderDataTable({
        datatable(import, options = list(pageLength = 20, order = list(1, 'desc')))
      })
      
      import_map_proxy %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = countries, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
          fillColor = ~ pal(log10(import + 1)),
          label = ~ paste0(`Country (region)`, ": ", format(import, big.mark = ",", scientific = FALSE))
        ) %>%
        addLegend(
          data = countries, title = "Count", position = "bottomleft", pal = pal, values =  ~log10(import + 1), opacity = 0.7,
          labFormat = labelFormat(transform = function(x) round(10^x))
        )
    }
    else if (input$tabs == "exportYear") {
      export <- rowSums(out2in[[input$yearExportYear - 2010]]) - diag(as.matrix(out2in[[input$yearExportYear - 2010]]))
      export <- as.data.frame(export)
      colnames(export) <- "Count"
      export$`Country (region)` <- sapply(rownames(export), function(x) code2name[x])
      export <- export[order(export$Count, decreasing = TRUE),]
      export <- export[complete.cases(export),]
      rownames(export) <- NULL
      countries$export <- export$Count[match(countries$ISO_A2, sapply(export$`Country (region)`, function(x) name2code[x]))]
      
      output$exportTableYear <- renderDataTable({
        datatable(export, options = list(pageLength = 20, order = list(1, 'desc')))
      })
      
      export_map_proxy %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = countries, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
          fillColor = ~ pal(log10(export + 1)),
          label = ~ paste0(`Country (region)`, ": ", format(export, big.mark = ",", scientific = FALSE))
        ) %>%
        addLegend(
          data = countries, title = "Count", position = "bottomleft", pal = pal, values =  ~log10(export + 1), opacity = 0.7,
          labFormat = labelFormat(transform = function(x) round(10^x))
        )
    }
    else if (input$tabs == "netExportYear") {
      export <- rowSums(out2in[[input$yearNetExportYear - 2010]]) - diag(as.matrix(out2in[[input$yearNetExportYear - 2010]]))
      import <- colSums(out2in[[input$yearNetExportYear - 2010]]) - diag(as.matrix(out2in[[input$yearNetExportYear - 2010]]))
      netExport <- export - import
      netExport <- as.data.frame(netExport)
      colnames(netExport) <- "Count"
      netExport$`Country (region)` <- sapply(rownames(netExport), function(x) code2name[x])
      netExport <- netExport[order(netExport$Count, decreasing = TRUE),]
      netExport <- netExport[complete.cases(netExport),]
      rownames(netExport) <- NULL
      countries$netExport <- netExport$Count[match(countries$ISO_A2, sapply(netExport$`Country (region)`, function(x) name2code[x]))]
      
      output$netExportTableYear <- renderDataTable({
        datatable(netExport, options = list(pageLength = 20, order = list(1, 'desc')))
      })
      
      net_export_map_proxy %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = countries, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
          fillColor = ~net_pal(net_log(netExport)),
          label = ~ paste0(`Country (region)`, ": ", format(netExport, big.mark = ",", scientific = FALSE))
        ) %>%
        addLegend(
          data = countries, title = "Count", position = "bottomleft", pal = net_pal, values = ~net_log(netExport), opacity = 0.7,
          labFormat = labelFormat(transform = function(x) round(net_exp(x)))
        )
    }
    else if (input$tabs == "globalityYear") {
      import <- colSums(out2in[[input$yearImportYear - 2010]]) - diag(as.matrix(out2in[[input$yearImportYear - 2010]]))
      total <- colSums(out2in[[input$yearImportYear - 2010]])
      globality <- import / total
      globality <- as.data.frame(globality)
      colnames(globality) <- "Count"
      globality$`Country (region)` <- sapply(rownames(globality), function(x) code2name[x])
      globality <- globality[order(globality$Count, decreasing = TRUE),]
      globality <- globality[complete.cases(globality),]
      rownames(globality) <- NULL
      countries$globality <- globality$Count[match(countries$ISO_A2, sapply(globality$`Country (region)`, function(x) name2code[x]))]
      
      output$globalityTableYear <- renderDataTable({
        datatable(globality, options = list(pageLength = 20, order = list(1, 'desc')))
      })
      
      globality_map_proxy %>% clearShapes() %>% clearControls() %>%
        addPolygons(
          data = countries, stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
          fillColor = ~ globality_pal(globality),
          label = ~ paste0(`Country (region)`, ": ", format(globality, big.mark = ",", scientific = FALSE))
        ) %>%
        addLegend(
          data = countries, title = "Count", position = "bottomleft", pal = globality_pal, values =  ~globality, opacity = 0.7
        )
    }
  })
}

shinyApp(ui, server)
