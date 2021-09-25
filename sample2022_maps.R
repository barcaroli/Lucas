library(shiny)
library(shinythemes)
library(sf)
library(mapview)
library(leaflet)
library(leafpop)

load("LUCAS_sample.RData")
samp <- samp[order(paste0(samp$NUTS0,samp$NUTS2)),]
samp_sf <- st_as_sf(samp, coords = c("X_LAEA", "Y_LAEA"),
                    crs=" +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")
samp_sf$LC <- as.factor(samp_sf$LC)
levels(samp_sf$LC) <- LETTERS[1:8]


ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("LUCAS 2022 Sample"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "background", label = strong("Maps background"),
                                choices = c("OpenStreetMap",
                                            "Esri.WorldImagery",
                                            "OpenTopoMap"),
                                selected = "OpenStreetMap"),
                    # Select countries and regions
                    # selectInput(inputId = "Country", label = strong("Country (NUTS0)"),
                    #             choices = unique(samp_sf$NUTS0),
                    #             selected = "AT"),
                    selectInput(inputId = "Region", label = strong("Region (NUTS2)"),
                                choices = unique(samp_sf$NUTS2),
                                selected = "AT11"),
                    selectInput(inputId = "ObsType", label = strong("Observation type (Field/PI)"),
                                choices = c("All values"="all",
                                            "Field"="FI",
                                            "Photo-Interpreted"="PI"),
                                selected = ""),
                    selectInput(inputId = "var", label = strong("Variable of interest"),
                                choices = c("Land cover"="LC",
                                            "Land use"="LU"),
                                selected = "LC"),
                    # Only show this panel if the variable is Land Cover
                    conditionalPanel(
                      condition = "input.var == 'LC'",
                      selectInput(inputId = "value_LC", label = "Select values of Land Cover:",
                                  choices = c("All values"="all",
                                              "Artificial"="A",
                                              "Cropland"="B",
                                              "Woodland"="C",
                                              "Shrubland"="D",
                                              "Grassland"="E",
                                              "Bareland"="F",
                                              "Water"="G",
                                              "Wetland"="H"),
                                  selected = "")
                    ),
                    # Only show this panel if the variable is Land Use
                    conditionalPanel(
                      condition = "input.var == 'LU'",
                      selectInput(inputId = "value_LU", label = "Select values of Land Use:",
                                  choices = c("All values"="all",
                                              "Primary Sector"="U1",
                                              "Secondary Sector"="U2",
                                              "Tertiary Sector"="U3",
                                              "Abandoned or unused areas"="U4"),
                                  selected = "")
                    ),
                    actionButton("do", "Click button"),
                    width = 3),
                # Output
                mainPanel(leafletOutput('map', width = "100%", height = 700))
            )
      )


server <- function(input, output, session) {
  observeEvent(input$do, {
    # Subset data
    selected_sample <- reactive({
      # req(input$Country)
      # validate(need(!is.na(input$Country) & (input$Country %in% levels(as.factor(samp_sf$NUTS0))), "Error: Please provide a valid country code"))
      # req(input$Region)
      validate(need( (input$Region %in% levels(as.factor(samp_sf$NUTS2))), "Error: Please provide  valid region code"))
      req(input$ObsType)
      validate(need(!is.na(input$ObsType), "Error: Please provide  valid observation type code"))
      req(input$var)
      validate(need(!is.na(input$var), "Error: Please provide  valid variable name"))
      req(input$value_LC)
      validate(need(!is.na(input$value_LC), "Error: Please provide  valid LC value"))
      req(input$value_LU)
      validate(need(!is.na(input$value_LU), "Error: Please provide  valid LU value"))
      samp <- samp_sf
      if (input$ObsType == "all" & input$var == "LC" & input$value_LC == "all")
          samp[samp$NUTS2 == input$Region,]
      else
      if (input$ObsType == "all" & input$var == "LU" & input$value_LU == "all")
          samp[samp$NUTS2 == input$Region,]
      else
      if (input$ObsType != "all" & input$var == "LC" & input$value_LC == "all")
          samp[samp$NUTS2 == input$Region  & samp$PI == input$ObsType,]
      else
      if (input$ObsType != "all" & input$var == "LU" & input$value_LU == "all")
          samp[samp$NUTS2 == input$Region  & samp$PI == input$ObsType,]
      else
      if (input$ObsType == "all" & input$value_LC != "all" & input$var == "LC")
          samp[samp$NUTS2 == input$Region & samp$LC == input$value_LC,]
      else
      if (input$ObsType == "all" & input$value_LU != "all" & input$var == "LU")
          samp[samp$NUTS2 == input$Region & samp$LU == input$value_LU,]
      else
      if (input$ObsType != "all" & input$value_LC != "all" & input$var == "LC")
          samp[samp$NUTS2 == input$Region & samp$LC == input$value_LC & samp$PI == input$ObsType,]
      else
      if (input$ObsType != "all" & input$value_LU != "all" & input$var == "LU")
          samp[samp$NUTS2 == input$Region & samp$LU == input$value_LU & samp$PI == input$ObsType,]
      
      
    })
    # Pull the map
    output$map <- renderLeaflet({
      if (input$background == "OpenStreetMap") 
        if (input$var == "LC")
          mapView(selected_sample()["LC"], map.types = c("OpenStreetMap"))@map
        else
          mapView(selected_sample()["LU"], map.types = c("OpenStreetMap"))@map
      else
        if (input$background == "Esri.WorldImagery") 
          if (input$var == "LC")
            mapView(selected_sample()["LC"], map.types = c("Esri.WorldImagery"))@map
          else
            mapView(selected_sample()["LU"], map.types = c("Esri.WorldImagery"))@map
      else
        if (input$background == "OpenTopoMap") 
          if (input$var == "LC")
            mapView(selected_sample()["LC"], map.types = c("OpenTopoMap"))@map
          else
            mapView(selected_sample()["LU"], map.types = c("OpenTopoMap"))@map
    })
    # output$desc <- renderText({
    #   paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
    # })  
  })
}



# Create Shiny object
shinyApp(ui, server)

