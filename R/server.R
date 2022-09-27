cols <- paletteer::paletteer_c("ggthemes::Classic Green", 30) %>%
  as.character()
server <- function(input, output, session){
  # Defining reactive values
  is_connected <- reactiveVal(curl::has_internet())
  observeEvent(list(input$send, input$submit), {
    is_connected(curl::has_internet())
  })
  connection_txt <- reactive({
    if(is_connected()){
      paste0("Internet Connection: ", span("PASSED", style = "color:green"))
    } else {
      paste0("Internet Connection: ", span("FAILED", style = "color:red"))
    }
  })
  nass_txt <- reactive({
    if(is_connected() & input$key == ""){
      paste0("<br>", span("Enter API key to access NASS data", style = "color:yellow"))
    }
  })
  key_txt <- reactiveVal("")
  observeEvent(input$send, {
    key_txt(format_key(input$key))
  })
  err_txt <- reactiveVal("")
  geo_key <- geoid(counties$GEO_ID)
  counties$longID <- paste0(geo_key$county, "-<br>", geo_key$state_name)
  params <- shiny::reactive({
    list(
      agg_level_desc = "COUNTY",
      source_desc = "CENSUS",
      domain_desc = "TOTAL",
      commodity_desc = toupper(input$species),
      year = as.numeric(input$year),
      short_desc = toupper(input$variable)
    )
  })
  querydata <- eventReactive(input$submit, {
    try(
      rnassqs::nassqs(params()) %>%
        dplyr::select(state_fips_code, county_code, Value, unit_desc) %>%
        dplyr::rename(STATE = state_fips_code, COUNTY = county_code) %>%
        dplyr::mutate(JOIN = paste0(STATE, "&", COUNTY)) %>%
        dplyr::select(JOIN, Value, unit_desc) %>%
        dplyr::right_join(data.frame(JOIN = paste0(counties$STATE,"&",counties$COUNTY),
                                     SORT = 1:nrow(counties)), by = "JOIN") %>%
        dplyr::arrange(SORT) %>%
        dplyr::mutate(Adjusted = (Value/counties$CENSUSAREA)*100)
    )
  })
  querymap <- shiny::reactiveVal(NULL)
  cols <- paletteer::paletteer_c("ggthemes::Classic Green", 30) %>%
    as.character()
  shiny::observeEvent(input$submit, {
    withProgress(message = "Processing Query:  ", value = 0, {
      incProgress(1/2, detail = "Downloading Data from NASS")
      if(is.data.frame(querydata())){
        if(sum(querydata()$Value, na.rm = T) > 0){
          pal = leaflet::colorNumeric(cols, domain = querydata()$Adjusted)
          titlestr <- querydata()$unit_desc %>%
            unique() %>%
            na.omit() %>%
            paste0(" Per 100 Square Miles")
          incProgress(2/2, detail = "Creating Map")
          leaflet::leafletProxy("map", data = counties) %>%
            leaflet::addTiles() %>%
            leaflet::addPolygons(
              fillColor = ~pal(querydata()$Adjusted),
              weight = 0.1,
              opacity = 1,
              color = "black",
              dashArray = "3",
              fillOpacity = 0.7,
              layerId = querydata()$JOIN,
              label = ~format_label(counties$longID, querydata()$Value, querydata()$unit_desc),
              labelOptions = leaflet::labelOptions(textsize = "15px",
                                                   offset = c(25,0),
                                                   direction = "right"),
              highlightOptions = leaflet::highlightOptions(weight = 3,
                                                           color = "#666",
                                                           dashArray = "",
                                                           fillOpacity = 0.7,
                                                           bringToFront = TRUE)) %>%
            leaflet::addLegend("bottomright", pal = pal,
                               values = querydata()$Adjusted,
                               title = titlestr, opacity = 1, layerId = "col_leg")
          output$report <- renderUI({
            summary_report(querydata()$Value, geo_key, input$variable,
                           na.omit(unique(querydata()$unit_desc)), input$species)
          })
          err_txt(paste0("<br>Valid Dataset: ", span("PASSED", style = "color:green")))
        } else {
          err_txt(paste0("<br>Valid Dataset: ", span("FAILED - Requested data unavailable", style = "color:red")))
        }
      } else {
        err_txt(paste0("<br>Valid Dataset: ", span("FAILED - HTTP 400 (Bad request)", style = "color:red")))
      }
    })
  })

  # Updating input parameters
  shiny::observeEvent(input$species, {
    shiny::updateSelectInput(session, "year",
                             choices = names(NASS[[toupper(input$species)]]),
                             selected = names(NASS[[toupper(input$species)]])[length(names(NASS[[toupper(input$species)]]))])
    shiny::updateSelectInput(session, "variable",
                             choices = stringr::str_to_title(NASS[[toupper(input$species)]][[input$year]]))
  })
  # Rendering
  output$link <- shiny::renderUI({
    paste0("Don't have a key yet? <br>",
           "Request one ",
           a("here", href = "https://quickstats.nass.usda.gov/api"), "!") %>%
      HTML()
  })
  output$status = shiny::renderUI({
    paste0(connection_txt(), nass_txt(), key_txt(), err_txt()) %>%
      HTML()
  })
  output$map <- leaflet::renderLeaflet(
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::setView(lat = 39.8283, lng = -98.5795, zoom = 4)
  )
  out_data <- reactive({
    data.frame(Commodity = input$species,
               Variable = input$variable,
               Year = input$year,
               Geo_ID = counties$GEO_ID,
               State_abb = geo_key$state,
               State_name = geo_key$state_name,
               County = geo_key$county,
               Units = querydata()$unit_desc,
               DataValue = querydata()$Value)
  })
  output$downloadData <- downloadHandler(
      filename = function() {
        paste('NASSdata-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(out_data(), con, row.names = F)
      }
    )
}
