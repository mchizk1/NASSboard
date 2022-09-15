
#' GUI for Interactive Mapping of NASS Census Data
#'
#' Running this function with no arguments will launch a general user interface for
#' mapping NASS data at the county level.
#'
#' @examples
#' NASSboard()
#'
#' @export

NASSboard <- function(){
  crops <- names(NASS) %>%
    stringr::str_to_title()
  variables <- NASS$ALMONDS[[length(NASS$ALMONDS)]] %>%
    stringr::str_to_title()
  cols <- paletteer::paletteer_c("ggthemes::Classic Green", 30) %>%
    as.character()
  ui <- shiny::fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    shiny::titlePanel(
      shiny::h1("NASSboard", align = "center")
    ),
    shiny::fluidRow(
      shiny::column(3,
                    shiny::h3("Control Panel", align = "center"),
                    shiny::textInput("key", "API Key:"),
                    shiny::actionButton("send", "Submit Key"),
                    shiny::uiOutput("link"),
                    shiny::headerPanel(""),
                    shiny::selectInput("species", "Select Crop:",
                                       crops),
                    shiny::selectInput("year", "Select Year:",
                                       names(NASS$ALMONDS),
                                       selected = names(NASS$ALMONDS)[length(NASS$ALMONDS)]),
                    shiny::selectInput("variable", "Select Variable:",
                                       variables),
                    shiny::actionButton("submit", "Submit Query")),
      shiny::column(6,
                    leaflet::leafletOutput("map", height = 600) %>%
                      shinycssloaders::withSpinner()),
      shiny::column(3,
                    shiny::h3("Status Log", align = "center"),
                    shiny::uiOutput("status"),
                    shiny::verbatimTextOutput("debug"))
    )
  )
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
    shiny::withProgress(message = "Downloading US County Maps- ", value = 0, {
      incProgress(1/2, detail = "This may take a minute")
      counties <- geojsonio::geojson_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_050_00_5m.json", what = "sp")
      counties$longID <- geoid(counties$GEO_ID)
    })
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
      rnassqs::nassqs(params()) %>%
        dplyr::select(state_fips_code, county_code, Value) %>%
        dplyr::rename(STATE = state_fips_code, COUNTY = county_code) %>%
        dplyr::mutate(JOIN = paste0(STATE, "&", COUNTY)) %>%
        dplyr::select(JOIN, Value) %>%
        dplyr::right_join(data.frame(JOIN = paste0(counties$STATE,"&",counties$COUNTY),
                                     SORT = 1:nrow(counties))) %>%
        dplyr::arrange(SORT)
    })
    querymap <- shiny::reactiveVal(NULL)
    cols <- paletteer::paletteer_c("ggthemes::Classic Green", 30) %>%
      as.character()
    shiny::observeEvent(input$submit, {
      withProgress(message = "Processing Query:  ", value = 0, {
        incProgress(1/2, detail = "Downloading Data from NASS")
        pal = leaflet::colorNumeric(cols, domain = querydata()$Value)
        titlestr <- stringr::str_replace_all(input$variable, "-|,", "<br>")
        incProgress(2/2, detail = "Creating Map")
        leaflet::leafletProxy("map", data = counties) %>%
          leaflet::addTiles() %>%
          leaflet::addPolygons(
            fillColor = ~pal(querydata()$Value),
            weight = 0.01,
            opacity = 1,
            color = "black",
            dashArray = "3",
            fillOpacity = 0.7,
            layerId = querydata()$JOIN,
            label = ~format_label(counties$longID, querydata()$Value)
          ) %>%
          leaflet::addLegend("bottomright", pal = pal,
                             values = querydata()$Value,
                             title = titlestr, opacity = 1, layerId = "col_leg")
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
      paste0(connection_txt(), nass_txt(), key_txt()) %>%
        HTML()
    })
    output$map <- leaflet::renderLeaflet(
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::setView(lat = 39.8283, lng = -98.5795, zoom = 4)
    )
  }
  shiny::shinyApp(ui = ui, server = server)
}
