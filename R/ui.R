ui <- function(){
  crops <- names(NASS) %>%
    stringr::str_to_title()
  variables <- NASS$ALMONDS[[length(NASS$ALMONDS)]] %>%
    stringr::str_to_title()
  shiny::fluidPage(
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
                    shiny::h3(htmltools::HTML("<br>")),
                    leaflet::leafletOutput("map", height = 500) %>%
                      shinycssloaders::withSpinner()),
      shiny::column(3,
                    shiny::h3("Status Log", align = "center"),
                    shiny::uiOutput("status"),
                    shiny::h3(htmltools::HTML("<br>Summary"), align = "center"),
                    shiny::uiOutput("report"))
    )
  )
}
