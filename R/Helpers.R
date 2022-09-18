
format_key <- function(key_input){
  key_input <- stringr::str_remove_all(key_input, "-") %>%
    toupper() %>%
    stringr::str_trim() %>%
    stringr::str_split("") %>%
    unlist()
  if(length(key_input) != 32){
    key_input <- paste0("<br>", span("Expecting 32 character API key", style = "color:red"))
  } else {
    key_input <- c(key_input[1:8],"-",
                   key_input[9:12],"-",
                   key_input[13:16],"-",
                   key_input[17:20],"-",
                   key_input[21:32]) %>%
      stringr::str_flatten()
    rnassqs::nassqs_auth(key_input)
    try(nass_connect <- rnassqs::nassqs_param_values("group_desc"), silent = T)
    if(exists("nass_connect")){
      key_input <- paste0("<br> Valid Key: ",
                          span("PASSED", style = "color:green"))
    } else {
      key_input <- paste0("<br> Valid Key: ",
                          span("FAILED", style = "color:red"))
    }
  }
  return(key_input)
}

geoid <- function(GEO_ID){
  purrr::map_df(GEO_ID, ~ dplyr::filter(fips_key, GEO_ID == .)[,4:6])
}

format_label <- function(name, data, varID){
  number <- formatC(data, format = "f", big.mark = ",", digits = 0)
  units <- stringr::str_extract(varID, "(?<=[,-][:space:])([^,-]*)$") %>%
    stringr::str_remove("(Measured[:space:]In[:space:])")
  paste0("<strong>",name, "</strong><br>", number, "<br>", units) %>%
    lapply(HTML)
}

summary_report <- function(data, geo_key, varname, unit, species){
  unit <- tolower(unit)
  species <- tolower(species)
  varname <- tolower(varname)
  national <- sum(data, na.rm = T)
  var <- stringr::str_extract(varname, "(?<=[-][:space:])([^,]*)")
  states <- cbind(data, geo_key) %>%
    dplyr::group_by(state_name) %>%
    dplyr::summarise(data = sum(data, na.rm = T)) %>%
    dplyr::arrange(desc(data))
  if(stringr::str_detect(varname, "Measured[:space:]In[:space:]\\$$")){
    nat_f <- paste0("$", formatC(national, format = "f", big.mark = ",", digits = 0))
    st_f <- paste0("$", formatC(states$data[1], format = "f", big.mark = ",", digits = 0))
  } else {
    nat_f <- paste0(formatC(national, format = "f", big.mark = ",", digits = 0)," ",unit)
    st_f <- paste0(formatC(states$data[1], format = "f", big.mark = ",", digits = 0)," ",unit)
  }
  report <- paste0("<strong>National Total: </strong>",
                   span(nat_f, style = "color:lightgreen"),
                   "<br><strong>State Report: </strong>",
                   span(states$state_name[1], style = "color:lightgreen"),
                   " contributes the most ", var, " with ",
                   span(st_f, style = "color:lightgreen"),
                   " of ", species, ". This accounts for ",
                   span(round((states$data[1]/national)*100, 2), "%", style = "color:lightgreen"),
                   " of the national total.") %>%
    htmltools::HTML()
  return(report)
}

withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "message")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}
