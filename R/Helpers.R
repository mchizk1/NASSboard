
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
      key_input <- paste0("<br> Valid key: ",
                          span("PASSED", style = "color:green"))
    } else {
      key_input <- paste0("<br> Valid key: ",
                          span("FAILED", style = "color:red"))
    }
  }
  return(key_input)
}

geoid <- function(GEO_ID){
  purrr::map_chr(GEO_ID, ~ paste0(dplyr::filter(fips_key, GEO_ID == .)[,c(5,6)], collapse = "-<br>"))
}

format_label <- function(name, data){
  number <- formatC(data, format = "f", big.mark = ",", digits = 0)
  # units <- stringr::str_extract(varID, "(?<=[,-][:space:])([^,-]*)$") %>%
  #   stringr::str_remove("(Measured[:space:]In[:space:])")
  paste0(name, "<br>", number) %>%
    lapply(HTML)
}
