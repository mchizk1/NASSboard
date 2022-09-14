
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
      key_input <- paste0("<br>API key: ", span(key_input, style = "color:green"),
                          "<br> Valid key: ",
                          span("PASSED", style = "color:green"))
    } else {
      key_input <- paste0("<br>API key: ", span(key_input, style = "color:red"),
                          "<br> Valid key: ",
                          span("FAILED", style = "color:red"))
    }
  }
  return(key_input)
}
