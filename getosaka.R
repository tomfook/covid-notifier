pref <- "osaka"

file_latest <- paste0("infections_", pref, ".csv")
file_latest_path <- paste0("data/", file_latest)
file_record <- paste0("infections_record_", pref, ".csv")
file_record_path <- paste0("data/", file_record)

osaka_url <- "https://raw.githubusercontent.com/codeforosaka/covid19/development/data/data.json"

infections <- GET(osaka_url) %>%
  content %>%
  parse_json %>%
  .[["patients"]] %>%
  .[["data"]] %>%
  bind_rows %>%
  mutate(No = as.character(No), date = ymd(date))

if (any(dir("data") %in% file_latest)){
  old_infections <- read_csv(file_latest_path, col_types = "cccccccD")
}else{
  old_infections <- infections
}

diff <- infections %>% anti_join(old_infections, by = "No") 

growth <- nrow(infections) - nrow(old_infections) 
check_health <- growth >= 0
if(check_health){
  write_csv(infections, file_latest_path, na = "")
  if(growth > 0){ 
    post_infection(diff, pref, slack_webhookurl, TEST) 
  }else{
    if(TEST){
      print("TEST: No infection in Osaka")
    }
  }
}else{
  if(TEST){
    print("TEST: error in getosaka.R")
  }else{
    POST(url = slack_webhookurl, encode = "json", body = list(text = "ERROR: Something happened in getosaka.R"))
  }
}

if(!TEST){
  write_csv(
      infections %>%
        mutate(
          timestamp = paste(now(), "JST"),
          check_health = check_health
        ),
      file_record_path, na = "", append = TRUE
  )
}
