pref <- "okayama"

file_latest <- paste0("infections_", pref, ".csv")
file_latest_path <- paste0("data/", file_latest)
file_record <- paste0("infections_record_", pref, ".csv")
file_record_path <- paste0("data/", file_record)

infections <- get_latest_okayama()

if (any(dir("data") %in% file_latest)){
  old_infections <- read_csv(file_latest_path, col_types = cols(.default = "c"))
}else{
  old_infections <- infections
}

diff <- infections %>% anti_join(old_infections, by = "index")


growth <- nrow(infections) - nrow(old_infections) 

check_health <- growth >= 0
if(check_health){
  write_csv(infections, file_latest_path, na = "")
  if(growth > 0){ 
    post_infection(diff, pref, slack_webhookurl, TEST) 
  }else{
    if(TEST){
      print(paste0("TEST: No infection in ", pref))
    }
  }
}else{
  if(TEST){
    print(paste0("TEST: alert in ", pref))
  }else{
    POST(url = slack_webhookurl, encode = "json", body = list(text = paste0("ALERT: Something happened in ", pref)))
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

