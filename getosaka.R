library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)


pref <- "osaka"

file_latest <- paste0("infections_", pref, ".csv")
file_record <- paste0("infections_record_", pref, ".csv") 
source("secret.R") #slack_webhookurl

osaka_url <- "https://raw.githubusercontent.com/codeforosaka/covid19/development/data/data.json"
url_guide <- "http://www.pref.osaka.lg.jp/hodo/index.php?site=fumin"

infections <- GET(osaka_url) %>%
  content %>%
  parse_json %>%
  .[["patients"]] %>%
  .[["data"]] %>%
  bind_rows %>%
  mutate(No = as.character(No), date = ymd(date))

if (any(dir() %in% file_latest)){
  old_infections <- read_csv(file_latest, col_types = "cccccccD")
}else{
  old_infections <- infections
}

diff <- infections %>% anti_join(old_infections, by = "No") 

growth <- nrow(infections) - nrow(old_infections) 
check_health <- growth >= 0
if(check_health){
  write_csv(infections, file_latest, na = "")
  if(growth > 0){ 
    for(i in seq(to = nrow(diff))){
      text <- paste0(
		     "大阪府発表\n",
		     "報道日：", diff[i,]$date, " 年代:", diff[i,]$年代, " 性別：", diff[i,]$性別, " 居住地：", diff[i,]$居住地, "\n",
		     url_guide
      )
      POST(url = slack_webhookurl, encode = "json", body = list(text = text))
    }
  }else{
    POST(url = slack_webhookurl, encode = "json", body = list(text = "Osaka: No new infections!"))
  }
}else{
  POST(url = slack_webhookurl, encode = "json", body = list(text = "ERROR: Something happened in getosaka.R"))
}

write_csv(
    infections %>%
      mutate(
        timestamp = paste(now(), "JST"),
        check_health = check_health
      ),
    file_record, na = "", append = TRUE
)
