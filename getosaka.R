library(tidyverse)
library(lubridate)
library(httr)
library(readxl)

osaka_url <- "http://www.pref.osaka.lg.jp/attach/23711/00346644/youseisyajyouhou.xlsx"
file_latest <- "infections_osaka.csv"
file_record <- "infections_record_osaka.csv"

source("secret.R") #slack_webhookurl

GET(osaka_url, write_disk(tf <- tempfile(fileext = ".xlsx")))                 
infections <- read_excel(
		tf,
		skip = 1,
		col_types = c("text", "date", "text", "text", "text", "date", "text")
              ) %>%
              mutate(報道提供日 = as_date(報道提供日), 発症日 = as_date(発症日))

if (any(dir() %in% file_latest)){
  old_infections <- read_csv(file_latest, col_types = "cDcccDc")
}else{
  old_infections <- infections
}

diff <- infections %>% anti_join(old_infections, by = "番号") 

growth <- nrow(infections) - nrow(old_infections) 
check_health <- growth >= 0
if(check_health){
  write_csv(infections, file_latest, na = "")
  if(growth > 0){ 
    for(i in seq(to = nrow(diff))){
      text <- paste0("大阪府発表\n", "報道提供日：", diff[i,]$報道提供日, " 年代:", diff[i,]$年代, " 性別：", diff[i,]$性別, " 居住地：", diff[i,]$居住地)
      POST(url = slack_webhookurl, encode = "json", body = list(text = text))
    }
  }else{
    POST(url = slack_webhookurl, encode = "json", body = list(text = "Osaka: No new infections!"))
  }
}

write_csv(
    infections %>%
      mutate(
        timestamp = paste(now(), "JST"),
        check_health = check_health
      ),
    file_record, na = "", append = TRUE
)
