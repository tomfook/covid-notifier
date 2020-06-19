library(tidyverse)
library(lubridate)
library(magrittr)
library(rvest)
library(httr)

file_latest <- "infections_kyoto.csv"
file_record <- "infections_record_kyoto.csv"
source("secret.R") #slack_webhookurl


url1 <- "http://www.pref.kyoto.jp/kentai/corona/hassei1-50.html"

urls <- c(url1)


colnames_def <- c("","発表日","年代", "性別", "居住地等", "資料")
colnames_mod <- c("index","発表日","年代", "性別", "居住地等", "資料")

get_infections <- function(url){
  read_html(url) %>%
    html_table %>%
    keep(~all(names(.)==colnames_def)) %>%
    map(magrittr::set_names, colnames_mod) %>%
    map(mutate, 年代 = as.character(年代)) %>%
    bind_rows
}

infections <- map_df(urls, get_infections) %>%
  mutate(
    gengo = str_extract(発表日, "^[^\\d]*"),
    wareki = str_extract(発表日, "^.*年") %>% parse_number,
    year = wareki + if_else(gengo == "令和", 2018, NA_real_),
    md = str_extract(発表日, "[^年]*$") %>% str_replace("月", "-") %>% str_replace("日", ""),
    date_public = paste0(year, "-", md) %>% ymd
  ) %>% 
  select(-gengo, -wareki, -year, -md) 


if (any(dir() %in% file_latest)){
  old_infections <- read_csv(file_latest, col_types = "ccccccD")
}else{
  old_infections <- infections
}

diff <- infections %>% anti_join(old_infections, by = "index")

#update <- infections %>% setdiff(old_infections) %>% anti_join(diff, by = "index")
#update_old <- semi_join(old_infections, update, by = "index")


growth <- nrow(infections) - nrow(old_infections) 

check_health <- growth >= 0
if(check_health){
  write_csv(infections, file_latest, na = "")
  if(growth > 0){ 
    for(i in seq(to = nrow(diff))){
      text <- paste0("京都府発表\n", "発表日：", diff[i,]$発表日, " 年代:", diff[i,]$年代, " 性別：", diff[i,]$性別, " 居住地等：", diff[i,]$居住地等)
      POST(url = slack_webhookurl, encode = "json", body = list(text = text))
    }
  }else{
    POST(url = slack_webhookurl, encode = "json", body = list(text = "Kyoto: No new infections!"))
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

