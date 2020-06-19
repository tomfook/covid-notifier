library(tidyverse)
library(lubridate)
library(magrittr)
library(rvest)
library(httr)

source("secret.R") #slack_webhookurl
file_latest <- "infections_kyoto.csv"
file_record <- "infections_record_kyoto.csv"


latest_url <- "http://www.pref.kyoto.jp/kentai/news/novelcoronavirus.html"
url1 <- "http://www.pref.kyoto.jp/kentai/corona/hassei1-50.html"
url2 <- "http://www.pref.kyoto.jp/kentai/corona/hassei51-100.html"
url3 <- "http://www.pref.kyoto.jp/kentai/corona/hassei206-309.html"

urls <- c(latest_url, url1, url2, url3)


colnames_def <- c("","発表日","年代", "性別", "居住地等", "資料", "備考", "状況")
colnames_mod <- c("index","発表日","年代", "性別", "居住地等", "資料", "備考", "状況")

get_infections <- function(url){
  read_html(url) %>%
    html_table %>%
    keep(~all(names(.)==colnames_def)) %>%
    .[[1]] %>%
    magrittr::set_names(colnames_mod) %>%
    mutate(年代 = as.character(年代)) %>%
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



old_infections <- read_csv(file_latest)
diff <- infections %>% anti_join(old_infections, by = "index")

update <- infections %>% setdiff(old_infections) %>% anti_join(diff, by = "index")
update_old <- semi_join(old_infections, update, by = "index")


growth <- nrow(infections) - nrow(old_infections) 

check_health <- growth >= 0
if(check_health){
  write_csv(infections, file_latest, na = "")
  if(growth > 0){
    #POST diff to slack
    ## POST(url = slack_webhookurl, body = '{"text":"Hello, World!"}' )
    #POST update to slack
  }else{
    POST(url = slack_webhookurl, encode = "json", body = list(text = "Today we don't have any infections!!"))
  }
} else {
  POST(url = slack_webhookurl, encode = "json", body = list(text = "ERROR: something happened. Check record 'infections_record.csv'"))
} 


write_csv(
    infections %>%
      mutate(
        timestamp = paste(now(), "JST"),
        check_health = check_health
      ),
    file_record, na = "", append = TRUE
)

