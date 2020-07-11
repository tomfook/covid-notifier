pref <- "kyoto"

file_latest <- paste0("infections_", pref, ".csv")
file_latest_path <- paste0("data/", file_latest)
file_record <- paste0("infections_record_", pref, ".csv")
file_record_path <- paste0("data/", file_record)


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


if (any(dir("data") %in% file_latest)){
  old_infections <- read_csv(file_latest_path, col_types = "ccccccD")
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
    post_infection(diff, pref, slack_webhookurl2, TEST)
  }else{
    if(TEST){
      print("TEST: No infection in Kyoto")
    }
  }
}else{
  if(TEST){
    print("TEST: ERROR in getkyoto.R")
  }else{
    POST(url = slack_webhookurl, encode = "json", body = list(text = "ERROR: Something happened in getkyoto.R"))
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

