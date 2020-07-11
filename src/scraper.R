get_latest_kyoto <- function(){ 
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

  return(infections)
}

get_latest_osaka <- function(){
  osaka_url <- "https://raw.githubusercontent.com/codeforosaka/covid19/development/data/data.json"
  
  infections <- GET(osaka_url) %>%
    content %>%
    parse_json %>%
    .[["patients"]] %>%
    .[["data"]] %>%
    bind_rows %>%
    mutate(No = as.character(No), date = ymd(date))

  return(infections)
}

get_latest_okayama <- function(){
  url1 <- "https://www.pref.okayama.jp/page/667843.html"
  
  urls <- c(url1)
  
  colnames_def <- c("","月日", "年代", "性別", "居住地", "備考")
  colnames_mod <- c("index","月日", "年代", "性別", "居住地", "備考")
  
  get_infections <- function(url){
    read_html(url) %>%
      html_table %>%
      keep(~all(names(.)==colnames_def)) %>%
      map(magrittr::set_names, colnames_mod) %>%
      map(mutate, 年代 = as.character(年代)) %>%
      bind_rows
  }
  
  infections <- map_df(urls, get_infections) %>%
    mutate_all(zentohan) %>%
    mutate(
      year = 2020,
      md = 月日 %>% str_replace("月", "-") %>% str_replace("日", ""),
      date_public = paste0(year, "-", md) %>% ymd
    ) %>%
    select(-year, -md)
}
