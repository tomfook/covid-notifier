# infection data type
# list of (df, str)
# df: ~index, ~発表日, ~年代, ~性別, ~居住地

scraper <- function(){ 
  url1 <- "http://www.pref.kyoto.jp/kentai/corona/hassei1-50.html" 
  urls <- c(url1) 
  
  colnames_def <- c("","発表日","年代", "性別", "居住地等", "資料")
  colnames_mod <- c("index","発表日","年代", "性別", "居住地等", "資料")
  
  get_infection <- function(url){
    read_html(url) %>%
      html_table %>%
      keep(~identical(names(.), colnames_def)) %>%
      map(magrittr::set_names, colnames_mod) %>%
      map(mutate, 年代 = as.character(年代)) %>%
      bind_rows
  }
  
  infection <- map_df(urls, get_infection) %>%
    mutate(
      gengo = str_extract(発表日, "^[^\\d]*"),
      wareki = str_extract(発表日, "^.*年") %>% parse_number,
      year = wareki + if_else(gengo == "令和", 2018, NA_real_),
      md = str_extract(発表日, "[^年]*$") %>% str_replace("月", "-") %>% str_replace("日", ""),
      date_public = paste0(year, "-", md)
    ) %>% 
    select(-gengo, -wareki, -year, -md) %>%
    rename(居住地 = 居住地等)

  return(infection)
}
