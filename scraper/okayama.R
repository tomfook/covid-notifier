# infection data type
# list of (df, str)
# df: ~index, ~発表日, ~年代, ~性別, ~居住地

scraper <- function(){
  url1 <- "https://www.pref.okayama.jp/page/667843.html"
  urls <- c(url1)
  
  colnames_def <- c("","月日", "年代", "性別", "居住地", "備考")
  colnames_mod <- c("index","月日", "年代", "性別", "居住地", "備考")
  
  get_infection <- function(url){
    read_html(url) %>%
      html_table %>%
      keep(~identical(names(.), colnames_def)) %>%
      map(magrittr::set_names, colnames_mod) %>%
      map(mutate, 年代 = as.character(年代)) %>%
      bind_rows
  }
  
  infection <- map_df(urls, get_infection) %>%
    mutate_all(zentohan) %>%
    mutate(
      year = 2020,
      md = 月日 %>% str_replace("月", "-") %>% str_replace("日", ""),
      date_public = paste0(year, "-", md)
    ) %>%
    select(-year, -md) %>%
    rename(発表日 = 月日)

  return(infection)
}
