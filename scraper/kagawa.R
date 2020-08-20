# infection data type
# list of (df, str)
# df: ~index, ~発表日, ~年代, ~性別, ~居住地

scraper <- function(){
  url1 <- "https://www.pref.kagawa.lg.jp/content/etc/subsite/kansenshoujouhou/kansen/se9si9200517102553.shtml"
  urls <- c(url1)
  
  colnames_def <- c("No", "確認日", "年齢", "性別", "住所地")
  colnames_mod <- c("index","発表日", "年代", "性別", "居住地")
  
  get_infection <- function(url){
    read_html(url) %>%
      html_table %>%
      keep(~identical(names(.), colnames_def)) %>%
      map(magrittr::set_names, colnames_mod) %>%
      map(mutate, 年代 = as.character(年代)) %>%
      map(mutate, index = as.character(index)) %>%
      bind_rows
  }
  
  infection <- map_df(urls, get_infection) %>%
    mutate(
      year = 2020,
      md = 発表日 %>% str_replace("月", "-") %>% str_replace("日.*", ""),
      date_public = paste0(year, "-", md)
    ) %>%
    select(-year, -md)

  return(infection)
}
