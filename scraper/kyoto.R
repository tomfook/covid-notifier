# infection data type
# list of (df, str)
# df: ~index, ~発表日, ~年代, ~性別, ~居住地

scraper <- function(){
  kyoto_url <- "https://raw.githubusercontent.com/stop-covid19-kyoto/covid19-kyoto/master/data/patients.json"
  
  infection <- GET(kyoto_url) %>%
    content %>%
    fromJSON %>%
    .[["data"]] %>%
    rename(発表日 = date) %>%
    mutate(
      年代 = substr(年代と性別, 1, nchar(年代と性別) - 2),
      性別 = substr(年代と性別, nchar(年代と性別) - 1, nchar(年代と性別))
    ) %>%
    arrange(リリース日, 居住地, 年代, 性別) %>%
    mutate(index = row_number() %>% as.character())

  return(infection) 
}     

