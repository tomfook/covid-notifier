# infection data type
# list of (df, str)
# df: ~index, ~発表日, ~年代, ~性別, ~居住地

scraper <- function(){
  osaka_url <- "https://raw.githubusercontent.com/codeforosaka/covid19/master/data/data.json"
  
  infection <- GET(osaka_url) %>%
    content %>%
    parse_json %>%
    .[["patients"]] %>%
    .[["data"]] %>%
    bind_rows %>%
    mutate(No = as.character(No)) %>%
    rename(発表日 = date, index = No)

  return(infection)
}
