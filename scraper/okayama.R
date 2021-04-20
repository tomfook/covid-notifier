# infection data type
# list of (df, str)
# df: ~index, ~発表日, ~年代, ~性別, ~居住地

scraper <- function(){
  okayama_url <- "https://raw.githubusercontent.com/stopcovid19-okayama/covid19/master/data/patients.json"
  
  infection <- GET(okayama_url) %>%
    content %>%
    parse_json %>%
    .[["data"]] %>% 
    bind_rows %>%
    rename(発表日 = date) %>%
    arrange(リリース日, 居住地, 年代, 性別) %>%
    mutate(index = row_number() %>% as.character())

  return(infection) 
}     

