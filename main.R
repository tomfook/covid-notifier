library(tidyverse)
library(lubridate)
library(magrittr)
library(rvest)
library(httr) 
library(jsonlite) 

TEST <- TRUE

source("secret.R") #slack_webhookurls

source("src/functions.R")
source("src/scraper.R")

infections_kyoto <- get_latest_kyoto() 
notify_infection(infections_kyoto, "kyoto", slack_webhookurl)
notify_infection(infections_kyoto, "kyoto", slack_webhookurl2)
update_record(infections_kyoto, "kyoto")

infections_osaka <- get_latest_osaka()
notify_infection(infections_osaka, "osaka", slack_webhookurl)
update_record(infections_osaka, "osaka")

infections_okayama <- get_latest_okayama()
notify_infection(infections_okayama, "okayama", slack_webhookurl)
update_record(infections_okayama, "okayama") 
