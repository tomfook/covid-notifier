library(tidyverse)
library(lubridate)
library(magrittr)
library(rvest)
library(httr) 
library(jsonlite) 

TEST <- FALSE

source("secret.R") #slack_webhookurls

source("src/functions.R")
source("src/scraper.R")

infections_kyoto <- get_latest_kyoto() 
notify_infection(infections_kyoto, "kyoto", slack_webhookurl, TEST)
notify_infection(infections_kyoto, "kyoto", slack_webhookurl2, TEST)
update_record(infections_kyoto, "kyoto")

infections_osaka <- get_latest_osaka()
notify_infection(infections_osaka, "osaka", slack_webhookurl, TEST)
update_record(infections_osaka, "osaka")

infections_okayama <- get_latest_okayama()
notify_infection(infections_okayama, "okayama", slack_webhookurl, TEST)
update_record(infections_okayama, "okayama") 
