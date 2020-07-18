library(tidyverse)
library(lubridate)
library(magrittr)
library(rvest)
library(httr) 
library(jsonlite) 

TEST <- FALSE

source("secret.R") #slack_webhookurls

source("function.R")
source("src/scraper.R")

infection_kyoto <- get_latest_kyoto() 
notify_infection(infection_kyoto, slack_webhookurl)
notify_infection(infection_kyoto, slack_webhookurl2)
update_record(infection_kyoto)

infection_osaka <- get_latest_osaka()
notify_infection(infection_osaka, slack_webhookurl)
update_record(infection_osaka)

infection_okayama <- get_latest_okayama()
notify_infection(infection_okayama, slack_webhookurl)
update_record(infection_okayama)
