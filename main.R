library(tidyverse)
library(lubridate)
library(magrittr)
library(rvest)
library(httr) 
library(jsonlite) 

source("param.R")
source("secret.R") #slack_webhookurls

source("function.R")

infection_kyoto <- get_infections("kyoto")
notify_infection(infection_kyoto, post_slack_message(slack_webhookurl), target_name = "slack1")
notify_infection(infection_kyoto, post_slack_message(slack_webhookurl2), target_name = "slack2")

infection_osaka <- get_infections("osaka")
#notify_infection(infection_osaka, post_slack_message(slack_webhookurl), target_name = "slack1")
notify_infection(infection_osaka, post_linenotify_message(line_token), target_name = "line1", location = "高槻市")

infection_okayama <- get_infections("okayama")
notify_infection(infection_okayama, post_slack_message(slack_webhookurl), target_name = "slack1")

infection_kagawa <- get_infections("kagawa")
notify_infection(infection_kagawa, post_linenotify_message(line_token), target_name = "line1")
