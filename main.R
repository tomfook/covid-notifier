library(tidyverse)
library(lubridate)
library(magrittr)
library(rvest)
library(httr) 
library(jsonlite) 

TEST <- FALSE

source("secret.R") #slack_webhookurl 

source("getkyoto.R")
source("getosaka.R")
source("getokayama.R")
