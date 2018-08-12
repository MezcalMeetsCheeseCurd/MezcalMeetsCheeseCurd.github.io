library(googlesheets)
library(tidyverse)
library(jsonlite)

gs_ls()

gs_ls("OaxacaRecs")

OaxRec <- gs_title("OaxacaRecs")

OaxRec %>% gs_read("Sheet1")

#connect to gmail

use_secret_file(filename = "./timesheet-notifications.json")

#get google sheet
googlesheets::gs_auth(token = "ShinyAppToken.rds")
sheet_key <- "1sjupiTXp940xpawCOn7776DuCsoamySFqoGWut-Qrwk" # get sheet key
Time_Sheet <- googlesheets::gs_key(sheet_key)
