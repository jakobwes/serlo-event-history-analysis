#Data reading and preprocessing for event-log analysis. Jakob Wessel January 2021.

library(tidyverse)
library(magrittr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggpubr)
library(gridExtra)
library(TTR)
library(dynlm)
library(forecast)
library(shiny)


event_file <- "events.csv"

#Reading data and formatting
events <- read.csv(event_file, header=TRUE)
events %<>% mutate(events, date= as.Date(date, format= "%Y-%m-%d"))


#Filter edits after migration serlo1 --> serlo2. Definition: https://docs.google.com/spreadsheets/d/1WoNiWyXJRckC1eNwed6et-7N02f2mlx9iMKF87F4f6I/edit#gid=1937612395 (last line)
events %<>% filter(name %in% c("entity/link/remove", "entity/link/remove", "entity/revision/add", "entity/create") & format(date, "%Y") > 2014)
events %<>% mutate(end_of_90_days = date+90)

#---------------------------------------

#Calculate authors

#Creates a dataframe with all authors, the respective days in which they had edits and the edits in the last 90 days
cum_authors_edits <- events %>%
  count(actor_id, date) %>%
  group_by(actor_id) %>%
  complete(date = full_seq(date, period = 1), fill = list(n = 0)) %>%
  #group_by(actor_id) %>% --> not needed
  mutate(cum_edits90days_rolling = rollapply(n, width = 90, FUN = sum, partial = TRUE, align="right")) %>%
  drop_na(cum_edits90days_rolling)

#Only select edits from 2016 onwards
cum_authors_edits %<>% filter(year(date)>2015)

#All authors which at one time had over 9 edits in 90 days, so that we counted as active authors once
cum_active_authors <- cum_authors_edits %>%
  filter(cum_edits90days_rolling > 9)




