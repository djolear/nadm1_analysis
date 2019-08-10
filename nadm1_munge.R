
library(tidyverse, lubridate)

subids <-
  read_csv("G:/My Drive/research/projects/nadm1/nadm1_data/nadm1_ssdm2_subids.csv")

redcap <-
  read_csv("G:/My Drive/research/projects/nadm1/nadm1_data/nadm1_redcap_data/nadm1_ema_01_29_1905.csv")

asa_24 <-
  read_csv("G:/My Drive/research/projects/nadm1/nadm1_data/nadm1_asa24_data/nadm1_2019-01-30_80657/nadm1_2019-01-30_80657_TNS.csv")

asa_24_total <-
  read_csv("G:/My Drive/research/projects/nadm1/nadm1_data/nadm1_asa24_data/nadm1_2019-01-30_80657/nadm1_2019-01-30_80657_Totals.csv")

ssdm2_survey_data_nadm <- read_csv("G:/My Drive/research/projects/ssdm2/ssdm2_group_data/ssdm2_survey_data.csv")

# Filter out people who didn't finish
subids <-
  subids %>% 
  filter(complete == "Y") %>% 
  select(-complete) %>% 
  mutate(subid = as.numeric(subid))

# Link subids to asa24
asa_24 <-
  subids %>% 
  left_join(
    asa_24 %>% mutate(asa24_id = UserName),
    by = "asa24_id"
  )

# Link ssdm2 survey to asa24
asa_24 <-
  asa_24 %>% 
  left_join(
    ssdm2_survey_data_nadm %>% 
      mutate(ssdm2_subid = as.character(subid)),
    by = "ssdm2_subid"
  )


# Link subids to asa24 totals
asa_24_total <-
  subids %>% 
  left_join(
    asa_24_total %>% mutate(asa24_id = UserName),
    by = "asa24_id"
  ) 

# Link ssdm2 survey to asa24 totals
asa_24_total <-
  asa_24_total %>% 
  left_join(
    ssdm2_survey_data_nadm %>% 
      mutate(ssdm2_subid = as.character(subid)),
    by = "ssdm2_subid"
  )


redcap_ema <-
  redcap %>%
  mutate(
    day = str_extract(redcap_event_name, "day_\\d"),
    survey = str_extract(redcap_event_name, "survey_\\d"),
    ema_survey_timestamp = lubridate::ymd_hms(ema_survey_timestamp),
    weekday = lubridate::wday(ema_survey_timestamp, label = TRUE)
  ) %>% 
  select(redcap_id = user_name, day, weekday, survey, ema_survey_timestamp:ema_survey_complete) %>% 
  filter(!is.na(day))

# Link subids to ema responses
redcap_ema <-
  redcap_ema %>% 
  filter(redcap_id %in% c(subids$redcap_record_id)) %>% 
  left_join(
    subids %>% 
      mutate(redcap_id = redcap_record_id), 
    by = "redcap_id"
  ) %>% 
  mutate(
    subid = as.numeric(ssdm2_subid)
  )

glimpse(redcap_ema)

# Link survey data to ema data
redcap_ema_survey <-
  redcap_ema %>% 
  left_join(
    ssdm2_survey_data_nadm, 
    by = "subid"
  )

# check into this!
subs <-
  redcap_ema_survey %>% 
  count(ssdm2_subid)

# 84, 97, 114 have too many