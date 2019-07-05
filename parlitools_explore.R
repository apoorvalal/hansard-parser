library(parlitools)
library(tidyverse)
library(rio)

load("output/clustering.RData")

# Get data and merge
west_hex_map <- parlitools::west_hex_map

party_col <- parlitools::party_colour


mps_covars <- import("input/twfy/tmp/speaker_info.csv") %>% setDT %>% mutate(member_id = str_replace(id, 
                'uk.org.publicwhip/member/', ''))

mps <- mps_on_date("2017-06-20")

mps_merge <- mps_covars %>% left_join(mps)
mps_merge <- mps_merge %>% select(V1, person_id, on_behalf_of_id, party_id, gss_code) %>% filter(on_behalf_of_id != "" & gss_code != "")
covars <- covars[!duplicated(covars[ , c('person_id')]),]
names(covars)[1] <- c("V1")