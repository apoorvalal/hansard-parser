# install.packages(c("httr", "jsonlite"))
library(httr)
library(here)
library(jsonlite)
library(rio)
library(tidyverse)
library(datatable)
options(stringsAsFactors = FALSE)

# Import orig. data
speechpeople <- import(here("/input/twfy/tmp/speaker_list.csv")) %>% rowwise %>% mutate(mergecol = if(V2 == ""){V3} else {V2})
peoplelist <- fromJSON(here("/input/twfy/jsons/people.json"))

# Fill in missing person IDs
merge1 <- speechpeople %>% left_join(peoplelist$memberships, by = c("mergecol" = "id")) %>% rowwise %>% mutate(person_id = if(V2 == ""){person_id} else{V2}) %>% select(V1, person_id)

# Merge person IDs with memberships
merge2 <- merge1 %>% left_join(peoplelist$memberships, by = c("person_id" = "person_id")) %>% select(-c(identifiers, redirect))
# Merge person IDs with person data
merge3 <- merge2 %>% left_join(peoplelist$persons, by = c("person_id" = "id"))

# Prep for export
outdf <- as.data.frame(merge3[-1, ]) %>% select(-c(identifiers, other_names, redirect, name, shortcuts, redirect))

fwrite(outdf, file = here("input/twfy/tmp/speaker_info.csv"))