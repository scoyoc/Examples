library("rwars")
library("tidyverse")
library("tidyjson")

people <- get_all_people(parse_result = T)
dat <- data.table::rbindlist(people$results, use.names = T, fill = T)

people <- get_all_people(getElement(people, "next"), parse_result = T)
dat <- data.table::rbindlist(people$results, use.names = T, fill = T)

# Look at Anakin Skywalker's data
people$results[[1]]
people$results[[1]][1] # print his name

data("starwars")
glimpse(starwars)
