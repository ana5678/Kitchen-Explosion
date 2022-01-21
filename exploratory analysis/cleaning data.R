library(tidyverse)
famous_dead_People <- read_csv("famous_dead_people.csv")
famous_dead_People <- famous_dead_People%>%
  filter(deathYear>birthYear, deathYear<2022, nchar(deathYear) == 4, nchar(birthYear) == 4)%>% #removing erroneous data
  mutate(deathYear = as.integer(deathYear), birthYear= as.integer(birthYear))%>% # reformatting
  mutate(deathAge = deathYear-birthYear)%>%
  filter(deathAge<120)