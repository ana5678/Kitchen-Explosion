#Data on population death in Wales, England, between 1994-2000
population_death_1994_1996 <- readxl::read_excel('death_pop_1994_1996.xls')
population_death_1997_1998 <- readxl::read_excel('death_pop_1997_1998.xlsx')
population_death_1999_2000 <- readxl::read_excel('death_pop_1999_2000.xlsx')
population_death <- rbind(population_death_1994_1996, population_death_1997_1998)
population_death <- rbind(population_death, population_death_1999_2000)

#Calculate the percentage of total deaths per each age group
population_death <- population_death %>%
  select(- ICD_9,-sex) %>%
  rename(year = yr, number_deaths = ndths)
population_death <- population_death %>%
  group_by(age) %>%
  mutate(total_deaths_per_age_group = sum(number_deaths)) %>%
  ungroup() %>%
  distinct(age, total_deaths_per_age_group) %>%
  mutate(total_deaths = sum(total_deaths_per_age_group)) %>%
  mutate(ptg_deaths = total_deaths_per_age_group/total_deaths)

ggplot(data = population_death) +
  aes(x = age, y= ptg_deaths) +
  geom_col() +
  labs(title = 'Distribution of Death per Age Category in Wales, England (1994-2000)') +
  xlab("Age Category") +
  ylab("Percentage of Total Death") +
  theme_light() +
  scale_y_continuous(labels = scales :: label_percent())
ggsave("distribution age category general pop.png")