library(tidyverse)
library(plyr)
famous_dead_People <- read_csv("famous_dead_people.csv")
#Filter for correct and relevant data
famous_dead_People <- famous_dead_People%>%
  filter(deathYear>birthYear, deathYear<2022, nchar(deathYear) == 4, nchar(birthYear) == 4)%>% #removing erroneous data
  mutate(deathYear = as.integer(deathYear), birthYear= as.integer(birthYear))%>% # reformatting
  mutate(age = deathYear-birthYear)%>%
  filter(age<120) 

#Frequency table for death causes
death_causes_freq <- count(famous_dead_People_deathCause, deathCause) %>%
  filter(n > 2 & deathCause != 'N/A')


#Label causes of death between 1990 and 2019
mapping <- readxl::read_excel('death_labels.xlsx')
famous_dead_People_deathCause <- famous_dead_People %>%
  filter(deathYear >= 1990 & deathYear <=2019) %>%
  select(Name, birthYear, deathCause, deathYear, age) %>%
  mutate %>%
  left_join(mapping, by=c('deathCause')) %>%
  filter(deathCause_label != 'N/A') %>%
  distinct()

#Calculate the percentage of deaths per type of death famous people
deathCause_label_freq <- count(famous_dead_People_deathCause, "deathCause_label") %>%
  mutate(total_number_deaths = sum(freq)) %>%
  mutate(percentage_deaths = freq/total_number_deaths) %>%
  select(-total_number_deaths) %>%
  arrange(percentage_deaths) %>%
  mutate(y_max = cumsum(percentage_deaths)) %>%
  mutate(y_min = c(0, head(y_max, n=-1))) %>%
  mutate(color = c("#EB8373", "#EDCD7B", "#CC67EB", "#50EBA6", "#5B82EB"))

#Calculate the percentage of natural/unnatural deaths famous people
death_type_freq <- count(famous_dead_People_deathCause, "deathType") %>%
  mutate(total_number_deaths = sum(freq)) %>%
  mutate(percentage_deaths = freq/total_number_deaths) %>%
  select(-total_number_deaths)

#Visualize the percentage of deaths per death category among famous people
ggplot(data = deathCause_label_freq) +
  aes(x = reorder(deathCause_label, -percentage_deaths), y = percentage_deaths) +
  geom_col(fill = '#A84D52') +
  labs(title = "Distribution of Causes of Death among Famous People (1990-2019)") +
  xlab("Category of Death") +
  ylab("Percentage of Total Death") +
  theme_light() +
  scale_y_continuous(labels = scales :: label_percent())
ggsave(paste("percentage_type_death_famous_people.png"))

#Donut chart of death causes 
ggplot(deathCause_label_freq, aes(ymax = y_max, ymin = y_min, xmax =4, xmin = 3, fill = deathCause_label)) +
  scale_fill_manual(values = c("#EB8373", "#EDCD7B", "#CC67EB", "#50EBA6", "#5B82EB")) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  labs(fill = "Types of Death")
ggsave(paste("distribution of types of death.png"))

#Visualize the percentage of natural and unnatural deaths among famous people
ggplot(data = death_type_freq) +
  aes(x = deathType, y = percentage_deaths) +
  geom_col(fill = '#A84D52') +
  labs(title = 'Distribution of Natural and Unnatural Death Causes among Famous People (1990-2019)') +
  xlab("Type of Death") +
  ylab("Percentage of Total Death") +
  theme_light() +
  scale_y_continuous(labels = scales :: label_percent())
ggsave(paste("percentage_of_natural_unatural_famous_people.png"))


    