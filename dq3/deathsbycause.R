library(tidyverse)
annual_global_deaths <- read.csv('annual-number-of-deaths-by-cause.csv')

# Average Causes of Death Across 1990 - 2019
avg_deathcause <- annual_global_deaths %>%
  # Selecting columns
  mutate(Natural = Deaths...Meningitis...Sex..Both...Age..All.Ages..Number.+
           Deaths...Neoplasms...Sex..Both...Age..All.Ages..Number.+ 
           Deaths...Acute.hepatitis...Sex..Both...Age..All.Ages..Number.+
           Deaths...Malaria...Sex..Both...Age..All.Ages..Number.+
           Deaths...HIV.AIDS...Sex..Both...Age..All.Ages..Number.+
           Deaths...Tuberculosis...Sex..Both...Age..All.Ages..Number.+
           Deaths...Maternal.disorders...Sex..Both...Age..All.Ages..Number.+
           Deaths...Lower.respiratory.infections...Sex..Both...Age..All.Ages..Number.+
           Deaths...Neonatal.disorders...Sex..Both...Age..All.Ages..Number.+
           Deaths...Diarrheal.diseases...Sex..Both...Age..All.Ages..Number.+
           Deaths...Diabetes.mellitus...Sex..Both...Age..All.Ages..Number.+
           Deaths...Cardiovascular.diseases...Sex..Both...Age..All.Ages..Number.+
           Deaths...Chronic.respiratory.diseases...Sex..Both...Age..All.Ages..Number.+
           Deaths...Chronic.kidney.disease...Sex..Both...Age..All.Ages..Number.+
           Deaths...Cirrhosis.and.other.chronic.liver.diseases...Sex..Both...Age..All.Ages..Number.+
           Deaths...Digestive.diseases...Sex..Both...Age..All.Ages..Number.+
           Deaths...Alzheimer.s.disease.and.other.dementias...Sex..Both...Age..All.Ages..Number.+
           Deaths...Parkinson.s.disease...Sex..Both...Age..All.Ages..Number.,
         Overdose = Deaths...Drug.use.disorders...Sex..Both...Age..All.Ages..Number.+
         Deaths...Alcohol.use.disorders...Sex..Both...Age..All.Ages..Number.,
         Accident = Deaths...Road.injuries...Sex..Both...Age..All.Ages..Number.+
           Deaths...Drowning...Sex..Both...Age..All.Ages..Number.+
           Deaths...Fire..heat..and.hot.substances...Sex..Both...Age..All.Ages..Number.) %>%
  rename(Suicide = Deaths...Self.harm...Sex..Both...Age..All.Ages..Number., 
         Homicide = Deaths...Interpersonal.violence...Sex..Both...Age..All.Ages..Number.) %>%
  select(Entity, Year, Suicide, Homicide, Overdose, Accident, Natural)%>%
  filter(Entity == 'World') 

death_average <- avg_deathcause %>%
  summarise(
    Suicide = mean(Suicide),
    Overdose = mean(Overdose),
    Natural = mean(Natural),
    Accident = mean(Accident),
    Homicide = mean(Homicide)) %>%
  pivot_longer(c(Suicide, Overdose, Natural, Accident, Homicide), 
               names_to = 'deathType', values_to = 'Average') %>%
  mutate(total_death_avg = sum(Average)) %>%
  mutate(ptg_death_avg = Average/total_death_avg) %>%
  select(-total_death_avg) %>%
  mutate(Type = case_when(
    deathType == 'Natural' ~ 'Natural',
    deathType == 'Suicide'| deathType == 'Overdose' | 
      deathType == 'Accident' | deathType == 'Homicide' ~ 'Unnatural'
  ))

write.csv(death_average, file = 'death_average_worldwide.csv')

ggplot(death_average) +
  aes(x = deathType, y = ptg_death_avg) +
  geom_col(fill = '#F5888E') +
  xlab('Category of Death') +
  ylab('Percentage of Total Death') +
  labs (title = 'Distribution of Causes of Death Worldwide (1990-2019)') +
  scale_y_continuous(labels = scales :: label_percent()) 
ggsave(paste('death_cause_worldwide.png'))



ggplot(death_average) +
  aes(x = Type, y = ptg_death_avg) +
  geom_col(fill = '#F5888E') +
  xlab('Type of Death') +
  ylab('Percentage of Total Death') +
  labs (title = 'Distribution of Natural and Unnatural Death Causes Worldwide (1990-2019)') +
  scale_y_continuous(labels = scales :: label_percent()) 
ggsave(paste('natural_v_unnatural.png'))


