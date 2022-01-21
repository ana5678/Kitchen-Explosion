library(tidyverse)

dq1famous <- famous_dead_People%>%
filter(deathYear > 1993, deathYear < 2001)%>%
  mutate(age=case_when(
    between(deathAge,0,1) ~ "<1",
    between(deathAge,1,4) ~ "01-04",
    between(deathAge,5,9) ~ "05-09",
    between(deathAge,10,14) ~ "10-14",
    between(deathAge,15,19) ~ "15-19",
    between(deathAge,20,24) ~ "20-24",
    between(deathAge,25,29) ~ "25-29",
    between(deathAge,30,34) ~ "30-34",
    between(deathAge,35,39) ~ "35-39",
    between(deathAge,40,44) ~ "40-44",
    between(deathAge,45,49) ~ "45-49",
    between(deathAge,50,54) ~ "50-54",
    between(deathAge,55,59) ~ "55-59",
    between(deathAge,60,64) ~ "60-64",
    between(deathAge,65,69) ~ "65-69",
    between(deathAge,70,74) ~ "70-74",
    between(deathAge,75,79) ~ "75-79",
    between(deathAge,80,84) ~ "80-84",
    between(deathAge,85,120) ~ "85+"))%>%
  group_by(age)
famousPercentage <- count(dq1famous,age)%>%
  mutate(n=n/nrow(dq1famous))%>%
  rename(Famous=n)
combinedPercentage<-merge(population_death,famousPercentage,by="age")%>%
  rename(Normal=ptg_deaths)%>%
  select(age,Famous,Normal)%>%
  pivot_longer(col=c("Famous","Normal"),names_to = "fameLevel", values_to="percentage")
ggplot(data = combinedPercentage) + aes(x = age, y = percentage, fill = fameLevel)+ geom_col(position = 'dodge')+
  theme(axis.text.x = element_text(angle = 60,vjust = 0.5)) +
  ylab("Percentage") + xlab("Age of death")+
  scale_y_continuous(labels = scales::label_percent())
ggsave("death age 1994-2000.png")


