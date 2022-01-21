library(tidyverse)
causeOfDeathAverage <- death_average %>%
  mutate(deathType = tolower(deathType))%>%
  rename(averagePercentage = ptg_death_avg)
  
causeOfDeathFamous<- read_csv("death_causes_famous_ppl.csv")%>%
  rename(deathType=deathCause_label, famousPercentage = percentage_deaths)

comparisondeathcause <- merge(causeOfDeathAverage,causeOfDeathFamous,by="deathType")%>%
  group_by(Type)%>%
  summarise(averagePercentage=sum(averagePercentage),famousPercentage=sum(famousPercentage))
ggplot(data=comparisondeathcause)+aes(fill=deathType,y=percentage,x=famousStatus) +
  geom_bar(position="stack",stat="identity") + ylab("Percentage") + xlab("Cause of death")
ggsave("comparison death cause.png")

uncommonDeathCause<- merge(causeOfDeathAverage,causeOfDeathFamous,by="deathType")%>%
  group_by(deathType)%>%
  summarise(averagePercentage=sum(averagePercentage),famousPercentage=sum(famousPercentage))%>%
  pivot_longer(col=c("averagePercentage","famousPercentage"),names_to = "famousStatus", values_to="percentage")
ggplot(data = uncommonDeathCause) + aes(fill=reorder(deathType,percentage), x = famousStatus, y = percentage)+
  geom_bar(position="stack",stat="identity")+ ylab("Percentage") + xlab("Cause of death")
ggsave("comparison uncommon death cause.png")
