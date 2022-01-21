library('tidyverse')
dataframe <- readxl::read_excel('natural v unnatural famous non famous.xlsx')%>%

ggplot(data = dataframe) +
  aes(x = deathType, y = Percentage, fill=Status ) +
  geom_col(position = 'dodge') +
  labs(title = 'Distribution of Causes of Death (1990-2019)') +
  xlab("Category of Death") +
  ylab('Percentage of Total Death') +
  theme_light() +
  scale_y_continuous(labels = scales :: label_percent())
ggsave(paste("natural v unnatural.png"))
