library(tidyverse)
# death age distribution
ggplot(data = famous_dead_People) + aes(x = deathAge) + geom_bar() + xlim(0, 120)
ggsave("death age distribution.png")

# death year 
ggplot(data = famous_dead_People) + aes(x = deathYear) + geom_bar() + xlim(1000,2022)
ggsave("death year distribution.png")


# is average lifespan of a famous person increasing?
for (i in  0:11) {
  decade <- famous_dead_People%>%
    filter(deathYear > 1900 + i*10, deathYear < 1910 + i*10)
  ggplot(data = decade) + aes(x = deathAge) + geom_bar() + xlim(0, 120)
  ggsave(paste(as.character(1900 + i*10),"-", as.character(1910 + i*10),".png"))
}