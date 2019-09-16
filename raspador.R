library(rtweet)
library(tidyverse)
library(lubridate)

perfis <- c("felipeneto",
            "opropiolavo",
            "LulaOficial",
            "JanainaDoBrasil",
            "jairbolsonaro",
            "Haddad_Fernando",
            "cirogomes",
            "MarinaSilva",
            "geraldoalckmin",
            "GuilhermeBoulos",
            "CaboDaciolo",
            "joaoamoedonovo",
            "wilsonwitzel",
            "jdoriajr",
            "costa_rui",
            "FlavioDino"
            )
tweets <- data.frame()
for(p in perfis){
  auxtweets <- get_timeline(p,n = 1000)
  auxtweets$timeline <- rep(p,nrow(auxtweets))
  tweets <- rbind(tweets, auxtweets)
}

saveRDS("data/tweets.rds")
tweets <- tweets %>% filter(created_at >= ymd("2019-09-06"))
tweets %>%
  group_by(timeline) %>%
  summarise(retweets = sum(retweet_count),
            favorites = sum(favourites_count),
            retPerFollower = sum(retweet_count/followers_count),
            favPerFollower = sum(favourites_count/followers_count),
            interactionPerFollower = sum((favourites_count+retweet_count)/followers_count)) %>%
  ggplot(aes(x = timeline, y = favPerFollower, fill = timeline))+
  geom_bar(stat="identity")
