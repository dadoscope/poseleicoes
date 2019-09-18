library(rtweet)
library(tidyverse)
library(lubridate)

perfis <- c("felipeneto",
            "alefrota77",
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
  cat(p,sep="\n")
  auxtweets <- get_timeline(p,n = 2000)
  auxtweets$timeline <- rep(p,nrow(auxtweets))
  tweets <- rbind(tweets, auxtweets)
}

saveRDS(tweets, file="data/tweets.rds")
#tweets <- readRDS("data/tweets.rds")

#### To analyze the downloaded data

tweets %>% mutate(date = round_date(created_at, unit = "day")) %>%
  group_by(date, timeline) %>%
  summarise(retweets = sum(retweet_count),
            favorites = sum(favourites_count),
            retPerFollower = sum(retweet_count/followers_count),
            favPerFollower = sum(favourites_count/followers_count),
            interactionPerFollower = sum((favourites_count+retweet_count)/followers_count)) %>%
  ggplot(aes(x = date, y = retweets, col = timeline))+
  geom_line(stat="identity")
  
  
tweets <- tweets %>% filter(created_at >= ymd("2019-09-06"))
tweets %>%
  group_by(timeline) %>%
  summarise(followers_count = mean(followers_count),
            retweets = sum(retweet_count),
            favorites = sum(favourites_count),
            retPerFollower = sum(retweet_count/followers_count),
            favPerFollower = sum(favourites_count/followers_count),
            interactionPerFollower = sum((favourites_count+retweet_count)/followers_count)) %>%
  ggplot(aes(x = timeline, y = followers_count, fill = timeline))+
  geom_bar(stat="identity")+
  coord_flip()
