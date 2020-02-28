library(Lahman)
library(tidyverse)

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

table_base <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb, yearID) %>% group_by(playerID) %>% summarize(mean_singles = mean(singles), mean_bb = mean(bb))
table_base = data.frame(table_base)

(lm(R ~ BB, data = Batting))

nrow(table_base %>% filter(bb > 0.2))

joined = inner_join(table_base, bat_02)

#model
joined %>% lm(bb ~ mean_bb, data = .)



library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
#set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>% filter(pair == 'mother_son')

galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == max(cor))



library(broom)
library(tidyverse)

get_slope <- function(data){
  fit <- lm(childHeight~ parentHeight, data = data)
  data.frame(slope = fit$coefficients[2], 
             praw = summary(fit)$coefficient[2,4],
              lower = confint(fit)[2,1],
             upper = confint(fit)[2,2])
}

galton %>%
  group_by(pair) %>%
  do(get_slope(.))

  
### Baseball Assessment pt 2
library(Lahman)

#effect of HR and BB on R
Teams %>% filter(yearID == 1971) %>% do(tidy(lm(R ~ HR + BB, data = .)))
  
res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

res %>% filter(res$term == "BB") %>% do(tidy(lm(BB ~ yearID, data = .)))


### Assessment LM

library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, runsPG = R/G, HRPG = HR/G)


get_slope <- function(data){ 
          fit <- lm(avg_attendance ~ runsPG + HRPG, data = data) 
          data.frame(slope = fit$coefficients[2], 
          se = summary(fit)$coefficient[2,2])}
  
### Question 4
Teams_small  %>% do(tidy(lm(avg_attendance ~ runsPG + HRPG + W + yearID, data = .)))