library(dplyr)
library(ggplot2)

# vids
# https://www.youtube.com/watch?v=82HMAkIZeBk&t=601s

# get data, add target column
raw = read.csv('adult.csv')
raw$target = ifelse(raw$income == '<=50K', 0, 1)

# income by race
ggplot(raw, aes(x=race, y=target)) + stat_summary(fun.y = 'mean', geom='point')

# combine white & asian into 1
whites = c('Asian-Pac-Islander', 'White')
raw = raw %>%
  mutate(is_white = ifelse(race %in% whites, 1, 0))

# all columns but the target
# setdiff(names(raw), 'target')

# create propensity
cols = c('age', 'is_white', 'gender')
props = glm(target ~ age + is_white + gender, data=raw, family=binomial(logit))
p_scores = props$fitted

raw$p_score = p_scores

ggplot(raw, aes(x=p_score, y=target)) + stat_summary(fun.y = 'mean', geom='point')
ggplot(raw, aes(x=p_score, fill=factor(target))) + geom_freqpoly() + facet_grid(target ~ .)

cor(raw$p_score, raw$target)
