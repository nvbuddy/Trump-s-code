library(tidyverse)
library(dplyr)
library(pscl)
library(pROC)

## T+1
cleaned_1 <- T_1 %>% 
  select(`Date (Y-mm dd)`, `Sentiment value`, Direction) %>%
  rename(
    sent_value = `Sentiment value`
    )
cleaned_1 <- na.omit(cleaned_1)

glm.fit_1 <- glm(Direction ~ sent_value, data = cleaned_1, family = binomial)

summary(glm.fit_1)
pR2(glm.fit_1)
## Evaluation
glm.probs_1 <- predict(glm.fit_1, type='response')
roc_1 <- roc(Direction ~ glm.probs_1, data = cleaned_1)
plot(roc_1)
auc(roc_1)
## T+5
options(scipen=999)
cleaned_5 <- T_5 %>% 
  select(`Date (Y-mm dd)`, `Sentiment value`, Direction) %>%
  rename(
    sent_value = `Sentiment value`
  )
cleaned_5 <- na.omit(cleaned_5)

glm.fit_5 <- glm(Direction ~ sent_value, data = cleaned_5, family = binomial)

summary(glm.fit_5)
pR2(glm.fit_5)

## Evaluation
glm.probs_5 <- predict(glm.fit_5, type='response')
roc_5 <- roc(Direction ~ glm.probs_5, data = cleaned_5)
plot(roc_5)
auc(roc_5)

## T+10
cleaned_10 <- T_10 %>% 
  select(`Date (Y-mm dd)`, `Sentiment value`, Direction) %>%
  rename(
    sent_value = `Sentiment value`
  )
cleaned_10 <- na.omit(cleaned_10)

glm.fit_10 <- glm(Direction ~ sent_value, data = cleaned_10, family = binomial)

summary(glm.fit_10)
pR2(glm.fit_10)

## Evaluation
glm.probs_10 <- predict(glm.fit_10, type='response')
roc_10 <- roc(Direction ~ glm.probs_10, data = cleaned_10)
plot(roc_10)
auc(roc_10)
