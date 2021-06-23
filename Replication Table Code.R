#Table 1 Replication
#setwd("/Users/juliawiersum/replication_what_do_editors_maximize")
library(stargazer)
library(gt)
library(glue)
library(tidyverse)
library(haven)
library(ggplot2)
library(kableExtra) 
#library(faraway)

# Load data -------------------------------------------------------
Pooled_cleaned <- read_dta("original/Data/Main/Pooled_cleaned.dta")
#JEEA <- ... 
# 1, 2, 3. ... 

# Filter the data
JEEA <- Pooled_cleaned %>% 
    filter(jeea == 1)

# -------------------------------------------------------

# View the data -------------------------------------------------------
Pooled_cleaned %>% glimpse()

# Tables -------------------------------------------------------

# Table 1

pooled_cleaned_all <- Pooled_cleaned %>% mutate(journal = "All")

# Summarize by journal and decision 
pooled_table <- Pooled_cleaned %>% 
  bind_rows(pooled_cleaned_all) %>% 
  group_by(journal, decision) %>%
  summarize(Gscites = mean(GScites)
      , WOScites = mean(WOScites)
      , Obs = n()
      )  

# Print the table 
pooled_table %>% 
  kableExtra::kbl(digit = 1) %>% 
  kable_classic(full = F)

# Compute Mean and SD. 
pooled_table_mean <- Pooled_cleaned %>% 
  bind_rows(pooled_cleaned_all) %>% 
  group_by(journal, decision) %>% 
  summarize_if(is.numeric, list(Mean = mean, SD = sd)) 

# Print the table   
pooled_table_mean %>% 
  select(journal, decision
      , starts_with('GScites')
      , -contains('jitter'))  %>% 
  kableExtra::kbl(digit = 1, caption = "Descriptives") %>% 
  kable_classic(full = F) %>% 
  save_kable(file = 'output/tables/pooled_gscites_mean_sd.html')
# ?kbl to see other options

system("open output/tables/pooled_gscites_mean_sd.html")

# kable(x, format, digits = getOption("digits"), row.names = NA,
#   col.names = NA, align, caption = NULL, label = NULL,
#   format.args = list(), escape = TRUE, ...)"
#knitr::kable(x)

# Figures -------------------------------------------------------

#Figure 1a
#Still need to change unit on y-axis !
ggplot(Pooled_cleaned, mapping = aes(x=decision, fill = journal)) + 
  geom_bar(stat = "count", position=position_dodge()) + 
  ggtitle("Figure 1a. Distribution of Editorial Decisions") + 
  xlab(NULL) + ylab(NULL)

#Figure 1b (why missing? look at README)
Pooled_cleaned %>% 
filter(decision != 'DeskRej') %>%
ggplot(mapping = aes(x=eval1, fill = journal)) + 
  geom_bar(stat = "count", position=position_dodge()) + 
  ggtitle("Figure 1b. Distribution of Referee Recommendations") + 
  xlab(NULL) + ylab(NULL)

#Figure 1c
ggplot(Pooled_cleaned, mapping = aes(x=authpub5)) + 
  geom_bar(stat = "count", position=position_dodge()) + 
  ggtitle("Figure 1c. Distribution of Author Prominence") + 
  xlab(NULL) + ylab(NULL)

#Figure 1d
ggplot(Pooled_cleaned, mapping = aes(x=refpub5_1)) + geom_bar(stat = "count", position=position_dodge()) + ggtitle("Figure 1d. Distribution of Referee Prominence") + xlab(NULL) + ylab(NULL)


# Models -------------------------------------------------------

pooled_model <- Pooled_cleaned %>% 
    mutate(is_desk_rej = ifelse(decision == 'DeskRej', 1, 0)) 

xtabs( ~ is_desk_rej + yearsubmit + journal
    , data = pooled_model)

#Model for Desk Rejection probabilty
fit <- glm(is_desk_rej ~ journal + yearsubmit + auth_count + authpub5 
    , family = "binomial", data = pooled_model)

stargazer(fit, type = "text")

#Non-numeric argument to binary operator

#Merge Journals' Data
alljrnls <- merge(JEEA, QJE, REStat, REStud, by = "ID")