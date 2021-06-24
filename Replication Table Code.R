#Table 1 Replication
setwd("/Users/juliawiersum/replication_what_do_editors_maximize")
library(stargazer)
library(glue)
library(tidyverse)
library(haven)
library(ggplot2)
library(kableExtra) 
library(faraway)
library(dplyr)
library(scales)

# Load data -------------------------------------------------------
Pooled_cleaned <- read_dta("original/Data/Main/Pooled_cleaned.dta")

# Filter the data
JEEA <- Pooled_cleaned %>% 
    filter(jeea == 1)

QJE <- Pooled_cleaned %>% 
  filter(qje == 1)

REStat <- Pooled_cleaned %>% 
  filter(restat == 1)

REStud <- Pooled_cleaned %>% 
  filter(restud == 1)

# -------------------------------------------------------

# View the data -------------------------------------------------------
Pooled_cleaned %>% glimpse()
JEEA %>% glimpse()
QJE %>% glimpse()
REStat %>% glimpse()
REStud %>% glimpse()

# Tables -------------------------------------------------------

# Table 1

pooled_cleaned_all <- Pooled_cleaned %>% mutate(journal = "All")

#Citations
# Summarize by journal and decision 
citations_table <- Pooled_cleaned %>% 
  bind_rows(pooled_cleaned_all) %>% 
  group_by(journal, decision) %>%
  summarize(Gscites = mean(GScites)
      , WOScites = mean(WOScites)
      , Obs = n())  

citations_table %>% 
  kableExtra::kbl(digit = 1, caption = "Citations") %>% 
  kable_classic(full = F) %>%
  save_kable(file = 'output/tables/citations_table.html')

system("open output/tables/citations_table.html")

#Editorial decisions
editorial_table <- Pooled_cleaned %>%
  bind_rows(pooled_cleaned_all) %>%
  group_by(journal, decision) %>%
  summarize(Obs = n())
  

editorial_table %>% 
  kableExtra::kbl(digit = 1, caption = "Editorial Decisions") %>% 
  kable_classic(full = F) %>%
  save_kable(file = 'output/tables/editorial_table.html')

system("open output/tables/editorial_table.html")

#Author publications in 35 high-impact journals 
authpub_table <- Pooled_cleaned %>%
  bind_rows(pooled_cleaned_all) %>%
  group_by(journal, pub0, pub1, pub2, pub3, pub45, pub6) %>%
  summarize(Obs = n())


authpub_table %>% 
  kableExtra::kbl(digit = 1, caption = "Author publications in 35 high-impact journals") %>% 
  kable_classic(full = F) %>%
  save_kable(file = 'output/tables/authpub_table.html')

system("open output/tables/authpub_table.html")

#Number of authors 
nauthors_table <- Pooled_cleaned %>%
  bind_rows(pooled_cleaned_all) %>%
  group_by(journal, auth_count) %>%
  summarize(Obs = n())


nauthors_table %>% 
  kableExtra::kbl(digit = 1, caption = "Number of authors ") %>% 
  kable_classic(full = F) %>%
  save_kable(file = 'output/tables/nauthors_table.html')

system("open output/tables/nauthors_table.html")


#Field of Paper 
field_table <- Pooled_cleaned %>%
  bind_rows(pooled_cleaned_all) %>%
  group_by(journal, micro, theory, metrics, macro, internat, fin, pub, labor, healthurblaw, hist, io, dev, lab, other, missingfield) %>%
  summarize(Obs = n()) 


field_table %>% 
  kableExtra::kbl(digit = 1, caption = "Field of paper") %>% 
  kable_classic(full = F) %>%
  save_kable(file = 'output/tables/field_table.html')

system("open output/tables/field_table.html")

# -------------------------------------------------------

# Compute Mean and SD. 
citations_table_mean <- Pooled_cleaned %>% 
  bind_rows(pooled_cleaned_all) %>% 
  group_by(journal, decision) %>% 
  summarize_if(is.numeric, list(Mean = mean, SD = sd)) 

# Print the table   
citations_table_mean %>% 
  select(journal, decision
      , starts_with('GScites')
      , -contains('jitter')  %>% 
  kableExtra::kbl(digit = 1, caption = "Descriptives") %>% 
  kable_classic(full = F) %>% 
  save_kable(file = 'output/tables/pooled_gscites_mean_sd.html')
# ?kbl to see other options

system("open output/tables/pooled_gscites_mean_sd.html")


# Figures -------------------------------------------------------

#Figure 1a

#Still need to change unit on y-axis !
ggplot(Pooled_cleaned, mapping = aes(x=decision, fill = journal)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count", position=position_dodge()) + 
  ggtitle("Figure 1a. Distribution of Editorial Decisions") + 
  xlab(NULL) + ylab(NULL)

#Figure 1b (why missing? look at README)
Pooled_cleaned %>% 
filter(decision != 'DeskRej') %>%
ggplot(mapping = aes(x=eval1, fill = journal)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count", position=position_dodge()) + 
  ggtitle("Figure 1b. Distribution of Referee Recommendations") + 
  xlab(NULL) + ylab(NULL)

#Figure 1c
ggplot(Pooled_cleaned, mapping = aes(x=authpub5, fill = journal)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count", position=position_dodge()) + 
  ggtitle("Figure 1c. Distribution of Author Prominence") + 
  xlab(NULL) + ylab(NULL)

#Figure 1d
ggplot(Pooled_cleaned, mapping = aes(x=refpub5_1, fill = journal)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count", position=position_dodge()) + 
  ggtitle("Figure 1d. Distribution of Referee Prominence") + 
  xlab(NULL) + ylab(NULL)


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
