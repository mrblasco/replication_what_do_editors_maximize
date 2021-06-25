#Table 1 Replication
#setwd("/Users/juliawiersum/replication_what_do_editors_maximize")
library(stargazer)
library(glue)
library(tidyverse)
library(haven)
library(ggplot2)
library(kableExtra) 
#library(faraway)
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


pooled_cleaned_all <- Pooled_cleaned %>% mutate(journal = "All")

# exploratory analysis 

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


# Create Table 1. -------------------------------------------------------


create_table <- function(temp) { 
   transpose_temp <- temp %>% 
     select(-journal) %>% 
      as.matrix %>% 
      t
  colnames(transpose_temp) <- temp$journal
  return(transpose_temp)
}

create_temp <- function(d) {
  d %>% 
      group_by(journal) %>% 
      select(GScites, WOScites, authpub5) %>% 
      mutate(GScites_asinh = asinh(GScites)) %>% 
      summarize_if(is.numeric, c("mean" = mean, SD = sd))  %>% 
      select(names(.) %>% sort)
}

temp_all <- Pooled_cleaned %>% 
    bind_rows(pooled_cleaned_all) %>% 
    create_temp()

temp_no_desk <- Pooled_cleaned %>% 
    filter(decision!="DeskRej") %>% 
    bind_rows(pooled_cleaned_all %>% filter(decision!="DeskRej")) %>% 
    create_temp()

table_left <- create_table(temp_all)
table_right <- create_table(temp_no_desk)

cbind(table_left, table_right) %>% 
kbl(digits = 1) %>% 
kable_classic(full = FALSE) %>% 
add_header_above(c("Variable", "All papers" = 5, "Non desk-rejected" = 5))


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
# Marging = 1 'row' = 2 'columns'
decision_journal_table <- Pooled_cleaned %>% 
  xtabs( ~ decision + journal, data = .) %>% 
  prop.table(margin = 2) 

decision_journal_table %>% t %>% barplot(beside = TRUE, legend = T)

#Still need to change unit on y-axis !
ggplot(Pooled_cleaned, mapping = aes(x=journal, fill = decision)) + 
  geom_bar(position = 'fill') +
  scale_fill_manual(values=hcl.colors(3)) + 
  ggtitle("Figure 1a. Distribution of Editorial Decisions") + 
  xlab(NULL) + ylab(NULL)

#Figure 1b (why missing? look at README)
Pooled_cleaned %>% 
mutate(eval1 = ifelse(eval1=="", "N/A", eval1)) %>% 
filter(decision != 'DeskRej') %>%
ggplot(mapping = aes(x = journal, fill = eval1)) + 
  geom_bar(position = 'fill') +
  scale_fill_manual(values=hcl.colors(8)) + 
  ggtitle("Figure 1b. Distribution of Referee Recommendations") + 
  xlab(NULL) + ylab(NULL)

#Figure 1c
Pooled_cleaned %>% 
mutate(authpub5 = factor(authpub5) %>% recode("6" = "6+")) %>% 
ggplot(mapping = aes(x=journal, fill = authpub5)) + 
  geom_bar(position = 'fill') +
  ggtitle("Figure 1c. Distribution of Author Prominence") + 
  xlab(NULL) + ylab(NULL)

#Figure 1d
ggplot(Pooled_cleaned, mapping = aes(x = journal, fill = refpub5_1)) + 
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
