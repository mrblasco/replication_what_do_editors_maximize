#Table 1 Replication
#setwd("/Users/juliawiersum/replication_what_do_editors_maximize")
library(stargazer)
library(glue)
library(tidyverse)
library(haven)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(kableExtra) 
<<<<<<< HEAD
=======
#library(faraway)
>>>>>>> 4978b71122330ab7e5435c66cb529ba931fc5b36
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
<<<<<<< HEAD
  bind_rows(pooled_cleaned_all) %>% 
  group_by(journal, decision) %>%
  summarize(Gscites = mean(GScites)
            , WOScites = mean(WOScites)
            , Obs = n())  
=======
    bind_rows(pooled_cleaned_all) %>% 
    group_by(journal, decision) %>%
    summarize(Gscites = mean(GScites)
        , WOScites = mean(WOScites)
        , Obs = n())  
>>>>>>> 4978b71122330ab7e5435c66cb529ba931fc5b36

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

<<<<<<< HEAD
=======

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
>>>>>>> 4978b71122330ab7e5435c66cb529ba931fc5b36

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
    select(GScites, WOScites, authpub5, ndr, rr, auth_count, micro, theory, metrics, macro, internat, fin, pub, labor, healthurblaw, hist, io, dev, lab, other, missingfield, frDefReject, frReject, frNoRec, frWeakRR, frRR, frStrongRR, frAccept,) %>% 
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

table_1 <- cbind(table_left, table_right) %>% 
  kbl(digits = 1) %>% 
  kable_classic(full = FALSE) %>% 
  add_header_above(c("Variable", "All papers" = 5, "Non desk-rejected" = 5))

         
# Figures -------------------------------------------------------
         
#Figure 1a - Journal replication
# Merging = 1 'row' = 2 'columns'
decision_journal_table <- Pooled_cleaned %>% 
xtabs( ~ decision + journal, data = .) %>% 
prop.table(margin = 2) 
         
decision_journal_table %>% t %>% barplot(beside = TRUE, legend = T)
         

#Figure 1a

decision_journal_table <- Pooled_cleaned %>% 
  xtabs( ~ decision + journal, data = .) %>% 
  prop.table(margin = 2) 

decision_journal_table %>% t %>% barplot(beside = TRUE, legend = T)

#Still need to change unit on y-axis !
Figure_1a <- ggplot(Pooled_cleaned, mapping = aes(x=journal, fill = decision)) + 
  geom_bar(position = 'fill') +
  scale_fill_manual(values=hcl.colors(3)) + 
  ggtitle("Figure 1a. Distribution of Editorial Decisions") + 
  xlab(NULL) + ylab(NULL)
         
#Figure 1b (why missing? look at README)
Figure_1b <- Pooled_cleaned %>% 
  mutate(eval1 = ifelse(eval1=="", "N/A", eval1)) %>% 
  filter(decision != 'DeskRej') %>%
  ggplot(mapping = aes(x = journal, fill = eval1)) + 
  geom_bar(position = 'fill') +
  scale_fill_manual(values=hcl.colors(8)) + 
  ggtitle("Figure 1b. Distribution of Referee Recommendations") + 
  xlab(NULL) + ylab(NULL)
         
#Figure 1c
Figure_1c <- Pooled_cleaned %>% 
  mutate(authpub5 = factor(authpub5) %>% recode("6" = "6+")) %>% 
  ggplot(mapping = aes(x=journal, fill = authpub5)) + 
  geom_bar(position = 'fill') +
  scale_fill_manual(values = hcl.colors(7)) +
  ggtitle("Figure 1c. Distribution of Author Prominence") + 
  xlab(NULL) + ylab(NULL)
         
#Figure 1d
Figure_1d <- Pooled_cleaned %>%
  filter(decision != 'DeskRej') %>%
  mutate(refpub5_1 = factor(refpub5_1) %>% recode("6" = "6+")) %>%
  ggplot(mapping = aes(x=journal, fill = refpub5_1)) +
  geom_bar(position = 'fill') +
  scale_fill_manual(values = hcl.colors(8)) +
  ggtitle("Figure 1d. Distribution of Referee Prominence") + 
  xlab(NULL) + ylab(NULL)

Figure_1 <- ggarrange(Figure_1a, Figure_1b, Figure_1c, Figure_1d,
                    ncol = 2, nrow = 2)
Figure_1

# Models -------------------------------------------------------
         
pooled_model <- Pooled_cleaned %>% 
 mutate(is_desk_rej = ifelse(decision == 'DeskRej', 1, 0) %>%
  mutate(is_rr = ifelse(decision == 'RR', 1, 0))
         
xtabs( ~ is_desk_rej + yearsubmit + journal
      , data = pooled_model)
         
#Model for Desk Rejection probabilty
dr_prob <- glm(is_desk_rej ~ journal + yearsubmit + auth_count + authpub5 + micro + theory + metrics + macro + internat + fin + pub + labor + healthurblaw + hist + io + dev + lab + other) + 
  (family = "binomial") +
  (data = pooled_model)
         
stargazer(dr_prob, type = "text")

#Model for R&R probability
rr_prob <- glm(is_rr ~ journal + yearsubmit + auth_count + authpub5 + micro + theory + metrics + macro + internat + fin + pub + labor + healthurblaw + hist + io + dev + lab + other) + 
  (family = "binomial") +
  (data = pooled_model)

stargazer(rr_prob, type = "text")
