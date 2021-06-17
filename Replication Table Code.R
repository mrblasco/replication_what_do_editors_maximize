#Table 1 Replication
setwd("/Users/juliawiersum/replication_what_do_editors_maximize")
library(stargazer)
library(gt)
library(glue)
library(tidyverse)
library(haven)
library(ggplot2)
library(faraway)
Pooled_cleaned <- read_dta("original/Data/Main/Pooled_cleaned.dta")

#Table 1

"kable(x, format, digits = getOption("digits"), row.names = NA,
  col.names = NA, align, caption = NULL, label = NULL,
  format.args = list(), escape = TRUE, ...)"
#knitr::kable(x)

#Figure 1a
#Still need to categorize by journal (Info not available in Pooled_cleaned), and change unit on y-axis
ggplot(Pooled_cleaned, mapping = aes(x=decision)) + geom_bar(stat = "count", position=position_dodge()) + ggtitle("Figure 1a. Distribution of Editorial Decisions") + xlab(NULL) + ylab(NULL)
#Figure 1b
ggplot(Pooled_cleaned, mapping = aes(x=eval1)) + geom_bar(stat = "count", position=position_dodge()) + ggtitle("Figure 1b. Distribution of Referee Recommendations") + xlab(NULL) + ylab(NULL)

#Figure 1c
ggplot(Pooled_cleaned, mapping = aes(x=authpub5)) + geom_bar(stat = "count", position=position_dodge()) + ggtitle("Figure 1c. Distribution of Author Prominence") + xlab(NULL) + ylab(NULL)

#Figure 1d
ggplot(Pooled_cleaned, mapping = aes(x=refpub5_1)) + geom_bar(stat = "count", position=position_dodge()) + ggtitle("Figure 1d. Distribution of Referee Prominence") + xlab(NULL) + ylab(NULL)

#Model for Desk Rejection probabilty
glm(formula = decision ~ eval1, family = "poisson", data = JEEA)
#Non-numeric argument to binary operator

#Merge Journals' Data
alljrnls <- merge(JEEA, QJE, REStat, REStud, by = "ID")