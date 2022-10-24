library(tidyverse)

people = 
  read.csv("personnel.csv")%>%
  mutate(Postdoc_Bool = case_when(
    Trainee_Status == "Postdoc" ~ TRUE
  )) %>%
  mutate(Professor_Bool = case_when(
    grepl("Professor", Current_Position, ignore.case = T) ~ TRUE,
    grepl("Investigator", Current_Position, ignore.case = T) ~ TRUE,
    grepl("Research Fellow", Current_Position, ignore.case = T) ~ TRUE,
    grepl("Research Associate", Current_Position, ignore.case = T) ~ TRUE,
    grepl("Group Leader", Current_Position, ignore.case = T) ~ TRUE,
    grepl("Staff Scientist", Current_Position, ignore.case = T) ~ TRUE,
    grepl("Chief, Department of Virology", Current_Position, ignore.case = T) ~ TRUE,
    grepl("Director of Scientific Operations", Current_Position, ignore.case = T) ~ TRUE,
    grepl("Co-Director", Current_Position, ignore.case = T) ~ TRUE,
    grepl("Member, French National", Current_Position, ignore.case = T) ~ TRUE
    )) %>%
  mutate(Postdoc_to_Prof = case_when(
    Postdoc_Bool == TRUE & Professor_Bool == TRUE ~ TRUE
  )) %>%
  replace(is.na(.), FALSE)

# How many Jan lab postdocs?
table(people$Trainee_Status)
table(people$Postdoc_Bool)

# How many total professors? (including Jan Lab graduate students who eventually become professors)
table(people$Professor_Bool)

# How many Jan Lab postdocs became professors? (excludes Jan Lab graduate students included above)
table(people$Postdoc_to_Prof)
