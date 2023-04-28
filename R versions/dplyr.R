# package loading has been troublesome in Posit Cloud. First, memory constraints mean that the meta-package tidyverse loads unreliably, so tidyverse packages are loaded individually. Second, troubleshooting installation trips up trainees at the start of the sessions. pacman is therefore used to automate installation and loading. 

install.packages(setdiff("pacman", rownames(installed.packages())))
library("pacman")
p_load(dplyr,lubridate,stringr,NHSRdatasets, glue, tidyr, purrr, knitr,ggplot2, waldo)

data_describer <- function(df) {

  # allows us to display the name of the df as a new subsection with a little bit of nicely formatted Rmarkdown  
  df_name <- deparse(substitute(df))
  
  cat(glue("  \n### {df_name}  \n  \n"))
  cat(glue("  \n`{df_name}` has {nrow(df)} rows and {length(df)} columns.  \n  \n"))
  display(df)

}

# function to save typing head() and kable() hundreds of times
display <- function(df) {
  df %>%
    head() %>%
    kable()
}

# NHS-R data ----
# 5 data sets

#- tidy data from A+E attendances
data_describer(ae_attendances)

#- tidy length of stay data
data_describer(LOS_model)

#- eng Wales mortality data
data_describer(ons_mortality)

#- synthetic data for ML models
data_describer(stranded_data)

#- synthetic early warning score data
data_describer(synthetic_news_data)

# Investigating data ----

## slice() ----
ae_attendances %>% 
  slice(1:6) %>%
  kable()

ae_attendances %>% 
  slice(1:3) %>%
  kable()

ae_attendances %>% 
  slice(2,5,918) %>%
  kable()

ae_attendances %>%
  filter(attendances >= 22000) %>%
  group_by(org_code) %>%
  slice(1) %>%
  kable()

ae_attendances %>%
  slice_max(breaches, n=6) %>%
  kable()

# equivalent to
ae_attendances %>%
  arrange(desc(breaches)) %>%
  slice(1:6) %>%
  kable()

ae_attendances %>%
  slice_sample(n=6) %>%
  kable()

# returns the last 5 rows only
ae_attendances %>% 
  slice(-1:-12760) %>%
  kable()

## glimpse ----

synthetic_news_data %>% 
  glimpse() 

# finding column classes # map_df
synthetic_news_data %>% map_df(class)

# equivalent in base R
lapply(synthetic_news_data, class)

synthetic_news_data %>% 
  glimpse() %>%
  filter(age == 71 & male == 0) %>%
  glimpse() %>%
  mutate(pulse_pres = syst-dias) %>%
  glimpse()

## rename ----
LOS_model %>%
  rename(age = Age) %>%
  display()

LOS_model %>% 
  rename_with(tolower) %>%
  display()

LOS_model %>%
  display()

## relocate ----

LOS_model %>%
  relocate(LOS) %>%
  display()

LOS_model %>%
  relocate(LOS, .after=Death) %>%
  display()

LOS_model %>%
  relocate(Death, .before=last_col()) %>%
  display()

# tidyselect ----
## contains ----
stranded_data %>%
  select(contains("care")) %>%
  display()

stranded_data %>%
  select(age, contains("care")) %>%
  display()

stranded_data %>%
  select(contains(c("care", "age"))) %>%
  display()

stranded_data %>%
  select(contains(c("age", "care"))) %>%
  display()

stranded_data %>%
  select(-contains(c("age", "care"))) %>%
  display()

## any_of ----
my_columns <- names(stranded_data)[1:4]
my_columns

stranded_data %>%
  select(any_of(my_columns)) %>%
  display()

stranded_data %>%
  select(-any_of(my_columns)) %>%
  select(-any_of(my_columns)) %>%
  display()

try(stranded_data %>%
  select(-age) %>%
  select(-age))

## everything ----

stranded_data %>% 
  select(2:5) %>%
  pivot_longer(everything()) %>%
  display()

# summarising ----

## group_by ----
ae_attendances %>% 
  group_by(period) %>%
  display()

ae_attendances %>% 
  group_by(org_code) %>%
  display()

ae_attendances %>% 
  count(breaches) %>%
  display()

ae_attendances %>% 
  group_by(org_code) %>%
  count(breaches) %>% 
  display()

ae_attendances %>% 
  group_by(org_code) %>%
  count(breaches) %>%
  arrange(desc(breaches)) %>%
  display()

ae_attendances %>% 
  group_by(org_code) %>%
  ungroup() %>%
  count(breaches) %>%
  display()

## summarise ----

ae_attendances %>% 
  summarise(sum(attendances)) %>%
  display()

ae_attendances %>% 
  group_by(org_code) %>%
  summarise(sum(attendances)) %>%
  display()

ae_attendances %>% 
  group_by(org_code) %>%
  summarise(sum(attendances)) %>%
  summarise(sum(`sum(attendances)`)) %>%
  display()

ae_attendances %>% 
  group_by(org_code) %>%
  summarise(total = sum(attendances)) %>%
  arrange(desc(total)) %>%
  display()

ae_attendances %>% 
  group_by(org_code) %>%
  summarise(non_admissions = sum(attendances - admissions)) %>%
  arrange(desc(non_admissions)) %>%
  display()

## reframe ----
sum <- ae_attendances %>%
  group_by(year = floor_date(period, unit = "year")) %>%
  summarise(
    year = year(year),
    non_admissions = sum(attendances - admissions)
  )

ref <- ae_attendances %>%
  group_by(year = floor_date(period, unit = "year")) %>%
  reframe(
    year = year(year),
    non_admissions = sum(attendances - admissions)
  )

waldo::compare(sum, ref)

## count and tally ----
synthetic_news_data %>%
  count(died) %>%
  display()

synthetic_news_data %>% 
  group_by(died) %>% 
  summarise(n = n())  %>%
  display()

synthetic_news_data %>% 
  # group_by(died) %>% 
  summarise(n = n())  %>%
  display()

synthetic_news_data %>%
  tally() %>%
  display()

synthetic_news_data %>%
  tally(age) %>%
  display()


synthetic_news_data %>%
  pull(age) %>%
  sum()

synthetic_news_data %>%
  count(syst, sort=T) %>%
  display()

### add_ variants ----
synthetic_news_data %>%
  add_count(syst, name="syst_BP_count") %>%
  select(syst, last_col()) %>%
  display()

synthetic_news_data %>%
  group_by(died) %>%
  add_tally() %>%
  slice(1:3) %>%
  display()


## rowwise ----

# to find the daily mean of attendances, breaches, and admissions
ae_attendances %>% 
  rowwise() %>%
  mutate(mean = mean(c(attendances, breaches, admissions))) %>%
  display()

ae_attendances %>% 
  mutate(mean=mean(c(attendances, breaches, admissions))) %>%
  display()

## ae_attendances %>%
##   rowwise() %>%
##   mutate(mean = mean(c_across(4:6))) %>%
##   display()




# joins ----

## binding ----

simple_a <- tribble(
  ~category, ~value, ~key,
  "a",5,3,
  "a",2,7,
  "a",4,2,
  "a",7,1,
  "a",9,1
  )

kable(simple_a)

simple_b <- tribble(
  ~category, ~value, ~key,
  "b",2,5,
  "b",7,2,
  "b",3,2,
  "b",2,8,
  "b",2,14
  )

kable(simple_b)

simple_a %>%
  bind_rows(simple_b) %>%
  kable()

simple_a %>%
  rename("wrong_name" = "value") %>%
  bind_rows(simple_b) %>%
  kable()

simple_a %>%
  bind_rows(simple_b, .id="ID") %>%
  kable()

bind_rows(list(simple_a, simple_b)) %>%
  kable()

simple_a %>%
  bind_cols(simple_b, .name_repair="universal") %>%
  kable()


## mutating joins ----

simple_a %>%
  inner_join(simple_b, by="key") %>%
  display()


simple_a %>%
  left_join(simple_b, by="key", suffix = c(".a", ".b")) %>%
  display()

simple_a %>%
  right_join(simple_b, by="key", keep=T) %>%
  display()

simple_a %>%
  full_join(simple_b, by="key") %>%
  display()

simple_a %>%
  anti_join(simple_b, by="key") %>%
  display()