install.packages(setdiff("pacman", rownames(installed.packages())))
library("pacman")
p_load(purrr, tidyr, ggplot2,dplyr,lubridate, stringr,NHSRdatasets, rlang, glue, knitr)

# function to display head() using kable()
display <- function(df) {
  if(nrow(df) == 0 | ncol(df) == 0) {cat("No data to display")}
  else{
    df %>%
    head() %>%
    kable()
  }
}

# tidyselect ----

## logical operators ----
stranded_data %>%
  display()


stranded_data %>%
  select(age:hcop) %>%
  display()

stranded_data %>%
  select(2:5) %>%
  display()

stranded_data %>%
  select(!stranded.label) %>%
  display()

stranded_data %>%
  select(!c(age, stranded.label)) %>%
  display()

stranded_data %>%
  select(stranded.label, hcop, age) %>%
  display()

stranded_data %>%
  select(starts_with("m") & ends_with("e")) %>%
  display()


## pattern matching ----

stranded_data %>%
  select(starts_with("care")) %>%
  display()

stranded_data %>%
  select(starts_with("Care"))  %>%
  display()

stranded_data %>%
  select(starts_with("Care", ignore.case = FALSE))  %>%
  display()

stranded_data %>%
  select(age, ends_with("care")) %>%
  display()

stranded_data %>%
  select(ends_with(c("label", "safe", "care"))) %>%
  display()

stranded_data %>%
  select(ends_with(c("care", "label", "safe"))) %>%
  display()

stranded_data %>%
  select(age, contains("care")) %>%
  display()

stranded_data %>%
  select(matches("me[dn]"))  %>%
  display()

stranded_data %>%
  select(matches("^[am]"))  %>%
  display()

stranded_data %>%
  select(matches("me."))  %>%
  display()

stranded_data %>%
  select(matches("\\."))  %>%
  display()


# using the man page example:
data(billboard)

billboard %>%
  dplyr::select(num_range("wk", 10:15)) %>%
  display()

ons_mortality %>%
  pivot_wider(names_from = week_no, values_from = counts, names_prefix = "week_") %>%
  select(1:12) %>%
  display()

ons_mortality %>%
  pivot_wider(names_from = week_no, values_from = counts, names_prefix = "week_") %>%
  select(1:12) %>%
  select(num_range("week_", 1:9)) %>%
  display()

ons_mortality %>%
  pivot_wider(names_from = week_no, values_from = counts, names_prefix = "week_0") %>%
  select(num_range("week_", 1:9, width=2)) %>%
  display()

## matching character vectors ----

my_columns <- names(stranded_data)[1:4]
my_columns

stranded_data %>%
  select(all_of(my_columns)) %>%
  display()

my_columns <- c(my_columns, "smoking_status")
my_columns

try(stranded_data %>%
  select(all_of(my_columns)) %>%
  display())

stranded_data %>%
  select(any_of(my_columns)) %>%
  display()

stranded_data %>%
  select(-any_of(my_columns)) %>%
  select(-any_of(my_columns)) %>%
  display()

## selection helpers for specific columns ----

mtcars %>%
  display()

mtcars %>% 
  pivot_longer(everything()) %>%
  display()

mtcars %>% 
  pivot_longer(everything()) %>%
  ggplot() +
  geom_histogram(aes(x=value, fill=name)) +
  facet_wrap(~ name, scales="free") + 
  theme(legend.position="none")

LOS_model %>%
  mutate(Organisation = as.integer(str_replace_all(Organisation, "Trust", ""))) %>%
  pivot_longer(everything()) %>%
  display()

LOS_model %>%
  select(Age, LOS) %>%
  pivot_longer(everything()) %>%
  ggplot() +
  geom_histogram(aes(x=value, fill=name)) +
  facet_wrap(~ name, scales="free") + 
  theme(legend.position="none")

stranded_data %>%
  select(last_col()) %>%
  display()

stranded_data %>%
  select(last_col(2)) %>% # selecting the column two before the last column
  display()

stranded_data %>%
  select(age:last_col(3)) %>%
  display()

stranded_data %>%
  mutate(age_months = age * 12) %>%
  select(last_col(1), last_col()) %>%
  display()

## where----
is.character(stranded_data$stranded.label)

# select all the character columns from stranded_data:
stranded_data %>% 
  select(where(is.character)) %>%
  display()


# data masking ----

## introduction ----

stranded_data %>% 
  select(age) %>%
  display()

new_stranded_data <- stranded_data %>%
  select(stranded.label, age)

my_cols <- c("age", "care.home.referral", "medicallysafe")

stranded_data %>%
  select(any_of(my_cols)) %>%
  display()

column_displayer <- function(col_name) {
  stranded_data %>%
  select(any_of(col_name)) %>%
  display()
}

try(column_displayer(age))

column_displayer("age")

## backgroud ----

### embracing ----
ae_attendances %>%
  filter(breaches > 100) %>%
  pull(attendances) %>%
  mean() %>%
  round(2)

ae_means <- function(colname) {
  ae_attendances %>%
    filter(breaches > 100) %>%
    pull(colname) %>%
    mean() %>%
    round(2)
}

try(ae_means(breaches))

ae_means <- function(colname) {
  ae_attendances %>%
    filter(breaches > 100) %>%
    pull({{colname}}) %>%
    mean() %>%
    round(2)
}

ae_means(breaches)
ae_means(admissions)

map(ae_attendances %>% select(where(is.numeric)) %>% names(), ae_means)

testo <- expand_grid(c("breaches", "admissions", "attendances"), c("attendances"), 5^(0:3))

### pronouns ----

variable <- c("type")

try(ae_attendances %>%
  count(variable) %>%
  display())

ae_attendances %>%
  count(.data[[variable]]) %>% 
  display()

attendances <- 800

ae_attendances %>%
  filter(breaches >= attendances) %>% 
  display()

ae_attendances %>%
  filter(.data[["breaches"]] >= .env[["attendances"]]) %>% 
  arrange(breaches) %>%
  display()

### injection ----
col_means <- function(column, cutoff) {

ae_attendances %>%
  filter({{column}} > {{cutoff}}) %>%
  group_by(type) %>%
  summarise("mean_{column}" := round(mean(.data[[column]]), 1)) %>% 
  display()
}

column <- c("breaches")
cutoff <- 400

cat(paste0("This is how we'd include the column (", column, "), and the cutoff (", cutoff, ") in Rmarkdown using `paste0`  \n  \n"))

cat(glue("This is how we'd include the column ({column}), and the cutoff ({cutoff}) in Rmarkdown using `glue`")) # easier to read

col_means("attendances", 400)

new_column_name <- c("Steve")

ae_attendances %>% 
  mutate("{new_column_name}" := round(attendances ^ 0.5, 2) ) %>%
  display()

ae_attendances %>% 
  rename("{new_column_name}" := attendances) %>%
  display()

ae_attendances %>% 
  group_by(org_code) %>%
  summarise("{new_column_name}" := n()) %>%
  arrange(.data[[new_column_name]]) %>%
  slice(1:10) %>%
  ggplot() +
  geom_col(aes(x=org_code, y=.data[[new_column_name]])) +
  ggtitle(glue("{new_column_name} by org_code"))
  
### quasiquotation ----

quoted_variable <- "Steve"

ae_attendances %>% 
  rename("{quoted_variable}" := attendances) %>%
  display()

ae_attendances %>% 
  rename("{quoted_variable}" := attendances) %>%
  select(!!quoted_variable) %>%
  display()

distinct_entries <- function(df, col_name){
  
  cat(glue("#### Results for {col_name}:  \n  \n")) # using glue syntax
  
  df %>% 
    select(!!sym(col_name)) %>% # using quasiquotation to select the supplied column
    rename("distinct_{col_name}" := col_name) %>% # using injection to rename the column
    distinct() %>%
    display()
}

distinct_entries(ae_attendances, "org_code")