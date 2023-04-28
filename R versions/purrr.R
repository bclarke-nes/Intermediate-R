install.packages(setdiff("pacman", rownames(installed.packages())))
library("pacman")
p_load(tidyr,readr,ggplot2,dplyr,lubridate,NHSRplotthedots,readxl,stringr,NHSRdatasets, purrr, rlang, glue, knitr)

# introduction ----
function_name <- function(argument1, argument2) {
  
  #some code here, using the two arguments
  argument1+argument2
  
}



function_name(8,11)

function_name(8,11) * function_name(9,105)

non_admissions <- function(single_org_code) {
  ae_attendances %>%
    filter(org_code == {{single_org_code}}) %>%
    group_by(org_code, year=floor_date(period, unit="year")) %>%
    summarise(non_admissions = sum(attendances-admissions), year=unique(year(year)))
}

non_admissions("AJN")

non_admissions("AJN") %>%
  kable()

# map introduction ----

short_org_codes <- ae_attendances %>%
  distinct(org_code) %>%
  arrange(org_code) %>%
  slice(80:82) %>%
  pull()

purrr::map(.x = short_org_codes, .f = non_admissions)

daft_codes <- map(.x = short_org_codes, ~ glue("ThIs OrG CoDe iS {.x}  \n  \n"))
daft_codes

daft_codes <- map(.x = short_org_codes, \(x) glue("ThIs OrG CoDe iS {x}  \n  \n"))
daft_codes

class(daft_codes)

summary(daft_codes)

daft_codes[1] # returns everything in the first sublist
daft_codes[[1]] # returns just the first item in the first sublist

for(i in seq_along(1:length(daft_codes))) {
  cat(daft_codes[[i]])
}

# map_lgl, map_int... ----

try(map_dbl(short_org_codes, non_admissions))

non_admissions_v <- function(single_org_code) {
  non_admissions(single_org_code) %>% 
    filter(year == "2018") %>% 
    pull() %>% 
    sum()
  }

#dbl for doubles - ordinary decimal numbers
map_dbl(short_org_codes, non_admissions_v)

non_admissions_c <- function(...) {
  non_admissions_v(...) %>%
    as.character()
  }

#chr for character
map_chr(short_org_codes, non_admissions_c)

non_admissions_int <- function(...) {
  non_admissions_v(...) %>%
    as.integer()    # to convert the result to integer
}

#int for integers
map_int(short_org_codes, non_admissions_int)

#lgl for logical (true/false)
non_admissions_lgl <- function(...) {
  result <- non_admissions_v(...)
  
  result[1] %% 2 == 0 # to test if the answer is even
}

map_lgl(short_org_codes, non_admissions_lgl)

## map_vec ----
map_vec(short_org_codes, non_admissions_v)
map_vec(short_org_codes, non_admissions_c)
map_vec(short_org_codes, non_admissions_int)
map_vec(short_org_codes, non_admissions_lgl)

## list_rbind() ----
map(short_org_codes, non_admissions) %>% 
  list_rbind() %>%
  head() %>% 
  kable()

#the simple version
files <- list.files("./data/sign_up_1", full.names = T)

files %>% 
  map(read_csv) %>%
  list_rbind() %>%
  head() %>%
  kable()

# the slightly more involved version, wrapping read_csv into a new function to allow indexing
labeller <- function(file) {
  read_csv(file, id="file_name")
}

files %>% 
  map(labeller) %>%
  list_rbind() %>%
  head() %>%
  kable()

random_nonsense <- function(name) {
  tibble({{name}} := rnorm(1:5))
}

map(letters[1:7], random_nonsense) %>% 
  list_cbind() %>%
  kable()

files %>% 
  map(labeller) %>%
  list_rbind() %>%
  pivot_wider(id_cols = c(email, reg_date), names_from = file_name, values_from = consent) %>%
  head() %>%
  kable()


# walk ----

walk(short_org_codes, ~ cat(glue("\n+ ThIs OrG CoDe iS {.x}\n\n")))

admission_graphs <- function(org_code_arg) {
  
  if (nrow(ae_attendances %>% filter(admissions < 5 & breaches >= 400 & org_code == {{org_code_arg}})) == 0) {
    
    cat(glue("  \n  \nNo data exists for {org_code_arg}.  \n"))
  
    } else{
  
      cat(glue("  \n  \n### Results for {org_code_arg}  \n  \n"))
  
  print(ae_attendances %>% 
    filter(admissions < 5 & breaches >= 400 & org_code == {{org_code_arg}}) %>%
    group_by(year = year(period), month = month(period)) %>%
    mutate(end = period+days_in_month(month)-1) %>%
    arrange(year, month) %>%
    ggplot() +
    geom_rect(aes(xmin=period, xmax=end, ymin=0, ymax=attendances)) +
    labs(title = glue("{org_code_arg} admissions for months with < 5 admissions and > 400 breaches")) +
    scale_x_date(limits=ymd("2016-04-01", "2019-03-01")) )
  cat('  \n  \n')
    }
}

admission_graphs("NLO10")

admission_graphs("RJ2")

walk(short_org_codes, admission_graphs)

## map2 ----
non_admissions_year <- function(single_org_code, year) {
  non_admissions(single_org_code) %>% 
    filter(year == {{year}}) %>% 
    pull() %>% 
    sum()
  }

non_admissions_year("RF4", "2018")

map2(short_org_codes, "2018", non_admissions_year)

try(list.files("./data/sign_up_2", full.names = T) %>%
      map(read_csv) %>%
      list_rbind() %>%
      head() %>%
      kable()
)

labeller_skips <- function(file, skip_rows=0) {
  read_csv(file, id="file_name", skip=skip_rows)
}

list.files("./data/sign_up_2", full.names = T) %>% 
  map2(.y=1, labeller_skips) %>%
  list_rbind() %>%
  head() %>%
  kable()


list.files("./data/sign_up_2", full.names = T) %>% 
  map2(.y=1, labeller_skips) %>%
  list_rbind() %>%
  mutate(reg_week = floor_date(dmy(reg_date), unit="weeks")) %>%
  group_by(reg_week) %>%
  summarise(signups = n(), month=unique(file_name)) %>%
  ggplot() +
  geom_col(aes(x=reg_week, y=signups, fill=month)) +
  xlab("Registration week") +
  ylab("New signups") +
  theme(legend.position="none")

## walk2() ----

admission_graphs_breaches <- function(org_code_arg, breach_count) {
  
  if (nrow(ae_attendances %>% filter(admissions < 5 & breaches >= {{breach_count}} & org_code == {{org_code_arg}})) == 0) {
    
    cat(glue("  \n  \nNo data exists for {org_code_arg}.  \n"))
  
    } else{
  
      cat(glue("  \n  \n### Results for {org_code_arg}  \n  \n"))
  
  print(ae_attendances %>% 
    filter(admissions < 5 & breaches >= {{breach_count}} & org_code == {{org_code_arg}}) %>%
    group_by(year = year(period), month = month(period)) %>%
    mutate(end = period+days_in_month(month)-1) %>%
    arrange(year, month) %>%
    ggplot() +
    geom_rect(aes(xmin=period, xmax=end, ymin=0, ymax=attendances)) +
    labs(title= glue("{org_code_arg} admissions for months with < 5 admissions and > {breach_count} breaches")) +
    scale_x_date(limits=ymd("2016-04-01", "2019-03-01")) )
  cat('  \n  \n')
    }
}

admission_graphs_breaches("R1H", 75)

walk2(.x="R1H", .y=c(10,20,50,100,150),admission_graphs_breaches )