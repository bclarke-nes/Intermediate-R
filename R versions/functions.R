install.packages(setdiff("pacman", rownames(installed.packages())))
library("pacman")
p_load(tidyr,readr,DiagrammeR,ggplot2,dplyr,lubridate,NHSRplotthedots,readxl,stringr,NHSRdatasets, purrr, rlang,glue, knitr)

# introduction ----
ae_attendances %>%
  filter(org_code == "RF4" & type == "1") %>%
  ggplot() +
  geom_col(aes(x=period, y=breaches)) +
  labs(title="RF4 breaches by month") + xlab("") + ylab("") +
  theme(axis.ticks.y = element_blank())

ae_attendances %>%
  filter(org_code == "RJF" & type == "1") %>%
  ggplot() +
  geom_col(aes(x=period, y=breaches)) +
  labs(title="RJF breaches by month") + xlab("") + ylab("") +
  theme(axis.ticks.y = element_blank())

ae_attendances %>%
  filter(org_code == "RBN" & type == "1") %>%
  ggplot() +
  geom_col(aes(x=period, y=breaches)) +
  labs(title="RBN breaches by month") + xlab("") + ylab("") +
  theme(axis.ticks.y = element_blank())

ae_attendances %>%
  filter(org_code == "RTR" & type == "1") %>%
  ggplot() +
  geom_col(aes(x=period, y=breaches)) +
  labs(title="RTR breaches by month") + xlab("") + ylab("") +
  theme(axis.ticks.y = element_blank())

ae_spark <- function(org, var) { # new code
  var <- sym(var) # turn the string containing the variable into a symbol so that we can use it inside aes()

  ae_attendances %>% # same
    filter(org_code == org & type == "1") %>% # replaced "RF4" with the org argument 
    ggplot() + # same
    geom_col(aes(x=period, y=!!var)) + # the !! unquotes the var symbol so that aes() recognises it
    labs(title=glue("{org} {var} by month")) + xlab("") + ylab("") +
    theme(axis.ticks.y = element_blank()) # using glue() to paste together the org and var to print the title
}

ae_spark("RBN", "breaches")
ae_spark("RTR", "attendances")

# a first function ----
my_fun <- function(user_name) {
  cat(glue("**Hello {user_name}, welcome to the wonderful world of functions**  \n  \n"))
}

my_fun("Susan")

# using functions ----
my_fun <- function(user_name) {
  cat(glue("**Hello {user_name}, welcome to the wonderful world of functions**  \n  \n"))
}

my_fun("Susan")

range_o_matic <- function() {
  ae_attendances %>%
  pull(admissions) %>%
  range()
}

## translating code into functions ----

raw_data <- c(2,7,6,4,5,8,6,3,4,5,6)  # our numbers
mean_raw_data <- mean(raw_data)       # the mean of all the numbers
sum(abs(raw_data - mean_raw_data))    # find the sum of differences from mean


diff_sum <- function() {

  raw_data <- c(3,11,8,4,7,5,2,9,6,3,45,2)  # our numbers
  mean_raw_data <- mean(raw_data)           # the mean of all the numbers
  sum(abs(raw_data - mean_raw_data))        # find the sum of differences from mean

  }

diff_sum <- function(data) {
                                        # nothing here now
  mean_raw_data <- mean(raw_data)       # the mean of all the numbers
  sum(abs(raw_data - mean_raw_data))    # find the sum of differences from mean

}

diff_sum <- function(data) {

  mean_raw_data <- mean(data)     # the mean of all the numbers
  sum(abs(data - mean_raw_data))  # find the sum of differences from mean

}

diff_sum(c(8,4,1,11,7,5,-17,12,-4,2,9,6,3,45,2))

# 1-argument functions -----

ae_attendances %>% 
    filter(admissions < 5 & breaches >= 400 & org_code == "RJ2") %>%
    group_by(year = year(period), month = month(period)) %>%
    arrange(year, month) %>%
    ggplot() +
    geom_col(aes(x=period, y=attendances)) +
    labs(title= "RJ2 admissions for months with < 5 admissions and > 400 breaches") +
    scale_x_date(limits=ymd("2016-04-01", "2019-03-01"))

# example of single argument passed to function
admissions_graph <- function(org_code_arg) {
  ae_attendances %>% 
    filter(admissions < 5 & breaches >= 400 & org_code == {{org_code_arg}}) %>% # note {{}}
    group_by(year = year(period), month = month(period)) %>%
    arrange(year, month) %>%
    ggplot() +
    geom_col(aes(x=period, y=attendances)) +
    labs(title= "RJ2 admissions for months with < 5 admissions and > 400 breaches") +
    scale_x_date(limits=ymd("2016-04-01", "2019-03-01"))
}

admissions_graph("RJ2")

# example of single argument passed to function
admissions_graph <- function(org_code_arg) {
  ae_attendances %>% 
    filter(admissions < 5 & breaches >= 400 & org_code == {{org_code_arg}}) %>%
    group_by(year = year(period), month = month(period)) %>%
    arrange(year, month) %>%
    ggplot() +
    geom_col(aes(x=period, y=attendances)) +
    labs(title= glue("{org_code_arg} admissions for months with < 5 admissions and > 400 breaches")) +
    scale_x_date(limits=ymd("2016-04-01", "2019-03-01"))
}

admissions_graph("RJ2")

admissions_graph("NLO10")

# conditional execution -----

14 %% 2 == 0
13 %% 2 == 0

if(14 %% 2 == 0){
  print("Even")
} else {
  print("Odd")
}

is_even <- function(n){
  
  if(n %% 2 == 0){
  print(glue("{n} is even"))
} else {
  print(glue("{n} is odd"))
}
  
}
is_even(14)
is_even(11)
is_even(-898543143130)

admissions_graph <- function(org_code_arg) {
  
  if (nrow(ae_attendances %>% filter(admissions < 5 & breaches >= 400 & org_code == {{org_code_arg}})) == 0) {
    
    cat(glue("No data exists for {org_code_arg}.  \n"))
  
    } else{
  
      cat(glue("  \n  \n### Results for {org_code_arg}  \n  \n"))
  
  print(ae_attendances %>% 
    filter(admissions < 5 & breaches >= 400 & org_code == {{org_code_arg}}) %>%
    group_by(year = year(period), month = month(period)) %>%
    mutate(end = period+days_in_month(month)-1) %>%
    arrange(year, month) %>%
    ggplot() +
    geom_rect(aes(xmin=period, xmax=end, ymin=0, ymax=attendances)) +
    labs(title= glue("{org_code_arg} admissions for months with < 5 admissions and > 400 breaches")) +
    scale_x_date(limits=ymd("2016-04-01", "2019-03-01")) )
  cat('  \n  \n')
    }
}

admissions_graph("NLO10")
admissions_graph("RJ2")

# dot dot dot ----

auto_personaliser <- function(name, org) {
  cat(glue('Report generated by {name} from {org} on {format(Sys.time(), "%d-%b-%Y")}  \n  \n'))
}

auto_personaliser("Steve", "NSS")

graph_caption <- function() {
  cat("  \n **Disclaimer** All rights reserved. Any similarity to actual persons, living or dead, is purely coincidental.  \n")
}

graph_caption()

graph_caption <- function() {
  cat("  \n **Disclaimer** All rights reserved. Any similarity to actual persons, living or dead, is purely coincidental.  \n")
  cat(glue('  \n  {auto_personaliser("Steve", "NSS")}'))
}

graph_caption()

graph_caption <- function(...) {
  cat("  \n **Disclaimer** All rights reserved. Any similarity to actual persons, living or dead, is purely coincidental.  \n")
  cat(glue('  \n  {auto_personaliser(...)}'))
}

graph_caption("Susan", "NHS 24")

authors <- tribble(
  ~name, ~org,
  "Steve", "Y03082",
  "Susan", "RJ2",
  "Ameet", "NR3",
  "Eilidh", "RN3"
)

full_graph <- function(name, org) {
  admissions_graph(org)
  graph_caption(name, org)
}

walk2(authors$name, authors$org, full_graph)