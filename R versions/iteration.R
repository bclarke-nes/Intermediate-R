install.packages(setdiff("pacman", rownames(installed.packages())))
library("pacman")
p_load(tidyr, ggplot2, dplyr, NHSRdatasets, glue, microbenchmark)

for (i in seq_along(1:10)) {
  cat(glue("{i} potato, "))
}

# introduction ----
setequal(3, 3)
setequal(3, 4)

setequal(synthetic_news_data[["age"]], synthetic_news_data[[2]])

setequal(synthetic_news_data %>% select(age) %>% pull(), synthetic_news_data[[2]])

# for-loops ---

word <- ""

x <- 1:10

for (i in seq_along(x)) {
  word[[i]] <- "iteration is"
}

cat(word)


word <- ""

x <- 1:10

for (i in seq_along(x)) {
  word[[i]] <- glue("iteration {i}")
}

cat(word)

words <- tibble(iteration="",contents="")

x <- 1:5
for (i in seq_along(x)) {
  words[i, 1] <- as.character(i)
  words[i, 2] <- glue("iteration {i}")
}
words %>%
  knitr::kable()

# output ----
mbm <- microbenchmark(multiplication={2*2*2}, addition = {2+2+2+2}, power = 2^3)
autoplot(mbm)

mbm <- microbenchmark(loop={
  
  x<-1:1000
  output <- vector("integer", length(x))
  
  for (i in seq_along(x)) {
    output[[i]] <- i^2
  }

})

autoplot(mbm)

mbm <- microbenchmark(loop={
  
  x<-1:1000
  output <- vector("integer", length(x)) # an empty vector the same length as our intended output

  for (i in seq_along(x)) {
    output[[i]] <- i^2
  }

}, loop_short_output={

  x<-1:1000
  output_2 <- vector("integer", 0) # just an empty vector

  for (i in seq_along(x)) {
    output_2 <- c(output_2, i^2)
  }

})

autoplot(mbm)

setequal(output, output_2)

# sequences ----
c(1,2,3,4)
1:10

rep(c(1:5), 5)

rep_len(letters, length.out=100)

letters 
LETTERS
month.abb

sort(unique(ae_attendances$org_code))[1:10] # giving a sorted list of the first 10 values in a column

colnames(ae_attendances) # giving column names

synthetic_news_data  %>%
  distinct(NEWS) %>%
  pull() %>%
  sort()

seq_along(synthetic_news_data) # cycles through the columns of a tibble

seq_along(letters) # seq_along() also works with vectors etc.

names(synthetic_news_data) # cycles through the sequence by column name (tibbles / data frames only)

t(synthetic_news_data) %>%
  rownames()

# while loops ----

bottles <- 10

while (bottles >= 1) {
cat(glue("{bottles} green bottles...  \n"))
bottles = bottles - 1
}

# vectorised functions ----

cube <- function(num) {num ^3}

cube_inputs <- c(3,6,5,4,3)
cube(cube_inputs)

conditional_cube <- function(num) {
  if(num %% 2 == 0) {
    num ^ 3 }
  else {
    num ^ 2 }
}

conditional_cube(3) # should square the input for odd numbers
conditional_cube(4) # should cube the input for even numbers

try(conditional_cube(cube_inputs))

vector_conditional_cube <- Vectorize(conditional_cube)
vector_conditional_cube(cube_inputs)

ifelse_conditional_cube <- function(num){
  ifelse(num %% 2 == 0, num^3, num^2 )
}

ifelse_conditional_cube(cube_inputs)

cube_outputs <- vector("double", length(cube_inputs))

for(i in seq_along(cube_inputs)) {
  cube_outputs[[i]] <- conditional_cube(cube_inputs[[i]])
}

cube_outputs

perd <- function(x) {
  glue("This value here is {x}")
}

synthetic_news_data %>%
  select(1:3) %>%
  mutate(age = perd(age))

synthetic_news_data %>%
  mutate(across(everything(), perd))

perd_v <- function(var_name, x) {
  glue("This value of {var_name} here is {x}")
}

synthetic_news_data %>%
  mutate(across(everything(), ~ perd_v(cur_column(),.)))

output <- synthetic_news_data
x <- names(output)

for (i in seq_along(x)) {
  output[[i]] <- perd_v(names(output[i]), output[[i]])
}
glimpse(output[1,])

# breaks and next ----

random_sample <- sample(seq(from=1, to=340, by=sample(1:10, 1, replace=T)), 10)

threshold_value <- 300

for(i in random_sample) {
  cat(glue("{i  }, "))
}

for(i in random_sample) {
  if(i > threshold_value) break
  cat(glue("{i  }, "))
}

for(i in 1:5) {
  for (j in 1:5) {
  cat(glue("{i+j} "))
    if(i+ j >= 7) break # breaks the inner loop at 7 each time
  }
      cat("  \n")
}

for(i in 1:20) {
  if(i %% 15 == 0) {
    cat("fizzbuzz, ")
    next
  }
  if(i %% 3 == 0) {
    cat("fizz, ")
    next
  }
  if(i %% 5 == 0) {
    cat("buzz, ")
    next
  }
cat(glue("{i}, "))
}

ae_attendances %>%
  slice(1:5) %>%
  knitr::kable()

# rmarkdown with loops ----

# note that our loop just displays things in our document, so we don't bother to create an output here

# create a vector of sample orgs to simplify the for() call and filter() indirection later on
orgs_seq <- as.character(sample(ae_attendances %>% 
                                  distinct(org_code) %>% 
                                  arrange(org_code) %>% 
                                  pull(), 20)
                         ) 

# print a single header message outside the body of the loop telling the reader what to expect
cat(glue("In this section we present a summary of admissions data, grouped by type of admission, for a sample of {length(orgs_seq)} organisations.  \n  \n")) 

# set up the main body of the loop
for (i in seq_along(orgs_seq)) {
  
    # calculating the total admissions for our org - we use this in the explanatory text, and in the graph
    org_total_admissions <- ae_attendances %>%
            filter(org_code == orgs_seq[i]) %>%
            summarise(sum(attendances)) %>%
            pull()
    
    # customised 4th level header containing org_code
    cat("  \n")
    cat(glue("  \n  \n#### {orgs_seq[i]} attendances  \n  \n"))
    cat("  \n")
    
    # customised explanatory text
    cat(glue("Some explanatory text: {orgs_seq[i]} had a total of {org_total_admissions} admissions.  \n"))
    
    # use kable() to display a summary of the data
    print(ae_attendances %>%
      drop_na(org_code) %>%
      filter(org_code == orgs_seq[i]) %>%
      group_by(type) %>%
      summarise(attendances = sum(attendances)) %>%
      knitr::kable()
      )
    
    cat("  \n") # otherwise the graph won't print - see https://github.com/yihui/knitr/issues/886
    
    # histogram of admissions across orgs, for comparison
    print(ae_attendances %>%
      group_by(org_code) %>%
      summarise(attendances = sum(attendances)) %>%
      ggplot() +
      geom_histogram(aes(x=attendances), bins=10) +
      geom_vline(xintercept=org_total_admissions) +
      xlab("Total admissions") +
      ylab("n orgs") +
      geom_label(aes(x=org_total_admissions, y=60), label = paste0(org_total_admissions, " total"), nudge_x = 0.35, size = 3)) + # label showing value of admissions for this org
      xlim(0,2000000) #little cheat to keep the label from being cut off for very high values

    cat("  \n") # absolutely essential - see https://github.com/yihui/knitr/issues/886
}