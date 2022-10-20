![](img/header.png) Intermediate R
================
Brendan Clarke
2022-10-22

# Intermediate R

## Introduction

This training course is an introduction to programming in R, with
particular emphasis on using [tidyverse](https://www.tidyverse.org/)
functions effectively. It is designed with data analysts working in
health, care, and housing in mind, and has a slight bias towards
[Rmarkdown](https://rmarkdown.rstudio.com/) as a likely endpoint of the
programming activities that it teaches.

The training expects you to have some basic familiarity with R and
Rmarkdown, including the [tidyverse](https://www.tidyverse.org/). While
the expectations are fairly low - you absolutely don’t need to be an R
expert to successfully complete this training - you should have some
previous experience using R. As the course has a tidyverse flavour,
trainees should have some familiarity with using core tidyverse
functions (dplyr especially).

Some self-assessment questions might be useful to calibrate these
expectations. Are you able to answer these questions fairly rapidly and
with confidence?

- what’s a tibble?
- what function would you use to load a .csv file?
- how would you make a new column in a tibble from some other column(s)?
- how would you draw a simple graph using `ggplot()`?

If you were able to answer these fluently, then great, this training is
meant for you. If they were a bit beyond your previous experience, then
you might like to enroll on the KIND Learning Network training
[Introductory R and Rmarkdown](https://learn.nes.nhs.scot/62249) as a
first step in your R journey.

## What this training will cover

This is a modular training course split into five sessions. Each session
runs as a small-group training session live on MS Teams, and consists of
a mixture of explanation, interaction, and live coding. The session
topics are:

### 1. Getting the best out of `dplyr`

Going beyond the basics of `dplyr` (`filter()`, `select()`, `mutate()`).
Using the `NHSRdatasets` as a model system for health and social care
analysis, this session introduces dplyr tools for investigating data,
gives an introduction to tidyselect, and provides several means of
summarising and joining data.

### 2. Functions

An introduction to writing functions in R. Includes advice about
translating existing code into functions, and brief primers on
indirection for tidyverse functions, conditional execution, and
dot-dot-dot (…).

### 3. Iteration

An introduction to the often-maligned for-loop in R. Also includes
training on vectorised and non-vectorised functions, and gives some
examples of using for-loops to create Rmarkdown.

### 4. The `purrr` package

An introduction and walk-through of the `purrr` package, showing how to
apply functions to several different kinds of object in R.

### 5. tidy evaluation

Tidy evaluation is a tidyverse-specific approach to programming. This
session introduces two different elements of tidy evaluation:
**tidyselect**, which are tidyverse-specific tools for working with
columns, and **data masking**, which are approaches to resolving
problems encountered when using tidyverse functions in more advanced
ways.

While the course as a whole has been designed to offer a logical
progression from the first to the last session, you can take any
combination of the sessions in any order. Of necessity, there is a bit
of overlap between sessions, but in general they are independent and can
be mixed-and-matched to suit your interests.

## Getting started

### How does it work?

The training is delivered as a series of live Teams session using [Posit
Cloud](https://rstudio.cloud/) (previously known as Rstudio Cloud).

If you’ve never used Posit Cloud before, it’s a
[SaaS](https://en.wikipedia.org/wiki/Software_as_a_service) version of
Rstudio in the browser. Note that because it’s a web service, it
requires you to upload your data to their servers, which might makes it
unsuitable for production work in health and care owing to information
governance concerns. That said, it’s an excellent venue for training,
because it solves many of the difficulties regarding R versions,
permissions, etc that are a feature of using R on the desktop. It’s also
very easy to transfer projects from Posit Cloud to an installed version
of R, so don’t worry that what you learn here will be tied to the cloud
forever.

### What you’ll need

As Posit Cloud is a web service, you don’t need a particularly
up-to-date computer to completed this training. As long as you have a
reliable internet connection, and are capable of making a video call
with Microsoft Teams (for the face-to-face part of the training), then
you should be fine. The demonstration has been tested on Windows 10,
Windows 11, and Ubuntu Linux 21.04 without platform-specific
difficulties.

It is extremely helpful, although not essential, to have a multi-monitor
setup. That way you can run the demonstration in one screen, and the
Teams call on the other.

You should also, as discussed above, be fairly comfortable with R basics
before you enrol on this training. Do please get in touch with [Brendan
Clarke](mailto:%20brendan.clarke2@nhs.scot?subject=Intermediate%20R%20training)
to discuss if you are at all unsure.

### Joining instructions

You’ll need to do a little bit of preparation before the first training
session, which should take about 15 minutes to complete. Please make
sure you have completed this before the start of the first session so
that we can make a prompt start. If you’re new to Posit Cloud, please
follow the step-by-step instructions below. If you’ve worked with Posit
Cloud before, you can just sign-in to your account at Posit Cloud, and
create a new project from the GitHub Repository at
<https://github.com/bclarke-nes/Intermediate-R>.

It is also possible to complete the training using the desktop version
of Rstudio. This training was written using R 4.2.1, and you may
encounter some difficulties related to versions if your installed R is
very different from this.

#### Step-by-step instructions

1.  Go to <https://rstudio.cloud/>
2.  If you have an account, you can log in as normal. Otherwise, please
    create a new account by selecting Get started for free, following
    the steps, and then signing-in
3.  Once you’ve signed-in to Posit Cloud, add a new project by clicking
    New Project \>\> New Project from a Git repository. When prompted,
    enter the URL <https://github.com/bclarke-nes/Intermediate-R>
4.  That will give you a new project containing the files needed for
    this training

## Aims and objectives

### Aims

- To introduce a series of R/tidyverse tools that are particularly
  helpful to analysts moving beyond R basics
- To provide a social learning space to support learners as they develop
  their skills into these more challenging areas
- To show how more advanced R techniques can be used in real-world data
  analysis in health and social care

### Objectives

By the end of these session, the user should:

- Have developed their understanding of functions, iteration, dplyr,
  purrr, and tidyevaluation
- Have connected these new functions to their own work
- Have participated in social learning during the training sessions,
  reviewing the code of other trainees
- Have confidence in reading third-party code that uses some of these
  advanced R techniques
