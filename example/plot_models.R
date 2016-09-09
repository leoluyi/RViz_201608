#' ---
#' title: "Plotting Models"
#' author: "Agilelearning, Leo Lu"
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     toc: yes
#' ---

#+ include=FALSE
# set root dir when rendering
knitr::opts_knit$set(root.dir = '..')

#' > This material is modified from Hadley's work
#' > (Creative Commons Attribution-Noncommercial 3.0 United States License.)
#' >
#' > https://youtu.be/rz3_FDVt9eg

library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(gapminder)
library(ggplot2)


ggplot(gapminder, aes(year, lifeExp)) +
  geom_line(aes(group = country))

by_country <- gapminder %>%
  group_by(continent, country) %>%
  nest()

by_country
by_country$data[[1]]

by_country <- by_country %>%
  mutate(model = purrr::map(data, ~ lm(lifeExp ~ year, data = .)))

by_country %>% unnest(data)

# Extract model summaries:
by_country %>% unnest(model %>% purrr::map(broom::glance))

# Extract coefficients:
by_country %>% unnest(model %>% purrr::map(broom::tidy))

# Extract residuals etc:
by_country %>% unnest(model %>% purrr::map(broom::augment))


