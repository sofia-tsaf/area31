## Preprocess data, write TAF data tables

## Before: Area37cuyrrentsofia.csv,
##         EffortindexRousseaAugNominal.csv (bootstrap/data)
## After:  catch_by_stock.png, catch_relative.png, catch_total.png,
##         driors_2.png, input.rds (data)

library(icesTAF)
library(dplyr)   # filter, group_by, left_join, mutate, summarise, ungroup
library(ggplot2)
library(janitor) # clean_names
library(purrr)   # map2
library(sraplus) # format_driors, plot_driors
library(stringr) # str_extract_all, str_replace_all, str_trim
library(tidyr)   # nest, pivot_longer

mkdir("data")

## Read catch data, convert to tibble (long format)
catch <- read.csv("bootstrap/data/Area37cuyrrentsofia.csv")
catch <- catch %>%
  pivot_longer(-c(Year, Total), names_to="stock", values_to="capture") %>%
  filter(!is.na(Year)) %>%
  clean_names()

## Plot catches
catch %>%
  ggplot(aes(year, capture, color=stock)) +
  geom_line(show.legend=FALSE) +
  geom_point()
ggsave("data/catch_by_stock.png")
catch %>%
  group_by(year) %>%
  summarise(total_capture=sum(capture)) %>%
  ggplot(aes(year, total_capture)) +
  geom_line()
ggsave("data/catch_total.png")

## Select stocks with min 10 years of non-zero catches...
viable_stocks <- catch %>%
  group_by(stock) %>%
  summarise(n_pos_catch=sum(capture > 0)) %>%
  filter(n_pos_catch > 10)

## ...and discard zero-catch years at the beginning or end of series
catch <- catch %>%
  filter(stock %in% viable_stocks$stock) %>%
  group_by(stock) %>%
  filter(year > min(year[capture > 0]),
         year <= max(year[capture > 0]))

## Plot relative catch
catch %>%
  group_by(stock) %>%
  mutate(capture = capture / max(capture)) %>%
  ggplot(aes(year, capture, group=stock)) +
  geom_point()
ggsave("data/catch_relative.png")

## Add columns 'stock_number_thing' and 'taxa'
catch <- catch %>%
  ungroup() %>%
  mutate(stock_number_thing=str_extract_all(stock, "\\d")) %>%
  mutate(taxa = str_replace_all(stock, "\\d", "")) %>%
  mutate(taxa = str_replace_all(taxa, "\\.", " ") %>% str_trim()) %>%
  mutate(taxa = str_replace_all(taxa, "  ", " ") %>% str_trim()) %>%
  filter(!is.na(taxa))

## Read effort data, add 'effort' column
effort <- read.csv("bootstrap/data/EffortindexRousseaAugNominal.csv")
index <- effort$E1
catch_effort <- catch %>%
  left_join(effort, by=c("year"="Year"))

## Create nested tibble with 'data' column (catch and effort)
stocks <- catch_effort %>%
  group_by(stock, taxa) %>%
  nest() %>%
  ungroup()

## Add nested 'driors' column (data and priors)
stocks <- stocks %>%
  mutate(
    driors=map2(
      taxa,
      data,
      ~
        format_driors(
          taxa = .x,shape_prior=2,  # use_heuristics=TRUE, shape_prior=2,
          catch = .y$capture,
          years = .y$year,
          initial_state = 0.75, initial_state_cv = 0.1, b_ref_type = "k",
          terminal_state = 0.41, terminal_state_cv = 0.23,
          effort = .y$E1[!is.na(.y$E1)], effort_years=.y$year[!is.na(.y$E1)],
          growth_rate_prior = NA,
          growth_rate_prior_cv = 0.2)
      ## initial_state = 0.5,initial_state_cv = 0.25,b_ref_type = "k",
      ## final_u = 1.2,final_u_cv = 0.25, f_ref_type = "fmsy"
      ## sar = 4,
      ## fmi = c(
      ##   "research" = .5,
      ##   "management" = .5,
      ##   "enforcement" = .35,
      ##   "socioeconomics" = 0.7
      ## )
    ))
saveRDS(stocks, "data/input.rds")

## Plot driors for one stock
plot_driors(stocks$driors[[2]])  # stock 2 is Sardinella aurita
ggsave("data/driors_2.png")
