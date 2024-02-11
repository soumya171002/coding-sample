
#The goal of the script is to use survey and census data to compute post stratification weights
#and run MRP (multi-level regression and post stratification)

library(tidyverse)
library(scales)
library(broom)
library(lme4)
library(gt)
library(rstanarm)

#reading survey,census and election datasets 
svy <- read_rds("data/survey_cces.rds")

cens <- read_rds("data/target_census.rds")

elec <- read_csv("data/elecresults_dailykos.csv")


#1.1---------------------------------------

#coming up with sample proportion of TRUMP Voters on the basis of education level.
svy$trump
svy_prop <- svy |>
  count(educ) |>
  mutate(sample_prop = n / sum(n))

#coming up with population porportion of Trump Voters on the basis of education level
cens_prop <- cens |>
  count(educ, wt = N) |>
  transmute(educ, pop_prop = n / sum(n))

#using post-stratification weights to account for sample and population differences 
weighted <- svy_prop |>
  left_join(cens_prop, by = "educ") |>
  mutate(wts = pop_prop / sample_prop)

#1.2-----------------------------------------------

##Creating a table to show differences in estimated and actual proportion of Trump Voters
#raw proportion of Trump voters in the survey
svy_group <- svy |>
  group_by(state) |>
  summarise(raw_prop = mean(trump))

#weighting trump voters by level of education and grouping by stata using post-stratification weights
wt_svy <- svy |>
  left_join(weighted, by = "educ") |>
  group_by(state) |>
  summarise(wt_educ_prop = weighted.mean(trump, wts))

#combining raw proportion with education weighted proportion for each state
svy_3 <- svy_group |>
  left_join(wt_svy, by = "state")

#creating population fraction on the basis of age,education, state 
cens_tract <- cens |>
  count(educ, age, st, wt = N, name = "N") |>
  mutate(frac = N / sum(N))

#creating survey fraction on the basis of same variables and filling no observation with 0 
svy_tract <- svy |>
  count(educ, age, st) |>
  mutate(sample_frac = n / sum(n)) |>
  complete(age, educ, st,
           fill = list(n = 0))
#creating post-stratification weights on the basis of education, age and state 
age_educ_wt <- svy_tract |>
  left_join(cens_tract, by = c("educ", "age", "st")) |>
  mutate(age_educ_wt = frac / sample_frac)

# joining the sample with weights data and grouping by state 
age_weighting <- svy |>
  left_join(age_educ_wt, by = c("educ", "age", "st")) |>
  group_by(state) |>
  summarise(wt_educ_age_prop = weighted.mean(trump, age_educ_wt))
#combining raw proportion, proportion weighted only by education and proportion weighted along with other demographics
svy_4 <- svy_3 |>
  left_join(age_weighting, by = "state")

#Figuring out actual trump voter proportion from election results
actual_voter <- elec |>
  select(state, division, pct_trump, totalvoters, cd) |>
  group_by(state, division) |>
  summarise(actual_voter = weighted.mean(pct_trump, totalvoters))

#joining raw, estimated and actual proportion of trump voters and grouping by state and division
svy_5 <-
  svy_4 |> left_join(actual_voter, by = "state") |> group_by(division)

#creating gt table with the joined data and formatting the table 
svy_5 |>
  select(state,
         division,
         raw_prop,
         wt_educ_prop,
         wt_educ_age_prop,
         actual_voter) |>
  gt() |>
  fmt_number(decimals = 3) |>
  cols_label (
    raw_prop = "raw proportion",
    wt_educ_prop = "estimated trump weight by education",
    wt_educ_age_prop = "estimated trump weight by education and age",
    actual_voter = "total trump voters"
  ) |>
  tab_options(table.margin.right = pct(60)) 



#2.2---------------------------------------

####Data Analysis 
#running Multilevel regression and post-stratification (MRP) estimates
fit <-
  glmer(trump ~ (1 |
                   age) + (1 | female) + (1 | educ) + (1 | race) + (1 | cd),
        svy,
        family = "binomial")

#predicting trump voters proportion on the basis of MRP estimates by congressional districts
cens_pred <- cens |>
  mutate(prop = predict(fit, cens, type = "response")) |>
  summarize(pred = weighted.mean(prop, N),
            .by = cd)
#computing raw proportion on the basis of congressional district
svy_raw <- svy |>
  group_by(cd) |>
  summarise(raw_prop = mean(trump))
graph <- svy_raw |>
  left_join(cens_pred, by = "cd")

#using ggplot to compare MRP estimates with raw proportion
graph |>
  ggplot(aes(x = raw_prop, y = pred)) +
  geom_point() +
  coord_equal() +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "raw proportion",
       y = "MRP estimate",
       title = "Comapring MRP estimate with raw proportion") +
  theme_classic()

## The result of the analysis suggests that MRP estimates are positively related to 
#raw proportion but there is less variation in MRP estimates and the result is pooled 
#towards global mean of 0.5





