# Practical Task Script 
# 12/4/2016


rm(list = ls())

# Load pre-reqs

require(readr)
require(haven)
require(readxl)

require(stringr)
require(tidyr)
require(plyr)
require(dplyr)
require(car)

require(ggplot2)

require(xlsx)


# Load raw data 

dta_raw <- read_csv("data/text/all_cause_singleyear_cdc_raw.txt")

# It's not a CSV file

dta_raw <- read_delim("data/text/all_cause_singleyear_cdc_raw.txt", delim = "\t")

# Tailing metadata after line 49996

dta_raw <- read_delim(
  "data/text/all_cause_singleyear_cdc_raw.txt", 
  delim = "\t",
  n_max = 49996
  )

# Now the data tidying begins

dta_raw  %>% 
  select(
    year = `Year Code`, 
    age = `Single-Year Ages Code`, 
    sex = `Gender Code`, 
    race = Race, 
    hisp = `Hispanic Origin`, 
    death_count = Deaths, 
    population_count = Population
    ) 

# which race categories?
dta_raw  %>% 
  select(
    year = `Year Code`, 
    age = `Single-Year Ages Code`, 
    sex = `Gender Code`, 
    race = Race, 
    hisp = `Hispanic Origin`, 
    death_count = Deaths, 
    population_count = Population
  )  %>% xtabs( ~ race, .)


dta_raw  %>% 
  select(
    year = `Year Code`, 
    age = `Single-Year Ages Code`, 
    sex = `Gender Code`, 
    race = Race, 
    hisp = `Hispanic Origin`, 
    death_count = Deaths, 
    population_count = Population
  )  %>% 
  filter(
    race %in% c("Black or African American", "White"),
    hisp %in% c("Hispanic or Latino", "Not Hispanic or Latino")
  ) 

# Now to produce the three mutually exclusive groups 

dta_raw  %>% 
  select(
    year = `Year Code`, 
    age = `Single-Year Ages Code`, 
    sex = `Gender Code`, 
    race = Race, 
    hisp = `Hispanic Origin`, 
    death_count = Deaths, 
    population_count = Population
  )  %>% 
  filter( # select groups of interest
    race %in% c("Black or African American", "White"),
    hisp %in% c("Hispanic or Latino", "Not Hispanic or Latino")
  ) %>% 
  mutate( # produce the three mutually exclusive groups
    ethnic = recode(
      race, 
      "
      'Black or African American' = 'b';
      'White' = 'w'
      "
    ),
    ethnic = ifelse( # note use of ifelse function referring to ethnic column only just created
      hisp == "Hispanic or Latino",
      "h", ethnic
    )
  ) %>% 
  mutate(sex = tolower(sex)) %>% 
  select(year, age, sex, ethnic, death_count, population_count) %>%
  filter(population_count != "Not Applicable") %>% # filter out non-numeric data
  filter(death_count != "Not Applicable") %>% 
  mutate(
    population_count = as.numeric(population_count),
    death_count = as.numeric(death_count) # convert to numbers from character
         )  -> dta_tidy # write out data 


# now the data analysis

# Question 1: Produce the death rate for white non-hispanics between the ages of 45 and 54 years inclusive 
# for each year from 1999 to 2014. 

dta_tidy %>% 
  filter(age >= 45 & age <= 54) %>% 
  filter(ethnic == "w") %>% 
  group_by(year) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  ggplot(.) + geom_line(aes(x = year, y = death_rate))

ggsave("figures/practical_q01.png", width = 10, height = 10, dpi = 150, units = "cm")

# Question 2: Do the above, but for males and females separately


dta_tidy %>% 
  filter(age >= 45 & age <= 54) %>% 
  filter(ethnic == "w") %>% 
  group_by(year, sex) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  ggplot(.) + geom_line(aes(x = year, y = death_rate, group = sex, linetype = sex))

ggsave("figures/practical_q02.png", width = 10, height = 10, dpi = 150, units = "cm")

# Question 3: Repeat the above, but index male and female death rates to their 1999 values

dta_tidy %>% 
  filter(age >= 45 & age <= 54) %>% 
  filter(ethnic == "w") %>% 
  group_by(year, sex) %>% 
  summarise(
    death_count = sum(death_count), 
    population_count = sum(population_count)) %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>%
  group_by(sex) %>% 
  mutate(death_trend = death_rate / death_rate[year == 1999]) %>% 
  ggplot(.) + geom_line(aes(x = year, y = death_trend, group = sex, linetype = sex))

ggsave("figures/practical_q03.png", width = 10, height = 10, dpi = 150, units = "cm")

# Question 4: Produce the death rate trends, indexed to 1999 values, for each sex and age in single years 

dta_tidy %>% 
  filter(age >= 45 & age <= 54) %>% 
  filter(ethnic == "w") %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>%
  group_by(sex, age) %>% 
  mutate(death_trend = death_rate / death_rate[year == 1999]) %>% 
  ggplot(.) + geom_line(aes(x = year, y = death_trend, group = sex, linetype = sex)) +
  facet_wrap(~ age)

ggsave("figures/practical_q04.png", width = 15, height = 10, dpi = 150, units = "cm")


# Question 5: Perform the age adjustment: i.e produce the average age/sex specific trend in all-cause mortality within this 
# period 

dta_tidy %>% 
  filter(age >= 45 & age <= 54) %>% 
  filter(ethnic == "w") %>% 
  select(-ethnic) %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>%
  group_by(sex, age) %>% 
  mutate(death_trend = death_rate / death_rate[year == 1999]) %>% 
  group_by(year) %>% 
  summarise(death_trend = mean(death_trend)) %>% 
  ggplot(.) + geom_line(aes(x = year, y = death_trend))

ggsave("figures/practical_q05.png", width = 10, height = 10, dpi = 150, units = "cm")


# Question 6: What has happened to the male and female black/white mortality gap between 1999 and 2014? 

dta_tidy %>% 
  filter(age >= 45 & age <= 54) %>% 
  filter(ethnic %in% c("b", "w")) %>%
  group_by(year, sex, ethnic) %>% 
  summarise(death_rate = 100000 * sum(death_count) / sum(population_count)) %>% 
  spread(ethnic, death_rate) %>% 
  mutate(bw_ratio = b / w) %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = bw_ratio, group = sex, linetype = sex)) 

ggsave("figures/practical_q06.png", width = 10, height = 10, dpi = 150, units = "cm")

