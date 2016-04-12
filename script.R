
rm(list = ls())


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


# Reading data ------------------------------------------------------------

# Reading data - reading text data ----------------------------------------

# Text data example


# example of reading in a csv file with leading metadata

census_2001_health <- read_csv(file = "data/text/S23.csv") # This fails

census_2001_health <- read_csv(
  file = "data/text/S23.csv",
  skip = 5) # This loads, but produces a warning

problems(census_2001_health)


census_2001_health <- read_csv(
  file = "data/text/S23.csv",
  skip = 5,
  na = "-" # This loads, and produces no warnings. However it has incorrectly defined '-' as missing data.
)

problems(census_2001_health)

census_2001_health <- read_csv(
  file = "data/text/S23.csv",
  skip = 5, 
  col_types = paste0(rep("c", 16), collapse = "")
)

problems(census_2001_health) 
glimpse(census_2001_health)



# Reading binary files ----------------------------------------------------


# Reading binary files part 1 - excel

lookup <- read_excel(
  path = "data/binary/replication_details.xlsx",
  sheet = "code_to_country_lookup"
  )

lookup
problems(lookup)
glimpse(lookup)

# Reading binary files part 2 - spss


poverty <- read_spss(path = "data/binary/poverty_dataset.sav")

poverty <- poverty %>% 
  mutate(
    country_group = as_factor(country_group, labels = "values"),
    high_deathrate = as_factor(high_deathrate, labels = "values")
  )

poverty
glimpse(poverty)



# String Tidying  ---------------------------------------------------------


# This section will work with the census_2001_health dataset, which has been typecase to 'chr' for all
# variables to avoid inducing and error


dta <- data.frame(
  place = rep(c("A", "B"), each = 3), 
  time = 1:3,
  count = c(4, "-", 1, 6, 2, 4)
) %>% tbl_df

dta

glimpse(dta)

dta2 <- dta %>% 
  mutate(
    count2 = as.numeric(str_replace(count, "-", "0"))
  )
dta2
glimpse(dta2)

rm(dta2)

dta <- dta %>% 
  mutate(
    count = as.numeric(str_replace(count, "-", "0"))
  )
dta
glimpse(dta)


# user defined function example of the above

change_dash_to_zero <- function(input){
  output <- input %>% 
    str_replace("-", "0") %>% 
    as.numeric

  return(output)  
}

dta <- data.frame(
  place = rep(c("A", "B"), each = 3), 
  time = 1:3,
  count = c(4, "-", 1, 6, 2, 4)
) %>% tbl_df
dta
glimpse(dta)

dta <- dta %>% 
  mutate(
    count = change_dash_to_zero(count)
  )
dta
glimpse(dta)

# Whitespace example
dta <- data.frame(
  sex = c("male", "male ", " female", "female"),
  value = c("500", "10 000", "40,000", "25")
) %>% tbl_df
dta
glimpse(dta)

clean_values <- function(input){
  output <- input %>% 
    str_replace(",", "") %>% 
    str_replace(" ", "") %>% 
    as.numeric
  
  return(output)
}

dta2 <- dta %>% 
  mutate(
    sex2 = str_trim(sex), 
    value2 = clean_values(value)
  )

dta2
glimpse(dta2)


# remove whitespace and convert to factor 
trim_and_factorise <- function(input){
  output <- input %>% 
    str_trim %>% 
    factor
  
  return(output)
}

dta2 <- dta %>% 
  mutate(
    sex2 = trim_and_factorise(sex), 
    value2 = clean_values(value),
    test1 = value > 2000,
    test2 = value2 > 2000
  )
dta2
glimpse(dta2)


rm(dta2)

dta <- dta %>% 
  mutate(
    sex = trim_and_factorise(sex),
    value = clean_values(value)
  )







# Tidy data extended example ----------------------------------------------

# using the census_2001_health dataframe loaded earlier

#Start by View ing the dataframe
View(census_2001_health)

# now to re-name the first three columns:

names(census_2001_health)[1:3] <- c("place", "sex", "age_and_health")

# Now to gather the data 
census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health)

# And now to split the age_and_health column using the '-' symbol

# First attempt

census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-")

# Attempt with extra argument set to drop
census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop")

# Attempt with extra argument set to 'merge' 

census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "merge")

# Same as drop, using 'drop' for now

# Filtering rows where there is no data in the health column
census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health))


# Filtering out ALL PEOPLE from sex by filtering out ALL PEOPLE
census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex != "ALL PEOPLE")

# Filtering out ALL PEOPLE from sex by filtering in Male and Female
census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female"))


# Filter ALL PEOPLE from occupational_group

census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE")

# Filter in larger places only from census table

census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE") %>% 
  filter(place == toupper(place)) 

# Filter to remove SCOTLAND as double counting

census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE") %>% 
  filter(place == toupper(place)) %>% 
  filter(place != "SCOTLAND")


# trim leading and trailing whitespace, for just two columns

census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE") %>% 
  filter(place == toupper(place)) %>% 
  filter(place != "SCOTLAND") %>% 
  mutate(
    age = str_trim(age), 
    health = str_trim(health)
  )

# trim leading and trailing whitespace for all columns, using mutate
census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE") %>% 
  filter(place == toupper(place)) %>% 
  filter(place != "SCOTLAND") %>% 
  mutate(
    place = str_trim(place),
    sex = str_trim(place),
    age = str_trim(age),
    health = str_trim(health),
    occupational_group = str_trim(occupational_group),
    count = str_trim(count)
  )

# Trim leading and trailing whitespace for all columns, using mutate_each
census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE") %>% 
  filter(place == toupper(place)) %>% 
  filter(place != "SCOTLAND") %>% 
  mutate_each(funs(str_trim))

# Trim leading and trailing whitespace for all columns, using mutate_each, excluding count
census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE") %>% 
  filter(place == toupper(place)) %>% 
  filter(place != "SCOTLAND") %>% 
  mutate_each(funs(str_trim), -count)

# Glimpse the data to reveal that count is still of type char
census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE") %>% 
  filter(place == toupper(place)) %>% 
  filter(place != "SCOTLAND") %>% 
  mutate_each(funs(str_trim), -count) %>% glimpse



census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE") %>% 
  filter(place == toupper(place)) %>% 
  filter(place != "SCOTLAND") %>% 
  mutate_each(funs(str_trim), -count) %>%
  mutate(count = change_dash_to_zero(count))


# Final solution to producing tidy data version of the census table
tidy_census_2001_health <- census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE") %>% 
  filter(place == toupper(place)) %>% 
  filter(place != "SCOTLAND") %>% 
  mutate_each(funs(str_trim), -count) %>%
  mutate(count = change_dash_to_zero(count))


#Save the file 
#write_csv(x = tidy_census_2001_health, path = "data/text/tidy_census_2001_health.csv")


#tidy_census_2001_health <- read_csv("data/text/tidy_census_2001_health.csv")
# Exploring the Tidy Data -------------------------------------------------

# Question 1: Did males or females in Scotland have better self-reported health in 2001?

tidy_census_2001_health %>%
  mutate(
    good_health = recode(
      health, recodes = "
      c('Good Health', 'Fairly Good Health') = 'yes';
      'Not Good Health' = 'no'
      "
    )
  ) %>% 
  group_by(good_health, sex) %>% 
  summarise(count = sum(count)) %>% 
  spread(good_health, count) %>% 
  mutate(
    ratio = yes / no,
    proportion = yes / (yes + no)
    )


# Question 2: Which places have the highest and lowest proportion 
# of males aged between 50 to 64 who report poor health?

# Ascending order 
tidy_census_2001_health %>%
  filter(age %in% c("50 to 54", "55 to 59", "60 to 64")) %>% 
  filter(sex == "Male") %>% 
  mutate(
    good_health = recode(
      health, recodes = "
      c('Good Health', 'Fairly Good Health') = 'yes';
      'Not Good Health' = 'no'
      "
    )
    ) %>% 
  select(place, age, good_health, count) %>% 
  group_by(place, good_health) %>% 
  summarise(count = sum(count)) %>% 
  spread(good_health, count) %>% 
  transmute(place = place, proportion = no / (no + yes)) %>% 
  arrange(proportion)

# Descending order 
tidy_census_2001_health %>%
  filter(age %in% c("50 to 54", "55 to 59", "60 to 64")) %>% 
  filter(sex == "Male") %>% 
  mutate(
    good_health = recode(
      health, recodes = "
      c('Good Health', 'Fairly Good Health') = 'yes';
      'Not Good Health' = 'no'
      "
    )
  ) %>% 
  select(place, age, good_health, count) %>% 
  group_by(place, good_health) %>% 
  summarise(count = sum(count)) %>% 
  spread(good_health, count) %>% 
  transmute(place = place, proportion = no / (no + yes)) %>% 
  arrange(desc(proportion))



# Visualisation
tidy_census_2001_health %>%
  filter(age %in% c("50 to 54", "55 to 59", "60 to 64")) %>% 
  filter(sex == "Male") %>% 
  mutate(
    good_health = recode(
      health, recodes = "
      c('Good Health', 'Fairly Good Health') = 'yes';
      'Not Good Health' = 'no'
      "
    )
    ) %>% 
  select(place, age, good_health, count) %>% 
  group_by(place, good_health) %>% 
  summarise(count = sum(count)) %>% 
  spread(good_health, count) %>% 
  transmute(place = place, proportion = no / (no + yes)) %>% 
  arrange(desc(proportion)) %>% 
  ggplot(.) + 
  geom_point(aes(y = reorder(place, proportion), x = proportion)) + 
  labs(x = "Proportion of older working age males reporting poor health", y = "Places within Scotland") + 
  coord_cartesian(xlim = c(0, 0.40))


# Example of using cumsum

tidy_census_2001_health %>% 
  group_by(place) %>% 
  summarise(count = sum(count)) %>% 
  arrange(desc(count)) %>% 
  mutate(
    cumulative_count = cumsum(count),
    cumulative_proportion = cumulative_count / sum(count)
    ) 



# Example of cumsum combined with group_by

tidy_census_2001_health %>% 
  group_by(place, sex) %>% 
  summarise(count = sum(count)) %>%
  group_by(sex) %>% 
  arrange(desc(count)) %>% 
  mutate(
    cumulative_count = cumsum(count),
    cumulative_proportion = cumulative_count / sum(count)
  ) 

# rearranging above to compare male and female by cumulative size

tidy_census_2001_health %>% 
  group_by(place, sex) %>% 
  summarise(count = sum(count)) %>%
  group_by(sex) %>% 
  arrange(desc(count)) %>% 
  mutate(
    cumulative_count = cumsum(count),
    cumulative_proportion = cumulative_count / sum(count)
  ) %>% 
  select(place, sex, cumulative_proportion) %>% 
  spread(key = sex, value = cumulative_proportion) %>% 
  mutate(
    rank_female = dense_rank(Female),
    rank_male = dense_rank(Male)
  ) %>% 
  arrange(rank_female) %>% 
  mutate(dif_ranks = rank_male - rank_female) %>% 
  View


# Example of anti-join to find small places in Scotland


tidy_census_2001_health_BIG <- census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE") %>% 
  filter(place == toupper(place)) %>% 
  filter(place != "SCOTLAND") %>% 
  mutate_each(funs(str_trim), -count) %>%
  mutate(count = change_dash_to_zero(count))

tidy_census_2001_health_all <- census_2001_health %>% 
  gather("occupational_group", "count", -place, -sex, -age_and_health) %>% 
  separate(age_and_health, into = c("age", "health"), sep = "-", extra = "drop") %>% 
  filter(!is.na(health)) %>% 
  filter(sex %in% c("Male", "Female")) %>% 
  filter(occupational_group != "ALL PEOPLE") %>% 
  mutate_each(funs(str_trim), -count) %>%
  mutate(count = change_dash_to_zero(count))


tidy_census_2001_health_small <- tidy_census_2001_health_all %>% 
  anti_join(tidy_census_2001_health_BIG) %>% 
  filter(place != "SCOTLAND")


tidy_census_2001_health <- tidy_census_2001_health %>% 
  mutate(year = 2001) %>% 
  select(place, year, sex, age, health, occupational_group, count)

tidy_census_1991_health <- tidy_census_1991_health %>% 
  mutate(year = 1991) %>% 
  select(place, year, sex, age, health, occupational_group, count)

tidy_census_2011_health <- tidy_census_2011_health %>% 
  mutate(year = 2011) %>% 
  select(place, year, sex, age, health, occupational_group, count)

# Example code for combining tables in the same tidy data format 
tidy_census_combined_health <- tidy_census_1991_health %>% 
  bind_rows(tidy_census_2001_health) %>% 
  bind_rows(tidy_census_2011_health) %>% 
  arrange(year, place, sex, age)



# Day 2. Part 1.1 - tables ------------------------------------------------

# 
tidy_census_2001_health  %>% 
  group_by(place)  %>% 
  summarise(count = sum(count))  %>% 
  summarise(
    lowest_quantile = quantile(count, 0.2), 
    highest_quantile = quantile(count, 0.8)
    )

# Simple illustration of Dorling figure
dta %>% 
  ggplot(.) + 
  geom_point(
    aes(
      x = health_care_needs, 
      y = health_care_provision, 
      size = population_size,
      fill = is_north
      )
    )

# moving mapping rules to ggplot function
# Note: this is just an example of the type of code to use, and will not work 
# as dta does not exist.
# dta %>% 
#   ggplot(
#     .,
#     aes(
#       x = health_care_needs, 
#       y = health_care_provision, 
#       size = population_size,
#       fill = is_north
#     )
#   ) + 
#   geom_point()
# 
# # qplot example of the above
# qplot(
#     x = health_care_needs,
#     y = health_care_provision,
#     size = population_size, 
#     fill = is_north,
#     geom = "point",
#     data = dta
# )




# plyr examples -----------------------------------------------------------


hmd <- read_csv("data/text/counts_germany_combined.csv")

# want to produce a regression of infant mortality rate against year for 
# each country

fn <- function(input){
  output <- lm(death_rate ~ year + sex, input)
  return(output)
}

models <- hmd %>% 
  filter(age == 0) %>% 
  filter(sex != "total") %>% 
  mutate(death_rate = death_count / population_count) %>% 
  dlply(., .(country), fn)


# now to extract coefficients from each lm object

fn <- function(input){
  output <- input$coefficients
}

coeffs <- ldply(models, fn)

# # visualise?
# 
# coeffs  %>% 
#   ggplot(., aes(x = `(Intercept)`, y = year)) + 
#   geom_point(aes(colour = country, group = country)) + 
#   stat_smooth(method = "lm", se = F)

# example - plotting mortality rates as separate files for each country

fn <- function(input){
  this_country <- unique(input$country) 
  
  ggplot(input) + 
    geom_line(aes(x = year, y = death_rate, group = sex, linetype = sex)) + 
    labs(x = "Year", y = "Infant mortality rate", title = this_country)
  
  ggsave(
    paste0("figures/countries/", this_country, ".png"), 
    width = 8, height = 8, dpi = 150
  )
  NULL
}

dir.create("figures/countries", recursive = T)

hmd %>% 
  filter(age == 0) %>% 
  filter(sex != "total") %>% 
  mutate(death_rate = death_count / population_count) %>% 
  d_ply(., .(country), fn, .progress = "text")
