rm(list = ls())
# Synthetic data explorations 


# Packages ----------------------------------------------------------------


pacman::p_load(
  stringr,
  forcats,
  lubridate,
  tidyverse
)


# Data --------------------------------------------------------------------


syn_deaths <- read_csv("data/synthetic/synthetic_data_deaths_small.csv")
syn_smr <- read_csv("data/synthetic/synthetic_data_smr01_small.csv")
syn_pres <- read_csv("data/synthetic/synthetic_data_prescribing_small.csv")
# Explorations ------------------------------------------------------------



# Explorations - Death data -----------------------------------------------



# How many observations by gender
syn_deaths %>% 
  group_by(sex) %>% 
  tally()

# Look at deaths by age and gender. 
# If this were real data, how might this break-down allow you to estimate the gender 
# from these distributions? Is the synthetic data similar enough to real data to allow this 
# estimation to be performed? 

# What value-label lookup table would you expect to see in practice? 

syn_deaths %>% 
  group_by(sex, age) %>% 
  tally() %>% 
  ggplot(., aes(x = age, colour = factor(sex), y = n)) +
  geom_line()
  
# Say that the codes for gender are 1 = male and 2 = female; 3 = unknown. How would you 
# create a labelled sex variable? 
# Re-do the tally and graph to add labels

syn_deaths %>% 
  mutate(sex = factor(sex, levels = c(1, 2, 9), labels = c("male", "female", "unknown"))) %>% 
  group_by(sex) %>% 
  tally()

syn_deaths %>% 
  mutate(sex = factor(sex, levels = c(1, 2, 9), labels = c("male", "female", "unknown"))) %>% 
  group_by(sex) %>% 
  tally() %>% 
  ggplot(., aes(x = sex, y = n)) +
  geom_bar(stat = "identity")

syn_deaths %>% 
  mutate(sex = factor(sex, levels = c(1, 2, 9), labels = c("male", "female", "unknown"))) %>% 
  group_by(sex, age) %>% 
  tally() %>% 
  ggplot(., aes(colour = sex, x = age, y = n)) +
  geom_line()

# How do you exclude observations with missing genders?

syn_deaths %>% 
  mutate(sex = factor(sex, levels = c(1, 2, 9), labels = c("male", "female", "unknown"))) %>%
  filter(sex != "unknown") %>% 
  group_by(sex, age) %>% 
  tally() %>% 
  ggplot(., aes(colour = sex, x = age, y = n)) +
  geom_line()


# How many observations by health board area?
syn_deaths %>% 
  group_by(health_board_area) %>% 
  tally()

# Group age into the following age groups:
# 0
# 1-4
# 5-10
# 11-15
# 16-20
# 21-25
# 26-30
# 31-40
# 41-50
# 51-60
# 60-65
# 66-75
# 76-80
# 81-85
# 86-90
# 91-95
# 96 or older

syn_deaths %>% 
  mutate(
    age_group = cut(
      age, 
      breaks = c(0, 1, 5, 11, 15, 20, 25, 33, 40, 50, 60, 65, 75, 80, 85, 90, 95, Inf),
      ordered = T,
      include.lowest = T,
      right = F
    )
  ) %>% 
#  select(age, age_group) %>% 
#  sample_n(10)
  group_by(age_group) %>% 
  tally()



# Cross tab by age and marital status
syn_deaths %>% 
  group_by(marital_status) %>% 
  tally()

# How should causes of death be separated into a separate table? What 
#   structure should this second table have?
syn_deaths %>% 
  select(unique_record_identifier, primary_cause_of_death:secondary_cause_of_death_9) %>% 
  gather(key = "death_sequence", value = "code", -unique_record_identifier)

#What is the distribution of number of causes of death by record?

syn_deaths %>% 
  select(unique_record_identifier, primary_cause_of_death:secondary_cause_of_death_9) %>% 
  gather(key = "death_sequence", value = "code", -unique_record_identifier) %>% 
  filter(!is.na(code)) %>% 
  group_by(unique_record_identifier) %>% 
  tally() %>% 
  group_by(n) %>% 
  tally() %>%
  rename(number_of_death_codes = n, freq = nn) %>% 
  ggplot(., aes(x = number_of_death_codes, y = freq)) + 
  geom_bar(stat = "identity")

# What's the mean number of death codes?

syn_deaths %>% 
  select(unique_record_identifier, primary_cause_of_death:secondary_cause_of_death_9) %>% 
  gather(key = "death_sequence", value = "code", -unique_record_identifier) %>% 
  filter(!is.na(code)) %>% 
  group_by(unique_record_identifier) %>% 
  tally() %>% 
  summarise(mean_n = mean(n))


# How can age be calculated from other fields?

# NOTE: This will benefit from the lubridate package
# Convert the character strings with dates to time objects of the correct format

syn_deaths %>% 
  select(unique_record_identifier, age, date_of_event, date_of_birth) %>% 
  mutate(
    date_of_event = dmy(date_of_event), # From lubridate package
    date_of_birth = dmy(date_of_birth)
         ) %>% 
  mutate(
    dif_events = date_of_event - date_of_birth,
    implied_age = dif_events %/% dyears(1),
    implied_age2 = as.numeric(dif_events) / 365.25 # Not quite right, as not every duration will span years
    # with exactly 365.25 days per year
  )


# smr data --------------------------------------------------------

# What are the durations of stay? 

glimpse(syn_smr)

syn_smr %>% 
  select(unique_record_identifier, date_of_admission, date_of_discharge) %>% 
  mutate(
    date_of_discharge = dmy(date_of_discharge),
    date_of_admission = dmy(date_of_admission)
  ) %>% 
  mutate(duration = date_of_discharge - date_of_admission)

# Which of these records are coded erroneously? (i.e. according to the records 
# the patients left before they arrived?) How many erroneous records are there of 
# this type?

syn_smr %>% 
  select(unique_record_identifier, date_of_admission, date_of_discharge) %>% 
  mutate(
    date_of_discharge = dmy(date_of_discharge),
    date_of_admission = dmy(date_of_admission)
  ) %>% 
  mutate(duration = date_of_discharge - date_of_admission) %>% 
  filter(duration < 0)

# Assuming for these records the entries are simply entered in the wrong field, 
# how could you swap the date of admission and date of discharge around ? 

syn_smr %>% 
  select(unique_record_identifier, date_of_admission, date_of_discharge) %>% 
  mutate(
    date_of_discharge = dmy(date_of_discharge),
    date_of_admission = dmy(date_of_admission)
  ) %>% 
  mutate(duration = date_of_discharge - date_of_admission) %>% 
  mutate(bad_coding = duration < 0) %>% 
  mutate(dd = date_of_discharge, da = date_of_admission) %>% 
  mutate(
    date_of_discharge = ifelse(bad_coding, as.character(da), as.character(dd)), # wrap with as.character as otherwise 
    date_of_admission = ifelse(bad_coding, as.character(dd), as.character(da))  # output returned is double
  ) %>% 
  mutate(
    date_of_discharge = ymd(date_of_discharge),
    date_of_admission = ymd(date_of_admission)
  ) %>% 
  select(-dd, -da)


# Is the admission date the same as the date of admission?

syn_smr %>% 
  select(unique_record_identifier, date_of_admission, admission_date) %>% 
  mutate(is_same = date_of_admission == admission_date) %>% 
  summarise(num_same = sum(is_same))

# Given this response, does the dataset need both variables?

# What's the median and upper and lower quintiles for the duration between 
# referral date (when on waiting list) and admission date? 

syn_smr %>% 
  select(unique_record_identifier, date_of_admission, waiting_list_date) %>% 
  mutate(
    waiting_list_date = dmy(waiting_list_date),
    date_of_admission = dmy(date_of_admission)
  ) %>% 
  mutate(waiting_duration = date_of_admission - waiting_list_date) %>% 
  summarise(
    med_wl_dur = median(waiting_duration),
    lq_wl_dur = quantile(waiting_duration, 0.25),
    up_wl_dur = quantile(waiting_duration, 0.75)
    )

# How plausible do these values seem if you were looking at real rather than synthetic data?


# prescribing data --------------------------------------------------------

glimpse(syn_pres)

# Is there any apparent relationship between prescriber gender and patient gender? 

syn_pres %>% 
  xtabs(~prescriber_gender + patient_gender_code, .)


# How has the number of prescriptions per year changed? 

syn_pres %>% 
  select(date_prescribed) %>% 
  mutate(date_prescribed = dmy(date_prescribed)) %>% 
  mutate(year_prescribed = year(date_prescribed)) %>% 
  arrange(year_prescribed) %>% 
  group_by(year_prescribed) %>% 
  tally() 




