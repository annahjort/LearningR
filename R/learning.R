# R basics ----------------------------------------------------------------

weight_kilos <- 100
weight_kilos <- 10

weight_kilos

colnames(airquality)

str(airquality)

summary(airquality)

2 + 2


# Packages ----------------------------------------------------------------

library(tidyverse)
library(NHANES)


# Looking at data ---------------------------------------------------------

glimpse(NHANES)
colnames(NHANES)
select(NHANES, Age, Weight, BMI)
select(NHANES, -HeadCirc)
select(NHANES, starts_with("BP"))
select(NHANES, ends_with("Day"))
select(NHANES, contains("Age"))

nhanes_small <- select(
  NHANES,
  Age,
  Gender,
  BMI,
  Diabetes,
  PhysActive,
  BPSysAve,
  BPDiaAve,
  Education
)
nhanes_small

nhanes_small <- rename_with(
  nhanes_small,
  snakecase::to_snake_case
)

nhanes_small <- rename(
  nhanes_small,
  sex = gender
)
# Fixing variable names ---------------------------------------------------


# Piping ------------------------------------------------------------------

colnames(nhanes_small)

nhanes_small %>%
  colnames()
nhanes_small %>%
  select(phys_active) %>%
  rename(
    physically_active = phys_active
  )

nhanes_small %>%
  select(bp_sys_ave, education)

nhanes_small %>%
  rename(
    bp_sys = bp_sys_ave,
    bp_dia = bp_dia_ave
  )

nhanes_small %>%
  select(bmi, contains("age"))

nhanes_small %>%
  filter(phys_active != "No")

nhanes_small %>%
  filter(bmi >= 25 &
    phys_active == "No")


nhanes_small %>%
  select(starts_with("bp_")) %>%
  rename(bp_systolic = bp_sys_ave)


# Mutating columns --------------------------------------------------------

nhanes_update <- nhanes_small %>%
  mutate(
    age_months = age * 12,
    logged_bmi = log(bmi),
    age_weeks = age_months * 4,
    old = if_else(
      age >= 30,
      "old",
      "young"
    )
  )


# Exercise ----------------------------------------------------------------

nhanes_small %>%
  filter(bmi >= 20 & bmi <= 40 & diabetes == "yes")

nhanes_modified <- nhanes_small %>%
  mutate(
    mean_arterial_pressure = ((2 * bp_dia_ave) + bp_sys_ave) / 3,
    young_child = if_else(age <= 6, "Yes", "No")
  )


# Summarizing -------------------------------------------------------------

nhanes_small %>%
  filter(!is.na(diabetes)) %>%
  group_by(diabetes, phys_active) %>%
  summarize(
    max_bmi = max(bmi, na.rm = TRUE),
    min_bmi = min(bmi, na.rm = TRUE)
  )
