## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, ggthemes, dplyr, lubridate, stringr, readxl, data.table, gdata, readr, arrow)

# Set working directory ----------------------------------------------------
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/foodborne illness")

# read in new data ---------------------------------------------------------
data = read.csv("data/output/foodborne_data.csv")

# pathogen counts
other = c("A05", "A059")
campylobacter = "A045"
listeria = c("A32", "A327")
salmonella = c("A02", "A020", "A021", "A029")
stec = "A043"
yersinia = "A046"

illness = c(other, campylobacter, listeria, salmonella, stec, yersinia)

icd_year_counts = data %>%
  mutate(
    A05  = if_any(c(ucod, all_of(records)), ~ .x == "A05"),
    A059 = if_any(c(ucod, all_of(records)), ~ .x == "A059"),
    A045 = if_any(c(ucod, all_of(records)), ~ .x == "A045"),
    A32  = if_any(c(ucod, all_of(records)), ~ .x == "A32"),
    A327 = if_any(c(ucod, all_of(records)), ~ .x == "A327"),
    A02  = if_any(c(ucod, all_of(records)), ~ .x == "A02"),
    A020 = if_any(c(ucod, all_of(records)), ~ .x == "A020"),
    A021 = if_any(c(ucod, all_of(records)), ~ .x == "A021"),
    A029 = if_any(c(ucod, all_of(records)), ~ .x == "A029"),
    A043 = if_any(c(ucod, all_of(records)), ~ .x == "A043"),
    A046 = if_any(c(ucod, all_of(records)), ~ .x == "A046")
  ) %>%
  group_by(year) %>%
  summarise(
    A05  = sum(A05, na.rm = TRUE),
    A059 = sum(A059, na.rm = TRUE),
    A045 = sum(A045, na.rm = TRUE),
    A32  = sum(A32, na.rm = TRUE),
    A327 = sum(A327, na.rm = TRUE),
    A02  = sum(A02, na.rm = TRUE),
    A020 = sum(A020, na.rm = TRUE),
    A021 = sum(A021, na.rm = TRUE),
    A029 = sum(A029, na.rm = TRUE),
    A043 = sum(A043, na.rm = TRUE),
    A046 = sum(A046, na.rm = TRUE))

write_csv(icd_year_counts, "results/icd_year_counts.csv")
