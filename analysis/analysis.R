# Preliminaries ------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, ggthemes, dplyr, lubridate, stringr, readxl, data.table, gdata, readr, arrow)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/foodborne illness")

# read in cleaned data
data = read.csv("data/output/foodborne_data.csv")
records = paste0("record_", 1:20)

# initial counts by ICD-10 codes
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
    A053 = if_any(c(ucod, all_of(records)), ~ .x == "A053"),
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
    A053 = sum(A053, na.rm = TRUE),
    A046 = sum(A046, na.rm = TRUE))

write_csv(icd_year_counts, "results/icd_year_counts.csv")

# pathogen counts
other = c("A05", "A059")
campylobacter = "A045"
listeria = c("A32", "A327")
salmonella = c("A02", "A020", "A021", "A029")
stec = "A043"
yersinia = "A046"

pathogen_counts = data %>%
  mutate(
    Other = if_any(c(ucod, all_of(records)), ~ .x %in% other),
    Campylobacter = if_any(c(ucod, all_of(records)), ~ .x %in% campylobacter),
    Listeria = if_any(c(ucod, all_of(records)), ~ .x %in% listeria),
    Salmonella = if_any(c(ucod, all_of(records)), ~ .x %in% salmonella),
    STEC = if_any(c(ucod, all_of(records)), ~ .x %in% stec),
    Yersinia = if_any(c(ucod, all_of(records)), ~ .x %in% yersinia)) %>%
  group_by(year) %>%
  summarise(
    Other = sum(Other, na.rm = TRUE),
    Campylobacter = sum(Campylobacter, na.rm = TRUE),
    Listeria = sum(Listeria, na.rm = TRUE),
    Salmonella = sum(Salmonella, na.rm = TRUE),
    STEC = sum(STEC, na.rm = TRUE),
    Yersinia = sum(Yersinia, na.rm = TRUE),
    .groups = "drop")

write_csv(pathogen_counts, "results/pathogen_year_counts")

# plotting pathogen counts
count_data = pathogen_counts %>%
  pivot_longer(
    cols = -year,
    names_to = "pathogen",
    values_to = "n")

ggplot(count_data, aes(x = as.numeric(year), y = n, color = pathogen)) +
  geom_line(linewidth = 1.1) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 120, by = 10)) +
  labs(
    title = "Foodborne Illness Deaths by Pathogen (1996-2020)",
    subtitle = "CDC NVSS Mortality Multiple Cause-of-Death data (NBER)",
    x = "Year",
    y = "Number of Deaths",
  ) +
  theme_stata() +
  theme(plot.title = element_text(size = 40, face = "bold", hjust = 0, color = "black"),
        plot.subtitle = element_text(size = 30, color = "black", margin = margin(b = 12), hjust = 0),
        legend.position = "bottom",
        legend.text = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.text.x = element_text(size = 35), 
        axis.text.y = element_text(size = 35, angle = 0, vjust = 0.5),
        plot.caption = element_text(size = 12),
        plot.background = element_rect(fill = "white"))
ggsave("results/deaths_by_pathogen.png", width = 20, height = 15)

## GASTROENTERITIS UPDATE ----------------------------------------------------------------------------------
# recoding / gastroenteritis definitions
gastro_codes = c("A009","A010", "A011", "A012", "A013", "A014", "A020", "A021", "A022", "A028", "A029",
  "A030", "A031", "A032", "A033", "A038", "A039", "A040", "A041", "A042", "A043", "A044", "A045", "A046", 
  "A048", "A049", "A050", "A052", "A053", "A054", "A058", "A059", "A060", "A061", "A062", "A063", "A064", 
  "A065", "A066", "A067", "A068", "A069", "A070", "A071", "A072", "A073", "A078", "A079", "A080", "A081", 
  "A082", "A083", "A084", "A085", "A09", "K529")

gastro_data = data %>%
  select(ucod, year, monthdth, all_of(records)) %>%
  collect() %>%
  filter(
    ucod %in% gastro_codes |
    if_any(all_of(records), ~ .x %in% gastro_codes))
    # resulted in 1791 rows

gastro_counts = data %>%
  group_by(year) %>%
  summarise(
    !!!setNames(
      map(gastro_codes, ~
        expr(sum(if_any(c(ucod, all_of(records)), ~ .x == !!.x), na.rm = TRUE))
      ),
      gastro_codes),
    .groups = "drop")
write_csv(gastro_counts, "results/gastro_counts.csv")

# adding a total pathogens column to gastroenteritis counts
gastro_total_counts = data %>%
  mutate(gastro_any = if_any(c(ucod, all_of(records)), ~ .x %in% gastro_codes)) %>%
  group_by(year) %>%
  summarise(total_gastro = sum(gastro_any, na.rm = TRUE),
    !!!setNames(
      map(gastro_codes, ~
        expr(sum(if_any(c(ucod, all_of(records)), ~ .x == !!.x), na.rm = TRUE))),
      gastro_codes),.groups = "drop")
write_csv(gastro_year_counts, "results/gastro_total_counts.csv")

# gastroenteritis plot
ggplot(gastro_total_counts, aes(x = as.numeric(year), y = total_gastro)) +
  geom_line(linewidth = 1.2, color = "#3043B4") +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 140, by = 20)) +
  labs(
    title = "Total Gastroenteritis-Related Deaths (1996-2020)",
    subtitle = "CDC NVSS Mortality Multiple Cause-of-Death data (NBER)",
    x = "Year",
    y = "Number of Deaths") +
  theme_stata() +
  theme(
    plot.title = element_text(size = 40, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 30, color = "black", margin = margin(b = 12), hjust = 0),
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    axis.title.y = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 35), 
    axis.text.y = element_text(size = 35, angle = 0, vjust = 0.5),
    plot.caption = element_text(size = 12),
    plot.background = element_rect(fill = "white"))
ggsave("results/gastroenteritis_deaths.png", width = 20, height = 15)

### FINAL CODES ### ------------------------------------------------------------------------------
# recoding / foodborne definitions
foodborne_codes = c("A000","A001","A009","A010","A011","A012","A013","A014",
"A020","A021","A022","A028","A029",
"A030","A031","A032","A033","A038","A039",
"A040","A041","A042","A043","A044","A045","A046","A048","A049",
"A050","A051","A052","A053","A054","A058","A059",
"A060","A061","A062","A063","A064","A065","A066","A067","A068","A069",
"A070","A071","A072","A073","A078","A079",
"A080","A081","A082","A083","A084","A085",
"A09","K529",
"A230","A231","A232","A233","A238","A239",
"A32")

foodborne_data = data %>%
  select(ucod, year, monthdth, all_of(records)) %>%
  collect() %>%
  filter(
    if_any(c(ucod, all_of(records)), ~ .x %in% foodborne_codes))
        # resulted in 1791 rows

# foodborne illness counts + total column
foodborne_total_counts = data %>%
  mutate(foodborne_any = if_any(c(ucod, all_of(records)), ~ .x %in% foodborne_codes)) %>%
  group_by(year) %>%
  summarise(total_foodborne = sum(foodborne_any, na.rm = TRUE),
    !!!setNames(
      map(foodborne_codes, ~
        expr(sum(if_any(c(ucod, all_of(records)), ~ .x == !!.x), na.rm = TRUE))),
      foodborne_codes),.groups = "drop")
write_csv(foodborne_total_counts, "results/foodborne_total_counts.csv")

# final plot
ggplot(foodborne_total_counts, aes(x = as.numeric(year), y = total_foodborne)) +
  geom_line(linewidth = 1.2, color = "#3043B4") +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 140, by = 20)) +
  labs(
    title = "Total Foodborne Illness Deaths (1996-2020)",
    subtitle = "CDC NVSS Mortality Multiple Cause-of-Death data (NBER)",
    x = "Year",
    y = "Number of Deaths") +
  theme_stata() +
  theme(
    plot.title = element_text(size = 40, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 30, color = "black", margin = margin(b = 12), hjust = 0),
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    axis.title.y = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 35), 
    axis.text.y = element_text(size = 35, angle = 0, vjust = 0.5),
    plot.caption = element_text(size = 12),
    plot.background = element_rect(fill = "white"))
ggsave("results/foodborne_deaths.png", width = 20, height = 15)
