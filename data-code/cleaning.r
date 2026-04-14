## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, ggthemes, dplyr, lubridate, stringr, readxl, data.table, gdata, readr)

# Set working directory ----------------------------------------------------
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/foodborne illness")

# Read in data -------------------------------------------------------------
# Loop for rewritten output files containing selected columns only
records = paste0("record_", 1:20)

columns = c("year", "monthdth", "ucod", records)

for (y in 1996:2020) {
    mort.path = paste0("data/input/mort", y, ".csv")
    mort.data = read_csv(mort.path, 
    col_select = any_of(columns), col_types = cols(.default = col_character()),
    show_col_types = FALSE)
    write_csv(mort.data, paste0("data/output/mort", y, ".csv"))}

# combining files
strings = schema(!!!setNames(rep(list(utf8()), length(columns)), columns))

data = open_dataset("data/output", format = "csv",
  schema = strings,
  convert_options = csv_convert_options(
    null_values = c("", "NA"),
    strings_can_be_null = TRUE))

# 
