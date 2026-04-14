## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, ggthemes, dplyr, lubridate, stringr, readxl, data.table, gdata, readr, arrow)

# Set working directory ----------------------------------------------------
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/foodborne illness")

# Opening output files -----------------------------------------------------
records = paste0("record_", 1:20)

columns = c("year", "monthdth", "ucod", records)

strings = schema(!!!setNames(rep(list(utf8()), length(columns)), columns))

data = open_dataset("data/output", format = "csv",
  schema = strings,
  convert_options = csv_convert_options(
    null_values = c("", "NA"),
    strings_can_be_null = TRUE))
