library(readr)
library(dplyr)
library(janitor)

# read raw data
base_path = "~/Documents/GitHub/MousePhenotypeGroup13/"
read_path = "originals/IMPC_parameter_description.csv"
param_raw <- read_csv(str_c(base_path, read_path), show_col_types = FALSE)
# create new table to store cleaned data
param_clean <- param_raw %>% janitor::clean_names()

# iterate through columns by name
for (col in names(param_clean)) {
  vec <- param_clean[[col]]
  if (is.factor(vec)) { # change to string if necessary
    vec <- as.character(vec)
  }
  if (is.character(vec)) {
    vec <- trimws(vec) # remove whitespace if necessary
    param_clean[[col]] <- vec
  } else {
    param_clean[[col]] <- vec
  }
}

param_clean <- param_clean %>%
  filter(!(if_all(everything(), ~ is.na(.) | . == "")))

param_clean <- param_clean %>% distinct()

if ("parameterid" %in% names(param_clean)) {
  param_clean <- param_clean %>%
    distinct(parameterid, .keep_all = TRUE)
}
if ("parameterid" %in% names(param_clean)) {
  param_clean <- param_clean %>% arrange(parameterid)
}

# write out cleaned file
write_path = "output/IMPC_parameter_description_clean.csv"
write_csv(param_clean, str_c(base_path, write_path))

cat("Original rows: ", nrow(param_raw), "\n")
cat("Cleaned rows:  ", nrow(param_clean), "\n")
cat("Cleaned file saved as IMPC_parameter_description_clean.csv\n")
