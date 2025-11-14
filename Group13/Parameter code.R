library(readr)
library(dplyr)
param_raw <- read_csv("IMPC_parameter_description.csv", show_col_types = FALSE)
param_clean <- param_raw %>%
  janitor::clean_names()

for (col in names(param_clean)) {
  vec <- param_clean[[col]]
  if (is.factor(vec)) {
    vec <- as.character(vec)
  }
  if (is.character(vec)) {
    vec <- trimws(vec)
    param_clean[[col]] <- vec
  } else {
    param_clean[[col]] <- vec
  }
}
param_clean <- param_clean %>%
  filter(!(if_all(everything(), ~ is.na(.) | . == "")))
param_clean <- param_clean %>%
  distinct()
if ("parameterid" %in% names(param_clean)) {
  param_clean <- param_clean %>%
    distinct(parameterid, .keep_all = TRUE)
}
if ("parameterid" %in% names(param_clean)) {
  param_clean <- param_clean %>% arrange(parameterid)
}

write_csv(param_clean, "IMPC_parameter_description_clean.csv")

cat("Original rows: ", nrow(param_raw), "\n")
cat("Cleaned rows:  ", nrow(param_clean), "\n")
cat("Cleaned file saved as IMPC_parameter_description_clean.csv\n")
