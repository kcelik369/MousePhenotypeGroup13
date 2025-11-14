library(readr)
library(dplyr)
library(stringr)

df <- read_lines("Disease_information.csv")

df_clean <- str_replace_all(df, "DOID:", "")
df_clean <- str_replace_all(df_clean, "MGI:", "")
#df_clean <- str_replace_all(df_clean, "OMIM:", "")

write_lines(df_clean, "Disease_information_clean2.csv")
