##Combining Data files
library(data.table)

#Set your folder containing the CSV files
data_folder <- "~/Documents/GitHub/MousePhenotypeGroup13/Group13/data"
files <- list.files(data_folder, full.names = TRUE, pattern = "\\.csv$")

# Step 1: Read the first file to get column headers
ref_raw <- fread(files[1], header = FALSE)

#capitalize all names in the first column 
ref_raw[, V1 := toupper(V1)]

# assign the field names as column headers
column_names <- ref_raw$V1   

# Transpose the first (reference) file's values to create reference row
# V1 = col 1 (headings), V2 = col 2
ref_t <- transpose(ref_raw[, .(V2)])   # transpose the values
colnames(ref_t) <- column_names

#step 2: Write the first row to the output CSV
output_file <- "~/Documents/GitHub/MousePhenotypeGroup13/Group13/combined_data.csv"
fwrite(ref_t, output_file)

# Step 3: Loop through the remaining files and append to CSV
for (i in 2:length(files)) {
  dt <- fread(files[i], header = FALSE) #removes headers
  dt[, V1 := toupper(V1)] #capitalize first column
  dt <- dt[match(column_names, V1)] #sort by the reference order
  t_dt <- transpose(dt[, .(V2)])  
  colnames(t_dt) <- column_names
  
  # Append to CSV without rewriting the header
  fwrite(t_dt, output_file, append = TRUE)
}



##Cleaning data

# Load libraries
library(dplyr)
library(readr)
install.packages("BiocManager")
BiocManager::install("stringr")

# Step 1: Read in combined data file
setwd("/Users/hayashireiko/Desktop/Group13")
combined_data <- read.csv("combined_data.csv")

# Check "analysis_id" values
analysis_id_check <- function(x) {
  id <- x$ANALYSIS_ID
  
  length_check <- nchar(id) ==15
  alphanumeric_check <- str_detect(id, "^[A-Za-z0-9]+$")
  unique_check <- !duplicated
}

strFieldCheck = function(currStr) {
  
}


