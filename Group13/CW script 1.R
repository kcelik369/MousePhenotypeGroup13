library(data.table)

#Set your folder containing the CSV files
data_folder <- "/Users/hayashireiko/Desktop/Group13/data"
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
output_file <- "/Users/hayashireiko/Desktop/combined_data.csv"
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
# install.packages("BiocManager")
# BiocManager::install("stringr")

# Read in combined data file
setwd("/Users/hayashireiko/Desktop/Group13")
combined_data <- read.csv("combined_data.csv")
sop <- read.csv("IMPC_SOP.csv")

#Capitalize variable names (rows) in "combined_data" file
sop[, 1] <- toupper(sop[, 1])  
names(sop)
write.csv(sop, "IMPC_sop_capitalized.csv", row.names = FALSE)
sop <- read.csv("IMPC_sop_capitalized.csv")

check_variable <- function(df, curr_sop, variable_name, output_file = NULL) {
  
  # Ensure dplyr is loaded
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it first.")
  }
  
  # Locate variable in SOP
  sop_row <- match(variable_name, curr_sop[, 1])
  if (is.na(sop_row)) stop(paste("Variable", variable_name, "not found in SOP"))
  
  # Extract min and max lengths
  min_len <- curr_sop[sop_row, 3]
  max_len <- curr_sop[sop_row, 4]
  
  # Ensure variable column is character
  values <- as.character(df[[variable_name]])
  
  # Compute lengths and validate
  value_lengths <- nchar(values)
  valid_vec <- dplyr::between(value_lengths, min_len, max_len)
  
  # Replace invalid values with "NA"
  df[[variable_name]][!valid_vec] <- "NA"
  
  # Set default output file if not provided
  if (is.null(output_file)) {
    # Create output folder if it doesn't exist
    if (!dir.exists("output")) dir.create("output")
    output_file <- paste0("output/", variable_name, "_checked.csv")
  }
  
  # Save updated dataframe
  write.csv(df, output_file, row.names = FALSE)
  message("Updated file saved to: ", output_file)
  
  # Return updated dataframe
  return(df)
}

combined_data <- check_variable(combined_data, sop, "PARAMETER_ID")








