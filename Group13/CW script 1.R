library(data.table)

#Set your folder containing the CSV files
data_folder <- "/Users/hayashireiko/Desktop/Group13/data"
files <- list.files(data_folder, full.names = TRUE, pattern = "\\.csv$")

# Step 1: Read the first file to get column headers
ref_raw <- fread(files[1], header = FALSE)
column_names <- ref_raw$V1   # assign the field names as column headers

# Transpose the first (reference) file's values to create reference row
# V1 = col 1 (headings), V2 = col 2
ref_t <- transpose(ref_raw[, .(V2)])   # transpose the values
colnames(ref_t) <- column_names

#step 2: Write the first row to the output CSV
output_file <- "/Users/hayashireiko/Desktop/combined_data.csv"
fwrite(final_data, output_file)

# Step 3: Loop through the remaining files and append to CSV
for (i in 2:length(files)) {
  dt <- fread(files[i], header = FALSE)
  t_dt <- transpose(dt[, .(V2)])
  
  # Append to CSV without rewriting the header
  fwrite(t_dt, output_file, append = TRUE)
}

# vec = rep(0, length(files))
# for (i in length(files)) {
#   t_dt <- transpose(dt[, 1]) # get header row
#   print(t_dt)
#   vec[i] = t_dt
# }
# unique(vec)
# print(vec)







  