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
install.packages("openxlsx")
library(openxlsx)
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


# 1.Function that checks P-values are float values between 0 and 1
clean_pvalues <- function(input_file,
                          output_folder = "output",
                          output_csv = "cleaned_pvalue.csv",
                          output_excel = "highlighted_invalid_pvalue.xlsx") {
  
  # Create output folder if needed
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # File paths
  csv_path   <- file.path(output_folder, output_csv)
  excel_path <- file.path(output_folder, output_excel)
  
  # Read file
  df <- read.csv(input_file, stringsAsFactors = FALSE)
  
  # Preserve original for Excel
  df_original <- df
  
  # Convert and validate
  df$PVALUE_numeric <- suppressWarnings(as.numeric(df$PVALUE))
  invalid_mask <- is.na(df$PVALUE_numeric) | df$PVALUE_numeric < 0 | df$PVALUE_numeric > 1
  
  # Clean for CSV
  df$PVALUE[invalid_mask] <- NA
  df$PVALUE_numeric <- NULL
  
  # Write cleaned CSV
  write.csv(df, csv_path, row.names = FALSE)
  
  #Excel with red cell highlighting
  
  wb <- createWorkbook()
  addWorksheet(wb, "Data")
  writeData(wb, "Data", df_original)
  red_style <- createStyle(fgFill = "red")
  
  invalid_rows <- which(invalid_mask) + 1  # +1 because Excel has header row
  pvalue_col <- which(names(df_original) == "PVALUE")
  
  if (length(invalid_rows) > 0) {
    addStyle(
      wb, sheet = "Data", style = red_style,
      rows = invalid_rows, cols = pvalue_col,
      gridExpand = TRUE
    )
  }
  
  saveWorkbook(wb, excel_path, overwrite = TRUE)
  
  message("Finished.\n",
          "Cleaned CSV saved to: ", csv_path, "\n",
          "Excel with red-highlighted invalid PVALUEs saved to: ", excel_path)
}

clean_pvalues("combined_data.csv")



# 2.Function that checks that all values (apart from PVALUE) are strings
# This function only outputs new csv files if there are any invalid data types
ensure_strings_in_other_columns <- function(input_clean_csv,
                                            output_folder = "output",
                                            output_csv = "string_checked_output.csv",
                                            output_excel = "highlighted_nonstring.xlsx") {

  # Read cleaned CSV
  df <- read.csv(input_clean_csv, stringsAsFactors = FALSE)
  
  # Check PVALUE column exists
  if (!"PVALUE" %in% names(df)) {
    stop("The file does not contain a column named PVALUE.")
  }
  
  # Copy for Excel output
  df_original <- df
  
  # Track invalid values
  changed_mask <- matrix(FALSE, nrow = nrow(df), ncol = ncol(df))
  colnames(changed_mask) <- names(df)
  
  # Check all non-PVALUE columns
  for (col in names(df)) {
    if (col != "PVALUE") {
      non_string <- !sapply(df[[col]], is.character)
      changed_mask[, col] <- non_string
      
      # Replace invalid with NA
      df[[col]][non_string] <- NA
    }
  }
  
  # Check if any invalid data was found
  if (!any(changed_mask)) {
    message("No invalid data types")
    return(invisible(NULL))
  }
  
  #If invalid data exists, create files
  
  # Create output folder if needed
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # Save updated CSV
  csv_path <- file.path(output_folder, output_csv)
  write.csv(df, csv_path, row.names = FALSE)
  
  #Create Excel highlighting invalid values
  
  wb <- createWorkbook()
  addWorksheet(wb, "Data")
  writeData(wb, "Data", df_original)
  
  # Red background highlight
  red_style <- createStyle(fgFill = "red")
  
  # Apply highlighting cell-by-cell
  for (col in names(df)) {
    if (col != "PVALUE") {
      rows_to_highlight <- which(changed_mask[, col]) + 1
      col_index <- which(names(df) == col)
      
      if (length(rows_to_highlight) > 0) {
        addStyle(
          wb, sheet = "Data", style = red_style,
          rows = rows_to_highlight, cols = col_index,
          gridExpand = TRUE
        )
      }
    }
  }
  
  excel_path <- file.path(output_folder, output_excel)
  saveWorkbook(wb, excel_path, overwrite = TRUE)
  
  message("Finished.\n",
          "CSV saved to: ", csv_path, "\n",
          "Excel saved to: ", excel_path)
}

ensure_strings_in_other_columns("output/cleaned_pvalue.csv")


# 3.Function that checks the length of values
check_length <- function(df, curr_sop, vars,
                                     final_csv = "output/all_lengths_cleaned.csv",
                                     final_excel = "output/invalid_lengths_highlight.xlsx") {
  
  # Make output folder if missing
  if (!dir.exists("output")) dir.create("output", recursive = TRUE)
  
  # Original (for Excel)
  df_original <- df
  
  # Track invalid cells
  invalid_matrix <- matrix(FALSE, nrow = nrow(df), ncol = ncol(df))
  colnames(invalid_matrix) <- names(df)
  
  #process each variable (except for pvalue)
  for (variable_name in vars) {
    
    if (variable_name == "PVALUE") {
      message("Skipping PVALUE (numeric column).")
      next
    }
    
    message("Checking variable: ", variable_name)
    
    # Find SOP entry
    sop_row <- match(variable_name, curr_sop[, 1])
    if (is.na(sop_row)) stop(paste("Variable", variable_name, "not found in SOP"))
    
    min_len <- curr_sop[sop_row, 3]
    max_len <- curr_sop[sop_row, 4]
    
    # Extract values as characters
    values <- as.character(df[[variable_name]])
    value_lengths <- nchar(values)
    
    valid_vec <- dplyr::between(value_lengths, min_len, max_len)
    
    # Replace invalid string lengths with NA in FINAL CSV
    df[[variable_name]][!valid_vec] <- NA
    
    # Record invalid cells for Excel
    invalid_matrix[, variable_name] <- !valid_vec
  }
  
  #SAVE ONE FINAL CLEANED CSV
  write.csv(df, final_csv, row.names = FALSE)
  message("FINAL CLEANED CSV saved to: ", final_csv)
  
  #BUILD ONE EXCEL FILE WITH HIGHLIGHTING
  wb <- createWorkbook()
  addWorksheet(wb, "Invalid_Highlight")
  writeData(wb, "Invalid_Highlight", df_original)
  
  red_style <- createStyle(fgFill = "red")
  
  # Highlight invalid cells (except PVALUE)
  for (col in vars) {
    if (col == "PVALUE") next    # skip highlighting PVALUE
    
    invalid_rows <- which(invalid_matrix[, col])
    if (length(invalid_rows) > 0) {
      addStyle(
        wb, "Invalid_Highlight", red_style,
        rows = invalid_rows + 1,
        cols = which(names(df) == col),
        gridExpand = TRUE
      )
    }
  }
  
  saveWorkbook(wb, final_excel, overwrite = TRUE)
  message("Final Excel with highlighted invalid values saved to: ", final_excel)
  
  return(df)
}

vars_to_check <- c(
  "GENE_ACCESSION_ID",
  "GENE_SYMBOL",
  "MOUSE_STRAIN",
  "MOUSE_LIFE_STAGE",
  "PARAMETER_ID",
  "PVALUE",
  "PARAMETER_NAME",
  "ANALYSIS_ID"
)

cleaned_pvalue <- read.csv("output/cleaned_pvalue.csv")
final_df <- check_length(cleaned_pvalue, sop, vars_to_check)

# 4.Function that checks that the values for ANALYSIS_ID and GENE_ACCESSION_ID are alphanumeric

check_alphanumeric_validity <- function(df,
                              alphanumeric_val = c("ANALYSIS_ID", "GENE_ACCESSION_ID"),
                              final_csv = "output/alphanumeric_validity_checked.csv",
                              final_excel = "output/alphanumeric_invalid_highlight.xlsx") {
  
  # Trim whitespace for entire df copy
  df_original <- df
  df_clean <- df
  
  # Track invalid cells
  invalid_matrix <- matrix(FALSE, nrow = nrow(df), ncol = ncol(df))
  colnames(invalid_matrix) <- names(df)
  
  results <- list()
  any_invalid <- FALSE
  
  for (var in alphanumeric_val) {
    
    if (!var %in% names(df)) next
    
    # Clean whitespace
    values <- trimws(as.character(df[[var]]))
    
    # Valid characters: letters, numbers, colon
    is_alphanumeric <- grepl("^[A-Za-z0-9:]+$", values)
    is_alphanumeric[is.na(values)] <- FALSE
    
    invalid_rows <- which(!is_alphanumeric)
    
    # Store invalid rows
    results[[var]] <- invalid_rows
    
    if (length(invalid_rows) > 0) {
      any_invalid <- TRUE
      
      # Mark for highlighting
      invalid_matrix[invalid_rows, var] <- TRUE
      
      # Replace invalid values with NA in cleaned CSV
      df_clean[[var]][invalid_rows] <- NA
    }
  }
  

  # CASE 1: No invalid values → print success and exit

  if (!any_invalid) {
    message('All values for "ANALYSIS_ID" and "GENE_ACCESSION_ID" are alphanumeric.')
    return(invisible(TRUE))
  }
  

  # CASE 2: Invalid values exist → create output files

  if (!dir.exists("output")) dir.create("output", recursive = TRUE)
  
  # Save cleaned CSV
  write.csv(df_clean, final_csv, row.names = FALSE)
  message("Invalid values found. Cleaned CSV saved to: ", final_csv)
  
  # Build Excel file
  wb <- createWorkbook()
  addWorksheet(wb, "Invalid_Highlight")
  writeData(wb, "Invalid_Highlight", df_original)
  
  red_style <- createStyle(fgFill = "red")
  
  for (var in alphanumeric_val) {
    bad_rows <- results[[var]]
    if (length(bad_rows) > 0) {
      addStyle(
        wb, "Invalid_Highlight", red_style,
        rows = bad_rows + 1,
        cols = which(names(df) == var),
        gridExpand = TRUE
      )
    }
  }
  
  saveWorkbook(wb, final_excel, overwrite = TRUE)
  message("Excel with highlighted invalid values saved to: ", final_excel)
  
  return(results)
}

df <- read.csv("output/all_lengths_cleaned.csv")

results <- check_alphanumeric_validity(df)


# 5. Function that updates the official mouse GENE_SYMBOL values to title format
#Look up official gene symbols using biomaRt (Ensembl)



validate_and_correct_mouse_genes <- function(
    df,
    gene_col = "GENE_SYMBOL",
    final_csv = "output/gene_symbols_corrected.csv",
    final_excel = "output/gene_symbols_highlighted.xlsx"
) {
  if (!gene_col %in% names(df)) {
    stop(paste("Column", gene_col, "not found in dataframe."))
  }
  
  if (!dir.exists("output")) dir.create("output", recursive = TRUE)
  
  df_original <- df   # keep original for Excel highlighting
  
  # Connect to Ensembl (mouse) with fallback mirror
  message("Connecting to Ensembl (mouse)...")
  mart <- tryCatch(
    useMart("ensembl", dataset = "mmusculus_gene_ensembl"),
    error = function(e) {
      message("Main server unavailable, switching to US mirror...")
      useEnsembl("ensembl", dataset = "mmusculus_gene_ensembl", mirror = "useast")
    }
  )
  
  # Retrieve official mouse gene symbols
  message("Downloading official mouse gene symbols...")
  official <- getBM(
    attributes = "external_gene_name",
    mart = mart
  )$external_gene_name
  
  # ---- FORMAT MOUSE GENES CORRECTLY ----
  format_mouse_gene <- function(x) {
    if (is.na(x) || x == "") return(NA)
    
    x <- trimws(x)
    
    # Case 1: clone-like IDs beginning with digits
    if (grepl("^[0-9]", x)) {
      # If ends with RIK (any case), normalize to Rik
      if (grepl("RIK$", x, ignore.case = TRUE)) {
        prefix <- substr(x, 1, nchar(x)-3)
        return(paste0(prefix, "Rik"))
      }
      return(x)
    }
    
    # Case 2: RIKEN clone genes starting with letters
    if (grepl("Rik$", x, ignore.case = TRUE)) {
      prefix <- substr(x, 1, nchar(x)-3)
      return(paste0(prefix, "Rik"))
    }
    
    # Case 3: ALL CAPS human-style → convert to mouse capitalization
    if (grepl("^[A-Z0-9]+$", x)) {
      first <- substr(x, 1, 1)
      rest  <- substr(x, 2, nchar(x))
      return(paste0(toupper(first), tolower(rest)))
    }
    
    # Case 4: normal mouse-style mixed-case gene → enforce capitalization
    first <- substr(x, 1, 1)
    rest  <- substr(x, 2, nchar(x))
    
    return(paste0(toupper(first), tolower(rest)))
  }
  
  # Apply formatting
  formatted <- sapply(df[[gene_col]], format_mouse_gene)
  
  # Determine which rows were changed
  changed_rows <- which(formatted != df_original[[gene_col]])
  
  # ---- VALIDATE AGAINST ENSEMBL ----
  is_valid <- formatted %in% official
  invalid_rows <- which(!is_valid)
  
  # Replace invalid values with NA
  corrected <- formatted
  corrected[!is_valid] <- NA
  
  # Store corrected version back in df
  df[[gene_col]] <- corrected
  
  # ---- SAVE CORRECTED CSV ----
  write.csv(df, final_csv, row.names = FALSE)
  message("Corrected CSV saved to: ", final_csv)
  
  # ---- EXCEL OUTPUT WITH HIGHLIGHTING OF ALL CHANGES ----
  wb <- createWorkbook()
  addWorksheet(wb, "GeneSymbol_Highlight")
  
  writeData(wb, "GeneSymbol_Highlight", df_original)
  
  if (length(changed_rows) > 0) {
    red_style <- createStyle(fgFill = "red")
    
    addStyle(
      wb, "GeneSymbol_Highlight",
      style = red_style,
      rows = changed_rows + 1,     # header offset
      cols = which(names(df_original) == gene_col),
      gridExpand = TRUE
    )
  }
  
  saveWorkbook(wb, final_excel, overwrite = TRUE)
  message("Excel highlighting all changed symbols saved to: ", final_excel)
  
  return(list(
    changed_rows = changed_rows,
    original_values = df_original[[gene_col]][changed_rows],
    corrected_values = corrected[changed_rows]
  ))
}


df <- read.csv("output/all_lengths_cleaned.csv", stringsAsFactors = FALSE)

results <- validate_and_correct_mouse_genes(df)

# 6. Function that checks that all ANALYSIS_ID and GENE_ACCESSION_ID values are unique (i.e. associated with only one GENE_SYMBOL)
check_id_gene_consistency <- function(
    df,
    id_cols = c("ANALYSIS_ID", "GENE_ACCESSION_ID"),
    gene_col = "GENE_SYMBOL"
) {
  # Check if the ID and gene columns exist in the dataframe
  if (!all(id_cols %in% names(df))) {
    stop("Not all ID columns exist in dataframe.")
  }
  if (!gene_col %in% names(df)) {
    stop("GENE_SYMBOL column not found.")
  }
  
  conflicts_list <- list()
  conflict_rows <- c()
  
  # Check if any ANALYSIS_ID or GENE_ACCESSION_ID values are duplicated
  for (id in id_cols) {
    message("Checking: ", id)
    
    multi_map <- df %>%
      group_by(.data[[id]]) %>%
      summarize(
        n_genes = n_distinct(.data[[gene_col]], na.rm = TRUE),
        genes = paste(unique(.data[[gene_col]]), collapse = ";"),
        .groups = "drop"
      ) %>%
      filter(n_genes > 1)
    
    conflicts_list[[id]] <- multi_map
    
    if (nrow(multi_map) > 0) {
      # Store rows where conflicts are found
      conflicted_ids <- multi_map[[id]]
      these_rows <- which(df[[id]] %in% conflicted_ids)
      conflict_rows <- c(conflict_rows, these_rows)
    }
  }
  
  conflict_rows <- sort(unique(conflict_rows))  # Remove duplicates
  
  # If there are conflicts, return message and highlight rows
  if (length(conflict_rows) > 0) {
    message("There are conflicts in the following rows:")
    return(list(
      conflicts = conflicts_list,
      conflict_rows = conflict_rows
    ))
  } else {
    message("All ANALYSIS_ID and GENE_ACCESSION_ID values are unique")
    return(NULL)  # No conflicts
  }
}

df <- read.csv("output/gene_symbols_corrected.csv", stringsAsFactors = FALSE)

results <- check_id_gene_consistency(df)

#7. Function that checks MOUSE_STRAIN
validate_mouse_strain <- function(
    input_csv,
    output_csv = "output/mouse_strain_cleaned.csv",
    output_excel = "output/mouse_strain_invalid_highlight.xlsx",
    column = "MOUSE_STRAIN",
    allowed = c("C57BL", "B6J", "C3H", "129SV")
) {
  df <- read.csv(input_csv, stringsAsFactors = FALSE)
  if (!column %in% names(df)) stop(paste("Column", column, "missing."))
  
  if (!dir.exists("output")) dir.create("output", recursive = TRUE)
  
  df_original <- df
  df[[column]] <- trimws(df[[column]])
  
  invalid <- !(df[[column]] %in% allowed)
  df[[column]][invalid] <- NA
  
  write.csv(df, output_csv, row.names = FALSE)
  
  wb <- createWorkbook()
  addWorksheet(wb, "Data")
  writeData(wb, "Data", df_original)
  addStyle(
    wb, "Data", createStyle(fgFill = "red"),
    rows = which(invalid) + 1,
    cols = which(names(df) == column),
    gridExpand = TRUE
  )
  saveWorkbook(wb, output_excel, overwrite = TRUE)
  
  df
}

validate_mouse_strain("output/gene_symbols_corrected.csv")


#8. Function that capitalizes all values for GENE_ACCESSION_ID and PARAMETER_ID
capitalize_gene_and_parameter_ids <- function(
    input_csv,
    output_csv = "output/capitalized_ids.csv",
    cols = c("GENE_ACCESSION_ID", "PARAMETER_ID")
) {
  
  df <- read.csv(input_csv, stringsAsFactors = FALSE)
  
  # Create output folder if needed
  if (!dir.exists("output")) dir.create("output", recursive = TRUE)
  
  # Capitalize values
  for (col in cols) {
    df[[col]] <- toupper(trimws(df[[col]]))
  }
  
  # Save updated CSV
  write.csv(df, output_csv, row.names = FALSE)
  
  df
}

capitalize_gene_and_parameter_ids("output/mouse_strain_cleaned.csv")


#9. Function that removes all non-IMPC values for PARAMETER_ID
clean_parameter_id_prefix <- function(
    input_csv,
    output_csv = "output/parameter_id_cleaned.csv",
    output_excel = "output/parameter_id_invalid_highlight.xlsx",
    column = "PARAMETER_ID"
) {
  
  # Read data
  df <- read.csv(input_csv, stringsAsFactors = FALSE)
  
  # Create output folder if absent
  if (!dir.exists("output")) dir.create("output", recursive = TRUE)
  
  df_original <- df
  
  # Trim whitespace
  df[[column]] <- trimws(df[[column]])
  
  # Identify invalid values = those NOT starting with IMPC
  invalid <- !grepl("^IMPC", df[[column]], ignore.case = FALSE)
  
  # Replace invalid values with NA
  df[[column]][invalid] <- NA
  
  # Save cleaned CSV
  write.csv(df, output_csv, row.names = FALSE)
  
  # Create Excel highlighting original invalid values
  wb <- createWorkbook()
  addWorksheet(wb, "Data")
  writeData(wb, "Data", df_original)
  
  red_style <- createStyle(fgFill = "red")
  
  if (any(invalid)) {
    addStyle(
      wb, "Data", red_style,
      rows = which(invalid) + 1,   # offset for header
      cols = which(names(df) == column),
      gridExpand = TRUE
    )
  }
  
  saveWorkbook(wb, output_excel, overwrite = TRUE)
  
  message("Finished.\n",
          "Cleaned CSV saved to: ", output_csv, "\n",
          "Excel with highlighted invalid PARAMETER_ID values saved to: ", output_excel)
  
  return(df)
}

clean_parameter_id_prefix("output/capitalized_ids.csv")




# 10. Function that removes any row duplicates

# Load the necessary libraries
library(dplyr)

# Function to check for duplicate rows across all columns
check_duplicate_rows <- function(df) {
  # Check for duplicate rows across all columns
  duplicate_mask <- duplicated(df) | duplicated(df, fromLast = TRUE)
  
  # Find the rows that are duplicates
  duplicate_rows <- df[duplicate_mask, ]
  
  # Check if there are any duplicates
  if (nrow(duplicate_rows) > 0) {
    message("Duplicate rows found. These are the duplicate rows:")
    return(duplicate_rows)
  } else {
    message("No duplicate rows found.")
    return(NULL)
  }
}

# Load your CSV file (make sure the file path is correct)
df <- read.csv("output/gene_symbols_corrected.csv", stringsAsFactors = FALSE)

# Call the function to check for duplicates
duplicate_rows <- check_duplicate_rows(df)

# Print the duplicate rows if there are any
if (!is.null(duplicate_rows)) {
  print(duplicate_rows)
} else {
  message("No duplicate rows in the file.")
}




