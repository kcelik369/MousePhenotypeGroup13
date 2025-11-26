# Overview
This repository features a mixture of R scripts and Quarto markdowns (based on personal preference of lead coder for that project section). The order to run the files in can be found below. Where relevant, the scripts feature a `base_path` variable that can be changed based on the location of the project on your local machine. This is the first thing to check if "file exists" or "path" errors occur during running.

Supporting files, such as disease information, parameter descriptions, and procedure data, were also standardized to ensure consistency and compatibility with the database schema.

# Run Order and File Explanation
1. packages.R : Installs the packages we used across all our scripts; ensures there should be no error on missing functions.
1. Data_merging_and_cleaning_script.R : IMPC CSV files were consolidated into a merged table and cleaned with comprehensive quality control checks, including p-value validation, identifier formatting, gene symbol standardization, and duplicate removal.
1. parameterCleaning.qmd : impc_parameter_descriptions file gets split into IMPC_procedure_params and parameters to retain unique IDs that can move from the previously merged analyses table to the procedures table created next.
1. procedureCleaning.qmd : eliminated many redundant rows by reforming relation with IMPC parameters to reference "procedure groups" which cover a range of IMPC parameters. Adds a new row to the IMPC_procedure_params table created just previously to maintain connectivity.
1. geneTable.qmd : Created from the gene-related rows of the merged analyses table, used to link from there to the table of diseases. Redundant storage of gene symbol information; that column was left in the merged analyses table to keep parity with the CSV referenced by the R Shiny app.
1. diseaseTables.qmd : Reduces the list of stored diseases to only those related to genes in our merged analyses table (via the genes table). Also unpacks the OMIM ID lists into its own table "OMIM groups" to ensure obedience of first normal form.
1. ElbowMethod.R : Demonstrates rationale for default cluster number value during PCA analysis in the below R Shiny app.
1. final_rshiny_app.R : Interactive R Shiny dashboard to show mouse phenotypic data from the IMPC in three different ways: 
    1. Per Gene View to see all the phenotypes linked to specific knockout genes, 
    1. Per Phenotype View to look at genes associated with particular phenotypes, 
    1. Gene Clusters tab that uses PCA with k-means clustering to find genes with similar phenotype profiles
    - The dashboard reads our cleaned CSV data, uses -log10(p-value) transformation to make the data easier to understand, and gives you interactive Plotly charts with DT data tables for a clear and accurate analysis.
