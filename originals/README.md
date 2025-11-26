# Overview
The "metadata" files given to us to use in addition to the unmerged IMPC analysis data. 

# File Explanations
- query_genes.csv : four gene symbols denoting genes of interest for us to investigate further; see SQL/ directory. The only table to not be queried during script execution.
- IMPC_SOP.csv : standard operating procedure for the IMPC records and their data found in the data/ directory. Used directly during merging and cleaning of the merged analyses table to ensure the validity of all values.
- All other .csv files : tables containing additional data related to the IMPC analysis records that can expand the scope of related information. Used directly to make new tables related to the merged analyses table.