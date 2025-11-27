## Data Cleaning and Management Group Project
Our assignment was to work from a disorganized initial dataset, formatting and cleaning it for analysis.

# Repository Organization
- `data/` stores the original unmerged and unclean analyses; these are separated from the other original files due to the their sheer quantity.
- `originals/` stores the original metadata files provided to the group; this includes the standard operating procedure which dictates the valid range of values for the data stored in `data/`, as well as some CSVs of related information to be converted into tables.
- `scripts/` stores the scripts created by the group; please see the README in that folder for the correct order to run our scripts to reproduce our work. The R Shiny app is also stored here.
- `intermediate/` stores a number of files we generate for review, but do not use in our final table creation or R Shiny app.
- `output/` stores the CSV files we generate to load into tables in SQL, as well as a manually generated file detailing the parameter groupings we have created. Again, see the README here for more details.
- `SQL/` stores our SQL scripts for creating the tables we will load our cleaned and transformed data into. It also stores a database diagram, the dump file for our SQL database, and the SQL queries used to investigate the genes of interest our group was given.