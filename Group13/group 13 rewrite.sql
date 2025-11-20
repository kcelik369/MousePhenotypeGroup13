create database skibidy_toilet;
use skibidy_toilet;


#combined data table
CREATE TABLE mouse_data (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    gene_accession_id VARCHAR(50),
    gene_symbol VARCHAR(50),
    mouse_strain VARCHAR(50),
    mouse_life_stage VARCHAR(50),
    parameter_id VARCHAR(50),
    pvalue DOUBLE,
    parameter_name VARCHAR(255),
    analysis_id VARCHAR(100)
    );
    
LOAD DATA LOCAL INFILE '/mnt/data/Cleaned_combined.csv'
INTO TABLE combined_data
FIELDS TERMINATED BY '\t'    -- your pasted sample looks tab-separated; if comma-separated use ','
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES
(gene_accession_id, gene_symbol, mouse_strain, mouse_life_stage, parameter_id, pvalue, parameter_name, analysis_id);

#table for parameter descriptions    
    CREATE TABLE parameter_description (
    parameter_id VARCHAR(80) PRIMARY KEY,
    parameter_name VARCHAR(512),
    parameter_description TEXT,
    procedure_group_id int,
    IMPC_parameter_orig_id int
);

LOAD DATA LOCAL INFILE '/mnt/data/IMPC_parameter_description_clean.csv'
INTO TABLE parameter
FIELDS TERMINATED BY ','    -- change to '\t' if that file is tab-separated
OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
IGNORE 1 LINES
(parameter_id, name, description);

CREATE TABLE procedure_table (
    is_mandatory boolean,
    procedure_group_id int PRIMARY KEY,
    procedure_name VARCHAR(255),
    procedure_description VARCHAR(255)
);

CREATE TABLE disease (
    disease_id VARCHAR(80),
    gene_symbol VARCHAR(80),
    disease_name VARCHAR(255),
    source VARCHAR(50),
    PRIMARY KEY (disease_id, gene_symbol),
    INDEX idx_disease_gene (gene_symbol)
);

create table disease_omims (
    group_id int,
    omim_id varchar(100)
    );

CREATE TABLE parameter_group (
    group_id INT AUTO_INCREMENT PRIMARY KEY,
    IMPC_parameter_orig_id varchar(100),
    group_name VARCHAR(100) UNIQUE,
    description TEXT
);

#potential
CREATE TABLE parameter_group_membership (
    group_id INT,
    parameter_id VARCHAR(80),
    PRIMARY KEY (group_id, parameter_id),
    INDEX idx_pg_parameter (parameter_id),
    FOREIGN KEY (group_id) REFERENCES parameter_group(group_id) ON DELETE CASCADE,
    FOREIGN KEY (parameter_id) REFERENCES parameter(parameter_id) ON DELETE CASCADE
);