CREATE DATABASE dcdm_project_db;
USE dcdm_project_db;

-- table holding impc procedures and their descriptions
-- relevant impc parameter ids stored in below procedure_groups
CREATE TABLE impc_procedures (
    is_mandatory BOOLEAN,
    group_id INT PRIMARY KEY,
    procedure_name VARCHAR(255),
    procedure_description VARCHAR(600)
);

-- table for parameter features    
CREATE TABLE parameters (
    parameter_id VARCHAR(100),
    parameter_name VARCHAR(512),
    parameter_description TEXT,
    PRIMARY KEY (parameter_id, parameter_name)
);

-- table matching procedures to all their impc parameter ids
CREATE TABLE impc_param_procedures (
	procedure_group_id int,
    parameter_id VARCHAR(100),
    parameter_name VARCHAR(512),
	impc_parameter_orig_id INT PRIMARY KEY,
	param_grouping VARCHAR(100),
    FOREIGN KEY (parameter_id, parameter_name) REFERENCES parameters(parameter_id, parameter_name),
    FOREIGN KEY (procedure_group_id) REFERENCES impc_procedures(group_id)
);

-- store gene symbols and their associated IDs
CREATE TABLE genes (
    gene_accession_id VARCHAR(100) PRIMARY KEY,
    gene_symbol VARCHAR (100)
);

-- combined data table
CREATE TABLE mouse_analyses (
    gene_accession_id VARCHAR(100),
    gene_symbol VARCHAR(100),
    mouse_strain VARCHAR(100),
    mouse_life_stage VARCHAR(100),
    parameter_id VARCHAR(100),
    pvalue VARCHAR(50),
    parameter_name VARCHAR(255),
    analysis_id VARCHAR(100) PRIMARY KEY,
    FOREIGN KEY (parameter_id, parameter_name) REFERENCES parameters(parameter_id, parameter_name),
    FOREIGN KEY (gene_accession_id) REFERENCES genes(gene_accession_id)
);

CREATE TABLE diseases (
    disease_id VARCHAR(100),
    disease_name VARCHAR(255),
    omim_group_id INT PRIMARY KEY,
    mouse_mgi_id VARCHAR(100),
    FOREIGN KEY (mouse_mgi_id) REFERENCES genes(gene_accession_id)
);

CREATE TABLE omim_groups (
	id INT PRIMARY KEY AUTO_INCREMENT,
    group_id INT,
    omim_id VARCHAR(100),
    FOREIGN KEY (group_id) REFERENCES diseases(omim_group_id)
);

-- RUN BELOW AFTER DATA IS LOADED ONLY
-- this handles a quirk of DBeaver not being able to convert NA values from a CSV to NULL
-- by manually performing this conversion; nonetheless, there is some truncation,
-- a sacrifice to be able to use a numeric datatype for the pvalue column
UPDATE mouse_analyses SET pvalue = NULL WHERE pvalue = 'NA';
ALTER TABLE mouse_analyses MODIFY COLUMN pvalue DECIMAL(65,30);