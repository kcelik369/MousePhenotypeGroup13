CREATE DATABASE dcdm_mice_db;
USE dcdm_mice_db;

-- DROP TABLE diseases;
-- DROP TABLE omim_groups;
-- DROP TABLE mouse_data;
-- DROP TABLE parameter_descriptions;
-- DROP TABLE impc_procedures;
-- DROP TABLE procedure_groups;

-- Please load table data in the same order as tables are created here.
-- This ensures that foreign keys properly reference values, not causing errors.

-- table holding impc procedures and their descriptions
-- relevant impc parameter ids stored in below procedure_groups
CREATE TABLE impc_procedures (
    is_mandatory BOOLEAN,
    group_id INT PRIMARY KEY,
    procedure_name VARCHAR(255),
    procedure_description VARCHAR(600)
);

-- table for parameter descriptions    
CREATE TABLE parameter_descriptions (
    parameter_id VARCHAR(100),
    parameter_name VARCHAR(512),
    parameter_description TEXT,
    impc_parameter_orig_id INT PRIMARY KEY,
    procedure_group_id INT,
    FOREIGN KEY (procedure_group_id) REFERENCES impc_procedures(group_id)
);

-- combined data table
CREATE TABLE mouse_analyses (
    gene_accession_id VARCHAR(100),
    gene_symbol VARCHAR(100),
    mouse_strain VARCHAR(100),
    mouse_life_stage VARCHAR(100),
    parameter_id VARCHAR(100),
    pvalue DOUBLE,
    parameter_name VARCHAR(255),
    analysis_id VARCHAR(100) PRIMARY KEY -- ,
    -- FOREIGN KEY (parameter_id) REFERENCES parameter_descriptions(parameter_id)
);

-- ALTER TABLE mouse_analyses ADD FOREIGN KEY (parameter_id) REFERENCES parameter_descriptions(parameter_id);
-- ALTER TABLE parameter_descriptions ADD FOREIGN KEY (impc_parameter_orig_id) REFERENCES procedure_groups(impc_parameter_orig_id);
-- ALTER TABLE impc_procedures MODIFY COLUMN procedure_description VARCHAR(600);
-- ALTER TABLE procedure_groups ADD FOREIGN KEY (group_id) REFERENCES impc_procedures(procedure_group_id);

CREATE TABLE diseases (
    disease_id VARCHAR(100),
    disease_name VARCHAR(255),
    omim_group_id INT PRIMARY KEY,
    mouse_mgi_id VARCHAR(100)
);

create table omim_groups (
	id INT PRIMARY KEY AUTO_INCREMENT,
    group_id INT,
    omim_id VARCHAR(100),
    FOREIGN KEY (group_id) REFERENCES diseases(omim_group_id)
);

-- alter table mouse_data drop constraint mouse_data_ibfk_1;

-- alter table diseases add foreign key (omim_group_id) references omim_groups(group_id);

-- # optional (?) link table for many-to-many relationship
-- # btwn mouse_data.gene_accession_id and diseases.mouse_mgi_id
-- #create table mice_diseases_link (