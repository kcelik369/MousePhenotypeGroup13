create database skibidy_toilet;
use skibidy_toilet;

CREATE TABLE combined_data (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    gene_accession_id VARCHAR(80),
    gene_symbol VARCHAR(80),
    mouse_strain VARCHAR(80),
    mouse_life_stage VARCHAR(80),
    parameter_id VARCHAR(80),
    pvalue DOUBLE,
    parameter_name VARCHAR(512),
    analysis_id VARCHAR(128)
    );
    
    CREATE TABLE parameter (
    parameter_id VARCHAR(80) PRIMARY KEY,
    name VARCHAR(512),
    description TEXT,
    procedure_id VARCHAR(80),
    INDEX idx_parameter_name (name(200))
);

CREATE TABLE procedure_table (
    procedure_id VARCHAR(80) PRIMARY KEY,
    procedure_name VARCHAR(255),
    procedure_category VARCHAR(100)
);

CREATE TABLE disease (
    disease_id VARCHAR(80),
    gene_symbol VARCHAR(80),
    disease_name VARCHAR(255),
    source VARCHAR(50),
    PRIMARY KEY (disease_id, gene_symbol),
    INDEX idx_disease_gene (gene_symbol)
);

CREATE TABLE parameter_group (
    group_id INT AUTO_INCREMENT PRIMARY KEY,
    group_name VARCHAR(100) UNIQUE,
    description TEXT
);

CREATE TABLE parameter_group_membership (
    group_id INT,
    parameter_id VARCHAR(80),
    PRIMARY KEY (group_id, parameter_id),
    INDEX idx_pg_parameter (parameter_id),
    FOREIGN KEY (group_id) REFERENCES parameter_group(group_id) ON DELETE CASCADE,
    FOREIGN KEY (parameter_id) REFERENCES parameter(parameter_id) ON DELETE CASCADE
);

