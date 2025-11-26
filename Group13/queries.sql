USE dcdm_project_db;

-- fields of interest for all genes of interest
-- can choose another field to dictate order by changing "ORDER BY" at end
SELECT DISTINCT ma.gene_symbol, ma.parameter_id, ma.parameter_name, ma.pvalue, proc.procedure_name FROM mouse_analyses ma
	LEFT JOIN parameters p ON ma.parameter_id = p.parameter_id AND ma.parameter_name = p.parameter_name
	LEFT JOIN impc_param_procedures ipp ON ipp.parameter_id = p.parameter_id AND ipp.parameter_name = p.parameter_name
	LEFT JOIN impc_procedures proc ON ipp.procedure_group_id = proc.group_id
	WHERE ma.gene_symbol IN ('Ctnnbip1', 'Dgcr2', 'Cdc20', 'Fcsk') ORDER BY ma.gene_symbol, ma.pvalue ASC;

-- version of the command for just one gene of interest; can replace 
-- gene symbol string with other gene symbols to access their relevant lists
-- symbols: 'Ctnnbip1', 'Dgcr2', 'Cdc20', 'Fcsk'
SELECT DISTINCT ma.gene_symbol, ma.parameter_id, ma.parameter_name, ma.pvalue, proc.procedure_name FROM mouse_analyses ma
	LEFT JOIN parameters p ON ma.parameter_id = p.parameter_id AND ma.parameter_name = p.parameter_name
	LEFT JOIN impc_param_procedures ipp ON ipp.parameter_id = p.parameter_id AND ipp.parameter_name = p.parameter_name
	LEFT JOIN impc_procedures proc ON ipp.procedure_group_id = proc.group_id
	WHERE ma.gene_symbol = 'Fcsk' ORDER BY pvalue ASC;

-- above command, but eliminates parameters unassociated with a procedure, in case procedure is the field of interest
SELECT DISTINCT ma.gene_symbol, ma.parameter_id, ma.parameter_name, ma.pvalue, proc.procedure_name FROM mouse_analyses ma
	INNER JOIN parameters p ON ma.parameter_id = p.parameter_id AND ma.parameter_name = p.parameter_name
	INNER JOIN impc_param_procedures ipp ON ipp.parameter_id = p.parameter_id AND ipp.parameter_name = p.parameter_name
	INNER JOIN impc_procedures proc ON ipp.procedure_group_id = proc.group_id
	WHERE ma.gene_symbol = 'Fcsk' ORDER BY pvalue ASC;

-- below disease query is empty for all genes of interest; can confirm by running commented 
-- commands and replacing strings with target gene symbol and resulting gene id, respectively
SELECT ma.gene_symbol, d.disease_name FROM mouse_analyses ma 
	INNER JOIN genes g ON g.gene_accession_id = ma.gene_accession_id
	INNER JOIN diseases d ON g.gene_accession_id = d.mouse_mgi_id
	WHERE ma.gene_symbol IN ('Ctnnbip1', 'Dgcr2', 'Cdc20', 'Fcsk') ORDER BY ma.gene_symbol;

-- double check below if doubting JOINs or table structure
-- SELECT DISTINCT gene_accession_id FROM mouse_analyses WHERE gene_symbol = 'Ctnnbip1';
-- SELECT disease_name FROM diseases WHERE mouse_mgi_id = "MGI:1915756";
-- SELECT DISTINCT gene_accession_id FROM mouse_analyses WHERE gene_symbol = 'Dgcr2';
-- SELECT disease_name FROM diseases WHERE mouse_mgi_id = "MGI:892866";
-- SELECT DISTINCT gene_accession_id FROM mouse_analyses WHERE gene_symbol = 'Cdc20';
-- SELECT disease_name FROM diseases WHERE mouse_mgi_id = "MGI:1859866";
-- SELECT DISTINCT gene_accession_id FROM mouse_analyses WHERE gene_symbol = 'Fcsk';
-- SELECT disease_name FROM diseases WHERE mouse_mgi_id = "MGI:1916071";

-- demonstrates that disease-relation does exist, but not for our target genes
SELECT DISTINCT ma.gene_symbol, d.disease_name FROM mouse_analyses ma 
	INNER JOIN genes g ON g.gene_accession_id = ma.gene_accession_id
	INNER JOIN diseases d ON g.gene_accession_id = d.mouse_mgi_id
	ORDER BY ma.gene_symbol;