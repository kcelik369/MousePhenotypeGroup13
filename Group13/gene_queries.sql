USE dcdm_mice_db;

-- fields of interest for all genes of interest
-- can choose another field to dictate order by changing "ORDER BY" at end
SELECT DISTINCT ma.gene_symbol, ma.parameter_id, ma.parameter_name, ma.pvalue, ip.procedure_name FROM mouse_analyses ma
	INNER JOIN parameter_descriptions pd ON ma.parameter_id = pd.parameter_id
	INNER JOIN impc_procedures ip ON pd.procedure_group_id = ip.group_id
	WHERE ma.gene_symbol IN ('Ctnnbip1', 'Dgcr2', 'Cdc20', 'Fcsk') ORDER BY ma.gene_symbol;

-- version of the command for just one gene of interest; can replace 
-- gene symbol string with other gene symbols to access their relevant lists
-- symbols: 'Ctnnbip1', 'Dgcr2', 'Cdc20', 'Fcsk'
SELECT DISTINCT ma.gene_symbol, ma.parameter_id, ma.parameter_name, ma.pvalue, ip.procedure_name FROM mouse_analyses ma
	INNER JOIN parameter_descriptions pd ON ma.parameter_id = pd.parameter_id
	INNER JOIN impc_procedures ip ON pd.procedure_group_id = ip.group_id
	WHERE ma.gene_symbol = 'Ctnnbip1';

-- below disease query is empty for all genes of interest; can confirm by running commented 
-- commands and replacing strings with target gene symbol and resulting gene id, respectively
SELECT ma.gene_symbol, d.disease_name FROM mouse_analyses ma 
	INNER JOIN diseases d ON ma.gene_accession_id = d.mouse_mgi_id 
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
	INNER JOIN diseases d ON ma.gene_accession_id = d.mouse_mgi_id 
	ORDER BY ma.gene_symbol; 