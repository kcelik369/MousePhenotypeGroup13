library(dplyr)
library(tidyr)
library(stringr)

# change base_path here if necessary
base_path = "~/Documents/GitHub/MousePhenotypeGroup13/"

# uses merged dataset with only IMPC parameters instead of full param set
# matches use in R Shiny app
combined_data <- read.csv(
  str_c(base_path, "intermediate/parameter_id_cleaned.csv"),
  stringsAsFactors = FALSE
)

score_data <- combined_data %>%
  transmute(
    STRAIN = MOUSE_STRAIN,
    GENE_SYMBOL = GENE_SYMBOL,
    PHENOTYPE = PARAMETER_ID,
    PHENOTYPE_NAME = PARAMETER_NAME,
    P_VALUE = as.numeric(PVALUE)
  ) %>%
  distinct()

p_cutoff <- 0.05
df <- score_data %>% filter(P_VALUE <= p_cutoff)

temp <- aggregate(P_VALUE ~ GENE_SYMBOL + PHENOTYPE, data = df, FUN = min)
temp$P_VALUE[is.na(temp$P_VALUE) | temp$P_VALUE == 0] <- 1e-5
temp$LOGP <- -log10(temp$P_VALUE)

mat <- reshape(
  temp[, c("GENE_SYMBOL", "PHENOTYPE", "LOGP")],
  idvar = "GENE_SYMBOL",
  timevar = "PHENOTYPE",
  direction = "wide"
)

mat[is.na(mat)] <- 1e-5
rownames(mat) <- mat$GENE_SYMBOL
mat_matrix <- as.matrix(mat[, -1])

wss <- numeric(10)
for (k in 1:10) {
  set.seed(123)
  km <- kmeans(mat_matrix, centers = k)
  wss[k] <- km$tot.withinss
}

plot(
  1:10, wss, type = "b", pch = 19,
  xlab = "Number of Clusters (k)",
  ylab = "Total Within-Cluster Sum of Squares",
  main = "Elbow Method for Determining Optimal k"
)
