library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(ggrepel)
library(DT)

# Change base path here
base_path = "~/Documents/GitHub/MousePhenotypeGroup13/"
comb_data_path = str_c(base_path, "output/combined_data.csv")

combined_data <- read.csv(comb_data_path, stringsAsFactors = FALSE)

score_data <- combined_data %>%
  transmute(
    STRAIN         = MOUSE_STRAIN,
    GENE_SYMBOL    = GENE_SYMBOL,
    PHENOTYPE      = PARAMETER_ID,
    PHENOTYPE_NAME = PARAMETER_NAME,
    P_VALUE        = as.numeric(PVALUE)
  ) %>%
  distinct()

# Prepare choices sorted alphabetically by phenotype name
phen_choices <- score_data %>%
  distinct(PHENOTYPE, PHENOTYPE_NAME) %>%
  arrange(PHENOTYPE_NAME)  # sort alphabetically

# UI
ui <- fluidPage(
  titlePanel("IMPC Phenotype Dashboard"),
  
  tabsetPanel(
    # Per Gene View
    tabPanel("Per Gene View",
             sidebarLayout(
               sidebarPanel(
                 selectInput("strain", "Select Mouse Strain:",
                             choices = c("All", sort(unique(score_data$STRAIN))),
                             selected = "All"),
                 selectInput("gene", "Select Knockout Gene:",
                             choices = sort(unique(score_data$GENE_SYMBOL)),
                             selected = sort(unique(score_data$GENE_SYMBOL))[1]),
                 sliderInput("cutoff_gene", "P-value cutoff:",
                             min = 0, max = 1, value = 0.05)
               ),
               mainPanel(
                 plotlyOutput("genePlot"),
                 br(),
                 DTOutput("gene_table")
               )
             )),
    # Per Phenotype View
    
    tabPanel("Per Phenotype View",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "phen", "Select Phenotype:",
                   choices = setNames(
                     phen_choices$PHENOTYPE,
                     paste0(phen_choices$PHENOTYPE_NAME, " - ", phen_choices$PHENOTYPE)
                   )
                 ),
                 sliderInput("cutoff_phen", "P-value cutoff:",
                             min = 0, max = 1, value = 0.05)
               ),
               mainPanel(
                 plotlyOutput("phenPlot"),
                 br(),
                 DTOutput("phen_table")
               )
             )),
    
    # PCA Cluster
    tabPanel("Gene Clusters",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("cutoff_pca", "P-value cutoff:",
                             min = 0, max = 1, value = 0.05),
                 sliderInput("k_clusters", "Number of clusters:",
                             min = 2, max = 10, value = 3, step = 1)
               ),
               mainPanel(
                 plotlyOutput("pcaPlot"),
                 br(),
                 DTOutput("clusterSummary")
               )
             ))
  )
)

# Server
server <- function(input, output, session) {
  
  gene_data <- reactive({
    df <- score_data
    if (input$strain != "All") df <- df[df$STRAIN == input$strain, ]
    df[df$GENE_SYMBOL == input$gene, ]
  })
  
  phen_data <- reactive({
    score_data[score_data$PHENOTYPE == input$phen, ]
  })
  
  # Per Gene Plot
  output$genePlot <- renderPlotly({
    df <- gene_data()
    df <- df[df$P_VALUE <= input$cutoff_gene, ]
    if (nrow(df) == 0) return(plotly_empty() %>% layout(title = "No phenotypes pass the p-value cutoff"))
    
    df$LOGP <- -log10(df$P_VALUE)
    
    p <- ggplot(df, aes(
      x = reorder(PHENOTYPE_NAME, LOGP),
      y = LOGP,
      text = paste0("<b>Phenotype:</b> ", PHENOTYPE_NAME,
                    "<br><b>ID:</b> ", PHENOTYPE,
                    "<br><b>P-value:</b> ", signif(P_VALUE, 3),
                    "<br><b>-log10(P):</b> ", round(LOGP, 2))
    )) +
      geom_col(fill = "#d62728") +
      coord_flip() +
      geom_hline(yintercept = -log10(input$cutoff_gene),
                 linetype = "dashed", color = "black") +
      labs(x = "Phenotype", y = "-log10(P-value)",
           title = paste("Significant Phenotypes for", input$gene),
           subtitle = paste("Showing phenotypes with p ≤", input$cutoff_gene)) +
      theme_bw() +
      theme(plot.title = element_text(size = 16, face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Per Gene Table
  output$gene_table <- renderDT({
    df <- gene_data()
    df <- df[df$P_VALUE <= input$cutoff_gene, ]
    if (nrow(df) == 0) return(NULL)
    
    df <- df %>%
      mutate(P_VALUE = ifelse(P_VALUE == 0, 1e-16, P_VALUE)) %>%
      arrange(PHENOTYPE) %>%
      select(GENE_SYMBOL, STRAIN, PHENOTYPE, PHENOTYPE_NAME, P_VALUE)
    
    datatable(
      df,
      options = list(
        pageLength = min(10, nrow(df)),  
        lengthChange = FALSE,
        scrollX = TRUE,
        paging = nrow(df) > 10
      ),
      rownames = FALSE
    ) %>%
      formatSignif("P_VALUE", digits = 4)
  })
  
  # Per Phenotype Plot
  output$phenPlot <- renderPlotly({
    df <- phen_data()
    df <- df[df$P_VALUE <= input$cutoff_phen, ]
    if (nrow(df) == 0) return(plotly_empty() %>% layout(title = "No genes pass the p-value cutoff"))
    
    df$LOGP <- -log10(df$P_VALUE)
    
    p <- ggplot(df, aes(
      x = reorder(GENE_SYMBOL, LOGP),
      y = LOGP,
      text = paste0("<b>Gene:</b> ", GENE_SYMBOL,
                    "<br><b>Phenotype:</b> ", PHENOTYPE_NAME,
                    "<br><b>P-value:</b> ", signif(P_VALUE, 3),
                    "<br><b>-log10(P):</b> ", round(LOGP, 2))
    )) +
      geom_col(fill = "#1f77b4") +
      coord_flip() +
      geom_hline(yintercept = -log10(input$cutoff_phen),
                 linetype = "dashed", color = "black") +
      labs(x = "Knockout Gene", y = "-log10(P-value)",
           title = paste("Genes Associated with Phenotype", input$phen),
           subtitle = paste("Showing genes with p ≤", input$cutoff_phen)) +
      theme_bw() +
      theme(plot.title = element_text(size = 16, face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Per Phenotype Table
  output$phen_table <- renderDT({
    df <- phen_data()
    df <- df[df$P_VALUE <= input$cutoff_phen, ]
    if (nrow(df) == 0) return(NULL)
    
    # Aggregate by gene to reduce number of rows
    df <- df %>%
      group_by(PHENOTYPE, PHENOTYPE_NAME, GENE_SYMBOL) %>%
      summarise(P_VALUE = min(P_VALUE, na.rm = TRUE), .groups = "drop") %>%
      arrange(GENE_SYMBOL)
    
    datatable(
      df,
      options = list(
        pageLength = min(10, nrow(df)),
        lengthChange = FALSE,
        scrollX = TRUE,
        paging = nrow(df) > 10
      ),
      rownames = FALSE
    ) %>%
      formatSignif("P_VALUE", digits = 4)
  })
  
  
  # PCA Cluster Plot
  output$pcaPlot <- renderPlotly({
    df <- score_data[score_data$P_VALUE <= input$cutoff_pca, ]
    temp <- aggregate(P_VALUE ~ GENE_SYMBOL + PHENOTYPE, data = df, FUN = min)
    temp$P_VALUE[is.na(temp$P_VALUE) | temp$P_VALUE == 0] <- 1e-16
    temp$LOGP <- -log10(temp$P_VALUE)
    
    mat <- reshape(temp[, c("GENE_SYMBOL", "PHENOTYPE", "LOGP")],
                   idvar = "GENE_SYMBOL", timevar = "PHENOTYPE", direction = "wide")
    mat[is.na(mat)] <- 1e-16
    rownames(mat) <- mat$GENE_SYMBOL
    mat_matrix <- as.matrix(mat[, -1])
    
    pca_res <- prcomp(mat_matrix, scale. = TRUE)
    set.seed(123)
    cluster_assign <- kmeans(pca_res$x[, 1:2], centers = input$k_clusters)$cluster
    
    pca_df <- data.frame(
      GENE_SYMBOL = rownames(mat_matrix),
      PC1 = pca_res$x[,1],
      PC2 = pca_res$x[,2],
      Cluster = factor(cluster_assign)
    )
    
    gg <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster, text = GENE_SYMBOL)) +
      geom_point(size = 3) +
      geom_text_repel(aes(label = GENE_SYMBOL), size = 3, max.overlaps = 10) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      geom_vline(xintercept = 0, linetype = "solid", color = "black") +
      labs(title = paste("PCA Cluster of Genes by Phenotype Score (P ≤", input$cutoff_pca, ")"),
           x = "PC1", y = "PC2") +
      theme_bw() +
      theme(panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_line(color = "gray90"))
    
    ggplotly(gg, tooltip = "text")
  })
  
  # PCA Cluster Summary Table
  output$clusterSummary <- renderDT({
    # Filter by cutoff
    df <- score_data[score_data$P_VALUE <= input$cutoff_pca, ]
    
    if(nrow(df) == 0) return(NULL)
    
    # Aggregate min P_VALUE per gene-phenotype
    temp <- aggregate(P_VALUE ~ GENE_SYMBOL + PHENOTYPE, data = df, FUN = min)
    temp$P_VALUE[is.na(temp$P_VALUE) | temp$P_VALUE == 0] <- 1e-5
    temp$LOGP <- -log10(temp$P_VALUE)
    
    # Reshape for PCA (full matrix)
    mat <- reshape(temp[, c("GENE_SYMBOL", "PHENOTYPE", "LOGP")],
                   idvar = "GENE_SYMBOL", timevar = "PHENOTYPE", direction = "wide")
    mat[is.na(mat)] <- 1e-5
    rownames(mat) <- mat$GENE_SYMBOL
    mat_matrix <- as.matrix(mat[, -1])
    
    # PCA & clustering
    pca_res <- prcomp(mat_matrix, scale. = TRUE)
    set.seed(123)
    cluster_assign <- kmeans(pca_res$x[, 1:2], centers = min(input$k_clusters, nrow(mat_matrix)))$cluster
    
    cluster_df <- data.frame(
      GENE_SYMBOL = rownames(mat_matrix),
      Cluster = factor(cluster_assign),
      stringsAsFactors = FALSE
    )
    
    cluster_table <- df %>%
      group_by(GENE_SYMBOL) %>%
      slice_min(P_VALUE, n = 1, with_ties = FALSE) %>%
      ungroup()
    
    cluster_table$P_VALUE[cluster_table$P_VALUE == 0] <- 1e-5
    
    # Merge clusters from PCA
    cluster_table <- merge(cluster_table, cluster_df, by = "GENE_SYMBOL")
    
    # Keep original columns + cluster
    cluster_table <- cluster_table %>%
      select(Cluster, GENE_SYMBOL, STRAIN, PHENOTYPE, PHENOTYPE_NAME, P_VALUE) %>%
      arrange(Cluster, P_VALUE)
    
    datatable(
      cluster_table,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatSignif("P_VALUE", digits = 4)
  })
  
}

shinyApp(ui, server) #Run App
