library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(ggrepel)

# ----------------------- Load + Clean Data -----------------------
combined_data <- read.csv(
  "/Users/hayashireiko/Documents/Cleaned_combined.csv",
  stringsAsFactors = FALSE
)

score_data <- combined_data %>%
  transmute(
    STRAIN         = MOUSE_STRAIN,
    GENE_SYMBOL    = GENE_SYMBOL,
    PHENOTYPE      = PARAMETER_ID,
    PHENOTYPE_NAME = PARAMETER_NAME,
    P_VALUE        = as.numeric(PVALUE)
  ) %>%
  distinct()

# ------------------------------ UI -------------------------------
ui <- fluidPage(
  titlePanel("IMPC Phenotype Dashboard"),
  
  tabsetPanel(
    # ----------------------- Per Gene View -----------------------
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
               mainPanel(plotlyOutput("genePlot"))
             )),
    
    # -------------------- Per Phenotype View --------------------
    tabPanel("Per Phenotype View",
             sidebarLayout(
               sidebarPanel(
                 selectInput("phen", "Select Phenotype:",
                             choices = sort(unique(score_data$PHENOTYPE))),
                 sliderInput("cutoff_phen", "P-value cutoff:",
                             min = 0, max = 1, value = 0.05)
               ),
               mainPanel(plotlyOutput("phenPlot"))
             )),

    
    # -------------------------- PCA Cluster -------------------------
    tabPanel("Gene Clusters",
             mainPanel(plotlyOutput("pcaPlot")))
  )
)

# ------------------------------ SERVER ----------------------------
server <- function(input, output, session) {
  
  # Update gene list dynamically when strain changes
  observeEvent(input$strain, {
    available_genes <- score_data %>%
      filter(if (input$strain != "All") STRAIN == input$strain else TRUE) %>%
      pull(GENE_SYMBOL) %>%
      unique() %>% sort()
    
    updateSelectInput(session, "gene",
                      choices = available_genes,
                      selected = available_genes[1])
  })
  
  # Filtered data
  filtered_data <- reactive({
    if (input$strain == "All") return(score_data)
    score_data %>% filter(STRAIN == input$strain)
  })
  
  gene_data <- reactive({
    filtered_data() %>% filter(GENE_SYMBOL == input$gene)
  })
  
  phen_data <- reactive({
    score_data %>% filter(PHENOTYPE == input$phen)
  })
  # ------------------------ Plot 1: Per Gene (Bar Chart with Cutoff Filter) ------------------------
  output$genePlot <- renderPlotly({
    df <- gene_data()
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No data available for this strain + gene"))
    }
    
    # Filter by p-value cutoff
    df <- df %>% filter(P_VALUE <= input$cutoff_gene)
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No phenotypes pass the selected p-value cutoff"))
    }
    
    # Transform p-values to -log10(p)
    df <- df %>%
      mutate(LOGP = -log10(P_VALUE)) %>%
      arrange(desc(LOGP)) 
    
    p <- ggplot(df, aes(
      x = reorder(PHENOTYPE_NAME, LOGP),
      y = LOGP,
      text = paste0(
        "<b>Phenotype:</b> ", PHENOTYPE_NAME,
        "<br><b>ID:</b> ", PHENOTYPE,
        "<br><b>P-value:</b> ", signif(P_VALUE, 3),
        "<br><b>-log10(P):</b> ", round(LOGP, 2)
      )
    )) +
      geom_col(fill = "#d62728") +   # all bars are "significant" now
      coord_flip() +
      geom_hline(yintercept = -log10(input$cutoff_gene),
                 linetype = "dashed",
                 color = "black") +
      labs(
        x = "Phenotype",
        y = "-log10(P-value)",
        title = paste("Significant Phenotypes for", input$gene),
        subtitle = paste("Showing phenotypes with p ≤", input$cutoff_gene)
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 16, face = "bold")
      )
    
    ggplotly(p, tooltip = "text")
  })
  # ---------------------- Plot 2: Per Phenotype (Bar Chart with Cutoff Filter) ---------------------
  output$phenPlot <- renderPlotly({
    df <- phen_data()
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No data available for this phenotype"))
    }
    
    # Filter by p-value cutoff
    df <- df %>% filter(P_VALUE <= input$cutoff_phen)
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No genes pass the selected p-value cutoff"))
    }
    
    # Compute -log10(p)
    df <- df %>%
      mutate(LOGP = -log10(P_VALUE)) %>%
      arrange(desc(LOGP))   # sort
    
    p <- ggplot(df, aes(
      x = reorder(GENE_SYMBOL, LOGP),
      y = LOGP,
      text = paste0(
        "<b>Gene:</b> ", GENE_SYMBOL,
        "<br><b>Phenotype:</b> ", PHENOTYPE_NAME,
        "<br><b>P-value:</b> ", signif(P_VALUE, 3),
        "<br><b>-log10(P):</b> ", round(LOGP, 2)
      )
    )) +
      geom_col(fill = "#1f77b4") +
      coord_flip() +
      geom_hline(
        yintercept = -log10(input$cutoff_phen),
        linetype = "dashed",
        color = "black"
      ) +
      labs(
        x = "Knockout Gene",
        y = "-log10(P-value)",
        title = paste("Genes Associated with Phenotype", input$phen),
        subtitle = paste("Showing genes with p ≤", input$cutoff_phen)
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 16, face = "bold")
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # --------------------------- PCA Cluster -----------------------------
  output$pcaPlot <- renderPlotly({
    # Build gene x phenotype matrix
    
    mat <- score_data %>%
      group_by(GENE_SYMBOL, PHENOTYPE) %>%
      summarise(P_VALUE = -log10(min(P_VALUE)), .groups = "drop") %>%
      pivot_wider(names_from = PHENOTYPE, 
                  values_from = P_VALUE,
                  value_fill = 0)
    
    #Convert to matrix with gene names as rownames
    rownames(mat) <- mat[[1]]
    mat=mat[,-1]
    
    #mat$GENE_SYMBOL <- NULL
    #mat <- as.matrix(mat)
    
    # Safe handling of missing/zero values
    #mat[is.na(mat)] <- 0
    #mat <- -log10(mat)
    
    # Keep columns with variation
    #mat <- mat[, apply(mat, 2, var) > 0, drop = FALSE]
    
    # Check if PCA is possible
   # if(nrow(mat) < 2 || ncol(mat) < 2){
     # return(plotly_empty() %>% layout(title = "Not enough variation to perform PCA"))
    
    
    # Run PCA
    pca_res <- prcomp(mat, scale. = TRUE)
    pca_df <- data.frame(
      GENE_SYMBOL = rownames(mat),
      PC1 = pca_res$x[,1],
      PC2 = pca_res$x[,2]
    )
    
    gg <- qplot(x=PC1, y=PC2, data = pca_df, label = GENE_SYMBOL) +
      geom_point(size = 3, color = "steelblue") +
      geom_text_repel(aes(label = GENE_SYMBOL), size = 3) +
      labs(title = "PCA Cluster of Genes by Phenotype Profiles",
           x = "PC1", y = "PC2") +
      theme_bw()
    
    ggplotly(gg, tooltip = "text")
  })
  
}

# ---------------------- Run App ----------------------
shinyApp(ui, server)
