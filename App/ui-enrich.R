
fluidPage(fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(
      selectInput(
        "chosendataset",
        "Choose your Organism",
        c("Drosophila melanogaster" = "org.Dm.eg.db",
          "Mus musculus" = "org.Mm.eg.db ",
          "Homo sapiens" = "org.Hs.eg.db", 
          "Caenorhabditis elegans" = "org.Ce.eg.db",
          "Escherichia coli" = "org.EcK12.eg.db")
      ),
      textAreaInput(
        "refseqids",
        "Paste Gene List",
        rows = 5,
        placeholder = "Input refseq ids, one gene per line."
        
      ),
      selectInput(
        "chosenGO",
        "Choose your Enrichment",
        c( "Biological Process" = "BP",
           "Molecular Fonction" = "MF",
           "Cellular Component" = "CC",
           "All" = 'ALL')
      ),
      sliderInput(
        "pval",
        "P-value Cut-off",
        min = 0.00001,
        max = 0.01,
        value = 0.001,
        step = 0.001
      ),
      selectInput(
        "pvaladj",
        "P-value Adjustment",
        c("None" = "none",
          "Bonferroni" = "bonferroni",
          "Holm" = "holm",
          "Hochberg" = "hochberg",
          "Hommel" = "hommel",
          "BH" = "BH",
          "BY" = "BY",
          "FDR" = "fdr")
      )
    ),
    do.call(actionBttn, c(
      list(
        inputId = "enrichmentgo",
        label = "Enrich",
        icon = icon("play")
      )))
  )),
  
  #result table 
  column(
    9,
    navbarPage("Results",
               id = "entabs",
               tabPanel(
                 title = tagList(icon("question"), "Info"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 includeMarkdown("documents/goinfo.Rmd")
               ),
               tabPanel(
                 title = tagList(icon("table"), "Result Table"),
                 value = 'redirectres',
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 uiOutput('EnrichResults')
               ),
               tabPanel(
                 title = tagList(icon("braille"), "Graph"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 plotlyOutput('statenrich', height = 800)%>% withSpinner()
               ),
               tabPanel(
                 title = tagList(icon("braille"), "Pie Chart"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 plotlyOutput('pieenrich', height = 800)%>% withSpinner()
               )
    ))))