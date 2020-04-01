fluidPage(fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(
      selectInput(
        "chosendatabase",
        "Choose your Organism",
        c("Drosophila melanogaster" = "org.Dm.eg.db",
          "Mus musculus" = "org.Mm.eg.db ",
          "Homo sapiens" = "org.Hs.eg.db", 
          "Caenorhabditis elegans" = "org.Ce.eg.db",
          "Escherichia coli" = "org.EcK12.eg.db")
      ),
      selectInput(
        "inputtype",
        "Choose your input type",
        c("EntrezID" = "ENTREZID",
          "EnsemblID" = "ENSEMBL",
          " Symbol" = "SYMBOL")
      ),
      textAreaInput(
        "inputids",
        "Paste Gene List",
        rows = 5,
        placeholder = "Input genes, one gene per line."
      )
    )
  ),
  do.call(actionBttn, c(
    list(
      inputId = "convgo",
      label = "Convert",
      icon = icon("play")
    )))
),

#result table 
column(
  9,
  navbarPage("Results",
             id = "convtabs",
             tabPanel(
               title = tagList(icon("question"), "Info"),
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               includeMarkdown("documents/convinfo.Rmd")
             ),
             tabPanel(
               title = tagList(icon("table"), "Result Table"),
               value = 'redirectconv',
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               uiOutput('ConversionResults')
             )
  ))))