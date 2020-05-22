#ui-conversion.R
# visible part of the ID conversion tool including selection of the organism, 
# the type of input and the place to paste the gene set. 
# provide a panel with a Rmd of informations and a panel with a result table

fluidPage(fluidRow(column(
  3,
  box(                                                 #parameter box
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(                                           # set of parameters
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
  do.call(actionBttn, c(                           #validation button 
    list(
      inputId = "convgo",
      label = "Convert",
      icon = icon("play")
    )))
),




column(
  9,
  navbarPage("Results",
             id = "convtabs",
             tabPanel(                         # panel of information 
               title = tagList(icon("question"), "Info"),
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               includeMarkdown("documents/convinfo.Rmd")
             ),
             tabPanel(                        # panel of result table 
               title = tagList(icon("table"), "Result Table"),
               value = 'redirectconv',
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               uiOutput('ConversionResults')
             )
  ))))