# ui-normalization.R


# parameters

fluidPage(useSweetAlert(), fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(
      selectInput(
        "normMethod",
        "Normalization Method",
        c("TMM" = "tmm",
          "DESeq2" = "deseq2")
      ),
      selectInput(
        "testMethod",
        "DEG Identification Method",
        c(
          "edgeR" = "edger",
          "DESeq2" = "deseq2",
          "baySeq" = "bayseq"
        )),
      numericInput(
        inputId = "fdr",
        label = "FDR Cut-off",
        min = 0.00001,
        value = 0.01,
        max = 1,
        step = 0.0001
      ),
      sliderInput(
        "floorpdeg",
        "Elimination of Potential DEGs",
        min = 0,
        max = 1,
        value = 0.05,
        step = 0.05
      ),
      do.call(actionBttn, c(
        list(
          inputId = "DEA",
          label = "Run Analysis",
          icon = icon("play")
        )))
    ))),
  
  #result table 
  column(
    9,
    navbarPage("Results",
               id = "tabs",
               tabPanel(
                 title = tagList(icon("question"), "TCC info"),
                 width = NULL,
                 solidHeader = T,
                 status = "primary",
                 includeMarkdown("documents/tccinfo.Rmd")
               ),
               tabPanel(
                 title = tagList(icon("table"), "Normalization Table"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 uiOutput("NormResultTable")
               ),
               tabPanel(
                 title = tagList(icon("table"), "Result Table"),
                 value = 'redirectres',
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 uiOutput("mainResultTable")
               ),
               tabPanel(
                 title = tagList(icon("table"), "DEG Table"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 uiOutput("mainsortedResultTable")
               )
    ))))