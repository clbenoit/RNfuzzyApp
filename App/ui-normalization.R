# ui-normalization.R


### The normalization is made using TCC. 
### TCC is a package for comparing tag count data 
### with robust normalization strategies
#https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-219
#https://bioconductor.org/packages/release/bioc/vignettes/TCC/inst/doc/TCC.pdf

# parameters

fluidPage(useSweetAlert(), fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "Normalization Parameters"),
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
  
      sliderInput(
        "fdr",
        "FDR Cut-off",
        min = 0,
        max = 1,
        value = 0.1,
        step = 0.05
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
          inputId = "TCC",
          label = "Run Normalization",
          icon = icon("play")
        )))
    ))),
  
  #result table 
column(
  9,
  navbarPage("Results",
  tabPanel(
    title = tagList(icon("question"), "TCC info"),
    width = NULL,
    solidHeader = T,
    status = "primary",
    includeMarkdown("documents/tccinfo.Rmd")
     ),
  tabPanel(
    title = tagList(icon("table"), "Result Table"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("mainResultTable")
  ),
  tabPanel(
    title = tagList(icon("table"), "Summary of Normalization"),
    width = NULL,
    solidHeader = TRUE,
    status = "info",
    uiOutput("tccSummationUI")
  )
))))