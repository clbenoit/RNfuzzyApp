#ui-enrich.R


fluidPage(fluidRow(column(
  3,
  box(                          #parameter box 
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(                    # set of parameters
      textAreaInput(
        "list_ids",
        "Paste Gene List",
        rows = 5,
        placeholder = "Input ids, one gene per line."
      ),
      sliderInput(
        "topres_enrich",
        "Top results to show",
        min = 1,
        max = 50,
        value = 10,
        step = 1
      ),
      selectInput(
        "inputorg",
        "Choose your Organism",
        c("Drosophila melanogaster" = "dmelanogaster",
          "Mus musculus" = "mmusculus",
          "Homo sapiens" = "hsapiens", 
          "Caenorhabditis elegans" = "celegans",
          "Zebrafish" = "drerio",
          "Aspergillus fumigatus Af293" = "afumigatus",
          "Bonobo" = "ppaniscus",
          "Cat" = "fcatus",
          "Chicken" = "ggallus",
          "Chimpanzee" = "ptroglodytes",
          "Common Carp" = "ccarpio",
          "Cow" = "btaurus",
          "Dog" = "clfamiliaris",
          "Dolphin" = "ttruncatus",
          "Goat" = "chircus",
          "Gorilla" = "ggorilla",
          "Guppy" = "preticulata",
          "Horse" = "ecaballus",
          "Pig" = "sscrofa",
          "Platypus" = "oanatinus",
          "Rabbit" = "ocuniculus")
      ),
      checkboxGroupInput(
        "chosenEnrich",
        "Choose your enrichment",
        c("GO : Biological Processes" = "GO:BP",
          "GO : Molecular Functions" = "GO:MF",
          "GO : Cellular Components" = "GO:CC",
          "KEGG Pathways" = "KEGG",
          "Reactome" = "REAC",
          "WikiPathways" = "WP"),
        selected = c("GO:BP", "GO:MF", "GO:CC", "KEGG", "REAC","WP")
      ),
      selectInput(
        "correction",
        "Multiple testing correction",
        c("gSCS threshold" = "gSCS",
          "Benjamini-Hochberg FDR" = "fdr", 
          "Bonferroni correction", "bonferroni")
      ),
      sliderInput(
        "userpval_cutoff",
        "Significant p-value threshold",
        min = 0,
        max = 1,
        value = 0.05,
        step = 0.01
      ), 
      selectInput(
        "chosenscope",
        "Statistical domain scope",
        c("Only annotated genes" = "annotated",
          "All known genes" = "known")
      )
    ),
    do.call(actionBttn, c(          # run button 
      list(
        inputId = "enrichmentgo",
        label = "Enrich",
        icon = icon("play")
      )))
  )),
  
  #result table 
  column(
    9,
    navbarPage(           # result panels
               theme=shinytheme("sandstone"),"Results",
               id = "entabs",
               tabPanel(  # Rmd info panel
                 title = tagList(icon("question"), "Info"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 includeMarkdown("documents/goinfo.Rmd")
               ),
               tabPanel(  # result table panel
                 title = tagList(icon("table"), "Result Table"),
                 value = 'redirectres',
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 uiOutput('EnrichResults')
               ),
               tabPanel(  # bar chart panel 
                 title = tagList(icon("braille"), "Enrichment Distribution"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 uiOutput('EnrichDist',height = 1000)%>% withSpinner()
               ),
               tabPanel(  # bar chart panel 
                 title = tagList(icon("braille"), "Bar Chart"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 uiOutput('EnrichBar',height = 1000)%>% withSpinner()
               )
    ))))