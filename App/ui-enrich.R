
fluidPage(fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(
      textAreaInput(
        "refseqids",
        "Paste Gene List",
        rows = 5,
        placeholder = "Input refseq ids, one gene per line."
        
      ),
      selectInput(
        "chosenGO",
        "Choose your Enrichment",
        c( "Biological Process" =  "GO_Biological_Process_2018",
           "Molecular Fonction" =  "GO_Molecular_Function_2018",
           "Cellular Component" =  "GO_Cellular_Component_2018")
      ),
      numericInput(
        "topres",
        "Top results",
        min = 1,
        max = 100,
        value = 30,
        step = 1
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
    navbarPage(theme=shinytheme("sandstone"),"Results",
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
                 title = tagList(icon("braille"), "Bar Chart"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 plotlyOutput('barenrich',height = 800)%>% withSpinner()
               )
    ))))