# ui-normalization.R


# parameters

fluidPage(useSweetAlert(), fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("CondDEAParams")
    )),
  
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