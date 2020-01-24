# ui-volcano-plot.R
fluidPage(fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "Volcano Plot Parameters"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("valcanoParameter")
  )
),
column(
  9,
  navbarPage("Results",
      tabPanel(
        title = tagList(icon("area-chart"), "Volcano Plot"),
        solidHeader = TRUE,
        status = "info",
        width = NULL,
        uiOutput("volcanoUI")
      ),
      tabPanel(
        title = tagList(icon("table"), "Result Table"),
        solidHeader = TRUE,
        status = "info",
        width = NULL,
        DT::dataTableOutput('resultTableInVolcanalPlot')
      )
))))