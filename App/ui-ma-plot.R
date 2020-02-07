# ui-ma-plot.R

fluidPage(fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "MA Plot Parameters"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("MAPlotParameter")
  )
),
column(
  9,
  navbarPage("Results",
    tabPanel(
        title = tagList(icon("line-chart"), "MA Plot"),
        solidHeader = TRUE,
        status = "info",
        width = NULL,
        uiOutput("MAPlotUI")
      ),
      tabPanel(
        title = tagList(icon("table"), "Result Table"),
        solidHeader = TRUE,
        status = "info",
        width = NULL,
        tagList(DT::dataTableOutput('resultTableMA'))
      ))
)))