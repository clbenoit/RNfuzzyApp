# ui-ma-plot.R

fluidPage(fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "MA Plot Parameters"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("CondMAPlotParams")
  ),
),
column(
  9,
  navbarPage("Results",
             tabPanel(
               title = tagList(icon("line-chart"), "MA plot"),
               width = NULL,
               solidHeader = TRUE,
               status = "info", 
               uiOutput("MAPlotUI"),
             ),
             tabPanel(
               title = tagList(icon("table"), "Result table"),
               width = NULL,
               solidHeader = TRUE,
               status = "info", 
               DT::dataTableOutput("resultTableInPlot")
             )
))))