# ui-heatmap.R

fluidPage(fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "Heatmap Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
  uiOutput("heatmapParameter"))),
column(
  9,
  navbarPage("Results",
    tabPanel(
      title = tagList(icon("delicious"), "Heatmap"),
      width = NULL,
      solidHeader = TRUE,
      status = "info", 
      uiOutput("heatmapPlot"),
      footer = "This part is time consuming, please wait patiently.",
    )
))))