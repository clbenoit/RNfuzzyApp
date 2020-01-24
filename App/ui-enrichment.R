fluidPage(fluidRow(column(
  6,
  box(
    title = tagList(icon("cogs"), "Enrichment Parameters"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("EnrichParameter")
  )
),
column(
  6,
  box(
    title = tagList(icon("file-code"), "Generate Report"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("EnrichReport"),
    helpText("This button doesn't work. Whole enrichment analysis also.")
))))