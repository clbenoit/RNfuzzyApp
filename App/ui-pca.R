# ui-pca.R

fluidPage(column(
  3,
  box(
    title = tagList(icon("cogs"), "PCA Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("pcaParameter")
  )
),
column(
  9,
  navbarPage("Results",
    tabPanel(tagList(icon("square-o"), "PCA Plot (2D)"),
             uiOutput("pca2dPlotUI")),
    tabPanel(tagList(icon("cube"), "PCA Plot (3D)"),
             uiOutput("pca3dPlotUI"))

  )
))