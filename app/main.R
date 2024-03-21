box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
)

box::use(
    app/view/pca,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    pca$ui(ns("pca"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    pca$server("pca")
    
  })
}
