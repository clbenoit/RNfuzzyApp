box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
)

box::use(
    app/view/data_import,
    app/view/dea,
    app/view/pca,
    app/logic/dataManager[DataManager],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    data_import$ui(ns("data_import")),
    dea$ui(ns("dea")),
    pca$ui(ns("pca"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    DataManager <- DataManager$new()
    data_import$server("data_import", data = DataManager)
    dea$server("dea", data = DataManager)
    pca$server("pca", data = DataManager)
    
    
  })
}
