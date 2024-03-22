box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
)

box::use(
    app/view/data_import,
    app/view/data_filter,
    app/view/dea,
    app/view/pca,
    app/logic/dataManager[DataManager],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    data_import$ui(ns("data_import")),
    data_filter$ui(ns("data_filter")),
    dea$ui(ns("dea")),
    pca$ui(ns("pca"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    options(shiny.trace = TRUE)
    DataManager <- DataManager$new()
    data_import$server("data_import", data = DataManager)
    data_filter$server("data_filter", data = DataManager)
    dea$server("dea", data = DataManager)
    pca$server("pca", data = DataManager)
    
    
  })
}
