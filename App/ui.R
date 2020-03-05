# ui.R


tagList(dashboardPage(
  dashboardHeader(
    title = span(tagList(icon("react"),"RNApp : a RNA-seq Analysis App")),
    titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      id = "sider",
      menuItem(
        "Data visualization",
        tabName = "dateImport",
        icon = icon("eye")),
      menuItem(
        "DE Analysis",
        icon = icon("flask"),
        menuSubItem(
          "Normalization",
          tabName = "deanalysisTab",
          icon = icon("calculator")),
        menuSubItem(
          "Volcano Plot",
          tabName = "volcanoplotTab",
          icon = icon("area-chart")),
        menuSubItem(
          "Heatmap",
          tabName = "heatmapTab",
          icon = icon("delicious")),
        menuSubItem(
          "PCA",
          tabName = "pcaTab",
          icon = icon("bar-chart")))
    )
  ),
  
  
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"),
    tabItems(
      tabItem(tabName = "dateImport", source(
        file = "ui-data-import.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value),
      tabItem(tabName = "deanalysisTab", source(
        file = "ui-deanalysis.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value),
      tabItem(tabName = "volcanoplotTab", source(
        file = "ui-volcano-plot.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value),
      tabItem(tabName = "heatmapTab", source(
        file = "ui-heatmap.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value),
      tabItem(tabName = "pcaTab", source(
        file = "ui-pca.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value)
    )
  )
))