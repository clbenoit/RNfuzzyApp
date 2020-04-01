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
        tabName = "dataImport",
        icon = icon("eye")),
      menuItem(
        "DE Analysis",
        icon = icon("flask"),
        menuSubItem(
          "Normalization & Analysis",
          tabName = "deanalysisTab",
          icon = icon("calculator")),
        menuSubItem(
          "MA Plot",
          tabName = "maTab",
          icon = icon("line-chart")),
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
          icon = icon("bar-chart"))),
      menuItem(
        "GO Enrichment",
        tabName = "enrichTab",
        icon = icon("project-diagram")),
      menuItem(
        "ID Conversion",
        tabName = "conversionTab",
        icon = icon("sync"))
    )
  ),
  
  
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"),
    tabItems(
      tabItem(tabName = "dataImport", source(
        file = "ui-data-import.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value),
      tabItem(tabName = "deanalysisTab", source(
        file = "ui-deanalysis.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value),
      tabItem(tabName = "maTab", source(
        file = "ui-ma.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value),
      tabItem(tabName = "volcanoplotTab", source(
        file = "ui-volcano.R",
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
      )$value),
      tabItem(tabName = "enrichTab", source(
        file = "ui-enrich.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value),
      tabItem(tabName = "conversionTab", source(
        file = "ui-conversion.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value)
    )
  )
))