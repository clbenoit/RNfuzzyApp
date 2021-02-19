# ui.R
# Menu of the app, Items with corresponding ui file 


tagList(dashboardPage(                  #the global app
  dashboardHeader(                      #header of the app
    title = span(tagList(icon("react"),"RNApp : a RNA-seq Analysis App")),
    titleWidth = 500),
  dashboardSidebar(                     #side menu and its items
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
        "Time Series clustering",
        tabName = "mfuzzTab",
        icon = icon("line-chart")),
      menuItem(
        "Enrichment",
        tabName = "enrichTab",
        icon = icon("map-signs")),
      menuItem(
        "Orthology",
        tabName = "orthoTab",
        icon = icon("share-alt")),
      menuItem(
        "ID Conversion",
        tabName = "conversionTab",
        icon = icon("sync"))
    )
  ),
  
  
  dashboardBody(                     #content of the app 
    shinyDashboardThemes(            #theme of the app 
      theme = "blue_gradient"),
    tabItems(                        #according menu items to their corresponding files
      tabItem(tabName = "dataImport", source(
        file = "ui-data-import.R",
        local = TRUE,
        encoding = "UTF-8"
      )$value),                     #according menu items to their corresponding files
      tabItem(tabName = "mfuzzTab", source(
        file = "ui-mfuzz.R",
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
      tabItem(tabName = "orthoTab", source(
        file = "ui-ortho.R",
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