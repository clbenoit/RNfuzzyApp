library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(rmarkdown)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(RColorBrewer)
library(utils)
library(tidyr)
library(cluster)
library(TCC)
library(heatmaply)



tagList(dashboardPage(
  dashboardHeader(
    title = "RNApp : a RNA-seq Analysis App",
    titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      id = "sider",
      menuItem(
        "Data visualization",
        tabName = "dateImport",
        icon = icon("receipt")),
      menuItem(
        "DE Analysis",
        icon = icon("flask"),
      menuSubItem(
        "Normalization",
        tabName = "deanalysisTab",
        icon = icon("calculator")),
      menuSubItem(
        "MA Plot",
        tabName = "maplotTab",
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
        icon = icon("bar-chart")))
      )
  ),
  
  
  dashboardBody(
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
      tabItem(tabName = "maplotTab", source(
        file = "ui-ma-plot.R",
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