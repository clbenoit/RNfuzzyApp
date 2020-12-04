# global.R
#libraries needed : installing automatically if not


# Packages
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}


# BioCmanager packages
BioCPackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    BiocManager::install(p, dep = TRUE)
  require(p, character.only = TRUE)
}


#theme 
if (!is.element("dashboardthemes", installed.packages()[,1]))
  install_github("nik01010/dashboardthemes")
require("dashboardthemes", character.only = TRUE)


usePackage("shiny")
usePackage("shinydashboard")
usePackage("shinycssloaders")
usePackage("shinythemes")
usePackage("shinyWidgets")
usePackage("shinyBS")
usePackage("rmarkdown")
usePackage("plotly")
usePackage("dplyr")
usePackage("DT")
usePackage("data.table")
usePackage("RColorBrewer")
usePackage("utils")
usePackage("tidyr")
usePackage("devtools")

BioCPackage("cluster")
BioCPackage("TCC")
BioCPackage("heatmaply")
BioCPackage("gprofiler2")
BioCPackage("Mfuzz")
BioCPackage("e1071")




