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

#theme 
if (!is.element("dashboardthemes", installed.packages()[,1]))
  install_github("nik01010/dashboardthemes")
require("dashboardthemes", character.only = TRUE)


BioCPackage("cluster")
BioCPackage("TCC")
BioCPackage("heatmaply")
BioCPackage("enrichR")
BioCPackage("clusterProfiler")
BioCPackage("org.Dm.eg.db")
BioCPackage("org.EcK12.eg.db")
BioCPackage("org.Ce.eg.db")
BioCPackage("org.Hs.eg.db")
BioCPackage("org.Mm.eg.db")



