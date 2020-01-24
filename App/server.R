# Define server
shinyServer(function(input, output, session) {
  variables = reactiveValues(
    count.data = NULL,
    CountData = data.frame(),
    groupList = NULL,
    groupListConvert = NULL,
    result = data.frame("Results will show here." = character(0)),
    tccObject = NULL,
    zeroValue = "",
    norData = "",
    runMAPlot = "",
    MAPlotObject = "",
    VolcanoPlotObject = "",
    runVolcanoPlot = "",

      
    data.pca = NULL,
    pcaParameter = NULL,
    pca2d = NULL,
    pca3d = NULL,

    
    logList = data.frame(
      "Time" = vector(),
      "Type" = vector(),
      "Action" = vector(),
      "Parameters" = vector()
    ))
  
  
  source(file = "server-data-import.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-normalization.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-ma-plot.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-volcano-plot.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-pca.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-heatmap.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-enrichment.R",
         local = TRUE,
         encoding = "UTF-8")
  
  
})
