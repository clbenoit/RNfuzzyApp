# Define server 
shinyServer(function(input, output, session) {
  
  source(file = "server-data-import.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-deanalysis.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-volcano.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-ma.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-heatmap.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-pca.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-enrich.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-conversion.R",
         local = TRUE, 
         encoding = "UTF-8")
  
  
  var = reactiveValues(
    count = NULL,
    InputTable = data.frame(),
    LowCountGenes = data.frame(),
    CountData = data.frame(),
    groupList = NULL,
    selectedgroups = NULL,
    result = data.frame("Results will show here." = character(0)),
    tccObject = NULL)
  
})
