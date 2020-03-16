# Define server 
shinyServer(function(input, output, session) {
  
  source(file = "server-data-import.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-deanalysis.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-volcano-plot.R",
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
    count.data = NULL,
    CountData = data.frame(),
    groupList = NULL,
    groupListConvert = NULL,
    result = data.frame("Results will show here." = character(0)),
    tccObject = NULL)
  
})
