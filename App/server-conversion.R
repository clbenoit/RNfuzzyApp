#server-conversion.R

ConvRun <- reactiveValues(ConvRunValue = FALSE)


observeEvent(input$convgo,{
  
  
  
  inputids <- unlist(strsplit(input$inputids, split = '\n'))
  conversion <- bitr(
    geneID = inputids, 
    fromType = input$inputtype,
    toType = c('ENTREZID','ENSEMBL','SYMBOL'),
    OrgDb = input$chosendatabase
  )
  
  
  
  conversion <- as.data.frame(conversion)
  output$ConvResults <-  DT::renderDataTable({
    DT::datatable(
      conversion,
      filter = "bottom",
      extensions = 'Buttons',
      option = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = list(list(
          extend = 'collection',
          buttons = list(extend='csv',
                         filename = "conversion_results"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display")
  },server=F)
  
  ConvRun$ConvRunValue <- input$convgo
  updateNavbarPage(session, "convtabs", "redirectconv")
  
  
})  

# result table render

output$ConversionResults <- renderUI({
  if(ConvRun$ConvRunValue){
    tagList(
      fluidRow(column(
        12, dataTableOutput('ConvResults') %>% withSpinner()
      )))} else {
        helpText("Run Conversion to obtain the Result Table.")
      }
})
