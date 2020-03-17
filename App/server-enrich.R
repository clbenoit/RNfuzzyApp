#server-enrich.R

EnrichRun <- reactiveValues(EnrichRunValue = FALSE)


observeEvent(input$enrichmentgo,{
  progressSweetAlert(
    session = session,
    id = "enrichProgress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )
  
  updateProgressBar(
    session = session,
    id = "enrichProgress",
    title = "Enrichment in progress...",
    value = 25
  )
  
  geneset <- unlist(strsplit(input$refseqids, split = '\n'))
  for(i in geneset){
    if (startsWith(i,"FBgn") | startsWith(i,"ENS")){
      return()
    }else{
      sendSweetAlert(
        session = session,
        title = "Input data error!",
        text = "Wrong identiers",
        type = "error"
      )
    }
  }
  res <- enrichGO(geneset, 
                  OrgDb = input$chosendataset, 
                  keyType = 'ENSEMBL', 
                  ont = input$chosenGO, 
                  pvalueCutoff = input$pval, 
                  pAdjustMethod = input$pvaladj,
                  qvalueCutoff = 0.2,
                  minGSSize = 10,
                  maxGSSize = 500,
                  readable = FALSE,
                  pool = FALSE
  )
  
  updateProgressBar(
    session = session,
    id = "enrichProgress",
    title = "Enrichement in progress...",
    value = 75
  )
  
  res <- as.data.frame(res)
  output$EnrichResultTable <-  DT::renderDataTable({
    DT::datatable(
      res,
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
                         filename = "enrichment_results"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display")
  },server=F)
  
  EnrichRun$EnrichRunValue <- input$enrichmentgo
  updateNavbarPage(session, "entabs", "redirectres")
  
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "Enrichment was successfully performed.",
                 type = "success")
  
  
  
output$statenrich <- renderPlotly({
  fig <- plot_ly(res, 
                 x = ~(-log(p.adjust)), 
                 y = ~Description, 
                 text = ~paste('GO term:', ID, '<br>Count :', Count, '<br>P-Value :', p.adjust, '<br>Q-Value :', qvalue),
                 type = 'scatter', 
                 mode = 'markers',
                 color = ~ONTOLOGY,
                 colors = "Reds",
                 marker = list(size = ~Count*2, opacity = 1)
                 )%>% layout(title = 'Statistics of the Enrichment',
                             yaxis = list(title = 'Description'),
                             xaxis = list(title = '-log(P-value)')
                             )
                  fig
  
})  
  
  

})

# result table render

output$EnrichResults <- renderUI({
  if(EnrichRun$EnrichRunValue){
    tagList(
      fluidRow(column(
        12, dataTableOutput('EnrichResultTable') %>% withSpinner()
      )))} else {
        helpText("Run Enrichment to obtain the Result Table.")
      }
})




