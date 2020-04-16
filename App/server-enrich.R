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
  
  
  
  geneset <- unlist(strsplit(input$refseqids, split = '\n'))
  
  updateProgressBar(
    session = session,
    id = "enrichProgress",
    title = "Enrichment in progress...",
    value = 50
  )
  
  
  res <- listEnrichrDbs()
  res <- input$chosenGO
  enriched <- enrichr(geneset, res)
  
  
  
  
  updateProgressBar(
    session = session,
    id = "enrichProgress",
    title = "Enrichement in progress...",
    value = 75
  )
  
  res_enrich <- as.data.frame(enriched)
  res_enrich <- res_enrich[,-4]
  res_enrich <- res_enrich[,-4]
  res_enrich <- res_enrich[,-4]
  colnames(res_enrich) <- c("Term","Overlap","P.value","Odd.Ratio","Combined.Score","Genes")
  res_enrich <- res_enrich[order(res_enrich[,3]),]
  n <- as.numeric(input$topres)
  res_enrich <- res_enrich[1:n,]
  output$EnrichResultTable <-  DT::renderDataTable({
    DT::datatable(
      res_enrich,        
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
                         filename = "results_enrichment"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  EnrichRun$EnrichRunValue <- input$enrichmentgo
  updateNavbarPage(session, "entabs", "redirectres")
  
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "Enrichment was successfully performed.",
                 type = "success")
  
  
  
  output$barenrich <- renderPlotly({
    fig <- plot_ly(
      res_enrich,
      x = ~(-log(P.value)),
      y = ~reorder(Term,(-log(P.value))),
      text = ~Term, 
      textposition = 'auto',
      type = "bar",
      colors = "Reds"
    )%>% layout(title = 'Statistics of the Enrichment',
                yaxis = list(title = 'Enrichment'),
                xaxis = list(title = '-log(P-value)'))
    
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




