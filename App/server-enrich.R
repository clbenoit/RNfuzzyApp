#server-enrich.R

EnrichRun <- reactiveValues(EnrichRunValue = FALSE) # to precise the run button has not been clicked


observeEvent(input$enrichmentgo,{  # when the button is clicked 
  progressSweetAlert(              # progress bar 
    session = session,
    id = "enrichProgress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )
  
  
  
  geneset <- unlist(strsplit(input$refseqids, split = '\n')) # takes the gene set 
  
  updateProgressBar(              # update progress bar 
    session = session,
    id = "enrichProgress",
    title = "Enrichment in progress...",
    value = 50
  )
  
  
  res <- listEnrichrDbs()       # look up to available databases on Rnchir
  res <- input$chosenGO         # chosen ontology to perform the enrichment 
  enriched <- enrichr(geneset, res)  # the enrichment 
  
  
  
  
  updateProgressBar(           # updating the progress bar 
    session = session,
    id = "enrichProgress",
    title = "Enrichement in progress...",
    value = 75
  )
  
  res_enrich <- as.data.frame(enriched)   # result as data frame
  res_enrich <- res_enrich[,-4]           # deleting useless columns to keep only the ones below
  res_enrich <- res_enrich[,-4]
  res_enrich <- res_enrich[,-4]
  colnames(res_enrich) <- c("Term","Overlap","P.value","Odd.Ratio","Combined.Score","Genes")
  res_enrich <- res_enrich[order(res_enrich[,3]),]   # class the results according to the pvalue
  n <- as.numeric(input$topres)
  res_enrich <- res_enrich[1:n,]
  
  
  output$EnrichResultTable <-  DT::renderDataTable({   # result table
    DT::datatable(
      res_enrich,        
      extensions = 'Buttons',    # download button 
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
        searchHighlight = TRUE,     # search bar 
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  EnrichRun$EnrichRunValue <- input$enrichmentgo   # precise the run button has been clicked
  updateNavbarPage(session, "entabs", "redirectres") # redirection to the result table after the enrichment is done
  
  closeSweetAlert(session = session)        # close alert that the enrichment is done 
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "Enrichment was successfully performed.",
                 type = "success")
  
  
  
  output$barenrich <- renderPlotly({     # bar chart of results using plotly  : term with respect of -log(p-value)
    fig <- plot_ly(
      res_enrich,
      x = ~(-log(P.value)),   
      y = ~reorder(Term,(-log(P.value))),
      textposition = 'auto',
      type = "bar",    # bar chart
      colors = "Reds",  # colors
      hoverinfo = "text",  # when hover over a point, the following info shows
      text = ~ paste(
        "</br>Term:",
        Term,
        "</br>Genes:",
        Genes)
    )%>% layout(title = 'Statistics of the Enrichment',  # titles
                yaxis = list(title = 'Enrichment'),
                xaxis = list(title = '-log(P-value)'))
    
    fig
  })
  
  
})

# result table render

output$EnrichResults <- renderUI({
  if(EnrichRun$EnrichRunValue){   # if the run button has been clicked, then show the results
    tagList(
      fluidRow(column(
        12, dataTableOutput('EnrichResultTable') %>% withSpinner()
      )))} else {                 # if not message to do it 
        helpText("Run Enrichment to obtain the Result Table.")
      }
})




