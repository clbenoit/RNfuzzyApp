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
  
  
  
  geneset <-unlist(strsplit(input$list_ids, split = '\n')) # takes the gene set
  
  updateProgressBar(              # update progress bar 
    session = session,
    id = "enrichProgress",
    title = "Enrichment in progress...",
    value = 50
  )
  
  enrichement <- unlist(strsplit(input$chosenEnrich, split = '\n'))
  res <- gost(geneset, 
              organism = input$inputorg,
              ordered_query = T,
              user_threshold = input$userpval_cutoff,
              correction_method = input$correction,
              domain_scope = input$chosenscope, 
              sources = enrichement,
              significant = F)
  
  
  
  
  updateProgressBar(           # updating the progress bar 
    session = session,
    id = "enrichProgress",
    title = "Enrichement in progress...",
    value = 75
  )
  
  res_enrich <- as.data.frame(res$result)# result as data frame
  res_enrich <- res_enrich[,-1]
  res_enrich <- res_enrich[1:as.numeric(input$topres_enrich),]
  
  
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
  
  ##########
  
  output$distribenrich <- renderPlotly({     # bar chart of results using plotly  : term with respect of -log(p-value)
    p <- gostplot(res, capped = FALSE, interactive = TRUE
                  )
    
    p
  })
  
  
  output$EnrichBarPlot <- renderPlotly({
    fig <- plot_ly(
      res_enrich,
      x = ~p_value,
      y = ~term_name,
      type = "bar",
      color = ~factor(source)
      
    )
    fig
  })
  
})

# result table render

output$EnrichResults <- renderUI({
  if(EnrichRun$EnrichRunValue){   # if the run button has been clicked, then show the results
    tagList(
      fluidRow(
        column(12, dataTableOutput('EnrichResultTable') %>% withSpinner())
      ))
    } else {                 # if not message to do it 
        helpText("Run Enrichment to obtain the Result Table.")
      }
})

output$EnrichDist <- renderUI({
  if(EnrichRun$EnrichRunValue){
    tagList(
      fluidRow(
        column(12, plotlyOutput("distribenrich") %>% withSpinner())
      )
    )
  }else{
    helpText(("Run Enrichment to obtain the Result Table."))
  }
})

output$EnrichBar <- renderUI({
  if(EnrichRun$EnrichRunValue){
    tagList(
      fluidRow(
        column(12, plotlyOutput("EnrichBarPlot") %>% withSpinner())
      )
    )
  }else{
    helpText(("Run Enrichment to obtain the Result Table."))
  }
})



