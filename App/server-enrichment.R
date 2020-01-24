#parameters
observeEvent(input$sider, {
  if (input$sider == "enrichTab") {
    output$EnrichParameter <- renderUI({
      tagList(
        selectInput("deaanalysisselect", 
                    "Select the analysis", 
                    c("WikiPathways analysis",
                      "MSigDb analysis",
                      "Gene Ontology Analysis",
                      "KEGG Analysis",
                      "Disease Ontology Analysis"), 
                    multiple = FALSE),
        selectInput("gotype", 
                    "Select collection for Molecular Signatures Database", 
                    c("Molecular Function"="MF",
                      "Cellular Component"="CC",
                      "Biological Process" = "BP"), 
                    multiple = FALSE),
        do.call(actionBttn, c(
          list(
            inputId = "MakeEnrichAnalysis",
            label = "Perform Erichment Analysis",
            icon = icon("play")
          )
        )))
    })
    
    output$EnrichReport <-renderUI({
      tagList(
        do.call(actionBttn, c(
          list(
            inputId = "EnrichmentReport",
            label = "Download report",
            icon = icon("download")
          )
        ))
        
      )
    })}})