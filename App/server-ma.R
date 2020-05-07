# server-ma-plot.R

runMA <- reactiveValues(runMAValues = FALSE)


output$CondMAPlotParams <- renderUI({
  if (AnalysisRun$AnalysisRunValue){
    uiOutput("MAPlotParams")
  }else{
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "You must perform a DEA before.",
      type = "info"
    )
    helpText("Please perform a DEA first.")
  }
  
  
})


observeEvent(input$sider, {
  output$MAPlotParams <- renderUI({
    tagList(
      sliderInput(
        "pointSize",
        "Point Size",
        min = 1,
        max = 5,
        value = 3,
        step = 0.2
      ),
      numericInput(
        inputId = "maFDR",
        label = "FDR Cut-off",
        min = 0.00001,
        value = input$fdr,
        max = 0.01,
        step = 0.0001
      ),
      spectrumInput(
        inputId = "fdrColor",
        label = tagList("DEGs Color", htmlOutput("maFDRpreview")),
        choices = list(
          list(
            "#B22222",
            'black',
            'white',
            'blanchedalmond',
            'steelblue',
            'forestgreen'
          ),
          as.list(brewer.pal(n = 9, name = "Reds")),
          as.list(brewer.pal(n = 9, name = "Greens")),
          as.list(brewer.pal(n = 11, name = "Spectral")),
          as.list(brewer.pal(n = 8, name = "Dark2"))
        ),
        options = list(`toggle-palette-more-text` = "Show more")
      ),
      do.call(actionBttn,c(
        list(
          inputId = "makeMAPlot",
          label = "Generate MA Plot",
          icon = icon("play")
        ))
      )
    )
  })
})



observeEvent(input$makeMAPlot, {
  output$maplotly <- renderPlotly({
    validate(need(resultTable()$a.value != "", "No MA values for ploting."))
    
    isolate({
      key <- resultTable()$gene_id
      DEGcut <- cut(resultTable()$q.value, breaks = c(0, input$maFDR, 1))
      
      p <- plot_ly(
        data = resultTable(),
        x = ~ a.value,
        y = ~ m.value,
        type = "scatter",
        mode = "markers",
        color = ~ DEGcut,
        colors = c(input$fdrColor, "#000000"),
        marker = list(size = input$pointSize),
        hoverinfo = "text+name",
        text = ~ paste(
          "</br>Gene:",
          resultTable()$gene_id,
          "</br>BaseMean (A value):",
          round(a.value, 4),
          "</br>Log2FC (M value):",
          round(m.value, 4),
          "</br>Rank:",
          rank
        ),
        key =  ~ key,
        source = "ma"
      ) %>%
        layout(
          xaxis = list(title = "BaseMean (A) = (log<sub>2</sub>(G2)+log<sub>2</sub>(G1))/2"),
          yaxis = list(title = "Log2FC (M) = log<sub>2</sub>(G2)-log<sub>2</sub>(G1)"),
          title = paste0(
            "MA Plot with q-value < ",
            input$maFDR
          ),
          legend = list(
            orientation = 'h',
            xanchor = "center",
            x = 0.5,
            y = 1.05
          )
        )
      p
    })
  })
  runMA$runMAValues <- input$makeMAPlot
})



output$geneBarPlot <- renderPlotly({
  eventdata <- event_data("plotly_hover", source = "ma")
  validate(need(
    !is.null(eventdata),
    "Hover a gene to show its expression."
  ))
  
  gene_id <- eventdata$key
  expression <-
    var$CountData[row.names(var$CountData) == gene_id, ]
  data <- var$CountData
  dataGroups <- var$selectedgroups
  expression <- t(expression[dataGroups != 0])
  
  xOrder <-
    data.frame("name" = row.names(expression), "group" = dataGroups)
  xOrderVector <- unique(xOrder[order(xOrder$group),]$name)
  xform <- list(categoryorder = "array",
                categoryarray = xOrderVector,
                title = "")
  
  plot_ly(
    x = ~ row.names(expression),
    y = ~ expression[, 1],
    color = as.factor(dataGroups),
    text = expression[, 1],
    textposition = "outside",
    showlegend = FALSE,
    type = "bar",
    name = "Raw"
  ) %>%
    layout(
      xaxis = xform,
      yaxis = list(title = "Raw Count"),
      title = colnames(expression)
    )
})



output$resultTableInPlot <- DT::renderDataTable({
  if (nrow(resultTable()) == 0) {
    DT::datatable(resultTable())
  } else {
    if (length(input$maFDR) > 0) {
      fdrCut <- input$maFDR
      fdrColor <- input$fdrColor
    } else {
      fdrCut <- 0
      fdrColor <- "#B22222"
    }
    
    DT::datatable(
      resultTable(),
      colnames = c(
        "Gene Name",
        "BaseMean (A Value)",
        "Log2FC (M Value)",
        "P Value",
        "FDR",
        "Rank",
        "estimated DEG"
      ),
      filter = "bottom",
      caption = tags$caption(
        tags$li("Gene Name was colored according to FDR cut-off.")
      ),
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
                         filename = "results_maplot"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
      )) %>% formatRound(
        columns = c("a.value",
                    "m.value",
                    "p.value",
                    "q.value"),
        digits = 3
      ) %>% formatStyle("gene_id",
                        "q.value",
                        color = styleInterval(fdrCut, c(fdrColor, "")))
  }
},server = F)


# Under FDR cutoff, preview the gene number ----
output$maFDRpreview <- renderText({
  count <- nrow(resultTable()[resultTable()$q.value <= input$maFDR,])
  paste0("<font color=\"",
         input$fdrColor,
         "\"><b>",
         count,
         " genes</b></font>")
})

#main  plot output 
output$MAPlotUI <- renderUI({
  if (length(var$groupList) > 2) {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "MA Plot is unavailable for multiple comparison now.",
      type = "info"
    )
    helpText("MA Plot is unavailable for multiple comparison now.")
  }else{
    if (runMA$runMAValues) {
      tagList(fluidRow(
        column(8, plotlyOutput("maplotly") %>% withSpinner()),
        column(4, plotlyOutput("geneBarPlot") %>% withSpinner())
      ))
    } else {
      helpText("Please click [Generate MA Plot] first.")
    }}
})



observeEvent(input$makeMAPlot, {
  output$runMAPlot <- renderText({
    if (resultTable()$a.value == "") {
      "No MA values for plotting."
    }
  })
})

