# server-volcano-plot.R

runVolcano <- reactiveValues(runVolcanoValue = FALSE)




observeEvent(input$sider, {
  if (input$sider == "volcanoplotTab") {
      output$valcanoParameter <- renderUI({
        tagList(
          textInput("graphicTitle", "Graphic Title", value = "Volcano Plot"),
          textInput("xlabs", "X-axis Label", value = "log<sub>2</sub>(Fold Change)"),
          textInput("ylabs", "Y-axis Label", value = "-log<sub>10</sub>(P-value)"),
          sliderInput(
            "CutFC",
            "Fold Change (X-axis) Cut-off",
            min = ceiling(min(resultTable()$m.value)),
            max = floor(max(resultTable()$m.value)),
            value = c(-2, 2),
            step = 0.5
          ),
          sliderInput(
            inputId = "Cutpvalue",
            label = "P-value Cut-off",
            min = 0.00001,
            value = 0.05,
            max = 1,
            step = 0.00001
          ),
          sliderInput(
            "volcanoPointSize",
            "Point Size",
            min = 1,
            max = 5,
            value = 3,
            step = 0.2
          ),
          spectrumInput(
            inputId = "downColor",
            label = tagList("Down-regulated in G2", htmlOutput("downPreview")),
          choices = list(
            list(
              "red",
              'black',
              'white',
              'blanchedalmond',
              'steelblue',
              'forestgreen'
            ),
            as.list(brewer.pal(n = 9, name = "Oranges")),
            as.list(brewer.pal(n = 9, name = "Reds")),
            as.list(brewer.pal(n = 11, name = "Spectral"))
          ),
          options = list(`toggle-palette-more-text` = "Show more")
        ),
          spectrumInput(
            inputId = "upColor",
            label = tagList("Up-regulated in G2", htmlOutput("upPreview")),
            choices = list(
              list(
                "green",
                'black',
                'white',
                'blanchedalmond',
                'steelblue',
                'forestgreen'
              ),
              as.list(brewer.pal(n = 9, name = "Blues")),
              as.list(brewer.pal(n = 9, name = "Greens")),
              as.list(brewer.pal(n = 11, name = "Spectral"))
            ),
            options = list(`toggle-palette-more-text` = "Show more")
            
          ),
          do.call(actionBttn, c(
            list(
              inputId = "makeVolcanoPlot",
              label = "Generate Volcano Plot",
              icon = icon("play")
            )
          ))
        )
      })
  }
})

# Preview up and down regulated genes under Color selection 
observeEvent({
  input$CutFC
  input$Cutpvalue
}, {
  dt <- resultTable()
  downCut <- input$CutFC[1]
  upCut <- input$CutFC[2]
  pvalueCut <- input$Cutpvalue
  
  downCount <-
    nrow(dt[dt$m.value <= downCut & dt[["p.value"]] <= pvalueCut,])
  upCount <-
    nrow(dt[dt$m.value >= upCut & dt[["p.value"]] <= pvalueCut,])
  
  output$downPreview <- renderText({
    paste0("<font color=\"",
           input$downColor,
           "\"><b>",
           downCount,
           " genes</b></font>")
  })
  output$upPreview <- renderText({
    paste0("<font color=\"",
           input$upColor,
           "\"><b>",
           upCount,
           " genes</b></font>")
  })
})

# Check the `Generate` button, if the botton has been clicked, generate volcano plot 

observeEvent(input$makeVolcanoPlot, {
  yaxis <- "p.value"
  output$volcanoPloty <- renderPlotly({
    validate(need(resultTable()[[yaxis]] != "", "No p-values for ploting."))
    req(input$makeVolcanoPlot)
    isolate({
      dt <- resultTable()
      downCut <- input$CutFC[1]
      upCut <- input$CutFC[2]
      
      dt$color <- "None"
      tryCatch({
        dt[dt$m.value <= downCut,]$color <- "Down"
        dt[dt$m.value >= upCut,]$color <- "Up"
        dt[dt[[yaxis]] > input$Cutpvalue,]$color <-
          "None"
      }, error = function(e) {
        sendSweetAlert(session = session, title = "ERROR", text = "No data was satisfied to your cut-off!")
      })
      
      x <- factor(dt$color)
      levels(x) <- list("Down" = 0,
                        "None" = 1,
                        "Up" = 2)
      
      # link to bar plot
      key <- resultTable()$gene_id
      
      if (is.null(input$resultTableInVolcanalPlot_rows_selected)) {
        annotation <- list()
      } else {
        markerSelect <- dt[input$resultTableInVolcanalPlot_rows_selected, ]
        
        annotation <- list(
          x = markerSelect$m.value,
          y = -log10(markerSelect[[yaxis]]),
          text = markerSelect$gene_id,
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = 20,
          ay = 40
        )
      }
      
      p <- plot_ly(
        data = dt,
        x = ~ m.value,
        y = ~ -log10(dt[[yaxis]]),
        type = "scatter",
        mode = "markers",
        color = ~ x,
        colors = c(input$downColor, "black", input$upColor),
        marker = list(size = input$volcanoPointSize),
        hoverinfo = "text",
        text = ~ paste(
          "</br>Gene:",
          resultTable()$gene_id,
          "</br>Log2FC:",
          m.value,
          "</br>p-value:",
          p.value,
          "</br>q-value:",
          q.value,
          "</br>Rank:",
          rank
        ),
        key =  ~ key,
        source = "volcano"
      ) %>%
        layout(
          xaxis = list(title = input$xlabs),
          yaxis = list(title = input$ylabs),
          title = input$graphicTitle,
          legend = list(
            orientation = 'h',
            xanchor = "center",
            x = 0.5,
            y = 1.05
          ),
          annotations = annotation,
          shapes = list(
            list(
              type = 'line',
              y0 =  ~ min(-log10(dt[[yaxis]])),
              y1 =  ~ max(-log10(dt[[yaxis]])),
              x0 = upCut,
              x1 = upCut,
              line = list(dash = 'dot', width = 2)
            ),
            list(
              type = 'line',
              y0 =  ~ min(-log10(dt[[yaxis]])),
              y1 =  ~ max(-log10(dt[[yaxis]])),
              x0 = downCut,
              x1 = downCut,
              line = list(dash = 'dot', width = 2)
            ),
            list(
              type = 'line',
              y0 = -log10(input$Cutpvalue),
              y1 = -log10(input$Cutpvalue),
              x0 =  ~ min(m.value),
              x1 =  ~ max(m.value),
              line = list(dash = 'dot', width = 2)
            )
          )
        )
      var$VolcanoPlotObject <- p
      p
    })
  })
  runVolcano$runVolcanoValue <- input$makeVolcanoPlot
})


# render barplot next to volcanoplot

output$VolcanoBarPlot <- renderPlotly({
  eventdata <- event_data("plotly_hover", source = "volcano")
  validate(need(
    !is.null(eventdata),
    "Hover over the point to show gene's expression level of interest."
  ))

  gene_id <- eventdata$key

  expression <-
    var$CountData[row.names(var$CountData) == gene_id,]

  expressionNor <-
    t(t(var$norData[row.names(var$norData) == gene_id,]))
  
  data <- var$CountData
  data.list <- var$groupListConvert
  
  expression <- t(expression[data.list != 0])
  data.list <- data.list[data.list != 0]
  
  xOrder <-
    data.frame("name" = row.names(expression), "group" = data.list)
  xOrderVector <- unique(xOrder[order(xOrder$group),]$name)
  xform <- list(categoryorder = "array",
                categoryarray = xOrderVector,
                title = "")
  
  plot_ly(
    x = ~ row.names(expression),
    y = ~ expression[, 1],
    color = as.factor(data.list),
    text = expression[, 1],
    textposition = 'outside',
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


# volcanoUI 
output$volcanoUI <- renderUI({
  if (length(var$groupList) > 2) {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Volcano Plot is unavailable for multiple comparison now.",
      type = "info"
    )
    helpText("Volcano Plot is unavailable for multiple comparison now.")
  }else{
  
  if(runVolcano$runVolcanoValue){
    tagList(
      fluidRow(
        column(8, plotlyOutput("volcanoPloty") %>% withSpinner()),
        column(4, plotlyOutput("VolcanoBarPlot") %>% withSpinner())
      )
    )
  } else {
    helpText("Please click [Generate Volcano Plot] first.")
  }}
})
# Render table result

output$resultTableVolc <- DT::renderDataTable({
  if (nrow(resultTable()) == 0) {
    DT::datatable(resultTable())
  } else {
    if (length(input$Cutpvalue) > 0) {
      fdrCut <- input$Cutpvalue
      
    } else {
      fdrCut <- 0
    }
    
    data <- var$norData
    gene_id <- row.names(data)
    data <- cbind(data, gene_id = gene_id)
    
    resultTable <- merge(resultTable(), data, by = "gene_id")
    
    t <- DT::datatable(
      resultTable,
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
                         filename = "volcano_results"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      class = "display",
      caption = tags$caption(
        tags$li(
          "Gene Name was colored according to Fold Change and set bold according to P-value cut-off."
        )
      ))
    
    if (!is.na(sum(resultTable()$m.value))) {
      t   %>% formatStyle("gene_id", "m.value",
                          color = styleInterval(input$CutFC,
                                                c(
                                                  input$downColor, "black", input$upColor
                                                ))) %>% formatStyle("gene_id",
                                                                    "p.value",
                                                                    fontWeight = styleInterval(fdrCut, c("bold", "normal")))
    } else {
      t
    }
  }
},server = F)


# final render 
output$MainResultTableVolc <- renderUI({
  if(runVolcano$runVolcanoValue){
    tagList(fluidRow(column(
      12, DT::dataTableOutput('resultTableVolc') %>% withSpinner()
    )))} else {
      helpText("Run Volcano to obtain Result Table.")
    }
})

############################################################DOWN REGULATED##################################

output$resultTabledown <- DT::renderDataTable({
  sortedvolc <- var$result
  if (nrow(sortedvolc) == 0) {
    DT::datatable(sortedvolc)
  } else {
    if (length(input$Cutpvalue) > 0) {
      downCut <- input$CutFC[1]
      upCut <- input$CutFC[2]
      fdrCut <- input$Cutpvalue
      sortedvolc <- sortedvolc[sortedvolc$p.value < fdrCut,]
      sortedvolc <- sortedvolc[sortedvolc$m.value < downCut,]
      
    } else {
      fdrCut <- 0
    }
    data <- var$norData
    gene_id <- row.names(data)
    data <- cbind(data, gene_id = gene_id)
    
    downresultTable <- merge(sortedvolc, data, by = "gene_id")
    
    t <- DT::datatable(
      downresultTable,
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
                         filename = "downregulated_genes"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display",
    caption = tags$caption(
      tags$li(
        "Please verify your Log2FC if changed from the default one."
      )
    ))
    
    if (!is.na(sum(resultTable()$m.value))) {
      t   %>% formatStyle("gene_id", "m.value",
                          color = styleInterval(input$CutFC,
                                                c(
                                                  input$downColor, "black", input$upColor
                                                ))) %>% formatStyle("gene_id",
                                                                    "p.value",
                                                                    fontWeight = styleInterval(fdrCut, c("bold", "normal")))
    } else {
      t
    }
  }
},server = F)


##########################################################UPREGULATED######################################

output$resultTableup <- DT::renderDataTable({
  sortedvolc <- var$result
  if (nrow(sortedvolc) == 0) {
    DT::datatable(sortedvolc)
  } else {
    if (length(input$Cutpvalue) > 0) {
      downCut <- input$CutFC[1]
      upCut <- input$CutFC[2]
      fdrCut <- input$Cutpvalue
      sortedvolc <- sortedvolc[sortedvolc$p.value < fdrCut,]
      sortedvolc <- sortedvolc[sortedvolc$m.value > upCut,]
      
    } else {
      fdrCut <- 0
    }
    data <- var$norData
    gene_id <- row.names(data)
    data <- cbind(data, gene_id = gene_id)
    
    upresultTable <- merge(sortedvolc, data, by = "gene_id")
    
    t <- DT::datatable(
      upresultTable,
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
                         filename = "upregulated_genes"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display",
    caption = tags$caption(
      tags$li(
        "Please verify your Log2FC if changed from the default one."
      )))
    
    if (!is.na(sum(resultTable()$m.value))) {
      t   %>% formatStyle("gene_id", "m.value",
                          color = styleInterval(input$CutFC,
                                                c(
                                                  input$downColor, "black", input$upColor
                                                ))) %>% formatStyle("gene_id",
                                                                    "p.value",
                                                                    fontWeight = styleInterval(fdrCut, c("bold", "normal")))
    } else {
      t
    }
  }
},server = F)